package com.yuuki14202028

import cats.syntax.all._
import cats.data.{ReaderT, StateT}

object TAnalyser {

  private type EitherS[A] = Either[String, A]
  private type Check[A] = ReaderT[EitherS, Env, A]
  private type TC[I] = Check[TypeRec[I]]

  private def fail[A](msg: String): Check[A] = ReaderT.liftF(Left(msg))
  private def guard(cond: Boolean, msg: => String): Check[Unit] = ReaderT.liftF(Either.cond(cond, (), msg))
  private def lift[A](e: EitherS[A]): Check[A] = ReaderT.liftF(e)
  private val ask: Check[Env] = ReaderT.ask[EitherS, Env]
  private def okT[I](t: TypeRec[I]): TC[I] = ReaderT.pure(t)

  private def expect(expected: TypeRec[Type], actual: TypeRec[Type]): Check[Unit] =
    guard(Equivalence.beta(expected, actual), s"Type mismatch: expected ${expected.show}, actual ${actual.show}")

  private def expectNumeric(actual: TypeRec[Type]): Check[Unit] =
    guard(isNumericType(actual), s"Type mismatch: expected numeric, actual ${actual.show}")

  private def expectEquatable(actual: TypeRec[Type]): Check[Unit] =
    guard(isEquatableType(actual), s"Type mismatch: expected numeric, char, or bool, actual ${actual.show}")

  private def isShortCircuit(op: BinOps): Boolean =
    op == BinOps.ShortAnd || op == BinOps.ShortOr

  private def thunkParam(body: TypeRec[Expr]): Variable = {
    val used = freeVars(body).map(_.name)
    LazyList.from(0)
      .map(i => Variable(s"__short_arg_$i"))
      .find(v => !used.contains(v.name))
      .get
  }

  private def resolveBinaryOperator(op: BinOps, left: TypeRec[Expr], right: TypeRec[Expr]): Check[TypeRec[Expr]] = for {
    env <- ask
    typeName <- lift(operatorTypeName(typeOf(left)).toRight(s"Cannot resolve operator $op for type ${typeOf(left).show}"))
    fn = StandardLibrary.binaryOperatorName(op, typeName)
    fnType <- lift(env.values.get(fn).toRight(s"Operator $op is not defined for type ${typeOf(left).show}; expected function ${fn.name}"))
    rightArg = if (isShortCircuit(op)) {
      val thunkType = arrowT(unitTypeT, typeOf(right))
      absT(thunkParam(right), thunkType, unitTypeT, right)
    } else right
    expectedRightType = if (isShortCircuit(op)) arrowT(unitTypeT, typeOf(right)) else typeOf(right)
    result <- destructArrow(fnType) match {
      case Some((leftParam, afterLeft)) if Equivalence.beta(leftParam, typeOf(left)) =>
        destructArrow(afterLeft) match {
          case Some((rightParam, resultType)) if Equivalence.beta(rightParam, expectedRightType) =>
            val fnRef = varrType(fn, fnType)
            val appliedLeft = appT(afterLeft, fnRef, left)
            okT(appT(resultType, appliedLeft, rightArg))
          case Some((rightParam, _)) =>
            fail(s"Type mismatch: expected ${rightParam.show}, actual ${expectedRightType.show}")
          case None =>
            fail(s"Operator function ${fn.name} must take two arguments: ${fnType.show}")
        }
      case Some((leftParam, _)) =>
        fail(s"Type mismatch: expected ${leftParam.show}, actual ${typeOf(left).show}")
      case None =>
        fail(s"Operator function ${fn.name} must be a function: ${fnType.show}")
    }
  } yield result

  private def resolveUnaryOperator(op: UnaryOps, body: TypeRec[Expr]): Check[TypeRec[Expr]] = for {
    env <- ask
    typeName <- lift(operatorTypeName(typeOf(body)).toRight(s"Cannot resolve operator $op for type ${typeOf(body).show}"))
    fn = StandardLibrary.unaryOperatorName(op, typeName)
    fnType <- lift(env.values.get(fn).toRight(s"Operator $op is not defined for type ${typeOf(body).show}; expected function ${fn.name}"))
    result <- destructArrow(fnType) match {
      case Some((paramType, resultType)) if Equivalence.beta(paramType, typeOf(body)) =>
        okT(appT(resultType, varrType(fn, fnType), body))
      case Some((paramType, _)) =>
        fail(s"Type mismatch: expected ${paramType.show}, actual ${typeOf(body).show}")
      case None =>
        fail(s"Operator function ${fn.name} must be a function: ${fnType.show}")
    }
  } yield result

  private def checkIntrinsic(op: IntrinsicOps, args: Seq[TypeRec[Expr]]): Check[TypeRec[Expr]] = op match {
    case IntrinsicOps.BinOp(binOp, operandTypeName) =>
      args match {
        case Seq(left, right) =>
          val operandType = primitiveT(operandTypeName)
          for {
            _ <- expect(operandType, typeOf(left))
            _ <- expect(operandType, typeOf(right))
            _ <- binOp match {
              case BinOps.Add | BinOps.Sub | BinOps.Mul | BinOps.Div =>
                guard(BuiltinTypes.numericTypes.contains(operandTypeName), s"Intrinsic $binOp requires numeric operands")
              case BinOps.Mod =>
                guard(BuiltinTypes.integerTypes.contains(operandTypeName), s"Intrinsic $binOp requires integer operands")
              case BinOps.Eq | BinOps.Neq =>
                guard(BuiltinTypes.equatableTypes.contains(operandTypeName), s"Intrinsic $binOp requires equatable operands")
              case BinOps.Lt | BinOps.Leq | BinOps.Gt | BinOps.Geq =>
                guard(BuiltinTypes.numericTypes.contains(operandTypeName), s"Intrinsic $binOp requires numeric operands")
              case BinOps.And | BinOps.Or | BinOps.Xor =>
                guard(
                  BuiltinTypes.numericTypes.contains(operandTypeName) || operandTypeName == "bool",
                  s"Intrinsic $binOp requires numeric or bool operands"
                )
              case BinOps.ShortAnd | BinOps.ShortOr =>
                fail(s"Intrinsic $binOp is not supported; define it as an operator function")
            }
            resultType = binOp match {
              case BinOps.Add | BinOps.Sub | BinOps.Mul | BinOps.Div | BinOps.Mod |
                  BinOps.And | BinOps.Or | BinOps.Xor => operandType
              case BinOps.Eq | BinOps.Neq | BinOps.Lt | BinOps.Leq | BinOps.Gt | BinOps.Geq => boolTypeT
              case BinOps.ShortAnd | BinOps.ShortOr => boolTypeT
            }
          } yield intrinsicT(op, resultType, args)
        case _ => fail(s"Binary intrinsic $binOp expects 2 arguments, got ${args.length}")
      }
    case IntrinsicOps.UnaryOp(unaryOp, operandTypeName) =>
      args match {
        case Seq(body) =>
          val operandType = primitiveT(operandTypeName)
          for {
            _ <- expect(operandType, typeOf(body))
            _ <- unaryOp match {
              case UnaryOps.Neg =>
                guard(BuiltinTypes.numericTypes.contains(operandTypeName), s"Intrinsic $unaryOp requires numeric operand")
              case UnaryOps.Not =>
                guard(operandTypeName == "bool", s"Intrinsic $unaryOp requires bool operand")
            }
            resultType = unaryOp match {
              case UnaryOps.Neg => operandType
              case UnaryOps.Not => boolTypeT
            }
          } yield intrinsicT(op, resultType, args)
        case _ => fail(s"Unary intrinsic $unaryOp expects 1 argument, got ${args.length}")
      }
  }

  private def foreignArity(t: TypeRec[Type]): Int = {
    @annotation.tailrec
    def loop(current: TypeRec[Type], count: Int): Int = destructArrow(current) match {
      case Some((_, to)) => loop(to, count + 1)
      case None => count
    }
    loop(t, 0)
  }

  private def expectForeignType(t: TypeRec[Type]): Check[Unit] =
    guard(foreignArity(t) > 0, s"Foreign function must have at least one argument: ${t.show}")

  private def dataResultType(owner: TypeVariable, params: Seq[(TypeVariable, Kind)]): TypeRec[Type] =
    applyTypeConstructor(owner, params.map { case (v, _) => typeVarT(v) })

  private def constructorType(owner: TypeVariable, params: Seq[(TypeVariable, Kind)], fields: Seq[TypeRec[Type]]): TypeRec[Type] = {
    val result = dataResultType(owner, params)
    val functionType = fields.foldRight(result)(arrowT)
    params.foldRight(functionType) { case ((v, k), body) => forallTypeT(v, k, body) }
  }

  private case class TypeSpine(head: TypeRec[Type], args: Seq[Expand[Type]]) {
    def append(arg: Expand[Type]): TypeSpine = copy(args = args :+ arg)
  }
  private case class TypeExpansion[I](value: EitherS[TypeRec[I]], spine: Option[TypeSpine] = None)
  private type Expand[I] = Env => TypeExpansion[I]

  private def rebuild[I](ann: TypeAnn[I], node: AST[[y] =>> (TypeRec[y], Expand[y]), I], env: Env): EitherS[TypeRec[I]] =
    summon[HTraverse[AST]]
      .traverse(node)([y] => (child: (TypeRec[y], Expand[y])) => child._2(env).value)
      .map(HCofree(ann, _))

  private def typeNameSpine(variable: TypeVariable): TypeSpine =
    TypeSpine(typeVarT(variable), Seq.empty)

  private def arityError(kind: String, variable: TypeVariable, expected: Int, actual: Int): EitherS[Nothing] =
    Left(s"$kind ${variable.name} expects $expected arguments, got $actual")

  private def expectArity[A](kind: String, variable: TypeVariable, expected: Int, actual: Int)(value: => EitherS[A]): EitherS[A] =
    if (expected == actual) value else arityError(kind, variable, expected, actual)

  private def aliasAsTypeAbs(params: Seq[(TypeVariable, Kind)], body: TypeRec[Type]): TypeRec[Type] =
    params.foldRight(body) { case ((v, k), acc) => typeAbsT(v, k, acc) }

  private def expandDefinedType(variable: TypeVariable, args: Seq[Expand[Type]], env: Env): EitherS[TypeRec[Type]] =
    env.typeAliases.get(variable) match {
      case Some(TypeAlias(params, body)) =>
        if (args.isEmpty) Right(aliasAsTypeAbs(params, body))
        else expectArity("Type alias", variable, params.length, args.length) {
          args.toList.traverse(arg => arg(env).value).map(expandedArgs => substMany(params.map(_._1), expandedArgs, body))
        }
      case None => env.dataTypes.get(variable) match {
        case Some(DataDef(params, _, _)) =>
          if (args.length > params.length)
            arityError("Data type", variable, params.length, args.length)
          else
            args.toList.traverse(arg => arg(env).value).map(expandedArgs => applyTypeConstructor(variable, expandedArgs))
        case None => Left(s"Type variable ${variable.name} is not defined")
      }
    }

  private def expandTypeName(variable: TypeVariable, env: Env): EitherS[TypeRec[Type]] =
    if (env.typeVars.contains(variable)) Right(typeVarT(variable))
    else expandDefinedType(variable, Seq.empty, env)

  private def expandHigherKindedVar(variable: TypeVariable, args: Seq[Expand[Type]], env: Env): EitherS[TypeRec[Type]] =
    args.toList.traverse(arg => arg(env).value).map { expandedArgs =>
      expandedArgs.foldLeft(typeVarT(variable)) { (acc, arg) => typeAppT(acc, arg) }
    }

  private def expandTypeSpine(spine: Option[TypeSpine], self: TypeRec[Type], env: Env): EitherS[TypeRec[Type]] =
    spine match {
      case Some(TypeSpine(head, args)) => head.project match {
        case AST.TypeVar(variable) if env.typeVars.contains(variable) => expandHigherKindedVar(variable, args, env)
        case AST.TypeVar(variable) => expandDefinedType(variable, args, env)
        case AST.Primitive(name) => expandPrimitive(name, args, env)
        case _ => Left(s"Type application is not supported: ${self.show}")
      }
      case None => Left(s"Type application is not supported: ${self.show}")
    }

  private def expandPrimitive(name: String, args: Seq[Expand[Type]], env: Env): EitherS[TypeRec[Type]] =
    BuiltinTypes.arity(name) match {
      case Some(expected) if expected == args.length =>
        args.toList.traverse(arg => arg(env).value).map(expandedArgs =>
          expandedArgs.foldLeft(primitiveT(name)) { (acc, arg) => typeAppT(acc, arg) }
        )
      case Some(expected) =>
        Left(s"Primitive type $name expects $expected arguments, got ${args.length}")
      case None =>
        Left(s"Primitive type $name is not defined")
    }

  private val expandAlg: RAlgebra[TypedAST, TypeRec, Expand] = [x] =>
    (he: TypedAST[[y] =>> (TypeRec[y], Expand[y]), x]) => env => he match {
    case HCofreeT(_, AST.TypeVar(variable)) =>
      TypeExpansion(expandTypeName(variable, env), Some(typeNameSpine(variable)))

    case HCofreeT(_, AST.Primitive(name)) =>
      TypeExpansion(expandPrimitive(name, Seq.empty, env), Some(TypeSpine(primitiveT(name), Seq.empty)))

    case HCofreeT(_, AST.ForAll(variable, kind, body)) =>
      val value = for {
        _ <- Either.cond(!env.typeVars.contains(variable), (), s"Type variable ${variable.name} is already defined")
        eb <- body._2(env.copy(typeVars = env.typeVars + (variable -> kind))).value
      } yield forallTypeT(variable, kind, eb)
      TypeExpansion(value)

    case HCofreeT(_, AST.TypeAbs(variable, kind, body)) =>
      val value = for {
        _ <- Either.cond(!env.typeVars.contains(variable), (), s"Type variable ${variable.name} is already defined")
        eb <- body._2(env.copy(typeVars = env.typeVars + (variable -> kind))).value
      } yield typeAbsT(variable, kind, eb)
      TypeExpansion(value)

    case HCofreeT(ann, node @ AST.TypeApp(function, argument)) =>
      val self = HCofree(ann, paraOriginals(node))
      val spine = function._2(env).spine.map(_.append(argument._2))
      TypeExpansion(expandTypeSpine(spine, self, env), spine)

    case HCofreeT(ann, ast) => TypeExpansion(rebuild(ann, ast, env))
  }

  private def expandType(t: TypeRec[Type], env: Env): EitherS[TypeRec[Type]] = {
    t.para(expandAlg)(env).value
  }

  private def expandAndCheckStar(t: TypeRec[Type], env: Env): EitherS[TypeRec[Type]] = for {
    expanded <- expandType(t, env)
    kind <- KAnalyser.kindOf(expanded, env)
    _ <- Either.cond(kind == Kind.Star, (), s"Type ${expanded.show} has kind ${kind.show}, expected *")
  } yield expanded

  private def expandWellKinded(types: TypeRec[Type]): Check[(TypeRec[Type], Kind)] =
    ask.flatMap { env =>
      lift(for {
        expanded <- expandType(types, env)
        kind <- KAnalyser.kindOf(expanded, env)
      } yield (expanded, kind))
    }

  private def expandChecked(types: TypeRec[Type]): Check[TypeRec[Type]] =
    expandWellKinded(types).flatMap { case (expanded, kind) =>
      guard(kind == Kind.Star, s"Type ${expanded.show} has kind ${kind.show}, expected *").as(expanded)
    }

  private val tcAlg: Algebra[AST, TC] = [x] => (node: AST[TC, x]) => node match {

    case AST.Program(decls) => decls.toList.traverse(identity).map(decls => programT(decls))

    case AST.TopLet(variable, types, value) => for {
      typedTypes <- types
      declaredType <- expandChecked(typedTypes)
      typedValue <- value
      _ <- expect(declaredType, typeOf(typedValue))
    } yield topLetT(variable, typedTypes, typedValue)

    case AST.TopLetRec(variable, types, value) => for {
      typedTypes <- types
      declaredType <- expandChecked(typedTypes)
      typedValue <- value.local((e: Env) => e.copy(values = e.values + (variable -> declaredType)))
      _ <- expect(declaredType, typeOf(typedValue))
    } yield topLetRecT(variable, typedTypes, typedValue)

    case AST.TopImport(path) =>
      fail(s"Unresolved import: $path")

    case AST.TopType(variable, params, alias) => for {
      env <- ask
      _ <- guard(
        !env.typeVars.contains(variable) && !env.typeAliases.contains(variable) && !env.dataTypes.contains(variable),
        s"Type alias ${variable.name} is already defined"
      )
      _ <- guard(params.map(_._1).distinct.length == params.length, s"Type alias ${variable.name} has duplicate parameters")
      _ <- guard(params.forall { case (p, _) => !env.typeVars.contains(p) }, s"Type alias ${variable.name} has a parameter that is already defined")
      typedAlias <- alias.local((e: Env) => e.copy(typeVars = e.typeVars ++ params))
    } yield topTypeT(variable, params, typedAlias)

    case AST.TopData(variable, params, constructors, recursive) => for {
      env <- ask
      _ <- guard(
        !env.typeVars.contains(variable) && !env.typeAliases.contains(variable) && !env.dataTypes.contains(variable),
        s"Data type ${variable.name} is already defined"
      )
      _ <- guard(params.map(_._1).distinct.length == params.length, s"Data type ${variable.name} has duplicate parameters")
      _ <- guard(params.forall { case (p, _) => !env.typeVars.contains(p) }, s"Data type ${variable.name} has a parameter that is already defined")
      constructorNames = constructors.map(_.name)
      _ <- guard(constructorNames.distinct.length == constructorNames.length, s"Data type ${variable.name} has duplicate constructors")
      _ <- guard(
        constructorNames.forall(name => !env.values.contains(name) && !env.constructors.contains(name)),
        s"Data type ${variable.name} has a constructor that is already defined"
      )
      placeholder = DataDef(params, Seq.empty, recursive)
      fieldEnv = env.copy(typeVars = env.typeVars ++ params, dataTypes = env.dataTypes + (variable -> placeholder))
      typedConstructors <- constructors.toList.traverse { c =>
        c.fields.toList.traverse(field => field.local((_: Env) => fieldEnv)).map(fs => DataConstructor(c.name, fs))
      }
    } yield topDataT(variable, params, typedConstructors, recursive)

    case AST.Abs(variable, types, body) => for {
      typedTypes <- types
      paramType <- expandChecked(typedTypes)
      typedBody <- body.local((e: Env) => e.copy(values = e.values + (variable -> paramType)))
      resultType = arrowT(paramType, typeOf(typedBody))
    } yield absT(variable, resultType, typedTypes, typedBody)

    case AST.TyAbs(variable, kind, body) => for {
      env <- ask
      _ <- guard(!env.typeVars.contains(variable), s"Type variable ${variable.name} is already defined")
      typedBody <- body.local((e: Env) => e.copy(typeVars = e.typeVars + (variable -> kind)))
      resultType = forallTypeT(variable, kind, typeOf(typedBody))
    } yield tyAbsT(variable, resultType, kind, typedBody)

    case AST.Let(variable, types, value, body) => for {
      typedTypes <- types
      declaredType <- expandChecked(typedTypes)
      typedValue <- value
      _ <- expect(declaredType, typeOf(typedValue))
      typedBody <- body.local((e: Env) => e.copy(values = e.values + (variable -> declaredType)))
      resultType = typeOf(typedBody)
    } yield letT(variable, resultType, typedTypes, typedValue, typedBody)

    case AST.LetRec(variable, types, value, body) => for {
      typedTypes <- types
      declaredType <- expandChecked(typedTypes)
      typedValue <- value.local((e: Env) => e.copy(values = e.values + (variable -> declaredType)))
      _ <- expect(declaredType, typeOf(typedValue))
      typedBody <- body.local((e: Env) => e.copy(values = e.values + (variable -> declaredType)))
      resultType = typeOf(typedBody)
    } yield letRecT(variable, resultType, typedTypes, typedValue, typedBody)

    case AST.TypeLet(variable, params, alias, body) => for {
      env <- ask
      _ <- guard(
        !env.typeVars.contains(variable) && !env.typeAliases.contains(variable) && !env.dataTypes.contains(variable),
        s"Type alias ${variable.name} is already defined"
      )
      _ <- guard(params.map(_._1).distinct.length == params.length, s"Type alias ${variable.name} has duplicate parameters")
      _ <- guard(params.forall { case (p, _) => !env.typeVars.contains(p) }, s"Type alias ${variable.name} has a parameter that is already defined")
      typedAlias <- alias.local((e: Env) => e.copy(typeVars = e.typeVars ++ params))
      expandedAlias <- lift(expandAndCheckStar(typedAlias, env.copy(typeVars = env.typeVars ++ params)))
      typedBody <- body.local((e: Env) => e.copy(typeAliases = e.typeAliases + (variable -> TypeAlias(params, expandedAlias))))
      resultType = typeOf(typedBody)
    } yield typeLetT(variable, params, resultType, typedAlias, typedBody)

    case AST.DataLet(variable, params, constructors, body, recursive) => for {
      env <- ask
      _ <- guard(
        !env.typeVars.contains(variable) && !env.typeAliases.contains(variable) && !env.dataTypes.contains(variable),
        s"Data type ${variable.name} is already defined"
      )
      _ <- guard(params.map(_._1).distinct.length == params.length, s"Data type ${variable.name} has duplicate parameters")
      _ <- guard(params.forall { case (p, _) => !env.typeVars.contains(p) }, s"Data type ${variable.name} has a parameter that is already defined")
      constructorNames = constructors.map(_.name)
      _ <- guard(constructorNames.distinct.length == constructorNames.length, s"Data type ${variable.name} has duplicate constructors")
      _ <- guard(
        constructorNames.forall(name => !env.values.contains(name) && !env.constructors.contains(name)),
        s"Data type ${variable.name} has a constructor that is already defined"
      )
      placeholder = DataDef(params, Seq.empty, recursive)
      fieldEnv = env.copy(typeVars = env.typeVars ++ params, dataTypes = env.dataTypes + (variable -> placeholder))
      typedConstructors <- constructors.toList.traverse { c =>
        c.fields.toList.traverse(field => field.local((_: Env) => fieldEnv)).map(fs => DataConstructor(c.name, fs))
      }
      expandedConstructors <- lift {
        typedConstructors.zipWithIndex.toList.traverse { case (c, tag) =>
          c.fields.toList.traverse(field => expandAndCheckStar(field, fieldEnv)).map(fs => ConstructorDef(c.name, variable, fs, tag))
        }
      }
      _ <- guard(
        recursive || !expandedConstructors.exists(_.fields.exists(field => containsDataApplicationOf(field, variable))),
        s"Recursive data type ${variable.name} must be declared with data rec"
      )
      dataDef = DataDef(params, expandedConstructors, recursive)
      constructorDefs = constructorNames.zip(expandedConstructors).toMap
      constructorTypes = constructorNames.zip(expandedConstructors).map { case (name, c) =>
        name -> constructorType(variable, params, c.fields)
      }.toMap
      typedBody <- body.local((e: Env) => e.copy(
        values = e.values ++ constructorTypes,
        dataTypes = e.dataTypes + (variable -> dataDef),
        constructors = e.constructors ++ constructorDefs
      ))
      resultType = typeOf(typedBody)
    } yield dataLetT(variable, params, resultType, typedConstructors, typedBody, recursive)

    case AST.Match(scrutinee, cases) => for {
      env <- ask
      typedScrutinee <- scrutinee
      dataApp <- lift(dataTypeApplication(typeOf(typedScrutinee), env.dataTypes)(_.paramVars))
      (dataName, dataDef, typeArgs) = dataApp
      caseNames = cases.map(_.constructor)
      _ <- guard(caseNames.distinct.length == caseNames.length, s"Match has duplicate cases")
      expectedConstructors <- lift {
        dataDef.constructors.toList.traverse { cdef =>
          env.constructors.collectFirst { case (name, c) if c == cdef => name }
            .toRight(s"Constructor for data type ${dataName.name} is not defined")
        }
      }
      _ <- {
        val missing = expectedConstructors.filterNot(caseNames.contains)
        val extra = caseNames.filterNot(expectedConstructors.contains)
        guard(
          missing.isEmpty && extra.isEmpty,
          s"Non-exhaustive or invalid match: missing ${missing.map(_.name).mkString(", ")}, invalid ${extra.map(_.name).mkString(", ")}"
        )
      }
      typedCases <- cases.toList.traverse { matchCase =>
        val cdef = env.constructors(matchCase.constructor)
        val fieldTypes = dataDef.paramVars.zip(typeArgs).foldLeft(cdef.fields) { case (fields, (param, arg)) =>
          fields.map(field => substType(param, arg, field))
        }
        for {
          _ <- guard(
            matchCase.binders.length == fieldTypes.length,
            s"Constructor ${matchCase.constructor.name} expects ${fieldTypes.length} binders, got ${matchCase.binders.length}"
          )
          _ <- guard(matchCase.binders.distinct.length == matchCase.binders.length, s"Match case ${matchCase.constructor.name} has duplicate binders")
          binderTypes = matchCase.binders.zip(fieldTypes).toMap
          typedBody <- matchCase.body.local((e: Env) => e.copy(values = e.values ++ binderTypes))
        } yield MatchCase(matchCase.constructor, matchCase.binders, typedBody)
      }
      resultType <- typedCases.headOption match {
        case Some(first) => typedCases.tail.traverse(c => expect(typeOf(first.body), typeOf(c.body))).as(typeOf(first.body))
        case None => fail("Match must have at least one case")
      }
    } yield matchExprT(resultType, typedScrutinee, typedCases)

    case AST.Fold(scrutinee, resultTypeNode, cases) => for {
      env <- ask
      typedScrutinee <- scrutinee
      typedResultType <- resultTypeNode
      resultType <- expandChecked(typedResultType)
      dataApp <- lift(dataTypeApplication(typeOf(typedScrutinee), env.dataTypes)(_.paramVars))
      (dataName, dataDef, typeArgs) = dataApp
      caseNames = cases.map(_.constructor)
      _ <- guard(caseNames.distinct.length == caseNames.length, s"Fold has duplicate cases")
      expectedConstructors <- lift {
        dataDef.constructors.toList.traverse { cdef =>
          env.constructors.collectFirst { case (name, c) if c == cdef => name }
            .toRight(s"Constructor for data type ${dataName.name} is not defined")
        }
      }
      _ <- {
        val missing = expectedConstructors.filterNot(caseNames.contains)
        val extra = caseNames.filterNot(expectedConstructors.contains)
        guard(
          missing.isEmpty && extra.isEmpty,
          s"Non-exhaustive or invalid fold: missing ${missing.map(_.name).mkString(", ")}, invalid ${extra.map(_.name).mkString(", ")}"
        )
      }
      typedCases <- cases.toList.traverse { foldCase =>
        val cdef = env.constructors(foldCase.constructor)
        val fieldTypes = dataDef.paramVars.zip(typeArgs).foldLeft(cdef.fields) { case (fields, (param, arg)) =>
          fields.map(field => substType(param, arg, field))
        }
        val binderFieldTypes = fieldTypes.map { field =>
          if (isDataApplicationOf(field, dataName)) resultType else field
        }
        for {
          _ <- guard(
            foldCase.binders.length == fieldTypes.length,
            s"Constructor ${foldCase.constructor.name} expects ${fieldTypes.length} binders, got ${foldCase.binders.length}"
          )
          _ <- guard(foldCase.binders.distinct.length == foldCase.binders.length, s"Fold case ${foldCase.constructor.name} has duplicate binders")
          binderTypes = foldCase.binders.zip(binderFieldTypes).toMap
          typedBody <- foldCase.body.local((e: Env) => e.copy(values = e.values ++ binderTypes))
          _ <- expect(resultType, typeOf(typedBody))
        } yield MatchCase(foldCase.constructor, foldCase.binders, typedBody)
      }
    } yield foldExprT(resultType, typedScrutinee, typedResultType, typedCases)

    case AST.App(function, argument) => for {
      typedFunction <- function
      typedArgument <- argument
      resultType <- destructArrow(typeOf(typedFunction)) match {
        case Some((from, to)) if Equivalence.beta(from, typeOf(typedArgument)) => okT(to)
        case Some((from, _)) => fail(s"Type mismatch: expected ${from.show}, actual ${typeOf(typedArgument).show}")
        case None => fail(s"Not a function: ${typeOf(typedFunction).show}")
      }
    } yield appT(resultType, typedFunction, typedArgument)

    case AST.TyApp(function, argument) => for {
      typedFunction <- function
      typedArgument <- argument
      expanded <- expandWellKinded(typedArgument)
      (argumentType, argKind) = expanded
      resultType <- destructForAllK(typeOf(typedFunction)) match {
        case Some((variable, expectedKind, bodyType)) =>
          if (expectedKind == argKind) okT(substType(variable, argumentType, bodyType))
          else fail(s"Kind mismatch in type application: expected ${expectedKind.show}, got ${argKind.show}")
        case None => fail(s"Not a polymorphic function: ${typeOf(typedFunction).show}")
      }
    } yield tyAppT(resultType, typedFunction, typedArgument)

    case AST.Foreign(value, types) => for {
      typedTypes <- types
      declaredType <- expandChecked(typedTypes)
      _ <- expectForeignType(declaredType)
    } yield foreignT(value, declaredType, typedTypes)

    case AST.Var(value) => for {
      env <- ask
      t <- lift(env.values.get(value).toRight(s"Variable $value is not defined"))
    } yield varrType(value, t)

    case AST.Num(value, typeName) =>
      if (BuiltinTypes.numericTypes.contains(typeName)) okT(numT(value, typeName, primitiveT(typeName)))
      else fail(s"Numeric literal type $typeName is not defined")
    case AST.Char(value) => okT(charT(value, charTypeT))
    case AST.StringLit(value) => okT(stringLitT(value, stringTypeT))
    case AST.Bool(value) => okT(boolT(value, boolTypeT))
    case AST.UnitLit() => okT(unitLitT(unitTypeT))

    case AST.Block(discarded, result) => for {
      typedDiscarded <- discarded.toList.traverse(identity)
      typedResult <- result.traverse(identity)
      resultType = typedResult.map(typeOf).getOrElse(unitTypeT)
    } yield blockT(resultType, typedDiscarded, typedResult)

    case AST.BinOp(op, left, right) => for {
      typedLeft <- left
      typedRight <- right
      resolved <- resolveBinaryOperator(op, typedLeft, typedRight)
    } yield resolved

    case AST.Intrinsic(op, args) => for {
      typedArgs <- args.toList.traverse(identity)
      checked <- checkIntrinsic(op, typedArgs)
    } yield checked

    case AST.UnaryOp(op, body) => for {
      typedBody <- body
      resolved <- resolveUnaryOperator(op, typedBody)
    } yield resolved

    case AST.If(cond, thenBranch, elseBranch) => for {
      typedCond <- cond
      typedThen <- thenBranch
      typedElse <- elseBranch
      condType = typeOf(typedCond)
      thenType = typeOf(typedThen)
      elseType = typeOf(typedElse)
      _ <- expect(boolTypeT, condType)
      _ <- expect(thenType, elseType)
    } yield ifT(thenType, typedCond, typedThen, typedElse)

    case AST.Primitive(name) =>
      BuiltinTypes.arity(name) match {
        case Some(_) => okT(primitiveT(name))
        case None => fail(s"Primitive type $name is not defined")
      }
    case AST.TypeVar(variable) => for {
      env <- ask
      _ <- guard(
        env.typeVars.contains(variable) || env.typeAliases.contains(variable) || env.dataTypes.contains(variable),
        s"Type variable ${variable.name} is not defined"
      )
    } yield typeVarT(variable)
    case AST.Arrow(from, to) => (from, to).mapN(arrowT)
    case AST.ForAll(variable, kind, body) => for {
      env <- ask
      _ <- guard(!env.typeVars.contains(variable), s"Type variable ${variable.name} is already defined")
      typedBody <- body.local((e: Env) => e.copy(typeVars = e.typeVars + (variable -> kind)))
    } yield forallTypeT(variable, kind, typedBody)
    case AST.TypeAbs(variable, kind, body) => for {
      env <- ask
      _ <- guard(!env.typeVars.contains(variable), s"Type variable ${variable.name} is already defined")
      typedBody <- body.local((e: Env) => e.copy(typeVars = e.typeVars + (variable -> kind)))
    } yield typeAbsT(variable, kind, typedBody)
    case AST.TypeApp(function, argument) => (function, argument).mapN(typeAppT)
  }

  // Checks one declaration against the current environment and yields the
  // environment extended with it, so declarations are scoped left-to-right.
  private def checkDecl(decl: Rec[Decl]): StateT[EitherS, Env, TypeRec[Decl]] =
    StateT { env =>
      for {
        typedDecl <- decl.cata(tcAlg).run(env)
        nextEnv <- extendEnv(typedDecl, env)
      } yield (nextEnv, typedDecl)
    }

  private def extendEnv(decl: TypeRec[Decl], env: Env): EitherS[Env] = decl.project match {
    case AST.TopLet(variable, typedTypes, _) =>
      expandAndCheckStar(typedTypes, env).map(declaredType =>
        env.copy(values = env.values + (variable -> declaredType)))

    case AST.TopLetRec(variable, typedTypes, _) =>
      expandAndCheckStar(typedTypes, env).map(declaredType =>
        env.copy(values = env.values + (variable -> declaredType)))

    case AST.TopImport(path) =>
      Left(s"Unresolved import: $path")

    case AST.TopType(variable, params, typedAlias) =>
      val aliasEnv = env.copy(typeVars = env.typeVars ++ params)
      expandAndCheckStar(typedAlias, aliasEnv).map(expandedAlias =>
        env.copy(typeAliases = env.typeAliases + (variable -> TypeAlias(params, expandedAlias))))

    case AST.TopData(variable, params, typedConstructors, recursive) =>
      val placeholder = DataDef(params, Seq.empty, recursive)
      val fieldEnv = env.copy(typeVars = env.typeVars ++ params, dataTypes = env.dataTypes + (variable -> placeholder))
      for {
        expandedConstructors <- typedConstructors.zipWithIndex.toList.traverse { case (c, tag) =>
          c.fields.toList.traverse(field => expandAndCheckStar(field, fieldEnv)).map(fs => ConstructorDef(c.name, variable, fs, tag))
        }
        _ <- Either.cond(
          recursive || !expandedConstructors.exists(_.fields.exists(field => containsDataApplicationOf(field, variable))),
          (),
          s"Recursive data type ${variable.name} must be declared with data rec"
        )
        dataDef = DataDef(params, expandedConstructors, recursive)
        constructorNames = typedConstructors.map(_.name)
        constructorDefs = constructorNames.zip(expandedConstructors).toMap
        constructorTypes = constructorNames.zip(expandedConstructors).map { case (name, c) =>
          name -> constructorType(variable, params, c.fields)
        }.toMap
      } yield env.copy(
        values = env.values ++ constructorTypes,
        dataTypes = env.dataTypes + (variable -> dataDef),
        constructors = env.constructors ++ constructorDefs
      )
  }

  private def checkMain(env: Env): EitherS[Unit] = env.values.get(Variable("main")) match {
    case Some(t) if Equivalence.alpha(t, arrowT(unitTypeT, intTypeT)) => Right(())
    case Some(t) => Left(s"Top-level main must have type unit → i32, actual ${t.show}")
    case None => Left("Top-level main is not defined")
  }

  def validate(prog: Rec[AST.Program.type]): EitherS[TypeRec[AST.Program.type]] = prog.unfix match {
    case AST.Program(decls) => decls.traverse(checkDecl).run(Env.empty).flatMap { case (env, typedDecls) =>
      checkMain(env).as(programT(typedDecls, env))
    }
  }

}
