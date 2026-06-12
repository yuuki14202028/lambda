package com.yuuki14202028

import cats.syntax.all._
import cats.data.StateT
import Check.{ask, fail, guard, lift}

object TAnalyser {

  private type TC[I] = Check[TypeRec[I]]

  private def okT[I](t: TypeRec[I]): TC[I] = Check.pure(t)

  private def expect(expected: TypeRec[Type], actual: TypeRec[Type]): Check[Unit] =
    guard(Equivalence.alpha(expected, actual), CompileError.TypeMismatch(expected, actual))

  private def expectNumeric(actual: TypeRec[Type]): Check[Unit] =
    guard(isNumericType(actual), CompileError.ExpectedNumeric(actual))

  private def expectEquatable(actual: TypeRec[Type]): Check[Unit] =
    guard(isEquatableType(actual), CompileError.ExpectedEquatable(actual))

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
    typeName <- lift(operatorTypeName(typeOf(left)).toRight(CompileError.OperatorUnresolvable(op, typeOf(left))))
    fn = StandardLibrary.binaryOperatorName(op, typeName)
    fnType <- lift(env.values.get(fn).toRight(CompileError.OperatorUndefined(op, typeOf(left), fn)))
    rightArg = if (isShortCircuit(op)) {
      val thunkType = arrowT(unitTypeT, typeOf(right))
      absT(thunkParam(right), thunkType, unitTypeT, right)
    } else right
    expectedRightType = if (isShortCircuit(op)) arrowT(unitTypeT, typeOf(right)) else typeOf(right)
    result <- destructArrow(fnType) match {
      case Some((leftParam, afterLeft)) if Equivalence.alpha(leftParam, typeOf(left)) =>
        destructArrow(afterLeft) match {
          case Some((rightParam, resultType)) if Equivalence.alpha(rightParam, expectedRightType) =>
            val fnRef = varrType(fn, fnType)
            val appliedLeft = appT(afterLeft, fnRef, left)
            okT(appT(resultType, appliedLeft, rightArg))
          case Some((rightParam, _)) =>
            fail(CompileError.TypeMismatch(rightParam, expectedRightType))
          case None =>
            fail(CompileError.OperatorNotBinary(fn, fnType))
        }
      case Some((leftParam, _)) =>
        fail(CompileError.TypeMismatch(leftParam, typeOf(left)))
      case None =>
        fail(CompileError.OperatorNotAFunction(fn, fnType))
    }
  } yield result

  private def resolveUnaryOperator(op: UnaryOps, body: TypeRec[Expr]): Check[TypeRec[Expr]] = for {
    env <- ask
    typeName <- lift(operatorTypeName(typeOf(body)).toRight(CompileError.OperatorUnresolvable(op, typeOf(body))))
    fn = StandardLibrary.unaryOperatorName(op, typeName)
    fnType <- lift(env.values.get(fn).toRight(CompileError.OperatorUndefined(op, typeOf(body), fn)))
    result <- destructArrow(fnType) match {
      case Some((paramType, resultType)) if Equivalence.alpha(paramType, typeOf(body)) =>
        okT(appT(resultType, varrType(fn, fnType), body))
      case Some((paramType, _)) =>
        fail(CompileError.TypeMismatch(paramType, typeOf(body)))
      case None =>
        fail(CompileError.OperatorNotAFunction(fn, fnType))
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
                guard(BuiltinTypes.numericTypes.contains(operandTypeName), CompileError.IntrinsicOperandInvalid(binOp, OperandRequirement.Numeric))
              case BinOps.Mod =>
                guard(BuiltinTypes.integerTypes.contains(operandTypeName), CompileError.IntrinsicOperandInvalid(binOp, OperandRequirement.Integer))
              case BinOps.Eq | BinOps.Neq =>
                guard(BuiltinTypes.equatableTypes.contains(operandTypeName), CompileError.IntrinsicOperandInvalid(binOp, OperandRequirement.Equatable))
              case BinOps.Lt | BinOps.Leq | BinOps.Gt | BinOps.Geq =>
                guard(BuiltinTypes.numericTypes.contains(operandTypeName), CompileError.IntrinsicOperandInvalid(binOp, OperandRequirement.Numeric))
              case BinOps.And | BinOps.Or | BinOps.Xor =>
                guard(
                  BuiltinTypes.numericTypes.contains(operandTypeName) || operandTypeName == "bool",
                  CompileError.IntrinsicOperandInvalid(binOp, OperandRequirement.NumericOrBool)
                )
              case BinOps.ShortAnd | BinOps.ShortOr =>
                fail(CompileError.IntrinsicUnsupported(binOp))
            }
            resultType = binOp match {
              case BinOps.Add | BinOps.Sub | BinOps.Mul | BinOps.Div | BinOps.Mod |
                  BinOps.And | BinOps.Or | BinOps.Xor => operandType
              case BinOps.Eq | BinOps.Neq | BinOps.Lt | BinOps.Leq | BinOps.Gt | BinOps.Geq => boolTypeT
              case BinOps.ShortAnd | BinOps.ShortOr => boolTypeT
            }
          } yield intrinsicT(op, resultType, args)
        case _ => fail(CompileError.IntrinsicArityMismatch(binOp, args.length))
      }
    case IntrinsicOps.UnaryOp(unaryOp, operandTypeName) =>
      args match {
        case Seq(body) =>
          val operandType = primitiveT(operandTypeName)
          for {
            _ <- expect(operandType, typeOf(body))
            _ <- unaryOp match {
              case UnaryOps.Neg =>
                guard(BuiltinTypes.numericTypes.contains(operandTypeName), CompileError.IntrinsicOperandInvalid(unaryOp, OperandRequirement.Numeric))
              case UnaryOps.Not =>
                guard(operandTypeName == "bool", CompileError.IntrinsicOperandInvalid(unaryOp, OperandRequirement.Bool))
            }
            resultType = unaryOp match {
              case UnaryOps.Neg => operandType
              case UnaryOps.Not => boolTypeT
            }
          } yield intrinsicT(op, resultType, args)
        case _ => fail(CompileError.IntrinsicArityMismatch(unaryOp, args.length))
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
    guard(foreignArity(t) > 0, CompileError.ForeignNotFunction(t))

  // ---- 文字列埋め込みの脱糖 ----
  // `a = {e}` は型検査時に concat("a = ")(show[τ](e)) へ展開する。
  // concat はスコープ中の変数ではなくランタイムの C 関数 concat を Foreign として直接参照する。
  private val showMethod = Variable("show")

  private def interpConcatType: TypeRec[Type] = arrowT(stringTypeT, arrowT(stringTypeT, stringTypeT))

  // String 型でない埋め込み式 e: τ を show[τ](e) に包む(show は trait メソッドとして TraitEncoder が辞書解決する)
  private def stringifyPart(part: TypeRec[Expr]): Check[TypeRec[Expr]] = {
    val partType = typeOf(part)
    if (Equivalence.alpha(stringTypeT, partType)) okT(part)
    else for {
      env <- ask
      surface <- lift(env.values.get(showMethod).toRight(
        CompileError.StrInterpShowMissing(partType)))
      destructed <- lift(destructForAllK(surface).toRight(
        CompileError.StrInterpShowNotPolymorphic(surface)))
      (variable, kind, body) = destructed
      _ <- guard(kind == Kind.Star, CompileError.StrInterpShowKindInvalid(kind))
      applied = Equivalence.normalize(substType(variable, partType, body))
      arrowParts <- lift(destructArrow(applied).toRight(
        CompileError.StrInterpShowNotFunction(applied)))
      (from, to) = arrowParts
      _ <- guard(Equivalence.alpha(from, partType), CompileError.StrInterpShowParamMismatch(from, partType))
      _ <- guard(Equivalence.alpha(stringTypeT, to), CompileError.StrInterpShowResultMismatch(stringTypeT, to))
    } yield appT(to, tyAppT(applied, varrType(showMethod, surface), partType), part)
  }

  private def dataResultType(owner: TypeVariable, params: Seq[(TypeVariable, Kind)]): TypeRec[Type] =
    applyTypeConstructor(owner, params.map { case (v, _) => typeVarT(v) })

  private def constructorType(owner: TypeVariable, params: Seq[(TypeVariable, Kind)], fields: Seq[TypeRec[Type]]): TypeRec[Type] = {
    val result = dataResultType(owner, params)
    val functionType = fields.foldRight(result)(arrowT)
    params.foldRight(functionType) { case ((v, k), body) => forallTypeT(v, k, body) }
  }

  private def aliasAsTypeAbs(params: Seq[(TypeVariable, Kind)], body: TypeRec[Type]): TypeRec[Type] =
    params.foldRight(body) { case ((v, k), acc) => typeAbsT(v, k, acc) }

  private val resolveNamesAlg: Algebra[TypedAST, TC] = [x] => he => he.ast match {
    case AST.TypeVar(variable) => ask.flatMap { env =>
      if (env.typeVars.contains(variable)) okT(typeVarT(variable))
      else env.typeAliases.get(variable) match {
        case Some(alias) => okT(aliasAsTypeAbs(alias.params, alias.body))
        case None if env.dataTypes.contains(variable) => okT(typeVarT(variable))
        case None => fail(CompileError.UndefinedTypeVariable(variable))
      }
    }
    case AST.ForAll(variable, kind, body) => for {
      env <- ask
      _ <- guard(!env.typeVars.contains(variable), CompileError.AlreadyDefined(NameKind.TypeVariableName, variable.name))
      resolvedBody <- body.local((e: Env) => e.copy(typeVars = e.typeVars + (variable -> kind)))
    } yield forallTypeT(variable, kind, resolvedBody)
    case AST.TypeAbs(variable, kind, body) => for {
      env <- ask
      _ <- guard(!env.typeVars.contains(variable), CompileError.AlreadyDefined(NameKind.TypeVariableName, variable.name))
      resolvedBody <- body.local((e: Env) => e.copy(typeVars = e.typeVars + (variable -> kind)))
    } yield typeAbsT(variable, kind, resolvedBody)
    case node => node.htraverse([y] => (child: TC[y]) => child).map(HCofree(he.ann, _))
  }

  private def resolveNames(t: TypeRec[Type]): Check[TypeRec[Type]] = t.cata(resolveNamesAlg)

  // 展開済みの型は常に β 正規形になる（比較は Equivalence.alpha で足りる）
  def expandType(t: TypeRec[Type]): Check[TypeRec[Type]] =
    resolveNames(t).map(Equivalence.normalize)

  private def expandAndCheckStar(t: TypeRec[Type], env: Env): EitherS[TypeRec[Type]] =
    expandChecked(t).run(env)

  // 正規化は well-kinded な型に対してのみ停止が保証されるため、カインドを先に確定する
  private def expandWellKinded(types: TypeRec[Type]): Check[(TypeRec[Type], Kind)] = for {
    resolved <- resolveNames(types)
    kind <- KAnalyser.kindOf(resolved)
  } yield (Equivalence.normalize(resolved), kind)

  private def expandChecked(types: TypeRec[Type]): Check[TypeRec[Type]] =
    expandWellKinded(types).flatMap { case (expanded, kind) =>
      guard(kind == Kind.Star, CompileError.KindNotStar(expanded, kind)).as(expanded)
    }

  // 制約列 C[τ̄] の検査: trait の存在・引数の数・各引数のカインド一致を確かめ、型付き Constraint を返す
  private def checkConstraints(site: DeclRef, constraints: Seq[Constraint[TC]], scope: Env => Env): Check[Seq[Constraint[TypeRec]]] =
    constraints.traverse { c =>
      for {
        env <- ask
        traitDef <- lift(env.traits.get(c.name).toRight(CompileError.UndefinedTrait(c.name, Some(site))))
        _ <- guard(
          c.arg.length == traitDef.param.length,
          CompileError.ConstraintArityMismatch(site, c.name, traitDef.param.length, c.arg.length)
        )
        typedArgs <- c.arg.traverse(_.local(scope))
        _ <- typedArgs.zip(traitDef.param).traverse_ { case (arg, (_, kind)) =>
          expandWellKinded(arg).local(scope).flatMap { case (expanded, argKind) =>
            guard(argKind == kind, CompileError.ConstraintKindMismatch(site, c.name, expanded, argKind, kind))
          }
        }
      } yield Constraint[TypeRec](c.name, typedArgs)
    }

  private val tcAlg: Algebra[AST, TC] = [x] => (node: AST[TC, x]) => node match {

    case AST.Program(decls) => decls.traverse(identity).map(decls => programT(decls))

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
      fail(CompileError.UnresolvedImport(path))

    case AST.TopType(variable, params, alias) => for {
      env <- ask
      _ <- guard(
        !env.typeVars.contains(variable) && !env.typeAliases.contains(variable) && !env.dataTypes.contains(variable),
        CompileError.AlreadyDefined(NameKind.TypeAliasName, variable.name)
      )
      _ <- guard(params.map(_._1).distinct.length == params.length, CompileError.DuplicateParams(DeclRef.TypeAliasDecl(variable)))
      _ <- guard(params.forall { case (p, _) => !env.typeVars.contains(p) }, CompileError.ParamAlreadyDefined(DeclRef.TypeAliasDecl(variable)))
      typedAlias <- alias.local((e: Env) => e.copy(typeVars = e.typeVars ++ params))
    } yield topTypeT(variable, params, typedAlias)

    case AST.TopData(variable, params, constructors, recursive) => for {
      env <- ask
      _ <- guard(
        !env.typeVars.contains(variable) && !env.typeAliases.contains(variable) && !env.dataTypes.contains(variable),
        CompileError.AlreadyDefined(NameKind.DataTypeName, variable.name)
      )
      _ <- guard(params.map(_._1).distinct.length == params.length, CompileError.DuplicateParams(DeclRef.DataDecl(variable)))
      _ <- guard(params.forall { case (p, _) => !env.typeVars.contains(p) }, CompileError.ParamAlreadyDefined(DeclRef.DataDecl(variable)))
      constructorNames = constructors.map(_.name)
      _ <- guard(constructorNames.distinct.length == constructorNames.length, CompileError.DuplicateMembers(DeclRef.DataDecl(variable), MemberKind.ConstructorMember))
      _ <- guard(
        constructorNames.forall(name => !env.values.contains(name) && !env.constructors.contains(name)),
        CompileError.MemberAlreadyDefined(DeclRef.DataDecl(variable), MemberKind.ConstructorMember)
      )
      placeholder = DataDef(params, Seq.empty, recursive)
      fieldEnv = env.copy(typeVars = env.typeVars ++ params, dataTypes = env.dataTypes + (variable -> placeholder))
      typedConstructors <- constructors.traverse { c =>
        c.fields.traverse(field => field.local((_: Env) => fieldEnv)).map(fs => DataConstructor(c.name, fs))
      }
    } yield topDataT(variable, params, typedConstructors, recursive)

    case AST.TopTrait(variable, params, supers, methods) => for {
      env <- ask
      _ <- guard(
        !env.typeVars.contains(variable) && !env.typeAliases.contains(variable) && !env.dataTypes.contains(variable),
        CompileError.AlreadyDefined(NameKind.TraitName, variable.name)
      )
      _ <- guard(params.nonEmpty, CompileError.TraitNeedsParameter(variable))
      _ <- guard(params.map(_._1).distinct.length == params.length, CompileError.DuplicateParams(DeclRef.TraitDecl(variable)))
      _ <- guard(params.forall { case (p, _) => !env.typeVars.contains(p) }, CompileError.ParamAlreadyDefined(DeclRef.TraitDecl(variable)))
      methodNames = methods.map(_.name)
      _ <- guard(methodNames.distinct.length == methodNames.length, CompileError.DuplicateMembers(DeclRef.TraitDecl(variable), MemberKind.MethodMember))
      _ <- guard(
        methodNames.forall(name => !env.values.contains(name) && !env.constructors.contains(name)),
        CompileError.MemberAlreadyDefined(DeclRef.TraitDecl(variable), MemberKind.MethodMember)
      )
      ctorName = dictionaryConstructor(variable)
      _ <- guard(
        !env.values.contains(ctorName) && !env.constructors.contains(ctorName),
        CompileError.DictionaryCtorAlreadyDefined(variable, ctorName)
      )
      _ <- guard(methods.forall(_.body.isEmpty), CompileError.DefaultMethodsUnsupported(variable))
      typedSupers <- checkConstraints(DeclRef.TraitDecl(variable), supers, e => e.copy(typeVars = e.typeVars ++ params))
      typedMethods <- methods.traverse { m =>
        m.sig.local((e: Env) => e.copy(typeVars = e.typeVars ++ params))
          .map(typedSig => MethodSig[TypeRec](m.name, typedSig, None))
      }
    } yield topTraitT(variable, params, typedSupers, typedMethods)

    case AST.TopImpl(traitName, implParams, targets, context, methods) => for {
      env <- ask
      traitDef <- lift(env.traits.get(traitName).toRight(CompileError.UndefinedTrait(traitName, None)))
      paramVars = traitDef.param.map(_._1)
      _ <- guard(
        targets.length == traitDef.param.length,
        CompileError.ImplTargetArity(traitName, traitDef.param.length, targets.length)
      )
      _ <- guard(implParams.map(_._1).distinct.length == implParams.length, CompileError.DuplicateParams(DeclRef.ImplDecl(traitName, Seq.empty)))
      _ <- guard(implParams.forall { case (p, _) => !env.typeVars.contains(p) }, CompileError.ParamAlreadyDefined(DeclRef.ImplDecl(traitName, Seq.empty)))
      implScope = (e: Env) => e.copy(typeVars = e.typeVars ++ implParams)
      typedTargets <- targets.traverse(_.local(implScope))
      expandedKinded <- typedTargets.traverse(t => expandWellKinded(t).local(implScope))
      expandedTargets = expandedKinded.map(_._1)
      _ <- expandedKinded.zip(traitDef.param).traverse_ { case ((expanded, targetKind), (_, paramKind)) =>
        guard(
          targetKind == paramKind,
          CompileError.ImplKindMismatch(traitName, expanded, paramKind, targetKind)
        )
      }
      headNames <- expandedTargets.traverse { expanded =>
        lift(typeConstructorHead(expanded).map(_._1).toRight(
          CompileError.InstanceHeadInvalid(traitName, expanded)
        ))
      }
      implRef = DeclRef.ImplDecl(traitName, headNames)
      _ <- guard(
        headNames.forall(h => !implParams.map(_._1).contains(TypeVariable(h))),
        CompileError.InstanceHeadTypeParameter(implRef)
      )
      _ <- guard(
        implParams.forall { case (p, _) => expandedTargets.exists(t => freeTypeVars(t).contains(p)) },
        CompileError.ImplParamUnused(implRef)
      )
      _ <- guard(
        !env.instances.contains(instanceKey(traitName, headNames)),
        CompileError.OverlappingInstance(traitName, headNames)
      )
      typedContext <- checkConstraints(implRef, context, implScope)
      traitMethodNames = traitDef.methods.map(_._1)
      implMethodNames = methods.map(_.name)
      _ <- guard(implMethodNames.distinct.length == implMethodNames.length, CompileError.DuplicateMembers(implRef, MemberKind.MethodMember))
      missing = traitMethodNames.filterNot(implMethodNames.contains)
      _ <- guard(missing.isEmpty, CompileError.MissingMethods(implRef, missing))
      extra = implMethodNames.filterNot(traitMethodNames.contains)
      _ <- guard(extra.isEmpty, CompileError.ExtraMethods(implRef, extra))
      sigScope = (e: Env) => implScope(e).copy(typeVars = implScope(e).typeVars ++ traitDef.param)
      typedMethods <- methods.traverse { m =>
        val expected = Equivalence.normalize(substMany(paramVars, expandedTargets, traitDef.methods.find(_._1 == m.name).get._2))
        for {
          typedBody <- m.body.local(implScope)
          _ <- guard(
            Equivalence.alpha(expected, typeOf(typedBody)),
            CompileError.MethodBodyTypeMismatch(implRef, m.name, expected, typeOf(typedBody))
          )
          typedSig <- m.sig.traverse(_.local(sigScope))
          _ <- typedSig.fold(Check.pure(())) { s =>
            for {
              expandedSig <- expandType(s).local(sigScope)
              declared = Equivalence.normalize(substMany(paramVars, expandedTargets, expandedSig))
              _ <- guard(
                Equivalence.alpha(expected, declared),
                CompileError.MethodSigTypeMismatch(implRef, m.name, declared, expected)
              )
            } yield ()
          }
        } yield MethodImpl[TypeRec](m.name, typedSig, typedBody)
      }
    } yield topImplT(traitName, implParams, typedTargets, typedContext, typedMethods)

    case AST.TopLetWhere(variable, params, constraints, types, value, recursive) => for {
      env <- ask
      _ <- guard(constraints.nonEmpty, CompileError.EmptyWhereClause(variable))
      _ <- guard(params.map(_._1).distinct.length == params.length, CompileError.DuplicateParams(DeclRef.LetDecl(variable)))
      _ <- guard(params.forall { case (p, _) => !env.typeVars.contains(p) }, CompileError.ParamAlreadyDefined(DeclRef.LetDecl(variable)))
      typedConstraints <- checkConstraints(DeclRef.LetDecl(variable), constraints, e => e.copy(typeVars = e.typeVars ++ params))
      typedTypes <- types
      declaredType <- expandChecked(typedTypes)
      stripped <- lift(stripLeadingForalls(declaredType, params.length).left.map(got =>
        CompileError.WhereLeadingForalls(variable, params.length, got)
      ))
      _ <- guard(
        destructForAllK(stripped._2).isEmpty,
        CompileError.WherePolymorphic(variable)
      )
      typedValue <-
        if (recursive) value.local((e: Env) => e.copy(values = e.values + (variable -> declaredType)))
        else value
      _ <- expect(declaredType, typeOf(typedValue))
    } yield topLetWhereT(variable, params, typedConstraints, typedTypes, typedValue, recursive)

    case AST.Abs(variable, types, body) => for {
      typedTypes <- types
      paramType <- expandChecked(typedTypes)
      typedBody <- body.local((e: Env) => e.copy(values = e.values + (variable -> paramType)))
      resultType = arrowT(paramType, typeOf(typedBody))
    } yield absT(variable, resultType, typedTypes, typedBody)

    case AST.TyAbs(variable, kind, body) => for {
      env <- ask
      _ <- guard(!env.typeVars.contains(variable), CompileError.AlreadyDefined(NameKind.TypeVariableName, variable.name))
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
        CompileError.AlreadyDefined(NameKind.TypeAliasName, variable.name)
      )
      _ <- guard(params.map(_._1).distinct.length == params.length, CompileError.DuplicateParams(DeclRef.TypeAliasDecl(variable)))
      _ <- guard(params.forall { case (p, _) => !env.typeVars.contains(p) }, CompileError.ParamAlreadyDefined(DeclRef.TypeAliasDecl(variable)))
      typedAlias <- alias.local((e: Env) => e.copy(typeVars = e.typeVars ++ params))
      expandedAlias <- expandChecked(typedAlias).local((e: Env) => e.copy(typeVars = e.typeVars ++ params))
      typedBody <- body.local((e: Env) => e.copy(typeAliases = e.typeAliases + (variable -> TypeAlias(params, expandedAlias))))
      resultType = typeOf(typedBody)
    } yield typeLetT(variable, params, resultType, typedAlias, typedBody)

    case AST.DataLet(variable, params, constructors, body, recursive) => for {
      env <- ask
      _ <- guard(
        !env.typeVars.contains(variable) && !env.typeAliases.contains(variable) && !env.dataTypes.contains(variable),
        CompileError.AlreadyDefined(NameKind.DataTypeName, variable.name)
      )
      _ <- guard(params.map(_._1).distinct.length == params.length, CompileError.DuplicateParams(DeclRef.DataDecl(variable)))
      _ <- guard(params.forall { case (p, _) => !env.typeVars.contains(p) }, CompileError.ParamAlreadyDefined(DeclRef.DataDecl(variable)))
      constructorNames = constructors.map(_.name)
      _ <- guard(constructorNames.distinct.length == constructorNames.length, CompileError.DuplicateMembers(DeclRef.DataDecl(variable), MemberKind.ConstructorMember))
      _ <- guard(
        constructorNames.forall(name => !env.values.contains(name) && !env.constructors.contains(name)),
        CompileError.MemberAlreadyDefined(DeclRef.DataDecl(variable), MemberKind.ConstructorMember)
      )
      placeholder = DataDef(params, Seq.empty, recursive)
      fieldEnv = env.copy(typeVars = env.typeVars ++ params, dataTypes = env.dataTypes + (variable -> placeholder))
      typedConstructors <- constructors.traverse { c =>
        c.fields.traverse(field => field.local((_: Env) => fieldEnv)).map(fs => DataConstructor(c.name, fs))
      }
      expandedConstructors <- typedConstructors.zipWithIndex.traverse { case (c, tag) =>
        c.fields.traverse(field => expandChecked(field).local((_: Env) => fieldEnv)).map(fs => ConstructorDef(c.name, variable, fs, tag))
      }
      _ <- guard(
        recursive || !expandedConstructors.exists(_.fields.exists(field => containsDataApplicationOf(field, variable))),
        CompileError.RecursiveDataNotDeclared(variable)
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
      _ <- guard(caseNames.distinct.length == caseNames.length, CompileError.DuplicateCases(MatchKindRef.MatchExpr))
      expectedConstructors <- lift {
        dataDef.constructors.traverse { cdef =>
          env.constructors.collectFirst { case (name, c) if c == cdef => name }
            .toRight(CompileError.UndefinedDataConstructor(dataName))
        }
      }
      _ <- {
        val missing = expectedConstructors.filterNot(caseNames.contains)
        val extra = caseNames.filterNot(expectedConstructors.contains)
        guard(
          missing.isEmpty && extra.isEmpty,
          CompileError.NonExhaustive(MatchKindRef.MatchExpr, missing, extra)
        )
      }
      typedCases <- cases.traverse { matchCase =>
        val cdef = env.constructors(matchCase.constructor)
        val fieldTypes = cdef.fields.map(field => Equivalence.normalize(substMany(dataDef.paramVars, typeArgs, field)))
        for {
          _ <- guard(
            matchCase.binders.length == fieldTypes.length,
            CompileError.BinderArityMismatch(matchCase.constructor, fieldTypes.length, matchCase.binders.length)
          )
          _ <- guard(matchCase.binders.distinct.length == matchCase.binders.length, CompileError.DuplicateBinders(MatchKindRef.MatchExpr, matchCase.constructor))
          binderTypes = matchCase.binders.zip(fieldTypes).toMap
          typedBody <- matchCase.body.local((e: Env) => e.copy(values = e.values ++ binderTypes))
        } yield MatchCase(matchCase.constructor, matchCase.binders, typedBody)
      }
      resultType <- typedCases.headOption match {
        case Some(first) => typedCases.tail.traverse(c => expect(typeOf(first.body), typeOf(c.body))).as(typeOf(first.body))
        case None => fail(CompileError.EmptyMatch)
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
      _ <- guard(caseNames.distinct.length == caseNames.length, CompileError.DuplicateCases(MatchKindRef.FoldExpr))
      expectedConstructors <- lift {
        dataDef.constructors.traverse { cdef =>
          env.constructors.collectFirst { case (name, c) if c == cdef => name }
            .toRight(CompileError.UndefinedDataConstructor(dataName))
        }
      }
      _ <- {
        val missing = expectedConstructors.filterNot(caseNames.contains)
        val extra = caseNames.filterNot(expectedConstructors.contains)
        guard(
          missing.isEmpty && extra.isEmpty,
          CompileError.NonExhaustive(MatchKindRef.FoldExpr, missing, extra)
        )
      }
      typedCases <- cases.traverse { foldCase =>
        val cdef = env.constructors(foldCase.constructor)
        val fieldTypes = cdef.fields.map(field => Equivalence.normalize(substMany(dataDef.paramVars, typeArgs, field)))
        val binderFieldTypes = fieldTypes.map { field =>
          if (isDataApplicationOf(field, dataName)) resultType else field
        }
        for {
          _ <- guard(
            foldCase.binders.length == fieldTypes.length,
            CompileError.BinderArityMismatch(foldCase.constructor, fieldTypes.length, foldCase.binders.length)
          )
          _ <- guard(foldCase.binders.distinct.length == foldCase.binders.length, CompileError.DuplicateBinders(MatchKindRef.FoldExpr, foldCase.constructor))
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
        case Some((from, to)) if Equivalence.alpha(from, typeOf(typedArgument)) => okT(to)
        case Some((from, _)) => fail(CompileError.TypeMismatch(from, typeOf(typedArgument)))
        case None => fail(CompileError.NotAFunction(typeOf(typedFunction)))
      }
    } yield appT(resultType, typedFunction, typedArgument)

    case AST.TyApp(function, argument) => for {
      typedFunction <- function
      typedArgument <- argument
      expanded <- expandWellKinded(typedArgument)
      (argumentType, argKind) = expanded
      resultType <- destructForAllK(typeOf(typedFunction)) match {
        case Some((variable, expectedKind, bodyType)) =>
          if (expectedKind == argKind) okT(Equivalence.normalize(substType(variable, argumentType, bodyType)))
          else fail(CompileError.KindMismatchInTypeApp(expectedKind, argKind, None))
        case None => fail(CompileError.NotPolymorphic(typeOf(typedFunction)))
      }
    } yield tyAppT(resultType, typedFunction, typedArgument)

    case AST.Foreign(value, types) => for {
      typedTypes <- types
      declaredType <- expandChecked(typedTypes)
      _ <- expectForeignType(declaredType)
    } yield foreignT(value, declaredType, typedTypes)

    case AST.Var(value) => for {
      env <- ask
      t <- lift(env.values.get(value).toRight(CompileError.UndefinedVariable(value)))
    } yield varrType(value, t)

    case AST.Num(value, typeName) =>
      if (BuiltinTypes.numericTypes.contains(typeName)) okT(numT(value, typeName, primitiveT(typeName)))
      else fail(CompileError.NumericLiteralUnknown(typeName))
    case AST.Char(value) => okT(charT(value, charTypeT))
    case AST.StringLit(value) => okT(stringLitT(value, stringTypeT))
    case AST.StrInterp(parts) => for {
      typedParts <- parts.toList.traverse(identity)
      stringified <- typedParts.traverse(stringifyPart)
    } yield stringified match {
      case Nil => stringLitT("", stringTypeT)
      case head :: tail =>
        tail.foldLeft(head) { (acc, part) =>
          val concatRef = foreignT(Variable("concat"), interpConcatType, interpConcatType)
          appT(stringTypeT, appT(arrowT(stringTypeT, stringTypeT), concatRef, acc), part)
        }
    }
    case AST.Bool(value) => okT(boolT(value, boolTypeT))
    case AST.UnitLit() => okT(unitLitT(unitTypeT))

    case AST.Block(discarded, result) => for {
      typedDiscarded <- discarded.traverse(identity)
      typedResult <- result.traverse(identity)
      resultType = typedResult.map(typeOf).getOrElse(unitTypeT)
    } yield blockT(resultType, typedDiscarded, typedResult)

    case AST.Context(monad, bindings, result) => for {
      typedMonad <- monad
      expandedMonad <- expandWellKinded(typedMonad)
      (monadType, monadKind) = expandedMonad
      _ <- guard(
        monadKind == Kind.Arrow(Kind.Star, Kind.Star),
        CompileError.ContextMonadKindInvalid(monadType, monadKind)
      )
      checkedBindings <- bindings.toList.foldLeftM(
        (Map.empty[Variable, TypeRec[Type]], List.empty[ContextBinding[TypeRec]])
      ) { case ((scope, done), b) =>
        val scopeFn = (e: Env) => e.copy(values = e.values ++ scope)
        for {
          typedAnn <- b.annotation.local(scopeFn)
          annType <- expandChecked(typedAnn)
          valueScope =
            if (!b.monadic && b.recursive) (e: Env) => e.copy(values = e.values ++ scope + (b.name -> annType))
            else scopeFn
          typedValue <- b.value.local(valueScope)
          _ <-
            if (b.monadic) expect(typeAppT(monadType, annType), typeOf(typedValue))
            else expect(annType, typeOf(typedValue))
        } yield (scope + (b.name -> annType), done :+ ContextBinding[TypeRec](b.name, annType, typedValue, b.monadic, b.recursive))
      }
      (bindersScope, typedBindings) = checkedBindings
      typedResult <- result.local((e: Env) => e.copy(values = e.values ++ bindersScope))
      env <- ask
      chain <- lift(ContextDesugar.desugarContext(env.values, monadType, typedBindings, typedResult))
    } yield contextExprT(typeOf(chain), monadType, typedBindings, typedResult)

    case AST.BinOp(op, left, right) => for {
      typedLeft <- left
      typedRight <- right
      resolved <- resolveBinaryOperator(op, typedLeft, typedRight)
    } yield resolved

    case AST.Intrinsic(op, args) => for {
      typedArgs <- args.traverse(identity)
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
        case None => fail(CompileError.UndefinedPrimitive(name))
      }
    case AST.TypeVar(variable) => for {
      env <- ask
      _ <- guard(
        env.typeVars.contains(variable) || env.typeAliases.contains(variable) || env.dataTypes.contains(variable),
        CompileError.UndefinedTypeVariable(variable)
      )
    } yield typeVarT(variable)
    case AST.Arrow(from, to) => (from, to).mapN(arrowT)
    case AST.ForAll(variable, kind, body) => for {
      env <- ask
      _ <- guard(!env.typeVars.contains(variable), CompileError.AlreadyDefined(NameKind.TypeVariableName, variable.name))
      typedBody <- body.local((e: Env) => e.copy(typeVars = e.typeVars + (variable -> kind)))
    } yield forallTypeT(variable, kind, typedBody)
    case AST.TypeAbs(variable, kind, body) => for {
      env <- ask
      _ <- guard(!env.typeVars.contains(variable), CompileError.AlreadyDefined(NameKind.TypeVariableName, variable.name))
      typedBody <- body.local((e: Env) => e.copy(typeVars = e.typeVars + (variable -> kind)))
    } yield typeAbsT(variable, kind, typedBody)
    case AST.TypeApp(function, argument) => (function, argument).mapN(typeAppT)
  }

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
      Left(CompileError.UnresolvedImport(path))

    case AST.TopType(variable, params, typedAlias) =>
      val aliasEnv = env.copy(typeVars = env.typeVars ++ params)
      expandAndCheckStar(typedAlias, aliasEnv).map(expandedAlias =>
        env.copy(typeAliases = env.typeAliases + (variable -> TypeAlias(params, expandedAlias))))

    case AST.TopData(variable, params, typedConstructors, recursive) =>
      val placeholder = DataDef(params, Seq.empty, recursive)
      val fieldEnv = env.copy(typeVars = env.typeVars ++ params, dataTypes = env.dataTypes + (variable -> placeholder))
      for {
        expandedConstructors <- typedConstructors.zipWithIndex.traverse { case (c, tag) =>
          c.fields.traverse(field => expandAndCheckStar(field, fieldEnv)).map(fs => ConstructorDef(c.name, variable, fs, tag))
        }
        _ <- Either.cond(
          recursive || !expandedConstructors.exists(_.fields.exists(field => containsDataApplicationOf(field, variable))),
          (),
          CompileError.RecursiveDataNotDeclared(variable)
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

    case AST.TopTrait(variable, params, typedSupers, typedMethods) =>
      val sigEnv = env.copy(typeVars = env.typeVars ++ params)
      for {
        expandedSupers <- expandConstraints(typedSupers, sigEnv)
        expandedSigs <- typedMethods.traverse(m => expandAndCheckStar(m.sig, sigEnv).map(sig => m.name -> sig))
      } yield {
        val ctorName = dictionaryConstructor(variable)
        val superFields = expandedSupers.map(tc => applyTypeConstructor(tc.name, tc.arg))
        val fields = superFields ++ expandedSigs.map(_._2)
        val ctorDef = ConstructorDef(ctorName, variable, fields, 0)
        val constraint = TypeConstraint(variable, params.map { case (p, _) => typeVarT(p) })
        val surfaceTypes = expandedSigs.map { case (name, sig) =>
          name -> params.foldRight(sig) { case ((p, k), acc) => forallTypeT(p, k, acc) }
        }
        env.copy(
          values = env.values ++ surfaceTypes + (ctorName -> constructorType(variable, params, fields)),
          dataTypes = env.dataTypes + (variable -> DataDef(params, Seq(ctorDef), recursive = false)),
          constructors = env.constructors + (ctorName -> ctorDef),
          traits = env.traits + (variable -> TraitDef(params, expandedSigs, expandedSupers)),
          constrains = env.constrains ++ expandedSigs.map { case (name, _) => name -> Seq(constraint) }
        )
      }

    case AST.TopImpl(traitName, implParams, typedTargets, typedContext, _) =>
      val scope = env.copy(typeVars = env.typeVars ++ implParams)
      for {
        expandedTargets <- typedTargets.traverse(t => expandType(t).run(scope))
        headNames <- expandedTargets.traverse { expanded =>
          typeConstructorHead(expanded).map(_._1).toRight(
            CompileError.InstanceHeadInvalid(traitName, expanded)
          )
        }
        key = instanceKey(traitName, headNames)
        _ <- Either.cond(
          !env.instances.contains(key), (),
          CompileError.OverlappingInstance(traitName, headNames)
        )
        expandedContext <- expandConstraints(typedContext, scope)
        dictName = instanceDictionaryName(traitName, headNames)
        inst = InstanceDef(traitName, expandedTargets, expandedContext, dictName, implParams)
      } yield env.copy(
        instances = env.instances + (key -> inst),
        values = env.values + (dictName -> instanceType(inst))
      )

    case AST.TopLetWhere(variable, params, typedConstraints, typedTypes, _, _) =>
      val scope = env.copy(typeVars = env.typeVars ++ params)
      for {
        declaredType <- expandAndCheckStar(typedTypes, env)
        expandedConstraints <- expandConstraints(typedConstraints, scope)
      } yield env.copy(
        values = env.values + (variable -> declaredType),
        constrains = env.constrains + (variable -> expandedConstraints)
      )
  }

  private def expandConstraints(constraints: Seq[Constraint[TypeRec]], env: Env): EitherS[Seq[TypeConstraint]] =
    constraints.traverse(c => c.arg.traverse(a => expandType(a).run(env)).map(TypeConstraint(c.name, _)))

  private def checkMain(env: Env): EitherS[Unit] = env.values.get(Variable("main")) match {
    case Some(t) if Equivalence.alpha(t, arrowT(unitTypeT, intTypeT)) => Right(())
    case Some(t) => Left(CompileError.MainTypeInvalid(t))
    case None => Left(CompileError.MainMissing)
  }

  def validate(prog: Rec[AST.Program.type]): Either[CompileError, TypeRec[AST.Program.type]] = prog.unfix match {
    case AST.Program(decls) => decls.traverse(checkDecl).run(Env.empty).flatMap { case (env, typedDecls) =>
      checkMain(env).as(programT(typedDecls, env))
    }
  }

}
