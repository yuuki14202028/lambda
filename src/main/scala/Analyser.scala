package com.yuuki14202028

import cats.syntax.all._
import cats.data.ReaderT

object Analyser {

  case class TypeAlias(params: Seq[TypeVariable], body: TypeRec[Type])
  case class DataTypeDef(params: Seq[TypeVariable], constructors: Seq[ConstructorDef])
  case class ConstructorDef(owner: TypeVariable, fields: Seq[TypeRec[Type]], tag: Int)
  case class Env(
      values: Map[Variable, TypeRec[Type]],
      typeVars: Set[TypeVariable],
      typeAliases: Map[TypeVariable, TypeAlias],
      dataTypes: Map[TypeVariable, DataTypeDef],
      constructors: Map[Variable, ConstructorDef]
  )
  type EitherS[A] = Either[String, A]
  type Check[A] = ReaderT[EitherS, Env, A]
  type TC[I] = Check[TypeRec[I]]

  private def fail[A](msg: String): Check[A] = ReaderT.liftF(Left(msg))
  private def guard(cond: Boolean, msg: => String): Check[Unit] = ReaderT.liftF(Either.cond(cond, (), msg))
  private def lift[A](e: EitherS[A]): Check[A] = ReaderT.liftF(e)
  private val ask: Check[Env] = ReaderT.ask[EitherS, Env]
  private def okT[I](t: TypeRec[I]): TC[I] = ReaderT.pure(t)

  private def expect(expected: TypeRec[Type], actual: TypeRec[Type]): Check[Unit] =
    guard(sameType(expected, actual), s"Type mismatch: expected ${expected.show}, actual ${actual.show}")

  private def expectNumeric(actual: TypeRec[Type]): Check[Unit] =
    guard(isNumericType(actual), s"Type mismatch: expected Int or Char, actual ${actual.show}")

  private def expectEquatable(actual: TypeRec[Type]): Check[Unit] =
    guard(isEquatableType(actual), s"Type mismatch: expected Int, Char, or Bool, actual ${actual.show}")

  private def expectForeignType(t: TypeRec[Type]): Check[Unit] = destructArrow(t) match {
    case Some((_, to)) if destructArrow(to).isEmpty => ReaderT.pure(())
    case _ => fail(s"Foreign function must have exactly one argument: ${t.show}")
  }

  private def dataResultType(owner: TypeVariable, params: Seq[TypeVariable]): TypeRec[Type] =
    applyTypeConstructor(owner, params.map(typeVarT))

  private def constructorType(owner: TypeVariable, params: Seq[TypeVariable], fields: Seq[TypeRec[Type]]): TypeRec[Type] = {
    val result = dataResultType(owner, params)
    val functionType = fields.foldRight(result)(arrowT)
    params.foldRight(functionType)(forallTypeT)
  }

  private def dataTypeApplication(t: TypeRec[Type], env: Env): EitherS[(TypeVariable, DataTypeDef, Seq[TypeRec[Type]])] = {
    val (head, args) = collectTypeApps(t)
    head.projectT match {
      case AST.TypeVar(variable) =>
        env.dataTypes.get(variable) match {
          case Some(dataDef) if dataDef.params.length == args.length => Right((variable, dataDef, args))
          case Some(dataDef) => Left(s"Data type ${variable.name} expects ${dataDef.params.length} arguments, got ${args.length}")
          case None => Left(s"Not a data type: ${t.show}")
        }
      case _ => Left(s"Not a data type: ${t.show}")
    }
  }

  private def expandType(t: TypeRec[Type], env: Env): EitherS[TypeRec[Type]] = t.projectT match {
    case AST.Primitive("Int") => Right(intTypeT)
    case AST.Primitive("Char") => Right(charTypeT)
    case AST.Primitive("String") => Right(stringTypeT)
    case AST.Primitive("Bool") => Right(boolTypeT)
    case AST.Primitive("Unit") => Right(unitTypeT)
    case AST.Primitive(name) => Left(s"Primitive type $name is not defined")

    case AST.TypeVar(variable) if env.typeVars.contains(variable) => Right(typeVarT(variable))
    case AST.TypeVar(variable) =>
      env.typeAliases.get(variable) match {
        case Some(TypeAlias(params, body)) if params.isEmpty => expandType(body, env)
        case Some(TypeAlias(params, _)) => Left(s"Type alias ${variable.name} expects ${params.length} arguments, got 0")
        case None =>
          env.dataTypes.get(variable) match {
            case Some(DataTypeDef(params, _)) if params.isEmpty => Right(typeVarT(variable))
            case Some(DataTypeDef(params, _)) => Left(s"Data type ${variable.name} expects ${params.length} arguments, got 0")
            case None => Left(s"Type variable ${variable.name} is not defined")
          }
      }

    case AST.Arrow(from, to) => for {
      ef <- expandType(from, env)
      et <- expandType(to, env)
    } yield arrowT(ef, et)

    case AST.ForAll(variable, body) => for {
      _ <- Either.cond(!env.typeVars.contains(variable), (), s"Type variable ${variable.name} is already defined")
      eb <- expandType(body, env.copy(typeVars = env.typeVars + variable))
    } yield forallTypeT(variable, eb)

    case AST.TypeApp(_, _) =>
      val (head, args) = collectTypeApps(t)
      head.projectT match {
        case AST.TypeVar(variable) if !env.typeVars.contains(variable) =>
          env.typeAliases.get(variable) match {
            case Some(TypeAlias(params, body)) if params.length == args.length =>
              for {
                expandedArgs <- args.toList.traverse(arg => expandType(arg, env))
                substituted = params.zip(expandedArgs).foldLeft(body) { case (acc, (param, arg)) =>
                  substType(param, arg, acc)
                }
                expanded <- expandType(substituted, env)
              } yield expanded
            case Some(TypeAlias(params, _)) =>
              Left(s"Type alias ${variable.name} expects ${params.length} arguments, got ${args.length}")
            case None =>
              env.dataTypes.get(variable) match {
                case Some(DataTypeDef(params, _)) if params.length == args.length =>
                  args.toList.traverse(arg => expandType(arg, env)).map(es => applyTypeConstructor(variable, es))
                case Some(DataTypeDef(params, _)) =>
                  Left(s"Data type ${variable.name} expects ${params.length} arguments, got ${args.length}")
                case None =>
                  Left(s"Type variable ${variable.name} is not defined")
              }
          }
        case _ =>
          Left(s"Type application is not supported: ${t.show}")
      }
  }

  private def expandChecked(types: TypeRec[Type]): Check[TypeRec[Type]] =
    ask.flatMap(env => lift(expandType(types, env)))

  private val tcAlg: Algebra[AST, TC] = [x] => (node: AST[TC, x]) => node match {

    case AST.Program(body) => body.toList.traverse(identity).map(programT)

    case AST.Abs(variable, types, body) => for {
      typedTypes <- types
      paramType <- expandChecked(typedTypes)
      typedBody <- body.local((e: Env) => e.copy(values = e.values + (variable -> paramType)))
      resultType = arrowT(paramType, typeOf(typedBody))
    } yield absT(variable, resultType, typedTypes, typedBody)

    case AST.TyAbs(variable, body) => for {
      env <- ask
      _ <- guard(!env.typeVars.contains(variable), s"Type variable ${variable.name} is already defined")
      typedBody <- body.local((e: Env) => e.copy(typeVars = e.typeVars + variable))
      resultType = forallTypeT(variable, typeOf(typedBody))
    } yield tyAbsT(variable, resultType, typedBody)

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
      _ <- guard(params.distinct.length == params.length, s"Type alias ${variable.name} has duplicate parameters")
      _ <- guard(params.forall(p => !env.typeVars.contains(p)), s"Type alias ${variable.name} has a parameter that is already defined")
      typedAlias <- alias.local((e: Env) => e.copy(typeVars = e.typeVars ++ params))
      expandedAlias <- lift(expandType(typedAlias, env.copy(typeVars = env.typeVars ++ params)))
      typedBody <- body.local((e: Env) => e.copy(typeAliases = e.typeAliases + (variable -> TypeAlias(params, expandedAlias))))
      resultType = typeOf(typedBody)
    } yield typeLetT(variable, params, resultType, typedAlias, typedBody)

    case AST.DataLet(variable, params, constructors, body) => for {
      env <- ask
      _ <- guard(
        !env.typeVars.contains(variable) && !env.typeAliases.contains(variable) && !env.dataTypes.contains(variable),
        s"Data type ${variable.name} is already defined"
      )
      _ <- guard(params.distinct.length == params.length, s"Data type ${variable.name} has duplicate parameters")
      _ <- guard(params.forall(p => !env.typeVars.contains(p)), s"Data type ${variable.name} has a parameter that is already defined")
      constructorNames = constructors.map(_.name)
      _ <- guard(constructorNames.distinct.length == constructorNames.length, s"Data type ${variable.name} has duplicate constructors")
      _ <- guard(
        constructorNames.forall(name => !env.values.contains(name) && !env.constructors.contains(name)),
        s"Data type ${variable.name} has a constructor that is already defined"
      )
      placeholder = DataTypeDef(params, Seq.empty)
      fieldEnv = env.copy(typeVars = env.typeVars ++ params, dataTypes = env.dataTypes + (variable -> placeholder))
      typedConstructors <- constructors.toList.traverse { c =>
        c.fields.toList.traverse(field => field.local((_: Env) => fieldEnv)).map(fs => DataConstructor(c.name, fs))
      }
      expandedConstructors <- lift {
        typedConstructors.zipWithIndex.toList.traverse { case (c, tag) =>
          c.fields.toList.traverse(field => expandType(field, fieldEnv)).map(fs => ConstructorDef(variable, fs, tag))
        }
      }
      dataDef = DataTypeDef(params, expandedConstructors)
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
    } yield dataLetT(variable, params, resultType, typedConstructors, typedBody)

    case AST.Match(scrutinee, cases) => for {
      env <- ask
      typedScrutinee <- scrutinee
      dataApp <- lift(dataTypeApplication(typeOf(typedScrutinee), env))
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
        val fieldTypes = dataDef.params.zip(typeArgs).foldLeft(cdef.fields) { case (fields, (param, arg)) =>
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

    case AST.App(function, argument) => for {
      typedFunction <- function
      typedArgument <- argument
      resultType <- destructArrow(typeOf(typedFunction)) match {
        case Some((from, to)) if sameType(from, typeOf(typedArgument)) => okT(to)
        case Some((from, _)) => fail(s"Type mismatch: expected ${from.show}, actual ${typeOf(typedArgument).show}")
        case None => fail(s"Not a function: ${typeOf(typedFunction).show}")
      }
    } yield appT(resultType, typedFunction, typedArgument)

    case AST.TyApp(function, argument) => for {
      typedFunction <- function
      typedArgument <- argument
      argumentType <- expandChecked(typedArgument)
      resultType <- destructForAll(typeOf(typedFunction)) match {
        case Some((variable, bodyType)) => okT(substType(variable, argumentType, bodyType))
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

    case AST.Num(value) => okT(numT(value, intTypeT))
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
      leftType = typeOf(typedLeft)
      rightType = typeOf(typedRight)
      _ <- op match {
        case BinOps.Add | BinOps.Sub | BinOps.Mul | BinOps.Div => expectNumeric(leftType)
        case BinOps.Eq | BinOps.Neq => expectEquatable(leftType)
        case BinOps.Lt | BinOps.Leq | BinOps.Gt | BinOps.Geq => expectNumeric(leftType)
      }
      _ <- expect(leftType, rightType)
      resultType = op match {
        case BinOps.Add | BinOps.Sub | BinOps.Mul | BinOps.Div => leftType
        case BinOps.Eq | BinOps.Neq | BinOps.Lt | BinOps.Leq | BinOps.Gt | BinOps.Geq => boolTypeT
      }
    } yield binopT(op, resultType, typedLeft, typedRight)

    case AST.UnaryOp(op, body) => for {
      typedBody <- body
      bodyType = typeOf(typedBody)
      _ <- op match {
        case UnaryOps.Neg => expectNumeric(bodyType)
        case UnaryOps.Not => expect(boolTypeT, bodyType)
      }
    } yield unopT(op, bodyType, typedBody)

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

    case AST.Primitive(name) => name match {
      case "Int" | "Char" | "String" | "Bool" | "Unit" => okT(primitiveT(name))
      case _ => fail(s"Primitive type $name is not defined")
    }
    case AST.TypeVar(variable) => for {
      env <- ask
      _ <- guard(
        env.typeVars.contains(variable) || env.typeAliases.contains(variable) || env.dataTypes.contains(variable),
        s"Type variable ${variable.name} is not defined"
      )
    } yield typeVarT(variable)
    case AST.Arrow(from, to) => (from, to).mapN(arrowT)
    case AST.ForAll(variable, body) => for {
      env <- ask
      _ <- guard(!env.typeVars.contains(variable), s"Type variable ${variable.name} is already defined")
      typedBody <- body.local((e: Env) => e.copy(typeVars = e.typeVars + variable))
    } yield forallTypeT(variable, typedBody)
    case AST.TypeApp(function, argument) => (function, argument).mapN(typeAppT)
  }

  def validate(prog: Rec[AST.Program.type]): EitherS[TypeRec[AST.Program.type]] = {
    prog.cata(tcAlg).run(Env(Map.empty, Set.empty, Map.empty, Map.empty, Map.empty))
  }

}