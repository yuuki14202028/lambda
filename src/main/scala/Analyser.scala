package com.yuuki14202028

import cats.syntax.traverse._
import cats.data.ReaderT
import cats.implicits.catsSyntaxTuple2Semigroupal

object Analyser {

  case class TypeAlias(params: Seq[TypeVariable], body: Rec[Type])
  case class DataTypeDef(params: Seq[TypeVariable], constructors: Seq[ConstructorDef])
  case class ConstructorDef(owner: TypeVariable, fields: Seq[Rec[Type]], tag: Int)
  case class Env(
      values: Map[Variable, Rec[Type]],
      typeVars: Set[TypeVariable],
      typeAliases: Map[TypeVariable, TypeAlias],
      dataTypes: Map[TypeVariable, DataTypeDef],
      constructors: Map[Variable, ConstructorDef]
  )
  type EitherS[A] = Either[String, A]
  type TC[I] = ReaderT[EitherS, Env, TypeRec[I]]

  private def expect(expected: Rec[Type], actual: Rec[Type]): EitherS[Unit] = {
    Either.cond(sameType(expected, actual), (), s"Type mismatch: expected ${expected.show}, actual ${actual.show}")
  }

  private def expectNumeric(actual: Rec[Type]): EitherS[Unit] = {
    Either.cond(isNumericType(actual), (), s"Type mismatch: expected Int or Char, actual $actual")
  }

  private def expectEquatable(actual: Rec[Type]): EitherS[Unit] = {
    Either.cond(isEquatableType(actual), (), s"Type mismatch: expected Int, Char, or Bool, actual $actual")
  }

  private def expectForeignType(t: Rec[Type]): EitherS[Unit] = {
    destructArrow(t) match {
      case Some((_, to)) if destructArrow(to).isEmpty => Right(())
      case _ => Left(s"Foreign function must have exactly one argument: ${t.show}")
    }
  }

  private def collectTypeApps(t: Rec[Type]): (Rec[Type], Seq[Rec[Type]]) = t.unfix match {
    case AST.TypeApp(function, argument) =>
      val (head, args) = collectTypeApps(function)
      (head, args :+ argument)
    case _ => (t, Seq.empty)
  }

  private def applyTypeConstructor(head: TypeVariable, args: Seq[Rec[Type]]): Rec[Type] =
    args.foldLeft(typeVar(head))(typeApp)

  private def dataResultType(owner: TypeVariable, params: Seq[TypeVariable]): Rec[Type] =
    applyTypeConstructor(owner, params.map(typeVar))

  private def constructorType(owner: TypeVariable, params: Seq[TypeVariable], fields: Seq[Rec[Type]]): Rec[Type] = {
    val result = dataResultType(owner, params)
    val functionType = fields.foldRight(result)(arrow)
    params.foldRight(functionType)(forallType)
  }

  private def dataTypeApplication(t: Rec[Type], env: Env): EitherS[(TypeVariable, DataTypeDef, Seq[Rec[Type]])] = {
    val (head, args) = collectTypeApps(t)
    head.unfix match {
      case AST.TypeVar(variable) =>
        env.dataTypes.get(variable) match {
          case Some(dataDef) if dataDef.params.length == args.length =>
            Right((variable, dataDef, args))
          case Some(dataDef) =>
            Left(s"Data type ${variable.name} expects ${dataDef.params.length} arguments, got ${args.length}")
          case None =>
            Left(s"Not a data type: ${t.show}")
        }
      case _ =>
        Left(s"Not a data type: ${t.show}")
    }
  }

  private def expandType(t: Rec[Type], env: Env): EitherS[Rec[Type]] = t.unfix match {
    case AST.Primitive("Int") => Right(intType)
    case AST.Primitive("Char") => Right(charType)
    case AST.Primitive("String") => Right(stringType)
    case AST.Primitive("Bool") => Right(boolType)
    case AST.Primitive("Unit") => Right(unitType)
    case AST.Primitive(name) => Left(s"Primitive type $name is not defined")

    case AST.TypeVar(variable) if env.typeVars.contains(variable) => Right(typeVar(variable))
    case AST.TypeVar(variable) =>
      env.typeAliases.get(variable) match {
        case Some(TypeAlias(params, body)) if params.isEmpty => expandType(body, env)
        case Some(TypeAlias(params, _)) => Left(s"Type alias ${variable.name} expects ${params.length} arguments, got 0")
        case None =>
          env.dataTypes.get(variable) match {
            case Some(DataTypeDef(params, _)) if params.isEmpty => Right(typeVar(variable))
            case Some(DataTypeDef(params, _)) => Left(s"Data type ${variable.name} expects ${params.length} arguments, got 0")
            case None => Left(s"Type variable ${variable.name} is not defined")
          }
      }

    case AST.Arrow(from, to) => for {
      expandedFrom <- expandType(from, env)
      expandedTo <- expandType(to, env)
    } yield arrow(expandedFrom, expandedTo)

    case AST.ForAll(variable, body) => for {
      _ <- Either.cond(!env.typeVars.contains(variable), (), s"Type variable ${variable.name} is already defined")
      expandedBody <- expandType(body, env.copy(typeVars = env.typeVars + variable))
    } yield forallType(variable, expandedBody)

    case AST.TypeApp(_, _) =>
      val (head, args) = collectTypeApps(t)
      head.unfix match {
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
                  args.toList.traverse(arg => expandType(arg, env)).map(expandedArgs => applyTypeConstructor(variable, expandedArgs))
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

  private def expandCheckedType(types: TypeRec[Type]): ReaderT[EitherS, Env, Rec[Type]] = for {
    env <- ReaderT.ask[EitherS, Env]
    expanded <- ReaderT.liftF[EitherS, Env, Rec[Type]](expandType(eraseAnn[Type](types), env))
  } yield expanded

  private val tcAlg: Algebra[AST, TC] = [x] => (node: AST[TC, x]) => node match {

    case AST.Program(body) => body.traverse(identity).map(programT)

    case AST.Abs(variable, types, body) => for {
      typedTypes <- types
      paramType <- expandCheckedType(typedTypes)
      typedBody <- body.local((env: Env) => env.copy(values = env.values + (variable -> paramType)))
      resultType = arrow(paramType, typeOf(typedBody))
    } yield absT(variable, resultType, typedTypes, typedBody)

    case AST.TyAbs(variable, body) => for {
      env <- ReaderT.ask[EitherS, Env]
      _ <- ReaderT.liftF[EitherS, Env, Unit](
        Either.cond(!env.typeVars.contains(variable), (), s"Type variable ${variable.name} is already defined")
      )
      typedBody <- body.local((env: Env) => env.copy(typeVars = env.typeVars + variable))
      resultType = forallType(variable, typeOf(typedBody))
    } yield tyAbsT(variable, resultType, typedBody)

    case AST.Let(variable, types, value, body) => for {
      typedTypes <- types
      declaredType <- expandCheckedType(typedTypes)
      typedValue <- value
      _ <- ReaderT.liftF(expect(declaredType, typeOf(typedValue)))
      typedBody <- body.local((env: Env) => env.copy(values = env.values + (variable -> declaredType)))
      resultType = typeOf(typedBody)
    } yield letT(variable, resultType, typedTypes, typedValue, typedBody)

    case AST.LetRec(variable, types, value, body) => for {
      typedTypes <- types
      declaredType <- expandCheckedType(typedTypes)
      typedValue <- value.local((env: Env) => env.copy(values = env.values + (variable -> declaredType)))
      _ <- ReaderT.liftF(expect(declaredType, typeOf(typedValue)))
      typedBody <- body.local((env: Env) => env.copy(values = env.values + (variable -> declaredType)))
      resultType = typeOf(typedBody)
    } yield letRecT(variable, resultType, typedTypes, typedValue, typedBody)

    case AST.TypeLet(variable, params, alias, body) => for {
      env <- ReaderT.ask[EitherS, Env]
      _ <- ReaderT.liftF[EitherS, Env, Unit](
        Either.cond(
          !env.typeVars.contains(variable) && !env.typeAliases.contains(variable) && !env.dataTypes.contains(variable),
          (),
          s"Type alias ${variable.name} is already defined"
        )
      )
      _ <- ReaderT.liftF[EitherS, Env, Unit](
        Either.cond(params.distinct.length == params.length, (), s"Type alias ${variable.name} has duplicate parameters")
      )
      _ <- ReaderT.liftF[EitherS, Env, Unit](
        Either.cond(params.forall(p => !env.typeVars.contains(p)), (), s"Type alias ${variable.name} has a parameter that is already defined")
      )
      typedAlias <- alias.local((env: Env) => env.copy(typeVars = env.typeVars ++ params))
      expandedAlias <- ReaderT.liftF[EitherS, Env, Rec[Type]](
        expandType(eraseAnn[Type](typedAlias), env.copy(typeVars = env.typeVars ++ params))
      )
      typedBody <- body.local((env: Env) =>
        env.copy(typeAliases = env.typeAliases + (variable -> TypeAlias(params, expandedAlias)))
      )
      resultType = typeOf(typedBody)
    } yield typeLetT(variable, params, resultType, typedAlias, typedBody)

    case AST.DataLet(variable, params, constructors, body) => for {
      env <- ReaderT.ask[EitherS, Env]
      _ <- ReaderT.liftF[EitherS, Env, Unit](
        Either.cond(
          !env.typeVars.contains(variable) && !env.typeAliases.contains(variable) && !env.dataTypes.contains(variable),
          (),
          s"Data type ${variable.name} is already defined"
        )
      )
      _ <- ReaderT.liftF[EitherS, Env, Unit](
        Either.cond(params.distinct.length == params.length, (), s"Data type ${variable.name} has duplicate parameters")
      )
      _ <- ReaderT.liftF[EitherS, Env, Unit](
        Either.cond(params.forall(p => !env.typeVars.contains(p)), (), s"Data type ${variable.name} has a parameter that is already defined")
      )
      constructorNames = constructors.map(_.name)
      _ <- ReaderT.liftF[EitherS, Env, Unit](
        Either.cond(constructorNames.distinct.length == constructorNames.length, (), s"Data type ${variable.name} has duplicate constructors")
      )
      _ <- ReaderT.liftF[EitherS, Env, Unit](
        Either.cond(
          constructorNames.forall(name => !env.values.contains(name) && !env.constructors.contains(name)),
          (),
          s"Data type ${variable.name} has a constructor that is already defined"
        )
      )
      placeholder = DataTypeDef(params, Seq.empty)
      fieldEnv = env.copy(typeVars = env.typeVars ++ params, dataTypes = env.dataTypes + (variable -> placeholder))
      typedConstructors <- constructors.toList.traverse { constructor =>
        constructor.fields.toList.traverse(field => field.local(_ => fieldEnv)).map { typedFields =>
          DataConstructor(constructor.name, typedFields)
        }
      }
      expandedConstructors <- ReaderT.liftF[EitherS, Env, Seq[ConstructorDef]] {
        typedConstructors.zipWithIndex.toList.traverse { case (constructor, tag) =>
          constructor.fields.toList.traverse(field => expandType(eraseAnn[Type](field), fieldEnv)).map { fields =>
            ConstructorDef(variable, fields, tag)
          }
        }
      }
      dataDef = DataTypeDef(params, expandedConstructors)
      constructorDefs = constructorNames.zip(expandedConstructors).toMap
      constructorTypes = constructorNames.zip(expandedConstructors).map { case (name, constructorDef) =>
        name -> constructorType(variable, params, constructorDef.fields)
      }.toMap
      typedBody <- body.local((env: Env) =>
        env.copy(
          values = env.values ++ constructorTypes,
          dataTypes = env.dataTypes + (variable -> dataDef),
          constructors = env.constructors ++ constructorDefs
        )
      )
      resultType = typeOf(typedBody)
    } yield dataLetT(variable, params, resultType, typedConstructors, typedBody)

    case AST.Match(scrutinee, cases) => for {
      env <- ReaderT.ask[EitherS, Env]
      typedScrutinee <- scrutinee
      dataApp <- ReaderT.liftF[EitherS, Env, (TypeVariable, DataTypeDef, Seq[Rec[Type]])](
        dataTypeApplication(typeOf(typedScrutinee), env)
      )
      (dataName, dataDef, typeArgs) = dataApp
      caseNames = cases.map(_.constructor)
      _ <- ReaderT.liftF[EitherS, Env, Unit](
        Either.cond(caseNames.distinct.length == caseNames.length, (), s"Match has duplicate cases")
      )
      expectedConstructors <- ReaderT.liftF[EitherS, Env, Seq[Variable]] {
        dataDef.constructors.toList.traverse { constructorDef =>
          env.constructors.collectFirst {
            case (name, candidate) if candidate == constructorDef => name
          }.toRight(s"Constructor for data type ${dataName.name} is not defined")
        }
      }
      _ <- ReaderT.liftF[EitherS, Env, Unit] {
        val missing = expectedConstructors.filterNot(caseNames.contains)
        val extra = caseNames.filterNot(expectedConstructors.contains)
        Either.cond(
          missing.isEmpty && extra.isEmpty,
          (),
          s"Non-exhaustive or invalid match: missing ${missing.map(_.name).mkString(", ")}, invalid ${extra.map(_.name).mkString(", ")}"
        )
      }
      typedCases <- cases.toList.traverse { matchCase =>
        val constructorDef = env.constructors(matchCase.constructor)
        val fieldTypes = dataDef.params.zip(typeArgs).foldLeft(constructorDef.fields) { case (fields, (param, arg)) =>
          fields.map(field => substType(param, arg, field))
        }
        for {
          _ <- ReaderT.liftF[EitherS, Env, Unit](
            Either.cond(
              matchCase.binders.length == fieldTypes.length,
              (),
              s"Constructor ${matchCase.constructor.name} expects ${fieldTypes.length} binders, got ${matchCase.binders.length}"
            )
          )
          _ <- ReaderT.liftF[EitherS, Env, Unit](
            Either.cond(matchCase.binders.distinct.length == matchCase.binders.length, (), s"Match case ${matchCase.constructor.name} has duplicate binders")
          )
          binderTypes = matchCase.binders.zip(fieldTypes).toMap
          typedBody <- matchCase.body.local((env: Env) => env.copy(values = env.values ++ binderTypes))
        } yield MatchCase(matchCase.constructor, matchCase.binders, typedBody)
      }
      resultType <- ReaderT.liftF[EitherS, Env, Rec[Type]] {
        typedCases.headOption match {
          case Some(first) =>
            typedCases.tail.traverse(c => expect(typeOf(first.body), typeOf(c.body))).map(_ => typeOf(first.body))
          case None =>
            Left("Match must have at least one case")
        }
      }
    } yield matchExprT(resultType, typedScrutinee, typedCases)

    case AST.App(function, argument) => for {
      typedFunction <- function
      typedArgument <- argument
      resultType <- ReaderT.liftF[EitherS, Env, Rec[Type]] {
        destructArrow(typeOf(typedFunction)) match {
          case Some((from, to)) if sameType(from, typeOf(typedArgument)) => Right(to)
          case Some((from, _)) => Left(s"Type mismatch: expected ${from.show}, actual ${typeOf(typedArgument).show}")
          case None => Left(s"Not a function: ${typeOf(typedFunction).show}")
        }
      }
    } yield appT(resultType, typedFunction, typedArgument)

    case AST.TyApp(function, argument) => for {
      typedFunction <- function
      typedArgument <- argument
      argumentType <- expandCheckedType(typedArgument)
      resultType <- ReaderT.liftF[EitherS, Env, Rec[Type]] {
        destructForAll(typeOf(typedFunction)) match {
          case Some((variable, bodyType)) => Right(substType(variable, argumentType, bodyType))
          case None => Left(s"Not a polymorphic function: ${typeOf(typedFunction).show}")
        }
      }
    } yield tyAppT(resultType, typedFunction, typedArgument)

    case AST.Foreign(value, types) => for {
      typedTypes <- types
      declaredType <- expandCheckedType(typedTypes)
      _ <- ReaderT.liftF(expectForeignType(declaredType))
    } yield foreignT(value, declaredType, typedTypes)

    case AST.Var(value) => for {
      env <- ReaderT.ask[EitherS, Env]
      t <- ReaderT.liftF[EitherS, Env, Rec[Type]](
        env.values.get(value).toRight(s"Variable $value is not defined")
      )
    } yield varrType(value, t)

    case AST.Num(value) => ReaderT.pure(numT(value, intType))
    case AST.Char(value) => ReaderT.pure(charT(value, charType))
    case AST.StringLit(value) => ReaderT.pure(stringLitT(value, stringType))
    case AST.Bool(value) => ReaderT.pure(boolT(value, boolType))
    case AST.UnitLit() => ReaderT.pure(unitLitT(unitType))

    case AST.Block(discarded, result) => for {
      typedDiscarded <- discarded.toList.traverse(identity)
      typedResult <- result.traverse(identity)
      resultType = typedResult.map(typeOf).getOrElse(unitType)
    } yield blockT(resultType, typedDiscarded, typedResult)

    case AST.BinOp(op, left, right) => for {
      typedLeft <- left
      typedRight <- right
      leftType = typeOf(typedLeft)
      rightType = typeOf(typedRight)
      _ <- ReaderT.liftF(op match {
        case BinOps.Add | BinOps.Sub | BinOps.Mul | BinOps.Div => expectNumeric(leftType)
        case BinOps.Eq | BinOps.Neq => expectEquatable(leftType)
        case BinOps.Lt | BinOps.Leq | BinOps.Gt | BinOps.Geq => expectNumeric(leftType)
      })
      _ <- ReaderT.liftF(expect(leftType, rightType))
      resultType = op match {
        case BinOps.Add | BinOps.Sub | BinOps.Mul | BinOps.Div => leftType
        case BinOps.Eq | BinOps.Neq | BinOps.Lt | BinOps.Leq | BinOps.Gt | BinOps.Geq => boolType
      }
    } yield binopT(op, resultType, typedLeft, typedRight)

    case AST.UnaryOp(op, body) => for {
      typedBody <- body
      bodyType = typeOf(typedBody)
      _ <- ReaderT.liftF(op match {
        case UnaryOps.Neg => expectNumeric(bodyType)
        case UnaryOps.Not => expect(boolType, bodyType)
      })
    } yield unopT(op, bodyType, typedBody)

    case AST.If(cond, thenBranch, elseBranch) => for {
      typedCond <- cond
      typedThen <- thenBranch
      typedElse <- elseBranch
      condType = typeOf(typedCond)
      thenType = typeOf(typedThen)
      elseType = typeOf(typedElse)
      _ <- ReaderT.liftF(expect(boolType, condType))
      _ <- ReaderT.liftF(expect(thenType, elseType))
    } yield ifT(thenType, typedCond, typedThen, typedElse)

    case AST.Primitive("Int") => ReaderT.pure(primitiveT("Int"))
    case AST.Primitive("Char") => ReaderT.pure(primitiveT("Char"))
    case AST.Primitive("String") => ReaderT.pure(primitiveT("String"))
    case AST.Primitive("Bool") => ReaderT.pure(primitiveT("Bool"))
    case AST.Primitive("Unit") => ReaderT.pure(primitiveT("Unit"))
    case AST.Primitive(name) => ReaderT.liftF(Left(s"Primitive type $name is not defined"))
    case AST.TypeVar(variable) => for {
      env <- ReaderT.ask[EitherS, Env]
      _ <- ReaderT.liftF[EitherS, Env, Unit](
        Either.cond(
          env.typeVars.contains(variable) || env.typeAliases.contains(variable) || env.dataTypes.contains(variable),
          (),
          s"Type variable ${variable.name} is not defined"
        )
      )
    } yield typeVarT(variable)
    case AST.Arrow(from, to) => (from, to).mapN(arrowT)
    case AST.ForAll(variable, body) => for {
      env <- ReaderT.ask[EitherS, Env]
      _ <- ReaderT.liftF[EitherS, Env, Unit](
        Either.cond(!env.typeVars.contains(variable), (), s"Type variable ${variable.name} is already defined")
      )
      typedBody <- body.local((env: Env) => env.copy(typeVars = env.typeVars + variable))
    } yield forallTypeT(variable, typedBody)
    case AST.TypeApp(function, argument) => (function, argument).mapN(typeAppT)
  }

  def validate(prog: Rec[AST.Program.type]): EitherS[TypeRec[AST.Program.type]] = {
    prog.cata(tcAlg).run(Env(Map.empty, Set.empty, Map.empty, Map.empty, Map.empty))
  }

}
