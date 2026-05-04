package com.yuuki14202028

import cats.syntax.traverse._

object ChurchEncoder {
  case class DataDef(params: Seq[TypeVariable], constructors: Seq[ConstructorDef])
  case class ConstructorDef(name: Variable, fields: Seq[TypeRec[Type]], tag: Int)
  case class Env(dataTypes: Map[TypeVariable, DataDef])

  type EitherS[A] = Either[String, A]
  type Encoded[I] = Env => EitherS[TypeRec[I]]
  private type Child[I] = (TypeRec[I], Encoded[I])

  extension [I](self: Child[I]) {
    private def original: TypeRec[I] = self._1
    private def encoded(env: Env): EitherS[TypeRec[I]] = self._2(env)
  }

  private def isDataApplicationOf(t: TypeRec[Type], owner: TypeVariable): Boolean = {
    val (head, _) = collectTypeApps(t)
    head.projectT match {
      case AST.TypeVar(variable) => variable == owner
      case _ => false
    }
  }

  private def substMany(params: Seq[TypeVariable], args: Seq[TypeRec[Type]], in: TypeRec[Type]): TypeRec[Type] =
    params.zip(args).foldLeft(in) { case (acc, (param, arg)) => substType(param, arg, acc) }

  private def thunkIfNullary(fields: Seq[?], handlerT: TypeRec[Type], resultType: TypeRec[Type]): TypeRec[Type] =
    if (fields.isEmpty) arrowT(unitTypeT, resultType) else handlerT

  private def churchDataType(owner: TypeVariable, args: Seq[TypeRec[Type]], dataDef: DataDef, env: Env): EitherS[TypeRec[Type]] = {
    val resultVar = TypeVariable("R")
    val resultType = typeVarT(resultVar)

    dataDef.constructors.toList.traverse { constructor =>
      constructor.fields.toList.traverse { field =>
        val substituted = substMany(dataDef.params, args, field)
        if (isDataApplicationOf(substituted, owner)) {
          Left(s"Recursive data type ${owner.name} cannot be Church encoded as pattern matching yet")
        } else {
          encodeType(substituted, env)
        }
      }.map { encodedFields =>
        thunkIfNullary(constructor.fields, encodedFields.foldRight(resultType)(arrowT), resultType)
      }
    }.map { handlers =>
      forallTypeT(resultVar, handlers.foldRight(resultType)(arrowT))
    }
  }

  private def encodeType(t: TypeRec[Type], env: Env): EitherS[TypeRec[Type]] = t.projectT match {
    case AST.Primitive(name) => Right(primitiveT(name))
    case AST.TypeVar(variable) =>
      env.dataTypes.get(variable) match {
        case Some(dataDef) if dataDef.params.isEmpty => churchDataType(variable, Nil, dataDef, env)
        case Some(dataDef) => Left(s"Data type ${variable.name} expects ${dataDef.params.length} arguments, got 0")
        case None => Right(typeVarT(variable))
      }
    case AST.Arrow(from, to) => for {
      encodedFrom <- encodeType(from, env)
      encodedTo <- encodeType(to, env)
    } yield arrowT(encodedFrom, encodedTo)
    case AST.ForAll(variable, body) => encodeType(body, env).map(forallTypeT(variable, _))
    case AST.TypeApp(_, _) =>
      val (head, args) = collectTypeApps(t)
      head.projectT match {
        case AST.TypeVar(variable) =>
          env.dataTypes.get(variable) match {
            case Some(dataDef) if dataDef.params.length == args.length =>
              for {
                encodedArgs <- args.toList.traverse(arg => encodeType(arg, env))
                encodedData <- churchDataType(variable, encodedArgs, dataDef, env)
              } yield encodedData
            case Some(dataDef) =>
              Left(s"Data type ${variable.name} expects ${dataDef.params.length} arguments, got ${args.length}")
            case None =>
              args.toList.traverse(arg => encodeType(arg, env)).map(encodedArgs => applyTypeConstructor(variable, encodedArgs))
          }
        case _ => for {
          encodedFunction <- encodeType(head, env)
          encodedArgs <- args.toList.traverse(arg => encodeType(arg, env))
        } yield encodedArgs.foldLeft(encodedFunction)(typeAppT)
      }
  }

  private def mkTyAbs(variable: TypeVariable, body: TypeRec[Expr]): TypeRec[Expr] =
    tyAbsT(variable, forallTypeT(variable, typeOf(body)), body)

  private def mkAbs(variable: Variable, paramType: TypeRec[Type], body: TypeRec[Expr]): TypeRec[Expr] =
    absT(variable, arrowT(paramType, typeOf(body)), paramType, body)

  private def mkApp(function: TypeRec[Expr], argument: TypeRec[Expr]): EitherS[TypeRec[Expr]] =
    destructArrow(typeOf(function)) match {
      case Some((_, to)) => Right(appT(to, function, argument))
      case None => Left(s"ChurchEncoder expected function type, got ${typeOf(function).show}")
    }

  private def mkTyApp(function: TypeRec[Expr], argument: TypeRec[Type]): EitherS[TypeRec[Expr]] =
    destructForAll(typeOf(function)) match {
      case Some((variable, body)) => Right(tyAppT(substType(variable, argument, body), function, argument))
      case None => Left(s"ChurchEncoder expected polymorphic type, got ${typeOf(function).show}")
    }

  private def constructorType(owner: TypeVariable, dataDef: DataDef, constructor: ConstructorDef, env: Env): EitherS[TypeRec[Type]] = {
    val nominalResult = applyTypeConstructor(owner, dataDef.params.map(typeVarT))
    val nominalType = dataDef.params.foldRight(constructor.fields.foldRight(nominalResult)(arrowT))(forallTypeT)
    encodeType(nominalType, env)
  }

  private def handlerType(owner: TypeVariable, args: Seq[TypeRec[Type]], dataDef: DataDef, constructor: ConstructorDef, resultType: TypeRec[Type], env: Env): EitherS[TypeRec[Type]] =
    constructor.fields.toList.traverse { field =>
      val substituted = substMany(dataDef.params, args, field)
      if (isDataApplicationOf(substituted, owner)) {
        Left(s"Recursive data type ${owner.name} cannot be Church encoded as pattern matching yet")
      } else {
        encodeType(substituted, env)
      }
    }.map { encodedFields =>
      thunkIfNullary(constructor.fields, encodedFields.foldRight(resultType)(arrowT), resultType)
    }

  private def constructorValue(owner: TypeVariable, dataDef: DataDef, constructor: ConstructorDef, env: Env): EitherS[TypeRec[Expr]] = {
    val typeArgs = dataDef.params.map(typeVarT)
    val resultVar = TypeVariable("R")
    val resultType = typeVarT(resultVar)
    val fieldVars = constructor.fields.indices.map(i => Variable(s"__${constructor.name.name}_field_$i"))
    val handlerVars = dataDef.constructors.indices.map(i => Variable(s"__${constructor.name.name}_case_$i"))

    for {
      fieldTypes <- constructor.fields.toList.traverse(field => encodeType(substMany(dataDef.params, typeArgs, field), env))
      handlerTypes <- dataDef.constructors.toList.traverse(c => handlerType(owner, typeArgs, dataDef, c, resultType, env))
      selectedHandler = varrType(handlerVars(constructor.tag), handlerTypes(constructor.tag))
      thunkedHandler <- if (constructor.fields.isEmpty) mkApp(selectedHandler, unitLitT(unitTypeT)) else Right(selectedHandler)
      appliedHandler <- fieldVars.zip(fieldTypes).foldLeft[EitherS[TypeRec[Expr]]](Right(thunkedHandler)) {
        case (acc, (fieldVar, fieldType)) =>
          acc.flatMap(handler => mkApp(handler, varrType(fieldVar, fieldType)))
      }
      withHandlers = handlerVars.zip(handlerTypes).foldRight(appliedHandler) { case ((handlerVar, handlerT), body) =>
        mkAbs(handlerVar, handlerT, body)
      }
      withResultType = mkTyAbs(resultVar, withHandlers)
      withFields = fieldVars.zip(fieldTypes).foldRight(withResultType) { case ((fieldVar, fieldType), body) =>
        mkAbs(fieldVar, fieldType, body)
      }
      withTypeParams = dataDef.params.foldRight(withFields)(mkTyAbs)
    } yield withTypeParams
  }

  private def dataApplication(t: TypeRec[Type], env: Env): EitherS[(TypeVariable, DataDef, Seq[TypeRec[Type]])] = {
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

  private def encodeMatchCase(
      owner: TypeVariable,
      args: Seq[TypeRec[Type]],
      dataDef: DataDef,
      resultType: TypeRec[Type],
      matchCase: MatchCase[Child],
      env: Env
  ): EitherS[TypeRec[Expr]] = {
    val constructor = dataDef.constructors.find(_.name == matchCase.constructor)
      .toRight(s"Constructor ${matchCase.constructor.name} is not defined")
    constructor.flatMap { constructor =>
      for {
        encodedBody <- matchCase.body.encoded(env)
        fieldTypes <- constructor.fields.toList.traverse { field =>
          val substituted = substMany(dataDef.params, args, field)
          if (isDataApplicationOf(substituted, owner)) {
            Left(s"Recursive data type ${owner.name} cannot be Church encoded as pattern matching yet")
          } else {
            encodeType(substituted, env)
          }
        }
        _ <- Either.cond(matchCase.binders.length == fieldTypes.length, (), s"Constructor ${matchCase.constructor.name} expects ${fieldTypes.length} binders, got ${matchCase.binders.length}")
        innerHandler = matchCase.binders.zip(fieldTypes).foldRight(encodedBody) { case ((binder, fieldType), body) =>
          mkAbs(binder, fieldType, body)
        }
        handler = if (constructor.fields.isEmpty) mkAbs(Variable("__unit"), unitTypeT, innerHandler) else innerHandler
        expectedType = thunkIfNullary(constructor.fields, fieldTypes.foldRight(resultType)(arrowT), resultType)
        _ <- Either.cond(sameType(typeOf(handler), expectedType), (), s"Match case ${matchCase.constructor.name} has unexpected handler type")
      } yield handler
    }
  }

  private val encoderAlg: HCofreeParaAlgebra[AST, TypeAnn, Encoded] =
    [x] => (ann: TypeAnn[x], node: AST[Child, x]) => (env: Env) => ann match {
      case ProgramAnn => (node: @unchecked) match {
        case AST.Program(body) =>
          body.toList.traverse(child => child.encoded(env)).map(programT)
      }
      case TypeAnn => encodeType((node: @unchecked) match {
        case AST.Primitive(name) => primitiveT(name)
        case AST.TypeVar(variable) => typeVarT(variable)
        case AST.Arrow(from, to) => arrowT(from.original, to.original)
        case AST.ForAll(variable, body) => forallTypeT(variable, body.original)
        case AST.TypeApp(function, argument) => typeAppT(function.original, argument.original)
      }, env)
      case ExprAnn(t) => encodeType(t, env).flatMap { encodedTypeAnn =>
        (node: @unchecked) match {
          case AST.Abs(variable, types, body) => for {
            encodedTypes <- types.encoded(env)
            encodedBody <- body.encoded(env)
          } yield absT(variable, encodedTypeAnn, encodedTypes, encodedBody)
          case AST.TyAbs(variable, body) => for {
            encodedBody <- body.encoded(env)
          } yield tyAbsT(variable, encodedTypeAnn, encodedBody)
          case AST.Let(variable, types, value, body) => for {
            encodedTypes <- types.encoded(env)
            encodedValue <- value.encoded(env)
            encodedBody <- body.encoded(env)
          } yield letT(variable, encodedTypeAnn, encodedTypes, encodedValue, encodedBody)
          case AST.LetRec(variable, types, value, body) => for {
            encodedTypes <- types.encoded(env)
            encodedValue <- value.encoded(env)
            encodedBody <- body.encoded(env)
          } yield letRecT(variable, encodedTypeAnn, encodedTypes, encodedValue, encodedBody)
          case AST.TypeLet(variable, params, alias, body) => for {
            encodedAlias <- alias.encoded(env)
            encodedBody <- body.encoded(env)
          } yield typeLetT(variable, params, encodedTypeAnn, encodedAlias, encodedBody)
          case AST.DataLet(variable, params, constructors, body) =>
            val dataDef = DataDef(
              params,
              constructors.zipWithIndex.map { case (constructor, tag) =>
                ConstructorDef(constructor.name, constructor.fields.map(_.original), tag)
              }
            )
            val extendedEnv = env.copy(dataTypes = env.dataTypes + (variable -> dataDef))
            for {
              encodedBody <- body.encoded(extendedEnv)
              encoded <- dataDef.constructors.foldRight[EitherS[TypeRec[Expr]]](Right(encodedBody)) { case (constructor, acc) =>
                for {
                  bodyExpr <- acc
                  constructorT <- constructorType(variable, dataDef, constructor, extendedEnv)
                  value <- constructorValue(variable, dataDef, constructor, extendedEnv)
                } yield letT(constructor.name, typeOf(bodyExpr), constructorT, value, bodyExpr)
              }
            } yield encoded
          case AST.Match(scrutinee, cases) =>
            val scrutType = typeOf(scrutinee.original)
            for {
              encodedScrutinee <- scrutinee.encoded(env)
              dataApp <- dataApplication(scrutType, env)
              (owner, dataDef, args) = dataApp
              encodedArgs <- args.toList.traverse(arg => encodeType(arg, env))
              resultApplied <- mkTyApp(encodedScrutinee, encodedTypeAnn)
              handlers <- dataDef.constructors.toList.traverse { constructor =>
                cases.find(_.constructor == constructor.name)
                  .toRight(s"Match is missing constructor ${constructor.name.name}")
                  .flatMap(c => encodeMatchCase(owner, encodedArgs, dataDef, encodedTypeAnn, c, env))
              }
              encoded <- handlers.foldLeft[EitherS[TypeRec[Expr]]](Right(resultApplied)) {
                case (acc, handler) => acc.flatMap(expr => mkApp(expr, handler))
              }
            } yield encoded
          case AST.App(function, argument) => for {
            encodedFunction <- function.encoded(env)
            encodedArgument <- argument.encoded(env)
          } yield appT(encodedTypeAnn, encodedFunction, encodedArgument)
          case AST.TyApp(function, argument) => for {
            encodedFunction <- function.encoded(env)
            encodedArgument <- argument.encoded(env)
          } yield tyAppT(encodedTypeAnn, encodedFunction, encodedArgument)
          case AST.Foreign(value, types) => for {
            encodedTypes <- types.encoded(env)
          } yield foreignT(value, encodedTypeAnn, encodedTypes)
          case AST.Var(value) => Right(varrType(value, encodedTypeAnn))
          case AST.Num(value) => Right(numT(value, encodedTypeAnn))
          case AST.Char(value) => Right(charT(value, encodedTypeAnn))
          case AST.StringLit(value) => Right(stringLitT(value, encodedTypeAnn))
          case AST.Bool(value) => Right(boolT(value, encodedTypeAnn))
          case AST.UnitLit() => Right(unitLitT(encodedTypeAnn))
          case AST.Block(discarded, result) => for {
            encodedDiscarded <- discarded.toList.traverse(child => child.encoded(env))
            encodedResult <- result.traverse(child => child.encoded(env))
          } yield blockT(encodedTypeAnn, encodedDiscarded, encodedResult)
          case AST.BinOp(op, left, right) => for {
            encodedLeft <- left.encoded(env)
            encodedRight <- right.encoded(env)
          } yield binopT(op, encodedTypeAnn, encodedLeft, encodedRight)
          case AST.UnaryOp(op, body) => for {
            encodedBody <- body.encoded(env)
          } yield unopT(op, encodedTypeAnn, encodedBody)
          case AST.If(cond, thenBranch, elseBranch) => for {
            encodedCond <- cond.encoded(env)
            encodedThen <- thenBranch.encoded(env)
            encodedElse <- elseBranch.encoded(env)
          } yield ifT(encodedTypeAnn, encodedCond, encodedThen, encodedElse)
        }
      }
    }

  def encode(program: TypeRec[AST.Program.type]): EitherS[TypeRec[AST.Program.type]] =
    program.paraAnn(encoderAlg)(Env(Map.empty))
}