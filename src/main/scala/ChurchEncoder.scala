package com.yuuki14202028

import cats.data.ReaderT
import cats.syntax.all.*

object ChurchEncoder {
  case class Env(dataTypes: Map[TypeVariable, DataDef])

  type EitherS[A] = Either[String, A]
  private type Encode[A] = ReaderT[EitherS, Env, A]
  private type Encoded[I] = Encode[TypeRec[I]]
  private type Child[I] = (TypeRec[I], Encoded[I])

  extension [I](self: Child[I]) {
    private def original: TypeRec[I] = self._1
    private def encoded: Encoded[I] = self._2
  }

  private def fail[A](msg: String): Encode[A] = ReaderT.liftF(Left(msg))
  private def guard(cond: Boolean, msg: => String): Encode[Unit] = ReaderT.liftF(Either.cond(cond, (), msg))
  private def lift[A](e: EitherS[A]): Encode[A] = ReaderT.liftF(e)
  private val ask: Encode[Env] = ReaderT.ask[EitherS, Env]
  private def okT[I](t: TypeRec[I]): Encoded[I] = ReaderT.pure(t)

  private def isDataApplicationOf(t: TypeRec[Type], owner: TypeVariable): Boolean = {
    val (head, _) = collectTypeApps(t)
    head.projectT match {
      case AST.TypeVar(variable) => variable == owner
      case _ => false
    }
  }

  private def containsDataApplicationOf(t: TypeRec[Type], owner: TypeVariable): Boolean = {
    val (head, args) = collectTypeApps(t)
    val self = head.projectT match {
      case AST.TypeVar(variable) => variable == owner
      case _ => false
    }
    self || (t.projectT match {
      case AST.Arrow(from, to) => containsDataApplicationOf(from, owner) || containsDataApplicationOf(to, owner)
      case AST.ForAll(_, body) => containsDataApplicationOf(body, owner)
      case AST.TypeApp(_, _) => args.exists(arg => containsDataApplicationOf(arg, owner))
      case _ => false
    })
  }

  private def thunkIfNullary(fields: Seq[?], handlerT: TypeRec[Type], resultType: TypeRec[Type]): TypeRec[Type] =
    if (fields.isEmpty) arrowT(unitTypeT, resultType) else handlerT

  private def recursiveDataTypeError(owner: TypeVariable): String =
    s"Recursive data type ${owner.name} must be declared with data rec"

  private def encodeFieldType(owner: TypeVariable, dataDef: DataDef, args: Seq[TypeRec[Type]], field: TypeRec[Type]): Encoded[Type] = {
    val substituted = substMany(dataDef.params, args, field)
    if (containsDataApplicationOf(substituted, owner) && !dataDef.recursive) fail(recursiveDataTypeError(owner))
    else if (containsDataApplicationOf(substituted, owner)) okT(substituted)
    else encodeType(substituted)
  }

  private def encodeConstructorFields(owner: TypeVariable, args: Seq[TypeRec[Type]], dataDef: DataDef, constructor: ConstructorDef): Encode[List[TypeRec[Type]]] =
    constructor.fields.toList.traverse(field => encodeFieldType(owner, dataDef, args, field))

  private def churchDataType(owner: TypeVariable, args: Seq[TypeRec[Type]], dataDef: DataDef): Encoded[Type] = {
    val resultVar = TypeVariable("R")
    val resultType = typeVarT(resultVar)

    dataDef.constructors.toList.traverse { constructor =>
      encodeConstructorFields(owner, args, dataDef, constructor).map { encodedFields =>
        thunkIfNullary(constructor.fields, encodedFields.foldRight(resultType)(arrowT), resultType)
      }
    }.map { handlers =>
      forallTypeT(resultVar, handlers.foldRight(resultType)(arrowT))
    }
  }

  private def originalNode[I](ann: TypeAnn[I], node: AST[Child, I]): TypeRec[I] =
    HCofree(ann, paraOriginals(node))

  private def rebuildNode[I](ann: TypeAnn[I], node: AST[Child, I]): Encoded[I] = summon[HTraverse[AST]]
    .traverse(node)([x] => child => child.encoded)
    .map(encoded => HCofree(ann, encoded))

  private def rebuildExprNode(t: TypeRec[Type], node: AST[Child, Expr]): Encoded[Expr] =
    encodeType(t).flatMap(encodedTypeAnn => rebuildNode(ExprAnn(encodedTypeAnn), node))

  private val typeEncoderAlg: HCofreeParaAlgebra[AST, TypeAnn, Encoded] = [x] => (ann, node) => node match {
    case AST.TypeVar(variable) => ask.flatMap { env =>
      env.dataTypes.get(variable) match {
        case Some(dataDef) if dataDef.params.isEmpty => churchDataType(variable, Nil, dataDef)
        case Some(dataDef) => fail(s"Data type ${variable.name} expects ${dataDef.params.length} arguments, got 0")
        case None => rebuildNode(ann, node)
      }
    }
    case AST.TypeApp(_, _) =>
      val original = originalNode(ann, node)
      val (head, args) = collectTypeApps(original)
      head.projectT match {
        case AST.TypeVar(variable) => ask.flatMap { env =>
          env.dataTypes.get(variable) match {
            case Some(dataDef) if dataDef.params.length == args.length => for {
              encodedData <- churchDataType(variable, args, dataDef)
            } yield encodedData
            case Some(dataDef) => fail(s"Data type ${variable.name} expects ${dataDef.params.length} arguments, got ${args.length}")
            case None => rebuildNode(ann, node)
          }
        }
        case _ => rebuildNode(ann, node)
      }
    case _ => rebuildNode(ann, node)
  }

  private def encodeType(t: TypeRec[Type]): Encoded[Type] =
    t.paraAnn(typeEncoderAlg)

  private def encodeTypes(types: Seq[TypeRec[Type]]): Encode[List[TypeRec[Type]]] =
    types.toList.traverse(t => encodeType(t))

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

  private def constructorType(owner: TypeVariable, dataDef: DataDef, constructor: ConstructorDef): Encoded[Type] = {
    val nominalResult = applyTypeConstructor(owner, dataDef.params.map(typeVarT))
    val nominalType = dataDef.params.foldRight(constructor.fields.foldRight(nominalResult)(arrowT))(forallTypeT)
    encodeType(nominalType)
  }

  private def handlerType(owner: TypeVariable, args: Seq[TypeRec[Type]], dataDef: DataDef, constructor: ConstructorDef, resultType: TypeRec[Type]): Encoded[Type] =
    encodeConstructorFields(owner, args, dataDef, constructor).map { encodedFields =>
      thunkIfNullary(constructor.fields, encodedFields.foldRight(resultType)(arrowT), resultType)
    }

  private def constructorValue(owner: TypeVariable, dataDef: DataDef, constructor: ConstructorDef): Encoded[Expr] = {
    val typeArgs = dataDef.params.map(typeVarT)
    val resultVar = TypeVariable("R")
    val resultType = typeVarT(resultVar)
    val fieldVars = constructor.fields.indices.map(i => Variable(s"__${constructor.name.name}_field_$i"))
    val handlerVars = dataDef.constructors.indices.map(i => Variable(s"__${constructor.name.name}_case_$i"))

    for {
      fieldTypes <- encodeTypes(constructor.fields.map(field => substMany(dataDef.params, typeArgs, field)))
      handlerTypes <- dataDef.constructors.toList.traverse((c: ConstructorDef) => handlerType(owner, typeArgs, dataDef, c, resultType): Encode[TypeRec[Type]])
      selectedHandler = varrType(handlerVars(constructor.tag), handlerTypes(constructor.tag))
      thunkedHandler <- if (constructor.fields.isEmpty) lift(mkApp(selectedHandler, unitLitT(unitTypeT))) else okT(selectedHandler)
      appliedHandler <- fieldVars.zip(fieldTypes).foldLeft[Encode[TypeRec[Expr]]](okT(thunkedHandler)) {
        case (acc, (fieldVar, fieldType)) =>
          acc.flatMap(handler => lift(mkApp(handler, varrType(fieldVar, fieldType))))
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

  private def encodeMatchCase(
      owner: TypeVariable,
      args: Seq[TypeRec[Type]],
      dataDef: DataDef,
      resultType: TypeRec[Type],
      matchCase: MatchCase[Child]
  ): Encoded[Expr] = {
    dataDef.constructors.find(_.name == matchCase.constructor) match {
      case None => fail(s"Constructor ${matchCase.constructor.name} is not defined")
      case Some(constructor) => for {
        encodedBody <- matchCase.body.encoded
        fieldTypes <- encodeConstructorFields(owner, args, dataDef, constructor)
        _ <- guard(matchCase.binders.length == fieldTypes.length, s"Constructor ${matchCase.constructor.name} expects ${fieldTypes.length} binders, got ${matchCase.binders.length}")
        innerHandler = matchCase.binders.zip(fieldTypes).foldRight(encodedBody) { case ((binder, fieldType), body) =>
          mkAbs(binder, fieldType, body)
        }
        handler = if (constructor.fields.isEmpty) mkAbs(Variable("__unit"), unitTypeT, innerHandler) else innerHandler
        expectedType = thunkIfNullary(constructor.fields, fieldTypes.foldRight(resultType)(arrowT), resultType)
        _ <- guard(sameType(typeOf(handler), expectedType), s"Match case ${matchCase.constructor.name} has unexpected handler type")
      } yield handler
    }
  }

  private def encodeFoldCase(
      owner: TypeVariable,
      args: Seq[TypeRec[Type]],
      dataDef: DataDef,
      foldFunction: TypeRec[Expr],
      resultType: TypeRec[Type],
      foldCase: MatchCase[Child]
  ): Encoded[Expr] = {
    dataDef.constructors.find(_.name == foldCase.constructor) match {
      case None => fail(s"Constructor ${foldCase.constructor.name} is not defined")
      case Some(constructor) => for {
        encodedBody <- foldCase.body.encoded
        fieldTypes <- encodeConstructorFields(owner, args, dataDef, constructor)
        substitutedFields = constructor.fields.map(field => substMany(dataDef.params, args, field))
        _ <- guard(foldCase.binders.length == fieldTypes.length, s"Constructor ${foldCase.constructor.name} expects ${fieldTypes.length} binders, got ${foldCase.binders.length}")
        handlerParamVars = foldCase.binders.zip(substitutedFields).zipWithIndex.map {
          case ((binder, field), index) if isDataApplicationOf(field, owner) => Variable(s"__fold_${binder.name}_tail_$index")
          case ((binder, _), _) => binder
        }
        bodyWithAccs <- foldCase.binders.zip(handlerParamVars).zip(substitutedFields.zip(fieldTypes)).foldRight[Encode[TypeRec[Expr]]](okT(encodedBody)) {
          case (((binder, handlerParam), (field, fieldType)), acc) =>
            acc.flatMap { body =>
              if (isDataApplicationOf(field, owner)) {
                val tailValue = varrType(handlerParam, fieldType)
                for {
                  folded <- lift(mkApp(foldFunction, tailValue))
                } yield letT(binder, typeOf(body), resultType, folded, body)
              } else okT(body)
            }
        }
        innerHandler <- handlerParamVars.zip(fieldTypes).foldRight[Encode[TypeRec[Expr]]](okT(bodyWithAccs)) {
          case ((binder, fieldType), body) => body.map(expr => mkAbs(binder, fieldType, expr))
        }
        handler = if (constructor.fields.isEmpty) mkAbs(Variable("__unit"), unitTypeT, innerHandler) else innerHandler
        expectedType = thunkIfNullary(constructor.fields, fieldTypes.foldRight(resultType)(arrowT), resultType)
        _ <- guard(sameType(typeOf(handler), expectedType), s"Fold case ${foldCase.constructor.name} has unexpected handler type")
      } yield handler
    }
  }

  private val encoderAlg: HCofreeParaAlgebra[AST, TypeAnn, Encoded] = [x] => (ann, node) => (ann, node) match {
    case (ProgramAnn, _) => rebuildNode(ProgramAnn, node)
    case (DeclAnn, _) => rebuildNode(DeclAnn, node)
    case (TypeAnn, _) => encodeType(originalNode(TypeAnn, node))
    case (ExprAnn(t), AST.DataLet(variable, params, constructors, body, recursive)) =>
      val taggedConstructors = constructors.zipWithIndex.map { case (constructor, tag) =>
        ConstructorDef(constructor.name, variable, constructor.fields.map(_.original), tag)
      }
      val dataDef = DataDef(params, taggedConstructors, recursive)
      for {
        encodedBody <- body.encoded.local((env: Env) => env.copy(dataTypes = env.dataTypes + (variable -> dataDef)))
        encoded <- dataDef.constructors.foldRight(okT(encodedBody)) { (constructor, acc) => for {
          bodyExpr <- acc
          constructorT <- constructorType(variable, dataDef, constructor)
          value <- constructorValue(variable, dataDef, constructor)
        } yield letT(constructor.name, typeOf(bodyExpr), constructorT, value, bodyExpr) }
      } yield encoded
    case (ExprAnn(t), AST.Match(scrutinee, cases)) =>
      val scrutType = typeOf(scrutinee.original)
      for {
        resultType <- encodeType(t)
        encodedScrutinee <- scrutinee.encoded
        dataApp <- ask.flatMap(env => lift(dataTypeApplication(scrutType, env.dataTypes)(_.params)))
        (owner, dataDef, args) = dataApp
        resultApplied <- lift(mkTyApp(encodedScrutinee, resultType))
        handlers <- dataDef.constructors.toList.traverse { constructor =>
          cases.find(_.constructor == constructor.name)
          .fold(fail[TypeRec[Expr]](s"Match is missing constructor ${constructor.name.name}")) { c =>
            encodeMatchCase(owner, args, dataDef, resultType, c)
          }
        }
        encoded <- handlers.foldLeft(okT(resultApplied)) { (acc, handler) =>
          acc.flatMap(expr => lift(mkApp(expr, handler)))
        }
      } yield encoded
    case (ExprAnn(t), AST.Fold(scrutinee, _, cases)) =>
      val scrutType = typeOf(scrutinee.original)
      val foldVariable = Variable("__fold")
      val foldArgument = Variable("__fold_arg")
      for {
        resultType <- encodeType(t)
        encodedScrutineeType <- encodeType(scrutType)
        encodedScrutinee <- scrutinee.encoded
        dataApp <- ask.flatMap(env => lift(dataTypeApplication(scrutType, env.dataTypes)(_.params)))
        (owner, dataDef, args) = dataApp
        foldType = arrowT(encodedScrutineeType, resultType)
        foldRef = varrType(foldVariable, foldType)
        foldArgRef = varrType(foldArgument, encodedScrutineeType)
        resultApplied <- lift(mkTyApp(foldArgRef, resultType))
        handlers <- dataDef.constructors.toList.traverse { constructor =>
          cases.find(_.constructor == constructor.name)
            .fold(fail[TypeRec[Expr]](s"Fold is missing constructor ${constructor.name.name}")) { c =>
              encodeFoldCase(owner, args, dataDef, foldRef, resultType, c)
            }
        }
        encodedMatch <- handlers.foldLeft(okT(resultApplied)) { (acc, handler) =>
          acc.flatMap(expr => lift(mkApp(expr, handler)))
        }
        foldValue = absT(foldArgument, foldType, encodedScrutineeType, encodedMatch)
        folded <- lift(mkApp(foldRef, encodedScrutinee))
      } yield letRecT(foldVariable, resultType, foldType, foldValue, folded)
    case (ExprAnn(t), _) => rebuildExprNode(t, node)
  }

  private def encodeDecl(decl: TypeRec[Decl], env: Env): EitherS[(Seq[TypeRec[Decl]], Env)] = decl.tail match {
    case AST.TopData(variable, params, constructors, recursive) =>
      val taggedConstructors = constructors.zipWithIndex.map { case (constructor, tag) =>
        ConstructorDef(constructor.name, variable, constructor.fields, tag)
      }
      val dataDef = DataDef(params, taggedConstructors, recursive)
      val nextEnv = env.copy(dataTypes = env.dataTypes + (variable -> dataDef))
      taggedConstructors.toList.traverse { constructor => for {
        constructorT <- constructorType(variable, dataDef, constructor).run(nextEnv)
        value <- constructorValue(variable, dataDef, constructor).run(nextEnv)
      } yield topLetT(constructor.name, constructorT, value) }.map(_ -> nextEnv)

    case _ =>
      decl.paraAnn(encoderAlg).run(env).map(encodedDecl => (Seq(encodedDecl), env))
  }

  def encode(program: TypeRec[AST.Program.type]): EitherS[TypeRec[AST.Program.type]] = program.tail match {
    case AST.Program(decls) =>
      decls.foldLeft(Right((Vector.empty[TypeRec[Decl]], Env(Map.empty))): EitherS[(Vector[TypeRec[Decl]], Env)]) {
        case (acc, decl) => acc.flatMap { case (encodedDecls, env) =>
          encodeDecl(decl, env).map { case (newDecls, nextEnv) => (encodedDecls ++ newDecls, nextEnv) }
        }
      }.map { case (encodedDecls, _) => programT(encodedDecls) }
  }

}
