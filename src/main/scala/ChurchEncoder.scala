package com.yuuki14202028

import cats.data.ReaderT
import cats.syntax.all.*

object ChurchEncoder {
  enum EncodeError {
    case InvariantViolation(message: String)

    override def toString: String = this match {
      case InvariantViolation(message) => s"Compiler invariant violation: $message"
    }
  }

  private type EncodeResult[A] = Either[EncodeError, A]
  private type Encode[A] = ReaderT[EncodeResult, DataEnv, A]
  private type Encoded[I] = Encode[TypeRec[I]]
  private type Child[I] = (TypeRec[I], Encoded[I])

  extension [I](self: Child[I]) {
    private def original: TypeRec[I] = self._1
    private def encoded: Encoded[I] = self._2
  }

  private def fail[A](msg: String): Encode[A] = ReaderT.liftF(Left(EncodeError.InvariantViolation(msg)))
  private def guard(cond: Boolean, msg: => String): Encode[Unit] =
    ReaderT.liftF(Either.cond(cond, (), EncodeError.InvariantViolation(msg)))
  private def lift[A](e: Either[String, A]): Encode[A] =
    ReaderT.liftF(e.leftMap(EncodeError.InvariantViolation.apply))
  private val ask: Encode[DataEnv] = ReaderT.ask[EncodeResult, DataEnv]
  private def okT[I](t: TypeRec[I]): Encoded[I] = ReaderT.pure(t)

  private def thunkIfNullary(fields: Seq[?], handlerT: TypeRec[Type], resultType: TypeRec[Type]): TypeRec[Type] =
    if (fields.isEmpty) arrowT(unitTypeT, resultType) else handlerT

  private def encodeConstructorFields(owner: TypeVariable, args: Seq[TypeRec[Type]], dataDef: DataDef, constructor: ConstructorDef): Encode[Seq[TypeRec[Type]]] =
    constructor.fields.traverse { field =>
      val substituted = substMany(dataDef.params, args, field)
      val recursive = containsDataApplicationOf(substituted, owner)
      if (recursive && !dataDef.recursive)
        fail(s"Recursive data type ${owner.name} must be declared with data rec")
      else if (recursive) okT(substituted)
      else encodeType(substituted)
    }

  private def churchDataType(owner: TypeVariable, args: Seq[TypeRec[Type]], dataDef: DataDef): Encoded[Type] = {
    val resultVar = TypeVariable("R")
    val resultType = typeVarT(resultVar)

    dataDef.constructors.traverse { constructor =>
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

  private def mkTyAbs(variable: TypeVariable, body: TypeRec[Expr]): TypeRec[Expr] =
    tyAbsT(variable, forallTypeT(variable, typeOf(body)), body)

  private def mkAbs(variable: Variable, paramType: TypeRec[Type], body: TypeRec[Expr]): TypeRec[Expr] =
    absT(variable, arrowT(paramType, typeOf(body)), paramType, body)

  private def applyHandlers(
      target: TypeRec[Expr],
      handlers: Seq[TypeRec[Expr]],
      handlerTypes: Seq[TypeRec[Type]],
      resultType: TypeRec[Type]
  ): TypeRec[Expr] =
    handlers.zipWithIndex.foldLeft(target) { case (expr, (handler, index)) =>
      appT(handlerTypes.drop(index + 1).foldRight(resultType)(arrowT), expr, handler)
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

  private def constructorValue(owner: TypeVariable, dataDef: DataDef, constructor: ConstructorDef): Encoded[Expr] = for {
    typeArgs = dataDef.params.map(typeVarT)
    resultVar = TypeVariable("R")
    resultType = typeVarT(resultVar)
    fieldVars = constructor.fields.indices.map(i => Variable(s"__${constructor.name.name}_field_$i"))
    handlerVars = dataDef.constructors.indices.map(i => Variable(s"__${constructor.name.name}_case_$i"))
    fieldTypes <- constructor.fields.map(field => substMany(dataDef.params, typeArgs, field)).traverse(t => encodeType(t): Encode[TypeRec[Type]])
    handlerTypes <- dataDef.constructors.toList.traverse((c: ConstructorDef) => handlerType(owner, typeArgs, dataDef, c, resultType): Encode[TypeRec[Type]])
    selectedHandler = varrType(handlerVars(constructor.tag), handlerTypes(constructor.tag))
    thunkedHandler = if (constructor.fields.isEmpty) appT(resultType, selectedHandler, unitLitT(unitTypeT)) else selectedHandler
    appliedHandler = fieldVars.zip(fieldTypes).zipWithIndex.foldLeft(thunkedHandler) {
      case (handler, ((fieldVar, fieldType), index)) =>
        val nextType = fieldTypes.drop(index + 1).foldRight(resultType)(arrowT)
        appT(nextType, handler, varrType(fieldVar, fieldType))
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
                okT(letT(binder, typeOf(body), resultType, appT(resultType, foldFunction, tailValue), body))
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
    case (ann: ProgramAnn, _) => rebuildNode(ann, node)
    case (DeclAnn, _) => rebuildNode(DeclAnn, node)
    case (TypeAnn, _) => encodeType(originalNode(TypeAnn, node))
    case (ExprAnn(t), AST.DataLet(variable, params, constructors, body, recursive)) => for {
      taggedConstructors = constructors.zipWithIndex.map { case (constructor, tag) =>
        ConstructorDef(constructor.name, variable, constructor.fields.map(_.original), tag)
      }
      dataDef = DataDef(params, taggedConstructors, recursive)
      encodedBody <- body.encoded.local((env: DataEnv) => env.copy(dataTypes = env.dataTypes + (variable -> dataDef)))
      encoded <- dataDef.constructors.foldRight(okT(encodedBody)) { (constructor, acc) =>
        for {
          bodyExpr <- acc
          constructorT <- constructorType(variable, dataDef, constructor)
          value <- constructorValue(variable, dataDef, constructor)
        } yield letT(constructor.name, typeOf(bodyExpr), constructorT, value, bodyExpr)
      }
    } yield encoded
    case (ExprAnn(t), AST.Match(scrutinee, cases)) => for {
      scrutType = typeOf(scrutinee.original)
      resultType <- encodeType(t)
      encodedScrutinee <- scrutinee.encoded
      dataApp <- ask.flatMap(env => lift(dataTypeApplication(scrutType, env.dataTypes)(_.params)))
      (owner, dataDef, args) = dataApp
      handlerTypes <- dataDef.constructors.traverse((c: ConstructorDef) => handlerType(owner, args, dataDef, c, resultType): Encode[TypeRec[Type]])
      resultApplied = tyAppT(handlerTypes.foldRight(resultType)(arrowT), encodedScrutinee, resultType)
      handlers <- dataDef.constructors.traverse { constructor =>
        cases.find(_.constructor == constructor.name)
          .fold(fail[TypeRec[Expr]](s"Match is missing constructor ${constructor.name.name}")) { c =>
            encodeMatchCase(owner, args, dataDef, resultType, c)
          }
      }
      encoded = applyHandlers(resultApplied, handlers, handlerTypes, resultType)
    } yield encoded
    case (ExprAnn(t), AST.Fold(scrutinee, _, cases)) => for {
      scrutType = typeOf(scrutinee.original)
      foldVariable = Variable("__fold")
      foldArgument = Variable("__fold_arg")
      resultType <- encodeType(t)
      encodedScrutineeType <- encodeType(scrutType)
      encodedScrutinee <- scrutinee.encoded
      dataApp <- ask.flatMap(env => lift(dataTypeApplication(scrutType, env.dataTypes)(_.params)))
      (owner, dataDef, args) = dataApp
      foldType = arrowT(encodedScrutineeType, resultType)
      foldRef = varrType(foldVariable, foldType)
      foldArgRef = varrType(foldArgument, encodedScrutineeType)
      handlerTypes <- dataDef.constructors.traverse((c: ConstructorDef) => handlerType(owner, args, dataDef, c, resultType): Encode[TypeRec[Type]])
      resultApplied = tyAppT(handlerTypes.foldRight(resultType)(arrowT), foldArgRef, resultType)
      handlers <- dataDef.constructors.traverse { constructor =>
        cases.find(_.constructor == constructor.name)
          .fold(fail[TypeRec[Expr]](s"Fold is missing constructor ${constructor.name.name}")) { c =>
            encodeFoldCase(owner, args, dataDef, foldRef, resultType, c)
          }
      }
      encodedMatch = applyHandlers(resultApplied, handlers, handlerTypes, resultType)
      foldValue = absT(foldArgument, foldType, encodedScrutineeType, encodedMatch)
      folded = appT(resultType, foldRef, encodedScrutinee)
    } yield letRecT(foldVariable, resultType, foldType, foldValue, folded)
    case (ExprAnn(t), _) => rebuildExprNode(t, node)
  }

  private def encodeDecl(decl: TypeRec[Decl], env: DataEnv): EncodeResult[(Seq[TypeRec[Decl]], DataEnv)] = decl.tail match {
    case AST.TopData(variable, _, _, _) =>
      env.dataTypes.get(variable).toRight(EncodeError.InvariantViolation(s"Top-level data type ${variable.name} is missing from ProgramAnn")).flatMap { dataDef =>
        dataDef.constructors.traverse { constructor => for {
          constructorT <- constructorType(variable, dataDef, constructor).run(env)
          value <- constructorValue(variable, dataDef, constructor).run(env)
        } yield topLetT(constructor.name, constructorT, value) }.map(_ -> env)
      }

    case _ =>
      decl.paraAnn(encoderAlg).run(env).map(encodedDecl => (Seq(encodedDecl), env))
  }

  def encode(program: TypeRec[AST.Program.type]): EncodeResult[TypeRec[AST.Program.type]] = program.tail match {
    case AST.Program(decls) =>
      val initialEnv = program.head match {
        case ProgramAnn(env) => DataEnv.from(env)
      }
      decls.foldLeft(Right((Vector.empty[TypeRec[Decl]], initialEnv)): EncodeResult[(Vector[TypeRec[Decl]], DataEnv)]) {
        case (acc, decl) => acc.flatMap { case (encodedDecls, env) =>
          encodeDecl(decl, env).map { case (newDecls, nextEnv) => (encodedDecls ++ newDecls, nextEnv) }
        }
      }.map { case (encodedDecls, _) => programT(encodedDecls) }
  }

}
