package com.yuuki14202028

import cats.data.Reader
import cats.syntax.all.*

object ChurchEncoder {

  private type Encode[A] = Reader[DataEnv, A]
  private type Encoded[I] = Encode[TypeRec[I]]

  private def invariant(msg: String): Nothing = sys.error(s"Compiler invariant violation: $msg")
  private def orInvariant[A](e: Either[CompileError, A]): A = e.fold(err => invariant(err.render), identity)
  private def guard(cond: Boolean, msg: => String): Encode[Unit] =
    if (cond) Reader(_ => ()) else invariant(msg)
  private val ask: Encode[DataEnv] = Reader(identity)
  private def okT[I](t: TypeRec[I]): Encoded[I] = Reader(_ => t)

  private def thunkIfNullary(fields: Seq[?], handlerT: TypeRec[Type], resultType: TypeRec[Type]): TypeRec[Type] =
    if (fields.isEmpty) arrowT(unitTypeT, resultType) else handlerT

  private def encodeConstructorFields(owner: TypeVariable, args: Seq[TypeRec[Type]], dataDef: DataDef, constructor: ConstructorDef): Encode[Seq[TypeRec[Type]]] =
    constructor.fields.traverse { field =>
      val substituted = substMany(dataDef.paramVars, args, field)
      val recursive = containsDataApplicationOf(substituted, owner)
      if (recursive && !dataDef.recursive)
        invariant(s"Recursive data type ${owner.name} must be declared with data rec")
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
      forallTypeT(resultVar, Kind.Star, handlers.foldRight(resultType)(arrowT))
    }
  }

  private def originalNode[I](ann: TypeAnn[I], node: AST[Para[TypeRec, Encoded], I]): TypeRec[I] =
    HCofree(ann, paraOriginals(node))

  private def rebuildNode[I](ann: TypeAnn[I], node: AST[Para[TypeRec, Encoded], I]): Encoded[I] = summon[HTraverse[AST]]
    .traverse[Encode, Para[TypeRec, Encoded], TypeRec, I](node)([x] => child => child.result)
    .map(encoded => HCofree(ann, encoded))

  private def rebuildExprNode(t: TypeRec[Type], node: AST[Para[TypeRec, Encoded], Expr]): Encoded[Expr] =
    encodeType(t).flatMap(encodedTypeAnn => rebuildNode(ExprAnn(encodedTypeAnn), node))

  private def etaExpandDataType(variable: TypeVariable, dataDef: DataDef, providedArgs: Seq[TypeRec[Type]]): Encoded[Type] = {
    val missing = dataDef.params.drop(providedArgs.length)
    val freshParams = missing.zipWithIndex.map { case ((_, k), i) =>
      (TypeVariable(s"__eta_${variable.name}_${providedArgs.length + i}"), k)
    }
    val fullArgs = providedArgs ++ freshParams.map { case (v, _) => typeVarT(v) }
    churchDataType(variable, fullArgs, dataDef).map { encodedBody =>
      freshParams.foldRight(encodedBody) { case ((v, k), acc) => typeAbsT(v, k, acc) }
    }
  }

  private val typeEncoderAlg: RAlgebra[TypedAST, TypeRec, Encoded] = [x] => he => he.ast match {
    case AST.TypeVar(variable) => ask.flatMap { env =>
      env.dataTypes.get(variable) match {
        case Some(dataDef) => etaExpandDataType(variable, dataDef, Nil)
        case None => rebuildNode(he.ann, he.ast)
      }
    }
    case AST.TypeApp(_, _) =>
      val original = originalNode(he.ann, he.ast)
      val (head, args) = collectTypeApps(original)
      head.project match {
        case AST.TypeVar(variable) => ask.flatMap { env =>
          env.dataTypes.get(variable) match {
            case Some(dataDef) if args.length <= dataDef.params.length =>
              etaExpandDataType(variable, dataDef, args)
            case Some(dataDef) =>
              invariant(s"Data type ${variable.name} expects ${dataDef.params.length} arguments, got ${args.length}")
            case None => rebuildNode(he.ann, he.ast)
          }
        }
        case _ => rebuildNode(he.ann, he.ast)
      }
    case _ => rebuildNode(he.ann, he.ast)
  }

  private def encodeType(t: TypeRec[Type]): Encode[TypeRec[Type]] =
    t.para(typeEncoderAlg)

  private def mkTyAbs(variable: TypeVariable, kind: Kind, body: TypeRec[Expr]): TypeRec[Expr] =
    tyAbsT(variable, forallTypeT(variable, kind, typeOf(body)), kind, body)

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
    val nominalResult = applyTypeConstructor(owner, dataDef.paramVars.map(typeVarT))
    val nominalType = dataDef.params.foldRight(constructor.fields.foldRight(nominalResult)(arrowT)) {
      case ((v, k), body) => forallTypeT(v, k, body)
    }
    encodeType(nominalType)
  }

  private def handlerType(owner: TypeVariable, args: Seq[TypeRec[Type]], dataDef: DataDef, constructor: ConstructorDef, resultType: TypeRec[Type]): Encode[TypeRec[Type]] =
    encodeConstructorFields(owner, args, dataDef, constructor).map { encodedFields =>
      thunkIfNullary(constructor.fields, encodedFields.foldRight(resultType)(arrowT), resultType)
    }

  private def constructorValue(owner: TypeVariable, dataDef: DataDef, constructor: ConstructorDef): Encoded[Expr] = for {
    typeArgs = dataDef.paramVars.map(typeVarT)
    resultVar = TypeVariable("R")
    resultType = typeVarT(resultVar)
    fieldVars = constructor.fields.indices.map(i => Variable(s"__${constructor.name.name}_field_$i"))
    handlerVars = dataDef.constructors.indices.map(i => Variable(s"__${constructor.name.name}_case_$i"))
    fieldTypes <- constructor.fields.map(field => substMany(dataDef.paramVars, typeArgs, field)).traverse(encodeType)
    handlerTypes <- dataDef.constructors.traverse(c => handlerType(owner, typeArgs, dataDef, c, resultType))
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
    withResultType = mkTyAbs(resultVar, Kind.Star, withHandlers)
    withFields = fieldVars.zip(fieldTypes).foldRight(withResultType) { case ((fieldVar, fieldType), body) =>
      mkAbs(fieldVar, fieldType, body)
    }
    withTypeParams = dataDef.params.foldRight(withFields) { case ((v, k), body) => mkTyAbs(v, k, body) }
  } yield withTypeParams

  private def encodeMatchCase(
      owner: TypeVariable,
      args: Seq[TypeRec[Type]],
      dataDef: DataDef,
      resultType: TypeRec[Type],
      matchCase: MatchCase[Para[TypeRec, Encoded]]
  ): Encoded[Expr] = {
    dataDef.constructors.find(_.name == matchCase.constructor) match {
      case None => invariant(s"Constructor ${matchCase.constructor.name} is not defined")
      case Some(constructor) => for {
        encodedBody <- matchCase.body.result
        fieldTypes <- encodeConstructorFields(owner, args, dataDef, constructor)
        _ <- guard(matchCase.binders.length == fieldTypes.length, s"Constructor ${matchCase.constructor.name} expects ${fieldTypes.length} binders, got ${matchCase.binders.length}")
        innerHandler = matchCase.binders.zip(fieldTypes).foldRight(encodedBody) { case ((binder, fieldType), body) =>
          mkAbs(binder, fieldType, body)
        }
        handler = if (constructor.fields.isEmpty) mkAbs(Variable("__unit"), unitTypeT, innerHandler) else innerHandler
        expectedType = thunkIfNullary(constructor.fields, fieldTypes.foldRight(resultType)(arrowT), resultType)
        _ <- guard(Equivalence.alpha(typeOf(handler), expectedType), s"Match case ${matchCase.constructor.name} has unexpected handler type")
      } yield handler
    }
  }

  private def encodeFoldCase(
      owner: TypeVariable,
      args: Seq[TypeRec[Type]],
      dataDef: DataDef,
      foldFunction: TypeRec[Expr],
      resultType: TypeRec[Type],
      foldCase: MatchCase[Para[TypeRec, Encoded]]
  ): Encoded[Expr] = {
    dataDef.constructors.find(_.name == foldCase.constructor) match {
      case None => invariant(s"Constructor ${foldCase.constructor.name} is not defined")
      case Some(constructor) => for {
        encodedBody <- foldCase.body.result
        fieldTypes <- encodeConstructorFields(owner, args, dataDef, constructor)
        substitutedFields = constructor.fields.map(field => substMany(dataDef.paramVars, args, field))
        _ <- guard(foldCase.binders.length == fieldTypes.length, s"Constructor ${foldCase.constructor.name} expects ${fieldTypes.length} binders, got ${foldCase.binders.length}")
        handlerParamVars = foldCase.binders.zip(substitutedFields).zipWithIndex.map {
          case ((binder, field), index) if isDataApplicationOf(field, owner) => Variable(s"__fold_${binder.name}_tail_$index")
          case ((binder, _), _) => binder
        }
        bodyWithAccs = foldCase.binders.zip(handlerParamVars).zip(substitutedFields.zip(fieldTypes)).foldRight(encodedBody) {
          case (((binder, handlerParam), (field, fieldType)), body) =>
            if (isDataApplicationOf(field, owner)) {
              val tailValue = varrType(handlerParam, fieldType)
              letT(binder, typeOf(body), resultType, appT(resultType, foldFunction, tailValue), body)
            } else body
        }
        innerHandler = handlerParamVars.zip(fieldTypes).foldRight(bodyWithAccs) {
          case ((binder, fieldType), expr) => mkAbs(binder, fieldType, expr)
        }
        handler = if (constructor.fields.isEmpty) mkAbs(Variable("__unit"), unitTypeT, innerHandler) else innerHandler
        expectedType = thunkIfNullary(constructor.fields, fieldTypes.foldRight(resultType)(arrowT), resultType)
        _ <- guard(Equivalence.alpha(typeOf(handler), expectedType), s"Fold case ${foldCase.constructor.name} has unexpected handler type")
      } yield handler
    }
  }

  private val encoderAlg: RAlgebra[TypedAST, TypeRec, Encoded] = [x] =>
    (he: TypedAST[Para[TypeRec, Encoded], x]) => (he.ann, he.ast) match {
    case (ann: ProgramAnn, node) => rebuildNode(ann, node)
    case (DeclAnn, node) => rebuildNode(DeclAnn, node)
    case (TypeAnn, node) => encodeType(originalNode(TypeAnn, node))
    case (ExprAnn(t), AST.DataLet(variable, params, constructors, body, recursive)) => for {
      taggedConstructors = constructors.zipWithIndex.map { case (constructor, tag) =>
        ConstructorDef(constructor.name, variable, constructor.fields.map(_.original), tag)
      }
      dataDef = DataDef(params, taggedConstructors, recursive)
      encodedBody <- body.result.local((env: DataEnv) => env.copy(dataTypes = env.dataTypes + (variable -> dataDef)))
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
      encodedScrutinee <- scrutinee.result
      dataApp <- ask.map(env => orInvariant(dataTypeApplication(scrutType, env.dataTypes)(_.paramVars)))
      (owner, dataDef, args) = dataApp
      handlerTypes <- dataDef.constructors.traverse(c => handlerType(owner, args, dataDef, c, resultType))
      resultApplied = tyAppT(handlerTypes.foldRight(resultType)(arrowT), encodedScrutinee, resultType)
      handlers <- dataDef.constructors.traverse[Encode, TypeRec[Expr]] { constructor =>
        cases.find(_.constructor == constructor.name)
          .fold(invariant(s"Match is missing constructor ${constructor.name.name}")) { c =>
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
      encodedScrutinee <- scrutinee.result
      dataApp <- ask.map(env => orInvariant(dataTypeApplication(scrutType, env.dataTypes)(_.paramVars)))
      (owner, dataDef, args) = dataApp
      foldType = arrowT(encodedScrutineeType, resultType)
      foldRef = varrType(foldVariable, foldType)
      foldArgRef = varrType(foldArgument, encodedScrutineeType)
      handlerTypes <- dataDef.constructors.traverse(c => handlerType(owner, args, dataDef, c, resultType))
      resultApplied = tyAppT(handlerTypes.foldRight(resultType)(arrowT), foldArgRef, resultType)
      handlers <- dataDef.constructors.traverse[Encode, TypeRec[Expr]] { constructor =>
        cases.find(_.constructor == constructor.name)
          .fold(invariant(s"Fold is missing constructor ${constructor.name.name}")) { c =>
            encodeFoldCase(owner, args, dataDef, foldRef, resultType, c)
          }
      }
      encodedMatch = applyHandlers(resultApplied, handlers, handlerTypes, resultType)
      foldValue = absT(foldArgument, foldType, encodedScrutineeType, encodedMatch)
      folded = appT(resultType, foldRef, encodedScrutinee)
    } yield letRecT(foldVariable, resultType, foldType, foldValue, folded)
    case (ExprAnn(t), node) => rebuildExprNode(t, node)
  }

  private def encodeDecl(decl: TypeRec[Decl], env: DataEnv): (Seq[TypeRec[Decl]], DataEnv) = decl.project match {
    case AST.TopData(variable, _, _, _) =>
      val dataDef = env.dataTypes.getOrElse(variable, invariant(s"Top-level data type ${variable.name} is missing from ProgramAnn"))
      val constructorDecls = dataDef.constructors.map { constructor =>
        topLetT(constructor.name, constructorType(variable, dataDef, constructor).run(env), constructorValue(variable, dataDef, constructor).run(env))
      }
      (constructorDecls, env)

    case AST.TopTrait(variable, _, _, _) =>
      invariant(s"TopTrait ${variable.name} must be desugared by TraitEncoder before Church encoding")

    case AST.TopImpl(variable, _, _, _, _) =>
      invariant(s"TopImpl ${variable.name} must be desugared by TraitEncoder before Church encoding")

    case AST.TopLetWith(variable, _, _, _, _, _) =>
      invariant(s"TopLetWith ${variable.name} must be desugared by TraitEncoder before Church encoding")

    case AST.TopDerive(traitName, _) =>
      invariant(s"TopDerive ${traitName.name} must be desugared by Deriver before Church encoding")

    case _ =>
      (Seq(decl.para(encoderAlg).run(env)), env)
  }

  def encode(program: TypeRec[AST.Program.type]): TypeRec[AST.Program.type] = program.project match {
    case AST.Program(decls) =>
      val initialEnv = program.extract match {
        case ProgramAnn(env) => DataEnv.from(env)
      }
      val (encodedDecls, _) = decls.foldLeft((Vector.empty[TypeRec[Decl]], initialEnv)) {
        case ((acc, env), decl) =>
          val (newDecls, nextEnv) = encodeDecl(decl, env)
          (acc ++ newDecls, nextEnv)
      }
      programT(encodedDecls)
  }

}
