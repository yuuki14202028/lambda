package com.yuuki14202028

import cats.syntax.all._
import cats.data.{ReaderT, StateT}
import Check.{ask, fail, guard, lift}

object TAnalyser {

  private type TC[I] = Check[TypeRec[I]]

  private def invariant(msg: String): Nothing = sys.error(s"Compiler invariant violation: $msg")

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
    operator = StandardLibrary.binaryOperatorName(op, typeName)
    found <- lift(env.lookupValue(operator).toRight(CompileError.OperatorUndefined(op, typeOf(left), operator)))
    (fn, fnType) = found
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
    operator = StandardLibrary.unaryOperatorName(op, typeName)
    found <- lift(env.lookupValue(operator).toRight(CompileError.OperatorUndefined(op, typeOf(body), operator)))
    (fn, fnType) = found
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


  // ---- 制約 ----

  // 式の型が [C[τ̄]] → τ になった時点で制約を解決し、根拠を InstApp ノードに記録する
  private def discharge(e: TypeRec[Expr]): Check[TypeRec[Expr]] = destructInstArrow(typeOf(e)) match {
    case Some((constraint, to)) => Resolver.resolve(constraint).flatMap(evidence => discharge(instAppT(to, e, evidence)))
    case None => okT(e)
  }

  private def resolveTrait(name: TypeVariable): Check[TypeVariable] =
    ask.flatMap(env => lift(env.resolveType(name).toRight(CompileError.UndefinedTrait(name, None))))

  private def typedConstraint(c: Constraint[TC]): Check[Constraint[TypeRec]] =
    (resolveTrait(c.name), c.arg.traverse(identity)).mapN(Constraint[TypeRec](_, _))

  // trait の存在・引数の数・カインドを確かめ、引数を展開した制約を返す。site はエラー表示用の宣言
  private def expandConstraint(c: Constraint[TypeRec], site: Option[DeclRef]): Check[TypeConstraint] = for {
    resolved <- c.arg.traverse(resolveNames)
    _ <- KAnalyser.checkConstraint(TypeConstraint(c.name, resolved), site)
  } yield TypeConstraint(c.name, resolved.map(Equivalence.normalize))

  // ---- 文字列埋め込みの脱糖 ----
  // `a = {e}` は型検査時に concat("a = ")(show[τ](e)) へ展開する。
  // concat はスコープ中の変数ではなくランタイムの C 関数 concat を Foreign として直接参照する。
  private val showMethod = Variable("show")

  private def interpConcatType: TypeRec[Type] = arrowT(stringTypeT, arrowT(stringTypeT, stringTypeT))

  // String 型でない埋め込み式 e: τ を show[τ](e) に包む
  private def stringifyPart(part: TypeRec[Expr]): Check[TypeRec[Expr]] = {
    val partType = typeOf(part)
    if (Equivalence.alpha(stringTypeT, partType)) okT(part)
    else for {
      env <- ask
      found <- lift(env.lookupValue(showMethod).toRight(
        CompileError.StrInterpShowMissing(partType)))
      (show, surface) = found
      destructed <- lift(destructForAllK(surface).toRight(
        CompileError.StrInterpShowNotPolymorphic(surface)))
      (variable, kind, body) = destructed
      _ <- guard(kind == Kind.Star, CompileError.StrInterpShowKindInvalid(kind))
      applied <- discharge(tyAppT(Equivalence.normalize(substType(variable, partType, body)), varrType(show, surface), partType))
      arrowParts <- lift(destructArrow(typeOf(applied)).toRight(
        CompileError.StrInterpShowNotFunction(typeOf(applied))))
      (from, to) = arrowParts
      _ <- guard(Equivalence.alpha(from, partType), CompileError.StrInterpShowParamMismatch(from, partType))
      _ <- guard(Equivalence.alpha(stringTypeT, to), CompileError.StrInterpShowResultMismatch(stringTypeT, to))
    } yield appT(to, applied, part)
  }

  // ---- 型の展開 ----

  private def underTypeVar[A](variable: TypeVariable, kind: Kind)(body: Check[A]): Check[A] = for {
    env <- ask
    _ <- guard(!env.typeVars.contains(variable), CompileError.AlreadyDefined(NameKind.TypeVariableName, variable.name))
    result <- body.local((e: Env) => e.withTypeVar(variable, kind))
  } yield result

  private def aliasAsTypeAbs(params: Seq[(TypeVariable, Kind)], body: TypeRec[Type]): TypeRec[Type] =
    params.foldRight(body) { case ((v, k), acc) => typeAbsT(v, k, acc) }

  private val resolveNamesAlg: Algebra[TypedAST, TC] = [x] => he => he.ast match {
    case AST.TypeVar(variable) => ask.flatMap { env =>
      if (env.typeVars.contains(variable)) okT(typeVarT(variable))
      else env.typeAliases.get(variable) match {
        case Some(alias) => okT(aliasAsTypeAbs(alias.params, alias.body))
        case None if env.dataTypes.contains(variable) || env.abstractTypes.contains(variable) => okT(typeVarT(variable))
        case None => fail(CompileError.UndefinedTypeVariable(variable))
      }
    }
    case AST.ForAll(variable, kind, body) => underTypeVar(variable, kind)(body).map(forallTypeT(variable, kind, _))
    case AST.TypeAbs(variable, kind, body) => underTypeVar(variable, kind)(body).map(typeAbsT(variable, kind, _))
    case node => node.htraverse([y] => (child: TC[y]) => child).map(HCofree(he.ann, _))
  }

  private def resolveNames(t: TypeRec[Type]): Check[TypeRec[Type]] = t.cata(resolveNamesAlg)

  // 展開済みの型は常に β 正規形になる（比較は Equivalence.alpha で足りる）
  private def expandType(t: TypeRec[Type]): Check[TypeRec[Type]] =
    resolveNames(t).map(Equivalence.normalize)

  // 正規化は well-kinded な型に対してのみ停止が保証されるため、カインドを先に確定する
  private def expandWellKinded(types: TypeRec[Type]): Check[(TypeRec[Type], Kind)] = for {
    resolved <- resolveNames(types)
    kind <- KAnalyser.kindOf(resolved)
  } yield (Equivalence.normalize(resolved), kind)

  private def expandChecked(types: TypeRec[Type]): Check[TypeRec[Type]] =
    expandWellKinded(types).flatMap { case (expanded, kind) =>
      guard(kind == Kind.Star, CompileError.KindNotStar(expanded, kind)).as(expanded)
    }

  // ---- 宣言と式（top-level / in 式）で共有する検査 ----

  private def checkTypeNameFresh(kind: NameKind, name: TypeVariable): Check[Unit] =
    ask.flatMap(env => guard(!env.definesTypeName(name), CompileError.AlreadyDefined(kind, name.name)))

  private def checkParams(owner: DeclRef, params: Seq[(TypeVariable, Kind)]): Check[Unit] = for {
    env <- ask
    _ <- guard(params.map(_._1).distinct.length == params.length, CompileError.DuplicateParams(owner))
    _ <- guard(params.forall { case (p, _) => !env.typeVars.contains(p) }, CompileError.ParamAlreadyDefined(owner))
  } yield ()

  // 評価しても本体が走らない値か（Λ は実行時に残らないので剥がして見る）
  @annotation.tailrec
  private def isFunctionValue(e: TypeRec[Expr]): Boolean = e.project match {
    case AST.TyAbs(_, _, body) => isFunctionValue(body)
    case AST.Abs(_, _, _) | AST.InstAbs(_, _) => true
    case _ => false
  }

  // 自身を参照する let rec の値は関数に限る。そうでないと初期化前の自身を読んでしまう
  private def checkRecursiveValue(variable: Variable, value: TypeRec[Expr]): Check[Unit] =
    guard(isFunctionValue(value) || !freeVars(value).contains(variable), CompileError.RecursiveValueNotFunction(variable))

  // 式の中で制約の解決に使われたインスタンス（文脈として間接的に使われたものを含む）
  private def instancesUsed(e: TypeRec[Expr]): Set[(TypeVariable, Seq[Variable])] = {
    type Keys[I] = Set[(TypeVariable, Seq[Variable])]
    def inEvidence(evidence: Evidence[Keys]): Set[(TypeVariable, Seq[Variable])] = evidence match {
      case Evidence.Given(_) => Set.empty
      case Evidence.Instance(traitName, heads, _, context) => context.flatMap(inEvidence).toSet + instanceKey(traitName, heads)
      case Evidence.Super(_, _, of) => inEvidence(of)
    }
    val alg: Algebra[TypedAST, Keys] = [x] => he => {
      val inChildren = he.ast.htraverse[[a] =>> cats.data.Const[Set[(TypeVariable, Seq[Variable])], a], Keys](
        [y] => (child: Keys[y]) => cats.data.Const(child)
      ).getConst
      he.ast match {
        case AST.InstApp(_, evidence) => inChildren ++ inEvidence(evidence)
        case _ => inChildren
      }
    }
    e.cata(alg)
  }

  // let / let rec: (型付きの注釈, 型付きの値, 展開済みの宣言型)
  private def checkBinding(variable: Variable, types: TC[Type], value: TC[Expr], recursive: Boolean): Check[(TypeRec[Type], TypeRec[Expr], TypeRec[Type])] = for {
    typedTypes <- types
    declaredType <- expandChecked(typedTypes)
    typedValue <- if (recursive) value.local((e: Env) => e.withValue(variable, declaredType)) else value
    _ <- expect(declaredType, typeOf(typedValue))
    _ <- if (recursive) checkRecursiveValue(variable, typedValue) else Check.pure(())
  } yield (typedTypes, typedValue, declaredType)

  // 以下の宣言の検査は「型付きの宣言」と「その宣言が Env に足す定義」を返す。
  // 後者は型付きの宣言だけから決まり（aliasOf / dataOf / traitOf）、module を封印するときに読み直す

  private def aliasOf(params: Seq[(TypeVariable, Kind)], alias: TypeRec[Type]): Check[TypeAlias] =
    expandChecked(alias).local((e: Env) => e.withTypeVars(params)).map(TypeAlias(params, _))

  // type: (型付きの別名本体, 別名を登録する Env の拡張)
  private def checkAlias(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], alias: TC[Type]): Check[(TypeRec[Type], Env => Env)] = for {
    _ <- checkTypeNameFresh(NameKind.TypeAliasName, variable)
    _ <- checkParams(DeclRef.TypeAliasDecl(variable), params)
    typedAlias <- alias.local((e: Env) => e.withTypeVars(params))
    definition <- aliasOf(params, typedAlias)
  } yield (typedAlias, (e: Env) => e.withAlias(variable, definition))

  private def constructorType(owner: TypeVariable, params: Seq[(TypeVariable, Kind)], fields: Seq[TypeRec[Type]]): TypeRec[Type] = {
    val result = applyTypeConstructor(owner, params.map { case (v, _) => typeVarT(v) })
    params.foldRight(fields.foldRight(result)(arrowT)) { case ((v, k), body) => forallTypeT(v, k, body) }
  }

  // フィールドの検査中は、自身を参照できるようコンストラクタなしの定義を仮登録する
  private def fieldScope(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], recursive: Boolean)(e: Env): Env =
    e.withData(variable, DataDef(params, Seq.empty, recursive)).withTypeVars(params)

  private def dataOf(
      variable: TypeVariable,
      params: Seq[(TypeVariable, Kind)],
      constructors: Seq[DataConstructor[TypeRec]],
      recursive: Boolean
  ): Check[DataDef] =
    constructors.zipWithIndex.traverse { case (c, tag) =>
      c.fields.traverse(field => expandChecked(field).local(fieldScope(variable, params, recursive))).map(ConstructorDef(c.name, variable, _, tag))
    }.map(DataDef(params, _, recursive))

  private def defineData(variable: TypeVariable, dataDef: DataDef)(e: Env): Env =
    e.withData(variable, dataDef)
      .withValues(dataDef.constructors.map(c => c.name -> constructorType(variable, dataDef.params, c.fields)))
      .copy(constructors = e.constructors ++ dataDef.constructors.map(c => c.name -> c))

  // data: (型付きのコンストラクタ, データ型とコンストラクタを登録する Env の拡張)
  private def checkData(
      variable: TypeVariable,
      params: Seq[(TypeVariable, Kind)],
      constructors: Seq[DataConstructor[TC]],
      recursive: Boolean
  ): Check[(Seq[DataConstructor[TypeRec]], Env => Env)] = {
    val owner = DeclRef.DataDecl(variable)
    val names = constructors.map(_.name)
    for {
      env <- ask
      _ <- checkTypeNameFresh(NameKind.DataTypeName, variable)
      _ <- checkParams(owner, params)
      _ <- guard(names.distinct.length == names.length, CompileError.DuplicateMembers(owner, MemberKind.ConstructorMember))
      _ <- guard(
        names.forall(name => !env.values.contains(name) && !env.constructors.contains(name)),
        CompileError.MemberAlreadyDefined(owner, MemberKind.ConstructorMember)
      )
      typed <- constructors.traverse(c => c.fields.traverse(_.local(fieldScope(variable, params, recursive))).map(DataConstructor[TypeRec](c.name, _)))
      dataDef <- dataOf(variable, params, typed, recursive)
      _ <- guard(
        recursive || !dataDef.constructors.exists(_.fields.exists(field => containsDataApplicationOf(field, variable))),
        CompileError.RecursiveDataNotDeclared(variable)
      )
    } yield (typed, defineData(variable, dataDef))
  }

  // trait C[p̄] は辞書のデータ型 C[p̄] = MkC(スーパークラスの辞書…)(メソッド…) でもある
  private def traitOf(
      variable: TypeVariable,
      params: Seq[(TypeVariable, Kind)],
      supers: Seq[Constraint[TypeRec]],
      sigs: Seq[(Variable, TypeRec[Type])]
  ): Check[Env => Env] = {
    val scope = (e: Env) => e.withTypeVars(params)
    for {
      expandedSupers <- supers.traverse(expandConstraint(_, Some(DeclRef.TraitDecl(variable)))).local(scope)
      expandedSigs <- sigs.traverse { case (name, sig) => expandChecked(sig).local(scope).map(name -> _) }
    } yield {
      val ctorName = dictionaryConstructor(variable)
      val fields = expandedSupers.map(dictionaryType) ++ expandedSigs.map(_._2)
      val ctorDef = ConstructorDef(ctorName, variable, fields, 0)
      (e: Env) => e
        .withData(variable, DataDef(params, Seq(ctorDef), recursive = false))
        .withValues(expandedSigs.map { case (name, sig) => name -> methodType(variable, params, sig) } :+ (ctorName -> constructorType(variable, params, fields)))
        .copy(constructors = e.constructors + (ctorName -> ctorDef), traits = e.traits + (variable -> TraitDef(params, expandedSigs, expandedSupers)))
    }
  }

  // match / fold: 網羅性と束縛子を検査し、各分岐の本体を型付けする。
  // binderTypes は (データ型名, 型引数を適用したフィールド型) から束縛子の型を決める
  private def checkCases(kind: MatchKindRef, scrutineeType: TypeRec[Type], cases: Seq[MatchCase[TC]])(
      binderTypes: (TypeVariable, Seq[TypeRec[Type]]) => Seq[TypeRec[Type]]
  ): Check[Seq[MatchCase[TypeRec]]] = for {
    env <- ask
    dataApp <- lift(dataTypeApplication(scrutineeType, env.dataTypes)(_.paramVars))
    (dataName, dataDef, typeArgs) = dataApp
    // 分岐のコンストラクタ名も、式の中の名前と同じ規則で解決する
    named <- cases.traverse { c =>
      lift(env.resolveValue(c.constructor).toRight(CompileError.UndefinedVariable(c.constructor))).map(MatchCase[TC](_, c.binders, c.body))
    }
    caseNames = named.map(_.constructor)
    expected = dataDef.constructors.map(_.name)
    _ <- guard(caseNames.distinct.length == caseNames.length, CompileError.DuplicateCases(kind))
    missing = expected.filterNot(caseNames.contains)
    extra = caseNames.filterNot(expected.contains)
    _ <- guard(missing.isEmpty && extra.isEmpty, CompileError.NonExhaustive(kind, missing, extra))
    typedCases <- named.traverse { c =>
      val constructor = dataDef.constructors.find(_.name == c.constructor)
        .getOrElse(invariant(s"constructor ${c.constructor.name} passed the exhaustiveness check but is not defined"))
      val fieldTypes = constructor.fields.map(field => Equivalence.normalize(substMany(dataDef.paramVars, typeArgs, field)))
      for {
        _ <- guard(
          c.binders.length == fieldTypes.length,
          CompileError.BinderArityMismatch(c.constructor, fieldTypes.length, c.binders.length)
        )
        _ <- guard(c.binders.distinct.length == c.binders.length, CompileError.DuplicateBinders(kind, c.constructor))
        typedBody <- c.body.local((e: Env) => e.withValues(c.binders.zip(binderTypes(dataName, fieldTypes))))
      } yield MatchCase[TypeRec](c.constructor, c.binders, typedBody)
    }
  } yield typedCases

  // context[M] が使うメソッド参照 name[M]（M の制約もここで解決する）
  private def contextMethod(name: Variable, monad: TypeRec[Type]): Check[TypeRec[Expr]] = for {
    env <- ask
    found <- lift(env.lookupValue(name).toRight(CompileError.ContextMethodMissing(name)))
    (method, methodType) = found
    applied <- lift(ContextDesugar.applyTypeArg(varrType(method, methodType), monad, ContextDesugar.monadKind))
    resolved <- discharge(applied)
  } yield resolved

  // ---- 式と型 ----

  private def declarationNode(node: String): Nothing = invariant(s"$node is a declaration and must be checked by checkDecl")

  private val tcAlg: Algebra[AST, TC] = [x] => (node: AST[TC, x]) => node match {

    case AST.Program(_) => declarationNode("Program")
    case AST.TopLet(_, _, _) => declarationNode("TopLet")
    case AST.TopLetRec(_, _, _) => declarationNode("TopLetRec")
    case AST.TopImport(_) => declarationNode("TopImport")
    case AST.TopType(_, _, _) => declarationNode("TopType")
    case AST.TopData(_, _, _, _) => declarationNode("TopData")
    case AST.TopTrait(_, _, _, _) => declarationNode("TopTrait")
    case AST.TopImpl(_, _, _, _, _, _, _) => declarationNode("TopImpl")
    case AST.TopDerive(_, _) => declarationNode("TopDerive")
    case AST.TopModule(_, _) => declarationNode("TopModule")
    case AST.TopOpen(_) => declarationNode("TopOpen")
    case AST.TopModified(_, _) => declarationNode("TopModified")

    case AST.Abs(variable, types, body) => for {
      typedTypes <- types
      paramType <- expandChecked(typedTypes)
      typedBody <- body.local((e: Env) => e.withValue(variable, paramType))
    } yield absT(variable, arrowT(paramType, typeOf(typedBody)), typedTypes, typedBody)

    case AST.TyAbs(variable, kind, body) =>
      underTypeVar(variable, kind)(body).map(typedBody => tyAbsT(variable, forallTypeT(variable, kind, typeOf(typedBody)), kind, typedBody))

    case AST.InstAbs(constraint, body) => for {
      typed <- typedConstraint(constraint)
      assumed <- expandConstraint(typed, None)
      typedBody <- body.local((e: Env) => e.withGivens(Seq(assumed)))
    } yield instAbsT(instArrowT(assumed, typeOf(typedBody)), typed, typedBody)

    case AST.InstApp(_, _) => invariant("InstApp is inserted by the type checker and cannot appear in its input")

    case AST.Let(variable, types, value, body) => for {
      binding <- checkBinding(variable, types, value, recursive = false)
      (typedTypes, typedValue, declaredType) = binding
      typedBody <- body.local((e: Env) => e.withValue(variable, declaredType))
    } yield letT(variable, typeOf(typedBody), typedTypes, typedValue, typedBody)

    case AST.LetRec(variable, types, value, body) => for {
      binding <- checkBinding(variable, types, value, recursive = true)
      (typedTypes, typedValue, declaredType) = binding
      typedBody <- body.local((e: Env) => e.withValue(variable, declaredType))
    } yield letRecT(variable, typeOf(typedBody), typedTypes, typedValue, typedBody)

    case AST.TypeLet(variable, params, alias, body) => for {
      checked <- checkAlias(variable, params, alias)
      (typedAlias, extend) = checked
      typedBody <- body.local(extend)
    } yield typeLetT(variable, params, typeOf(typedBody), typedAlias, typedBody)

    case AST.DataLet(variable, params, constructors, body, recursive) => for {
      checked <- checkData(variable, params, constructors, recursive)
      (typedConstructors, extend) = checked
      typedBody <- body.local(extend)
    } yield dataLetT(variable, params, typeOf(typedBody), typedConstructors, typedBody, recursive)

    case AST.Match(scrutinee, cases) => for {
      typedScrutinee <- scrutinee
      typedCases <- checkCases(MatchKindRef.MatchExpr, typeOf(typedScrutinee), cases)((_, fieldTypes) => fieldTypes)
      resultType <- typedCases.headOption match {
        case Some(first) => typedCases.tail.traverse(c => expect(typeOf(first.body), typeOf(c.body))).as(typeOf(first.body))
        case None => fail(CompileError.EmptyMatch)
      }
    } yield matchExprT(resultType, typedScrutinee, typedCases)

    case AST.Fold(scrutinee, resultTypeNode, cases) => for {
      typedScrutinee <- scrutinee
      typedResultType <- resultTypeNode
      resultType <- expandChecked(typedResultType)
      // 再帰位置のフィールドは、畳み込み済みの結果として束縛される
      typedCases <- checkCases(MatchKindRef.FoldExpr, typeOf(typedScrutinee), cases) { (dataName, fieldTypes) =>
        fieldTypes.map(field => if (isDataApplicationOf(field, dataName)) resultType else field)
      }
      _ <- typedCases.traverse_(c => expect(resultType, typeOf(c.body)))
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
      resolved <- discharge(tyAppT(resultType, typedFunction, typedArgument))
    } yield resolved

    case AST.Foreign(value, types) => for {
      typedTypes <- types
      declaredType <- expandChecked(typedTypes)
      _ <- expectForeignType(declaredType)
    } yield foreignT(value, declaredType, typedTypes)

    case AST.Var(value) => for {
      env <- ask
      found <- lift(env.lookupValue(value).toRight(CompileError.UndefinedVariable(value)))
      (name, t) = found
      resolved <- discharge(varrType(name, t))
    } yield resolved

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

    case AST.Context(monad, bindings, result, _) => for {
      typedMonad <- monad
      expandedMonad <- expandWellKinded(typedMonad)
      (monadType, monadKind) = expandedMonad
      _ <- guard(
        monadKind == ContextDesugar.monadKind,
        CompileError.ContextMonadKindInvalid(monadType, monadKind)
      )
      checkedBindings <- bindings.toList.foldLeftM(
        (Map.empty[Variable, TypeRec[Type]], List.empty[ContextBinding[TypeRec]])
      ) { case ((scope, done), b) =>
        val scopeFn = (e: Env) => e.withValues(scope)
        for {
          typedAnn <- b.annotation.local(scopeFn)
          annType <- expandChecked(typedAnn)
          valueScope =
            if (!b.monadic && b.recursive) (e: Env) => scopeFn(e).withValue(b.name, annType)
            else scopeFn
          typedValue <- b.value.local(valueScope)
          _ <-
            if (b.monadic) expect(typeAppT(monadType, annType), typeOf(typedValue))
            else expect(annType, typeOf(typedValue))
          _ <- if (!b.monadic && b.recursive) checkRecursiveValue(b.name, typedValue) else Check.pure(())
        } yield (scope + (b.name -> annType), done :+ ContextBinding[TypeRec](b.name, annType, typedValue, b.monadic, b.recursive))
      }
      (bindersScope, typedBindings) = checkedBindings
      typedResult <- result.local((e: Env) => e.withValues(bindersScope))
      pureRef <- contextMethod(ContextDesugar.pureMethod, monadType)
      flatMapRef <- contextMethod(ContextDesugar.flatMapMethod, monadType)
      ops = ContextOps[TypeRec](pureRef, flatMapRef)
      // ノードは保持したまま、脱糖後の式の型をこのノードの型とする
      chain <- lift(ContextDesugar.desugarContext(ops, typedBindings, typedResult))
    } yield contextExprT(typeOf(chain), monadType, typedBindings, typedResult, ops)

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
      thenType = typeOf(typedThen)
      _ <- expect(boolTypeT, typeOf(typedCond))
      _ <- expect(thenType, typeOf(typedElse))
    } yield ifT(thenType, typedCond, typedThen, typedElse)

    case AST.Primitive(name) =>
      BuiltinTypes.arity(name) match {
        case Some(_) => okT(primitiveT(name))
        case None => fail(CompileError.UndefinedPrimitive(name))
      }
    case AST.TypeVar(variable) => for {
      env <- ask
      name <- lift(env.resolveType(variable).filter(env.definesTypeName).toRight(CompileError.UndefinedTypeVariable(variable)))
    } yield typeVarT(name)
    case AST.Arrow(from, to) => (from, to).mapN(arrowT)
    case AST.InstArrow(constraint, to) => (typedConstraint(constraint), to).mapN(instArrowT)
    case AST.ForAll(variable, kind, body) => underTypeVar(variable, kind)(body).map(forallTypeT(variable, kind, _))
    case AST.TypeAbs(variable, kind, body) => underTypeVar(variable, kind)(body).map(typeAbsT(variable, kind, _))
    case AST.TypeApp(function, argument) => (function, argument).mapN(typeAppT)
  }

  private val indexedAlg: Algebra[IndexedAST, TC] = [x] => he =>
    ReaderT((env: Env) => tcAlg(he.lower).run(env).left.map(_.at(he.ask)))

  private def check[I](t: IndexedRec[I]): TC[I] = t.cata(indexedAlg)

  // ---- 宣言 ----
  // 宣言は検査結果と、後続の宣言から見える Env を同時に返す

  private type Checked = Check[(TypeRec[Decl], Env)]

  private def extending(decl: TypeRec[Decl])(extend: Env => Env): Checked = ask.map(env => (decl, extend(env)))

  // module の中の宣言は、短い名前 x を完全な名前 M.x で定義する
  private def memberName(variable: Variable): Check[Variable] = ask.map(env => Variable(env.qualify(variable.name)))
  private def memberType(variable: TypeVariable): Check[TypeVariable] = ask.map(env => TypeVariable(env.qualify(variable.name)))

  private def shortName(name: String): String = name.substring(name.lastIndexOf('.') + 1)

  private def declRule(decl: AST[TC, Decl]): Checked = decl match {

    case AST.TopLet(variable, types, value) => for {
      name <- memberName(variable)
      binding <- checkBinding(name, types, value, recursive = false)
      (typedTypes, typedValue, declaredType) = binding
      checked <- extending(topLetT(name, typedTypes, typedValue))(_.withValue(name, declaredType).exportValue(variable))
    } yield checked

    case AST.TopLetRec(variable, types, value) => for {
      name <- memberName(variable)
      // 値の中からは、自身を短い名前で参照できる
      binding <- checkBinding(name, types, value.local((e: Env) => e.exportValue(variable)), recursive = true)
      (typedTypes, typedValue, declaredType) = binding
      checked <- extending(topLetRecT(name, typedTypes, typedValue))(_.withValue(name, declaredType).exportValue(variable))
    } yield checked

    case AST.TopImport(path) => fail(CompileError.UnresolvedImport(path))

    case AST.TopType(variable, params, alias) => for {
      name <- memberType(variable)
      checked <- checkAlias(name, params, alias)
      (typedAlias, extend) = checked
      result <- extending(topTypeT(name, params, typedAlias))(e => extend(e).exportType(variable))
    } yield result

    case AST.TopData(variable, params, constructors, recursive) => for {
      env <- ask
      name <- memberType(variable)
      // フィールドからは、自身を短い名前で参照できる
      members = constructors.map(c => DataConstructor[TC](Variable(env.qualify(c.name.name)), c.fields.map(_.local((e: Env) => e.exportType(variable)))))
      checked <- checkData(name, params, members, recursive)
      (typedConstructors, extend) = checked
      result <- extending(topDataT(name, params, typedConstructors, recursive)) { e =>
        constructors.foldLeft(extend(e).exportType(variable))((acc, c) => acc.exportValue(c.name))
      }
    } yield result

    case AST.TopTrait(variable, params, supers, methods) =>
      val scope = (e: Env) => e.withTypeVars(params)
      for {
        env <- ask
        name <- memberType(variable)
        owner = DeclRef.TraitDecl(name)
        methodNames = methods.map(m => Variable(env.qualify(m.name.name)))
        ctorName = dictionaryConstructor(name)
        _ <- checkTypeNameFresh(NameKind.TraitName, name)
        _ <- guard(params.nonEmpty, CompileError.TraitNeedsParameter(name))
        _ <- checkParams(owner, params)
        _ <- guard(methodNames.distinct.length == methodNames.length, CompileError.DuplicateMembers(owner, MemberKind.MethodMember))
        _ <- guard(
          methodNames.forall(method => !env.values.contains(method) && !env.constructors.contains(method)),
          CompileError.MemberAlreadyDefined(owner, MemberKind.MethodMember)
        )
        _ <- guard(
          !env.values.contains(ctorName) && !env.constructors.contains(ctorName),
          CompileError.DictionaryCtorAlreadyDefined(name, ctorName)
        )
        _ <- guard(methods.forall(_.body.isEmpty), CompileError.DefaultMethodsUnsupported(name))
        typedSupers <- supers.traverse(typedConstraint).local(scope)
        typedSigs <- methods.zip(methodNames).traverse { case (m, method) => m.sig.local(scope).map(method -> _) }
        define <- traitOf(name, params, typedSupers, typedSigs)
        typed = topTraitT(name, params, typedSupers, typedSigs.map { case (method, sig) => MethodSig[TypeRec](method, sig, None) })
        result <- extending(typed) { e =>
          (methods.map(_.name) :+ Variable(shortName(ctorName.name))).foldLeft(define(e).exportType(variable))((acc, short) => acc.exportValue(short))
        }
      } yield result

    case AST.TopImpl(writtenTrait, implParams, targets, context, writtenMethods, _, recursive) =>
      val implScope = (e: Env) => e.withTypeVars(implParams)
      for {
        env <- ask
        traitName <- resolveTrait(writtenTrait)
        traitDef <- lift(env.traits.get(traitName).toRight(CompileError.UndefinedTrait(traitName, None)))
        paramVars = traitDef.paramVars
        _ <- guard(
          targets.length == traitDef.param.length,
          CompileError.ImplTargetArity(traitName, traitDef.param.length, targets.length)
        )
        _ <- checkParams(DeclRef.ImplDecl(traitName, Seq.empty), implParams)
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
        key = instanceKey(traitName, headNames)
        _ <- guard(
          headNames.forall(h => !implParams.map(_._1).contains(TypeVariable(h))),
          CompileError.InstanceHeadTypeParameter(implRef)
        )
        _ <- guard(
          implParams.forall { case (p, _) => expandedTargets.exists(t => freeTypeVars(t).contains(p)) },
          CompileError.ImplParamUnused(implRef)
        )
        _ <- guard(!env.instances.contains(key), CompileError.OverlappingInstance(traitName, headNames))
        typedContext <- context.traverse(typedConstraint).local(implScope)
        expandedContext <- typedContext.traverse(expandConstraint(_, Some(implRef))).local(implScope)
        inst = InstanceDef(traitName, expandedTargets, expandedContext, instanceDictionaryName(traitName, headNames), implParams)
        // スーパークラスの解決とメソッド本体からは、impl の文脈がスコープ中の制約として見える。
        // impl rec ではさらに、メソッド本体から自身のインスタンスが見える
        contextScope = (e: Env) => implScope(e).withGivens(expandedContext)
        bodyScope = (e: Env) => if (recursive) contextScope(e).copy(instances = e.instances + (key -> inst)) else contextScope(e)
        traitMethodNames = traitDef.methods.map(_._1)
        // impl のメソッドは短い名前で書き、trait のメソッド（module の中なら M.m）に対応づける
        methods = writtenMethods.map { m =>
          MethodImpl[TC](traitMethodNames.find(t => shortName(t.name) == m.name.name).getOrElse(m.name), m.sig, m.body)
        }
        implMethodNames = methods.map(_.name)
        _ <- guard(implMethodNames.distinct.length == implMethodNames.length, CompileError.DuplicateMembers(implRef, MemberKind.MethodMember))
        missing = traitMethodNames.filterNot(implMethodNames.contains)
        _ <- guard(missing.isEmpty, CompileError.MissingMethods(implRef, missing))
        extra = implMethodNames.filterNot(traitMethodNames.contains)
        _ <- guard(extra.isEmpty, CompileError.ExtraMethods(implRef, extra))
        // メソッドの宣言シグネチャでは trait のパラメータ名も使える（ターゲットに読み替える）。
        // ただし impl の型パラメータと同名のものは impl 側を指す
        sigParams = traitDef.param.zip(expandedTargets).filterNot { case ((p, _), _) => implParams.exists(_._1 == p) }
        sigScope = (e: Env) => implScope(e).withTypeVars(sigParams.map(_._1))
        typedMethods <- methods.traverse { m =>
          val expected = Equivalence.normalize(substMany(paramVars, expandedTargets, traitDef.methods.find(_._1 == m.name).get._2))
          for {
            typedBody <- m.body.local(bodyScope)
            _ <- guard(
              Equivalence.alpha(expected, typeOf(typedBody)),
              CompileError.MethodBodyTypeMismatch(implRef, m.name, expected, typeOf(typedBody))
            )
            // 文脈のない impl rec の辞書は関数ではないので、自身を使うメソッドは関数でなければ初期化前の辞書を読む
            _ <- guard(
              !recursive || expandedContext.nonEmpty || isFunctionValue(typedBody) || !instancesUsed(typedBody).contains(key),
              CompileError.RecursiveInstanceMethodNotFunction(implRef, m.name)
            )
            typedSig <- m.sig.traverse(_.local(sigScope))
            _ <- typedSig.traverse_ { s =>
              expandType(s).local(sigScope).flatMap { expandedSig =>
                val declared = Equivalence.normalize(substMany(sigParams.map(_._1._1), sigParams.map(_._2), expandedSig))
                guard(
                  Equivalence.alpha(expected, declared),
                  CompileError.MethodSigTypeMismatch(implRef, m.name, declared, expected)
                )
              }
            }
          } yield MethodImpl[TypeRec](m.name, typedSig, typedBody)
        }
        superEvidence <- Resolver.superConstraints(traitDef, expandedTargets).traverse(Resolver.resolve).local(contextScope)
        // 後段がインスタンスを引けるよう、ターゲットは展開済みの型で持つ
        typed = topImplT(traitName, implParams, expandedTargets, typedContext, typedMethods, superEvidence, recursive)
      } yield (typed, env.copy(instances = env.instances + (key -> inst)))

    case AST.TopDerive(writtenTrait, writtenTarget) => for {
      env <- ask
      traitName <- resolveTrait(writtenTrait)
      target <- lift(env.resolveType(writtenTarget).toRight(CompileError.UndefinedTypeVariable(writtenTarget)))
      inst <- lift(Deriver.check(traitName, target, env))
    } yield (topDeriveT(traitName, target), env.copy(instances = env.instances + (instanceKey(traitName, Seq(target.name)) -> inst)))
    case AST.TopModule(_, _) => declarationNode("TopModule")
    case AST.TopOpen(_) => declarationNode("TopOpen")
    case AST.TopModified(_, _) => declarationNode("TopModified")
  }

  // ---- module ----

  /** module を閉じるとき、中の宣言が Env に足した定義を「外から見た形」に読み直す。
   * opaque type は module の中では通常の別名だが、ここで中身のわからない型（Env.abstractTypes）に変わり、
   * それより後の宣言の型は別名を展開しない形で登録し直される。インスタンスは module の中で登録した形のまま残る
   */
  private def sealDecls(decls: Seq[TypeRec[Decl]]): Check[Env] =
    decls.foldLeft(ask)((sealedSoFar, decl) => sealedSoFar.flatMap(env => sealDecl(decl).local((_: Env) => env)))

  private def sealDecl(decl: TypeRec[Decl]): Check[Env] = decl.project match {
    case AST.TopLet(variable, types, _) => (expandChecked(types), ask).mapN((t, env) => env.withValue(variable, t))
    case AST.TopLetRec(variable, types, _) => (expandChecked(types), ask).mapN((t, env) => env.withValue(variable, t))
    case AST.TopType(variable, params, alias) => (aliasOf(params, alias), ask).mapN((definition, env) => env.withAlias(variable, definition))
    case AST.TopData(variable, params, constructors, recursive) =>
      (dataOf(variable, params, constructors, recursive), ask).mapN((dataDef, env) => defineData(variable, dataDef)(env))
    case AST.TopTrait(variable, params, supers, methods) =>
      (traitOf(variable, params, supers, methods.map(m => m.name -> m.sig)), ask).mapN((define, env) => define(env))
    case AST.TopModule(_, decls) => sealDecls(decls)
    case AST.TopModified(Modifier.Opaque, inner) => inner.project match {
      case AST.TopType(variable, _, _) => ask.map { env =>
        env.typeAliases.get(variable).fold(env) { representation =>
          env.copy(typeAliases = env.typeAliases - variable, abstractTypes = env.abstractTypes + (variable -> representation))
        }
      }
      case _ => sealDecl(inner)
    }
    case AST.TopModified(Modifier.Private, inner) => sealDecl(inner)
    case AST.TopImpl(_, _, _, _, _, _, _) | AST.TopDerive(_, _) | AST.TopOpen(_) | AST.TopImport(_) => ask
  }

  private def checkModule(name: String, decls: Seq[IndexedRec[Decl]]): StateT[EitherS, Env, TypeRec[Decl]] = StateT { outer =>
    val full = outer.qualify(name)
    for {
      _ <- Either.cond(!outer.modules.contains(full), (), CompileError.AlreadyDefined(NameKind.ModuleName, full))
      checked <- decls.traverse(checkDecl).run(outer.copy(path = outer.path :+ name, members = Names.empty))
      (inner, typedDecls) = checked
      sealedEnv <- sealDecls(typedDecls).run(inner)
    } yield (
      sealedEnv.copy(
        path = outer.path,
        scope = outer.scope.withModule(name, full),
        members = outer.members.withModule(name, full),
        modules = sealedEnv.modules + (full -> inner.members)
      ),
      topModuleT(name, typedDecls)
    )
  }

  private def checkOpen(path: Seq[String]): StateT[EitherS, Env, TypeRec[Decl]] = StateT { env =>
    env.resolveModule(path).toRight(CompileError.UndefinedModule(path)).map(opened => (env.copy(scope = env.scope ++ opened), topOpenT(path)))
  }

  private def checkModified(modifier: Modifier, inner: IndexedRec[Decl]): StateT[EitherS, Env, TypeRec[Decl]] = for {
    before <- StateT.get[EitherS, Env]
    _ <- StateT.liftF[EitherS, Env, Unit](Either.cond(before.path.nonEmpty, (), CompileError.ModifierOutsideModule(modifier)))
    typed <- checkDecl(inner)
    _ <- StateT.modify[EitherS, Env] { after =>
      (modifier, typed.project) match {
        case (Modifier.Private, _) => after.copy(members = before.members)
        // opaque data は型の名前だけを公開する
        case (Modifier.Opaque, AST.TopData(_, _, constructors, _)) =>
          val hidden = constructors.map(_.name).toSet
          after.copy(members = after.members.copy(values = after.members.values.filterNot { case (_, full) => hidden(full) }))
        // opaque type は module を閉じるときに封印する（sealDecl）
        case (Modifier.Opaque, _) => after
      }
    }
  } yield topModifiedT(modifier, typed)

  private def checkDecl(decl: IndexedRec[Decl]): StateT[EitherS, Env, TypeRec[Decl]] = {
    val checked = decl.project match {
      case AST.TopModule(name, decls) => checkModule(name, decls)
      case AST.TopOpen(path) => checkOpen(path)
      case AST.TopModified(modifier, inner) => checkModified(modifier, inner)
      case node => StateT[EitherS, Env, TypeRec[Decl]] { env =>
        declRule(node.hmap([x] => (child: IndexedRec[x]) => check(child))).run(env).map { case (typedDecl, nextEnv) => (nextEnv, typedDecl) }
      }
    }
    StateT(env => checked.run(env).left.map(_.at(decl.extract)))
  }

  private def checkMain(env: Env): EitherS[Unit] = env.values.get(Variable("main")) match {
    case Some(t) if Equivalence.alpha(t, arrowT(unitTypeT, intTypeT)) => Right(())
    case Some(t) => Left(CompileError.MainTypeInvalid(t))
    case None => Left(CompileError.MainMissing)
  }

  def validate(prog: IndexedRec[AST.Program.type]): Either[CompileError, TypeRec[AST.Program.type]] = prog.project match {
    case AST.Program(decls) => decls.traverse(checkDecl).run(Env.empty).flatMap { case (env, typedDecls) =>
      checkMain(env).as(programT(typedDecls, env))
    }
  }

}
