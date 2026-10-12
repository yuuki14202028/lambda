package com.yuuki14202028

/** trait / impl と制約を辞書渡しへ脱糖する。
 *
 * 制約の解決は型検査が済ませて根拠（Evidence）を記録しているので、ここでは機械的に置き換えるだけで、
 * ユーザー起因のエラーは起こらない:
 *   [C[τ̄]] → τ  ↦  C[τ̄] → τ          λ[C[τ̄]]. e  ↦  λ$with_n: C[τ̄]. e          InstApp(f, 根拠)  ↦  f(辞書)
 */
object TraitEncoder {

  private def invariant(msg: String): Nothing = sys.error(s"Compiler invariant violation: $msg")

  // スコープ中の制約と、その辞書を指す変数参照
  private type Scope = Seq[(TypeConstraint, TypeRec[Expr])]
  private type Encoded[I] = Scope => TypeRec[I]

  def encode(program: TypeRec[AST.Program.type]): TypeRec[AST.Program.type] = program.project match {
    case AST.Program(decls) =>
      val env = program.extract match {
        case ProgramAnn(e) => e
      }
      programT(decls.flatMap(encodeDecl(_, env)), env.copy(dataTypes = env.dataTypes ++ env.traits.keys.map(name => name -> dictionaryData(name, env))))
  }

  // 後段（ChurchEncoder）が見る辞書のデータ型定義。フィールドは脱糖済みの型にする
  private def dictionaryData(name: TypeVariable, env: Env): DataDef = {
    val traitDef = traitOf(name, env)
    DataDef(traitDef.param, Seq(ConstructorDef(dictionaryConstructor(name), name, dictionaryFields(traitDef, env), 0)), recursive = false)
  }

  private def encodeDecl(decl: TypeRec[Decl], env: Env): Seq[TypeRec[Decl]] = decl.project match {
    case AST.TopTrait(name, params, _, _) => encodeTrait(name, params, env)
    case AST.TopImpl(name, _, targets, _, methods, superEvidence) => Seq(encodeImpl(name, targets, methods, superEvidence, env))
    case _ => Seq(encodeIn(decl, Seq.empty, env))
  }

  private def encodeIn[I](t: TypeRec[I], scope: Scope, env: Env): TypeRec[I] = t.cata(encodeAlg(env))(scope)

  private def encodeType(t: TypeRec[Type], env: Env): TypeRec[Type] = encodeIn(t, Seq.empty, env)

  private def encodeAlg(env: Env): Algebra[TypedAST, Encoded] = [x] => he => scope => (he.ann, he.ast) match {
    case (TypeAnn, AST.InstArrow(constraint, to)) =>
      arrowT(dictionaryType(Constraint[TypeRec](constraint.name, constraint.arg.map(_(scope)))), to(scope))

    case (ExprAnn(t), AST.InstAbs(_, body)) =>
      // 展開済みの制約は注釈の型が持っている
      val (assumed, _) = destructInstArrow(t).getOrElse(invariant(s"InstAbs must have an instance-implicit type, got ${t.show}"))
      val dictVar = Variable(s"$$with_${scope.length}")
      val dictType = encodeType(dictionaryType(assumed), env)
      mkAbs(dictVar, dictType, body(scope :+ (assumed -> varrType(dictVar, dictType))))

    case (ExprAnn(_), AST.InstApp(function, evidence)) =>
      mkApp(function(scope), dictionary(evidence.hmap([y] => (child: Encoded[y]) => child(scope)), scope, env))

    case (ExprAnn(t), node) => HCofree(ExprAnn(encodeType(t, env)), node.hmap([y] => (child: Encoded[y]) => child(scope)))
    case (ann, node) => HCofree(ann, node.hmap([y] => (child: Encoded[y]) => child(scope)))
  }

  // 根拠から辞書式を組み立てる
  private def dictionary(evidence: Evidence[TypeRec], scope: Scope, env: Env): TypeRec[Expr] = evidence match {
    case Evidence.Given(constraint) =>
      scope.collectFirst { case (assumed, ref) if Resolver.sameConstraint(assumed, constraint) => ref }
        .getOrElse(invariant(s"no dictionary in scope for ${dictionaryType(constraint).show}"))

    // $inst_C_h̄[ā := 束縛](文脈の辞書…)
    case Evidence.Instance(traitName, heads, typeArgs, context) =>
      val inst = env.instances.getOrElse(
        instanceKey(traitName, heads),
        invariant(s"instance ${traitName.name}${heads.map(h => s"[$h]").mkString} is missing from ProgramAnn")
      )
      val applied = typeArgs.foldLeft(varrType(inst.dictName, encodeType(instanceType(inst), env)))(mkTyApp)
      context.map(dictionary(_, scope, env)).foldLeft(applied)(mkApp)

    // $super_C_i[τ̄](C[τ̄] の辞書)
    case Evidence.Super(traitName, index, of) =>
      val dict = dictionary(of, scope, env)
      val traitDef = traitOf(traitName, env)
      val (_, args) = collectTypeApps(typeOf(dict))
      val projection = varrType(superDictionaryName(traitName, index), projectionType(traitName, traitDef, dictionaryType(traitDef.supers(index))))
      mkApp(args.foldLeft(projection)(mkTyApp), dict)
  }

  private def traitOf(name: TypeVariable, env: Env): TraitDef =
    env.traits.getOrElse(name, invariant(s"trait ${name.name} is missing from ProgramAnn"))

  // 辞書のフィールド: スーパークラスの辞書、続いてメソッド（trait の宣言順）
  private def dictionaryFields(traitDef: TraitDef, env: Env): Seq[TypeRec[Type]] =
    traitDef.supers.map(dictionaryType) ++ traitDef.methods.map { case (_, sig) => encodeType(sig, env) }

  // 辞書フィールド射影の型: ∀p̄. C[p̄] → fieldType
  private def projectionType(traitName: TypeVariable, traitDef: TraitDef, fieldType: TypeRec[Type]): TypeRec[Type] = {
    val dictType = applyTypeConstructor(traitName, traitDef.paramVars.map(typeVarT))
    traitDef.param.foldRight(arrowT(dictType, fieldType)) { case ((p, k), acc) => forallTypeT(p, k, acc) }
  }

  /** trait C[p̄] with S₁, …, Sₛ { def mᵢ: τᵢ }
   * data C[p̄] = | MkC(S₁)…(Sₛ)(τ₁)…(τₙ)
   * let $super_C_j : ∀p̄. C[p̄] → Sⱼ = Λp̄. λ$dict: C[p̄]. match $dict with | MkC(x̄) -> xⱼ
   * let mᵢ : ∀p̄. C[p̄] → τᵢ = Λp̄. λ$dict: C[p̄]. match $dict with | MkC(x̄) -> x₍ₛ₊ᵢ₎
   */
  private def encodeTrait(name: TypeVariable, params: Seq[(TypeVariable, Kind)], env: Env): Seq[TypeRec[Decl]] = {
    val traitDef = traitOf(name, env)
    val ctorName = dictionaryConstructor(name)
    val fields = dictionaryFields(traitDef, env)
    val dictType = applyTypeConstructor(name, params.map { case (p, _) => typeVarT(p) })
    val dictVar = Variable("$dict")
    val binders = fields.indices.map(i => Variable(s"$$field_$i"))
    def projection(letName: Variable, index: Int): TypeRec[Decl] = {
      val resultType = fields(index)
      val selected = matchExprT(resultType, varrType(dictVar, dictType), Seq(MatchCase(ctorName, binders, varrType(binders(index), resultType))))
      val value = params.foldRight(mkAbs(dictVar, dictType, selected)) { case ((p, k), acc) => mkTyAbs(p, k, acc) }
      topLetT(letName, typeOf(value), value)
    }
    val dictDecl = topDataT(name, params, Seq(DataConstructor[TypeRec](ctorName, fields)))
    val superProjections = traitDef.supers.indices.map(index => projection(superDictionaryName(name, index), index))
    val methodProjections = traitDef.methods.zipWithIndex.map { case ((methodName, _), index) =>
      projection(methodName, traitDef.supers.length + index)
    }
    (dictDecl +: superProjections) ++ methodProjections
  }

  /** impl[ā] C[T̄] with D₁, …, Dₖ { def mᵢ = eᵢ }
   * let $inst_C_h̄ : ∀ā. D₁ → … → Dₖ → C[T̄] =
   *   Λā. λ$ctx_1: D₁. … MkC[T̄](スーパークラスの辞書…)(e₁)…(eₙ)   (eᵢ は trait の宣言順)
   */
  private def encodeImpl(
      traitName: TypeVariable,
      targets: Seq[TypeRec[Type]],
      methods: Seq[MethodImpl[TypeRec]],
      superEvidence: Seq[Evidence[TypeRec]],
      env: Env
  ): TypeRec[Decl] = {
    val traitDef = traitOf(traitName, env)
    val heads = targets.map(t => typeConstructorHead(t).map(_._1).getOrElse(invariant(s"impl ${traitName.name}: target is not a type constructor")))
    val shown = s"${traitName.name}${heads.map(h => s"[$h]").mkString}"
    val inst = env.instances.getOrElse(instanceKey(traitName, heads), invariant(s"instance $shown is missing from ProgramAnn"))
    val context = inst.context.zipWithIndex.map { case (constraint, i) => (constraint, Variable(s"$$ctx_$i"), dictionaryType(constraint)) }
    val scope: Scope = context.map { case (constraint, dictVar, dictType) => constraint -> varrType(dictVar, dictType) }
    val superDicts = superEvidence.map(dictionary(_, scope, env))
    val bodies = traitDef.methods.map { case (methodName, _) =>
      val body = methods.collectFirst { case m if m.name == methodName => m.body }
        .getOrElse(invariant(s"impl $shown is missing method ${methodName.name}"))
      encodeIn(body, scope, env)
    }
    val dictType = applyTypeConstructor(traitName, traitDef.paramVars.map(typeVarT))
    val ctorType = traitDef.param.foldRight(dictionaryFields(traitDef, env).foldRight(dictType)(arrowT)) { case ((p, k), acc) => forallTypeT(p, k, acc) }
    val constructed = (superDicts ++ bodies).foldLeft(inst.targets.foldLeft(varrType(dictionaryConstructor(traitName), ctorType))(mkTyApp))(mkApp)
    val withContext = context.foldRight(constructed) { case ((_, dictVar, dictType), acc) => mkAbs(dictVar, dictType, acc) }
    val value = inst.params.foldRight(withContext) { case ((p, k), acc) => mkTyAbs(p, k, acc) }
    topLetT(inst.dictName, typeOf(value), value)
  }
}
