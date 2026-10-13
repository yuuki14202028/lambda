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
    DataDef(traitDef.param, Seq(ConstructorDef(dictionaryConstructor(name), name, dictionaryFields(traitDef, env), 0)))
  }

  private def encodeDecl(decl: TypeRec[Decl], env: Env): Seq[TypeRec[Decl]] = decl.project match {
    case AST.TopTrait(name, params, _, methods) => encodeTrait(name, params, methods, env)
    case AST.TopImpl(name, _, targets, _, methods, superEvidence) => Seq(encodeImpl(name, targets, methods, superEvidence, env))
    // rec impl のメソッド本体は組の辞書 $inst_C_h̄ を参照するので、辞書も rec の組にする
    case AST.TopRec(decls) => Seq(topRecT(decls.flatMap(encodeDecl(_, env))))
    case _ => Seq(encodeIn(decl, Seq.empty, env))
  }

  private def encodeIn[I](t: TypeRec[I], scope: Scope, env: Env): TypeRec[I] = t.cata(encodeAlg(env))(scope)

  private def encodeType(t: TypeRec[Type], env: Env): TypeRec[Type] = encodeIn(t, Seq.empty, env)

  private def encodeAlg(env: Env): Algebra[TypedAST, Encoded] = [x] => he => scope => (he.ann, he.ast) match {
    case (TypeAnn, AST.InstArrow(constraint, to)) =>
      arrowT(dictionaryType(constraint.hmap([y] => (child: Encoded[y]) => child(scope))), to(scope))

    case (ExprAnn(t), AST.InstAbs(_, body)) =>
      // 展開済みの制約は注釈の型が持っている
      val (assumed, _) = destructInstArrow(t).getOrElse(invariant(s"InstAbs must have an instance-implicit type, got ${t.show}"))
      val dictVar = Variable(s"$$with_${scope.length}")
      val dictType = encodeType(dictionaryType(assumed), env)
      // 内側で入った制約を先に調べる（型検査の Env.withGivens と同じ順）
      mkAbs(dictVar, dictType, body((assumed -> varrType(dictVar, dictType)) +: scope))

    // impl C[τ̄] { … }（式）↦ 辞書の値
    case (ExprAnn(_), AST.Instance(traitName, targets, methods, superEvidence)) =>
      val shown = s"${traitName.name}${targets.map(t => s"[${t(scope).show}]").mkString}"
      dictionaryValue(
        traitName, traitOf(traitName, env), targets.map(_(scope)), methods.map(m => m.name -> m.body(scope)),
        superEvidence.map(e => dictionary(e.hmap([y] => (child: Encoded[y]) => child(scope)), scope, env)), shown, env
      )

    // using d in e  ↦  let $with_n = d in e（e の中では $with_n がその制約の辞書）
    case (ExprAnn(_), AST.Using(dictionary, body)) =>
      val encodedDictionary = dictionary(scope)
      val assumed = constraintOfDictionary(typeOf(encodedDictionary), env.traits)
        .getOrElse(invariant(s"using must be given a dictionary, got ${typeOf(encodedDictionary).show}"))
      val dictVar = Variable(s"$$with_${scope.length}")
      val encodedBody = body((assumed -> varrType(dictVar, typeOf(encodedDictionary))) +: scope)
      letT(dictVar, typeOf(encodedBody), typeOf(encodedDictionary), encodedDictionary, encodedBody)

    case (ExprAnn(_), AST.Summon(_, evidence)) =>
      val resolved = evidence.getOrElse(invariant("summon reached TraitEncoder without evidence"))
      dictionary(resolved.hmap([y] => (child: Encoded[y]) => child(scope)), scope, env)

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

    // f(with d): 渡された辞書の値そのもの
    case Evidence.Explicit(dictionary) => dictionary

    // 量化された制約の辞書 d を具体化する: d[型引数…](前提の辞書…)
    case Evidence.Apply(of, typeArgs, premises) =>
      premises.map(dictionary(_, scope, env)).foldLeft(typeArgs.foldLeft(dictionary(of, scope, env))(mkTyApp))(mkApp)

    // 量化された制約 ∀ā. P̄ ⇒ C[τ̄] の辞書: Λā. λ$with_n: P の辞書. … C[τ̄] の辞書
    case Evidence.Abs(constraint, body) =>
      val premises = constraint.premises.zipWithIndex.map { case (premise, index) =>
        (premise, Variable(s"$$with_${scope.length + index}"), encodeType(dictionaryType(premise), env))
      }
      val inner = dictionary(body, premises.map { case (premise, dictVar, dictType) => premise -> varrType(dictVar, dictType) } ++ scope, env)
      val withPremises = premises.foldRight(inner) { case ((_, dictVar, dictType), acc) => mkAbs(dictVar, dictType, acc) }
      constraint.params.foldRight(withPremises) { case ((p, k), acc) => mkTyAbs(p, k, acc) }

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
  private def encodeTrait(name: TypeVariable, params: Seq[(TypeVariable, Kind)], methods: Seq[MethodSig[TypeRec]], env: Env): Seq[TypeRec[Decl]] = {
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
    val defaults = methods.flatMap(m => m.body.map(body => defaultMethod(name, traitDef, m.name, body, env)))
    (dictDecl +: superProjections) ++ methodProjections ++ defaults
  }

  // デフォルトが受け取るもの: スーパークラスの辞書と、必須メソッドの実装
  private def defaultParams(traitDef: TraitDef, env: Env): (Seq[(TypeConstraint, Variable, TypeRec[Type])], Seq[(Variable, TypeRec[Type])]) = (
    traitDef.supers.zipWithIndex.map { case (constraint, i) => (constraint, Variable(s"$$with_$i"), encodeType(dictionaryType(constraint), env)) },
    traitDef.required.map { case (method, sig) => method -> encodeType(sig, env) }
  )

  /** def m: τ = e  （デフォルト。e の中では必須メソッド rⱼ が普通の関数として見える）
   * let $default_C_m : ∀p̄. S₁ → … → ρ₁ → … → τ = Λp̄. λ$with_i: Sᵢ. … λrⱼ: ρⱼ. … e
   */
  private def defaultMethod(name: TypeVariable, traitDef: TraitDef, method: Variable, body: TypeRec[Expr], env: Env): TypeRec[Decl] = {
    val (supers, required) = defaultParams(traitDef, env)
    val scope: Scope = supers.map { case (constraint, dictVar, dictType) => constraint -> varrType(dictVar, dictType) }
    val withRequired = required.foldRight(encodeIn(body, scope, env)) { case ((required, sig), acc) => mkAbs(required, sig, acc) }
    val withSupers = supers.foldRight(withRequired) { case ((_, dictVar, dictType), acc) => mkAbs(dictVar, dictType, acc) }
    val value = traitDef.param.foldRight(withSupers) { case ((p, k), acc) => mkTyAbs(p, k, acc) }
    topLetT(defaultMethodName(name, method), typeOf(value), value)
  }

  private def defaultMethodType(traitDef: TraitDef, method: Variable, env: Env): TypeRec[Type] = {
    val (supers, required) = defaultParams(traitDef, env)
    val sig = traitDef.methods.collectFirst { case (name, sig) if name == method => encodeType(sig, env) }
      .getOrElse(invariant(s"method ${method.name} is missing from its trait"))
    val function = (supers.map(_._3) ++ required.map(_._2)).foldRight(sig)(arrowT)
    traitDef.param.foldRight(function) { case ((p, k), acc) => forallTypeT(p, k, acc) }
  }

  /** 辞書の値: MkC[T̄](スーパークラスの辞書…)(メソッド…)   (メソッドは trait の宣言順)
   * 省略されたメソッドはデフォルトで埋める。そのときは定義されたメソッドを一度だけ束縛して、デフォルトに渡す:
   *   let $method_j = eⱼ in MkC[T̄](スーパークラスの辞書…)($method_j | $default_C_m[T̄](スーパークラスの辞書…)(必須メソッド…))…
   */
  private def dictionaryValue(
      traitName: TypeVariable,
      traitDef: TraitDef,
      targets: Seq[TypeRec[Type]],
      methods: Seq[(Variable, TypeRec[Expr])],
      superDicts: Seq[TypeRec[Expr]],
      shown: String,
      env: Env
  ): TypeRec[Expr] = {
    val defined = traitDef.methods.flatMap { case (methodName, _) => methods.collectFirst { case found @ (name, _) if name == methodName => found } }
    val usesDefaults = defined.length < traitDef.methods.length
    // デフォルトを使うときだけ、定義されたメソッドを局所変数に束縛する
    val locals = if (usesDefaults) defined.zipWithIndex.map { case ((methodName, body), i) => (methodName, Variable(s"$$method_$i"), body) } else Seq.empty
    def definedMethod(methodName: Variable): Option[TypeRec[Expr]] =
      locals.collectFirst { case (name, local, body) if name == methodName => varrType(local, typeOf(body)) }
        .orElse(defined.collectFirst { case (name, body) if name == methodName => body })
    val bodies = traitDef.methods.map { case (methodName, _) =>
      definedMethod(methodName).getOrElse {
        if (!traitDef.defaults(methodName)) invariant(s"impl $shown is missing method ${methodName.name}")
        val default = varrType(defaultMethodName(traitName, methodName), defaultMethodType(traitDef, methodName, env))
        val required = traitDef.required.map { case (name, _) =>
          definedMethod(name).getOrElse(invariant(s"impl $shown is missing required method ${name.name}"))
        }
        (superDicts ++ required).foldLeft(targets.foldLeft(default)(mkTyApp))(mkApp)
      }
    }
    val dictType = applyTypeConstructor(traitName, traitDef.paramVars.map(typeVarT))
    val ctorType = traitDef.param.foldRight(dictionaryFields(traitDef, env).foldRight(dictType)(arrowT)) { case ((p, k), acc) => forallTypeT(p, k, acc) }
    val constructed = (superDicts ++ bodies).foldLeft(targets.foldLeft(varrType(dictionaryConstructor(traitName), ctorType))(mkTyApp))(mkApp)
    locals.foldRight(constructed) { case ((_, local, body), acc) => letT(local, typeOf(acc), typeOf(body), body, acc) }
  }

  /** impl[ā] C[T̄] with D₁, …, Dₖ { def mᵢ = eᵢ }
   * let $inst_C_h̄ : ∀ā. D₁ → … → Dₖ → C[T̄] =
   *   Λā. λ$ctx_1: D₁. … MkC[T̄](スーパークラスの辞書…)(e₁)…(eₙ)   (eᵢ は trait の宣言順)
   *
   * 省略されたメソッドはデフォルトで埋める。そのときは定義されたメソッドを一度だけ束縛して、デフォルトに渡す:
   *   let $method_j = eⱼ in … MkC[T̄](スーパークラスの辞書…)($method_j | $default_C_m[T̄](スーパークラスの辞書…)(必須メソッド…))…
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
    val context = inst.context.zipWithIndex.map { case (constraint, i) => (constraint, Variable(s"$$ctx_$i"), encodeType(dictionaryType(constraint), env)) }
    val scope: Scope = context.map { case (constraint, dictVar, dictType) => constraint -> varrType(dictVar, dictType) }
    val defined = methods.map(m => m.name -> encodeIn(m.body, scope, env))
    val constructed = dictionaryValue(traitName, traitDef, inst.targets, defined, superEvidence.map(dictionary(_, scope, env)), shown, env)
    val withContext = context.foldRight(constructed) { case ((_, dictVar, dictType), acc) => mkAbs(dictVar, dictType, acc) }
    val value = inst.params.foldRight(withContext) { case ((p, k), acc) => mkTyAbs(p, k, acc) }
    topLetT(inst.dictName, typeOf(value), value)
  }
}
