package com.yuuki14202028

import cats.syntax.all.*

/** derive 宣言（TopDerive）の検査と、型付き TopImpl への合成。
 *
 * 検査（check）は TAnalyser から宣言単位で呼ばれ、Env へ登録する InstanceDef を返す。
 * 合成（desugar）は TAnalyser の後・TraitEncoder の前のフェーズとして走り、TopDerive を
 * 手書き impl と同じ形の TopImpl へ置き換える。以降のフェーズは derive の存在を知らない。
 *
 * 導出対象は構造から一意に決まる trait に限る。
 *  - 最後の型パラメータについての Functor / Foldable / Traversable（インスタンスは D[p̄] に対するもの）
 *  - 値どうしの比較と表示 Eq / Ord / Show（インスタンスは D[ā] に対するもので、型パラメータごとに同じ trait を文脈に持つ）。
 *    Eq は構造が等しいか、Ord は構成子の宣言順、同じ構成子ならフィールドの辞書順、
 *    Show は C(x = …, …)（名前のないフィールドは値だけ）
 * trait は名前で選び、正準シグネチャとの α 同値で照合するため、同名の独自 trait に対して不正なコードを合成しない。
 */
object Deriver {

  private def invariant(msg: String): Nothing = sys.error(s"Compiler invariant violation: $msg")

  private enum Rule {
    case FunctorRule, FoldableRule, TraversableRule, EqRule, OrdRule, ShowRule

    // 値どうしの比較・表示（trait のパラメータのカインドは *、インスタンスは型パラメータごとの文脈を持つ）
    def firstOrder: Boolean = this match {
      case EqRule | OrdRule | ShowRule => true
      case _ => false
    }
  }

  /** Eq / Ord / Show の導出で、フィールド 1 つをどう扱うか。
   *  recursive は D[ā] そのもの（導出するメソッドの中の再帰関数で処理する）。
   *  evidence は trait[フィールドの型] の根拠（recursive なら None）、eqEvidence は Ord が等しさを調べるための Eq[フィールドの型] の根拠 */
  private final case class FieldCompare(recursive: Boolean, evidence: Option[Evidence[TypeRec]], eqEvidence: Option[Evidence[TypeRec]])

  // Eq / Ord / Show の導出が使う trait のメソッド（method は導出する trait の、eq は Ord のスーパークラス Eq の）
  private final case class Methods(method: Variable, eq: Option[Variable])

  /** フィールド型の分類。検査と合成でコード形状を共有する。
   *  Nested の container / inner は導出パラメータ置換前の形なので、合成時は
   *  置換済みフィールド型から collectTypeApps で取り直す。 */
  private enum FieldPlan {
    case Const                                    // 導出パラメータを含まない
    case Param                                    // パラメータそのもの
    case Recurse                                  // D[p̄, a]（正則な自己再帰）
    case Nested(innerPlan: FieldPlan)             // G[σ̄][τ'] で a は τ' のみ（G のインスタンスに委譲）
    case Func(toPlan: FieldPlan)                  // σ → ρ で a は ρ のみ（Functor のみ）
  }

  private final case class Plan(
      rule: Rule,
      target: TypeVariable,
      dataDef: DataDef,
      fieldPlans: Seq[Seq[FieldPlan]],
      inst: InstanceDef,
      superEvidence: Seq[Evidence[TypeRec]],
      compares: Seq[Seq[FieldCompare]] = Seq.empty,
      methods: Option[Methods] = None
  )

  private val functorMethod = Variable("map")
  private val foldRightMethod = Variable("foldRight")
  private val foldLeftMethod = Variable("foldLeft")
  private val traverseMethod = Variable("traverse")
  // Traversable の導出が使う Applicative（名前で引く。正準シグネチャと一致することを検査する）
  private val applicativeTrait = TypeVariable("Applicative")
  private val pureMethod = Variable("pure")
  private val applyMethod = Variable("apply")
  private val hkKind = Kind.Arrow(Kind.Star, Kind.Star)

  // ---- 正準シグネチャ ----

  private def canonicalSigs(rule: Rule, f: TypeVariable): Seq[(Variable, TypeRec[Type])] = {
    val (a, b) = (TypeVariable("a"), TypeVariable("b"))
    val (aT, bT) = (typeVarT(a), typeVarT(b))
    def fT(x: TypeVariable) = typeAppT(typeVarT(f), typeVarT(x))
    def poly(body: TypeRec[Type]) = forallTypeT(a, Kind.Star, forallTypeT(b, Kind.Star, body))
    rule match {
      // 受け手（f[a]）が最初の引数
      case Rule.FunctorRule => Seq(
        functorMethod -> poly(arrowT(fT(a), arrowT(arrowT(aT, bT), fT(b))))
      )
      case Rule.FoldableRule => Seq(
        foldRightMethod -> poly(arrowT(fT(a), arrowT(bT, arrowT(arrowT(aT, arrowT(bT, bT)), bT)))),
        foldLeftMethod -> poly(arrowT(fT(a), arrowT(bT, arrowT(arrowT(bT, arrowT(aT, bT)), bT))))
      )
      // traverse: ∀a. ∀(g: * → *). ∀b. [Applicative[g]] → f[a] → (a → g[b]) → g[f[b]]
      case Rule.TraversableRule =>
        val g = freshTypeVariable(TypeVariable("g"), Set(f, a, b))
        def gOf(t: TypeRec[Type]) = typeAppT(typeVarT(g), t)
        val body = instArrowT(TypeConstraint(applicativeTrait, Seq(typeVarT(g))), arrowT(fT(a), arrowT(arrowT(aT, gOf(bT)), gOf(fT(b)))))
        Seq(traverseMethod -> forallTypeT(a, Kind.Star, forallTypeT(g, hkKind, forallTypeT(b, Kind.Star, body))))
    }
  }

  // Applicative[F] の pure / apply の正準シグネチャ（F は trait のパラメータ）
  private def applicativeSigs(f: TypeVariable): Seq[(Variable, TypeRec[Type])] = {
    val (a, b) = (TypeVariable("a"), TypeVariable("b"))
    val (aT, bT) = (typeVarT(a), typeVarT(b))
    def fOf(t: TypeRec[Type]) = typeAppT(typeVarT(f), t)
    Seq(
      pureMethod -> forallTypeT(a, Kind.Star, arrowT(aT, fOf(aT))),
      applyMethod -> forallTypeT(a, Kind.Star, forallTypeT(b, Kind.Star, arrowT(fOf(arrowT(aT, bT)), arrowT(fOf(aT), fOf(bT)))))
    )
  }

  private def ruleFor(traitName: TypeVariable): Option[Rule] = traitName.name match {
    case "Functor" => Some(Rule.FunctorRule)
    case "Foldable" => Some(Rule.FoldableRule)
    case "Traversable" => Some(Rule.TraversableRule)
    case "Eq" => Some(Rule.EqRule)
    case "Ord" => Some(Rule.OrdRule)
    case "Show" => Some(Rule.ShowRule)
    case _ => None
  }

  // Eq / Ord / Show の必須メソッドの短い名前と正準シグネチャ（a は trait のパラメータ）
  private def firstOrderSig(rule: Rule, a: TypeVariable): (String, TypeRec[Type]) = {
    val aT = typeVarT(a)
    rule match {
      case Rule.EqRule => "eq" -> arrowT(aT, arrowT(aT, boolTypeT))
      case Rule.OrdRule => "lt" -> arrowT(aT, arrowT(aT, boolTypeT))
      case _ => "show" -> arrowT(aT, stringTypeT)
    }
  }

  private def shortName(v: Variable): String = v.name.substring(v.name.lastIndexOf('.') + 1)

  // trait の必須メソッドが、規則の正準シグネチャのものただ 1 つであることを確かめ、その（完全な）名前を返す
  private def firstOrderMethod(rule: Rule, traitName: TypeVariable, traitDef: TraitDef): EitherS[Variable] = for {
    _ <- Either.cond(traitDef.param.map(_._2) == Seq(Kind.Star), (), CompileError.DeriveUnsupportedTrait(traitName))
    (expectedName, expected) = firstOrderSig(rule, traitDef.param.head._1)
    method <- traitDef.required match {
      case Seq((name, _)) if shortName(name) == expectedName => Right(name)
      case _ => Left(CompileError.DeriveUnsupportedTrait(traitName))
    }
    actual = traitDef.required.head._2
    _ <- Either.cond(Equivalence.alpha(expected, actual), (), CompileError.DeriveTraitSigMismatch(traitName, method, expected, actual))
  } yield method

  // ---- 検査（TAnalyser から宣言単位で呼ばれる） ----

  /** derive が登録するインスタンス。フィールドの検査をせずに決まるので、rec の組では
   * 全員のインスタンスを先に求めて互いに見えるようにしてから、各 derive を check する */
  def instanceOf(traitName: TypeVariable, target: TypeVariable, env: Env): EitherS[InstanceDef] =
    if (ruleFor(traitName).exists(_.firstOrder)) firstOrderInstance(traitName, target, env)
    else for {
    dataDef <- env.dataTypes.get(target).filter(_ => !env.traits.contains(target))
      .toRight(CompileError.DeriveTargetNotData(target))
    _ <- Either.cond(dataDef.params.nonEmpty, (), CompileError.DeriveKindMismatch(traitName, target))
  } yield {
    val prefix = dataDef.params.init
    val targets = Seq(applyTypeConstructor(target, prefix.map { case (v, _) => typeVarT(v) }))
    InstanceDef(traitName, targets, Seq.empty, instanceDictionaryName(traitName, Seq(target.name)), prefix)
  }

  // Eq / Ord / Show: impl[ā] C[D[ā]] with C[a₁], …, C[aₙ]
  private def firstOrderInstance(traitName: TypeVariable, target: TypeVariable, env: Env): EitherS[InstanceDef] = for {
    dataDef <- env.dataTypes.get(target).filter(_ => !env.traits.contains(target))
      .toRight(CompileError.DeriveTargetNotData(target))
    _ <- dataDef.params.traverse_ { case (param, kind) =>
      Either.cond(kind == Kind.Star, (), CompileError.DeriveParamKind(traitName, target, param, kind))
    }
  } yield InstanceDef(
    traitName,
    Seq(applyTypeConstructor(target, dataDef.paramVars.map(typeVarT))),
    dataDef.paramVars.map(p => TypeConstraint(traitName, Seq(typeVarT(p)))),
    instanceDictionaryName(traitName, Seq(target.name)),
    dataDef.params
  )

  /** supers は、スーパークラスの解決に使えるインスタンスの Env。導出したインスタンスは文脈を持たず辞書がすぐ作られるので、
   * rec の組の中では、スーパークラスのインスタンスは先に宣言されたものに限る（フィールドの検査には組の全員を使う）。 */
  def check(traitName: TypeVariable, target: TypeVariable, env: Env, supers: Option[Env] = None): EitherS[InstanceDef] = for {
    _ <- Either.cond(
      !env.instances.contains(instanceKey(traitName, Seq(target.name))), (),
      CompileError.OverlappingInstance(traitName, Seq(target.name))
    )
    p <- plan(traitName, target, env, supers.getOrElse(env))
  } yield p.inst

  private def plan(traitName: TypeVariable, target: TypeVariable, env: Env): EitherS[Plan] = plan(traitName, target, env, env)

  private def plan(traitName: TypeVariable, target: TypeVariable, env: Env, supers: Env): EitherS[Plan] =
    ruleFor(traitName).filter(_.firstOrder) match {
      case Some(rule) => firstOrderPlan(rule, traitName, target, env, supers)
      case None => higherKindedPlan(traitName, target, env, supers)
    }

  private def firstOrderPlan(rule: Rule, traitName: TypeVariable, target: TypeVariable, env: Env, supers: Env): EitherS[Plan] = for {
    traitDef <- env.traits.get(traitName).toRight(CompileError.UndefinedTrait(traitName, None))
    method <- firstOrderMethod(rule, traitName, traitDef)
    // Ord はフィールドが等しいかも調べるので、スーパークラスの Eq のメソッドを使う
    eq <- if (rule != Rule.OrdRule) Right(None) else
      traitDef.supers.collectFirst { case c if ruleFor(c.name).contains(Rule.EqRule) && env.traits.contains(c.name) => c.name } match {
        case Some(eqTrait) => firstOrderMethod(Rule.EqRule, eqTrait, env.traits(eqTrait)).map(m => Some(eqTrait -> m))
        case None => Left(CompileError.DeriveUnsupportedTrait(traitName))
      }
    dataDef <- env.dataTypes.get(target).filter(_ => !env.traits.contains(target))
      .toRight(CompileError.DeriveTargetNotData(target))
    inst <- firstOrderInstance(traitName, target, env)
    superEvidence <- Resolver.superConstraints(traitDef, inst.targets).traverse(Resolver.resolve)
      .run(supers.withTypeVars(inst.params).withGivens(inst.context))
      .left.map(CompileError.RequiredBy(_, showTypeConstraint(TypeConstraint(traitName, inst.targets)),
        s"trait ${traitName.name}${traitDef.supers.map(showTypeConstraint).mkString(" with ", ", ", "")}"))
    // フィールドのインスタンスは、型パラメータの文脈の下で解決する
    fieldEnv = env.withTypeVars(inst.params).withGivens(inst.context)
    self = inst.targets.head
    compares <- dataDef.constructors.toList.traverse { c =>
      c.fields.toList.traverse { field =>
        def resolve(name: TypeVariable): EitherS[Evidence[TypeRec]] =
          Resolver.resolve(TypeConstraint(name, Seq(field))).run(fieldEnv)
            .left.map(_ => CompileError.DeriveFieldNoInstance(traitName, target, c.name, name, field))
        val recursive = Equivalence.alpha(field, self)
        for {
          evidence <- if (recursive) Right(None) else resolve(traitName).map(Some(_))
          eqEvidence <- eq.traverse { case (eqTrait, _) => resolve(eqTrait) }
        } yield FieldCompare(recursive, evidence, eqEvidence)
      }
    }
  } yield Plan(rule, target, dataDef, Seq.empty, inst, superEvidence, compares, Some(Methods(method, eq.map(_._2))))

  private def higherKindedPlan(traitName: TypeVariable, target: TypeVariable, env: Env, supers: Env): EitherS[Plan] = for {
    traitDef <- env.traits.get(traitName).toRight(CompileError.UndefinedTrait(traitName, None))
    dataDef <- env.dataTypes.get(target).filter(_ => !env.traits.contains(target))
      .toRight(CompileError.DeriveTargetNotData(target))
    rule <- ruleFor(traitName).toRight(CompileError.DeriveUnsupportedTrait(traitName))
    _ <- Either.cond(traitDef.param.map(_._2) == Seq(hkKind), (), CompileError.DeriveUnsupportedTrait(traitName))
    expectedSigs = canonicalSigs(rule, traitDef.param.head._1)
    _ <- Either.cond(
      traitDef.methods.map(_._1).toSet == expectedSigs.map(_._1).toSet
        && traitDef.methods.length == expectedSigs.length,
      (), CompileError.DeriveUnsupportedTrait(traitName)
    )
    _ <- expectedSigs.traverse_ { case (name, expected) =>
      val actual = traitDef.methods.collectFirst { case (n, sig) if n == name => sig }.get
      Either.cond(Equivalence.alpha(expected, actual), (), CompileError.DeriveTraitSigMismatch(traitName, name, expected, actual))
    }
    _ <- Either.cond(
      dataDef.params.nonEmpty && dataDef.params.last._2 == Kind.Star, (),
      CompileError.DeriveKindMismatch(traitName, target)
    )
    // Traversable の導出は Applicative の pure / apply を使う
    _ <- if (rule != Rule.TraversableRule) Right(()) else for {
      applicative <- env.traits.get(applicativeTrait).toRight(CompileError.UndefinedTrait(applicativeTrait, None))
      _ <- Either.cond(applicative.param.map(_._2) == Seq(hkKind), (), CompileError.DeriveUnsupportedTrait(traitName))
      _ <- applicativeSigs(applicative.param.head._1).traverse_ { case (name, expected) =>
        applicative.methods.collectFirst { case (n, sig) if n == name => sig } match {
          case Some(actual) if Equivalence.alpha(expected, actual) => Right(())
          case Some(actual) => Left(CompileError.DeriveTraitSigMismatch(applicativeTrait, name, expected, actual))
          case None => Left(CompileError.DeriveUnsupportedTrait(traitName))
        }
      }
    } yield ()
    fieldPlans <- dataDef.constructors.toList.traverse { c =>
      c.fields.toList.traverse(field => classify(rule, traitName, target, dataDef, c.name, field, env))
    }
    inst <- instanceOf(traitName, target, env)
    superEvidence <- Resolver.superConstraints(traitDef, inst.targets).traverse(Resolver.resolve).run(supers.withTypeVars(inst.params))
      .left.map(CompileError.RequiredBy(_, showTypeConstraint(TypeConstraint(traitName, inst.targets)),
        s"trait ${traitName.name}${traitDef.supers.map(showTypeConstraint).mkString(" with ", ", ", "")}"))
  } yield Plan(rule, target, dataDef, fieldPlans, inst, superEvidence)

  // フィールドの入れ子コンテナ G[σ̄] に対するインスタンス traitName[G[σ̄]] の根拠
  private def containerEvidence(traitName: TypeVariable, container: TypeRec[Type], env: Env): EitherS[Evidence[TypeRec]] =
    Resolver.resolve(TypeConstraint(traitName, Seq(container))).run(env)

  private def classify(
      rule: Rule,
      traitName: TypeVariable,
      target: TypeVariable,
      dataDef: DataDef,
      ctor: Variable,
      field: TypeRec[Type],
      env: Env
  ): EitherS[FieldPlan] = {
    val param = dataDef.params.last._1
    def unsupported = Left(CompileError.DeriveUnsupportedField(traitName, target, ctor, field))
    def go(t: TypeRec[Type]): EitherS[FieldPlan] =
      if (!freeTypeVars(t).contains(param)) Right(FieldPlan.Const)
      else t.project match {
        case AST.TypeVar(v) if v == param => Right(FieldPlan.Param)
        case AST.Arrow(from, to) =>
          if (freeTypeVars(from).contains(param))
            Left(CompileError.DeriveNegativeOccurrence(traitName, target, ctor, field))
          else rule match {
            case Rule.FunctorRule => go(to).map(FieldPlan.Func.apply)
            case Rule.FoldableRule | Rule.TraversableRule => unsupported
          }
        case AST.TypeApp(_, _) =>
          val (head, args) = collectTypeApps(t)
          head.project match {
            case AST.TypeVar(h) if h == target =>
              // 正則な自己適用 D[p̄, a] のみ再帰として扱う
              val expected = dataDef.paramVars.map(typeVarT)
              Either.cond(
                args.length == expected.length && args.zip(expected).forall(Equivalence.alpha),
                FieldPlan.Recurse,
                CompileError.DeriveUnsupportedField(traitName, target, ctor, field)
              )
            case AST.TypeVar(h) if env.dataTypes.contains(h) && !env.traits.contains(h) =>
              if (args.init.exists(a => freeTypeVars(a).contains(param))) unsupported
              else if (!env.instances.contains(instanceKey(traitName, Seq(h.name))))
                Left(CompileError.DeriveFieldInstanceMissing(traitName, target, ctor, h.name))
              else containerEvidence(traitName, args.init.foldLeft(head)(typeAppT), env) *> go(args.last).map(FieldPlan.Nested.apply)
            case _ => unsupported
          }
        case _ => unsupported
      }
    go(field)
  }

  // ---- 合成フェーズ（TAnalyser の後・TraitEncoder の前） ----

  def desugar(program: TypeRec[AST.Program.type]): TypeRec[AST.Program.type] = program.project match {
    case AST.Program(decls) =>
      val env = program.extract match {
        case ProgramAnn(e) => e
      }
      def expand(decl: TypeRec[Decl]): TypeRec[Decl] = decl.project match {
        case AST.TopDerive(traitName, target) => synthesize(traitName, target, env)
        case AST.TopRec(members) => topRecT(members.map(expand))
        case _ => decl
      }
      programT(decls.map(expand), env)
  }

  private def synthesize(traitName: TypeVariable, target: TypeVariable, env: Env): TypeRec[Decl] = {
    val p = plan(traitName, target, env)
      .fold(err => invariant(s"derive ${traitName.name}[${target.name}] failed after validation: ${err.render}"), identity)
    val methods = p.rule match {
      case Rule.EqRule | Rule.OrdRule | Rule.ShowRule =>
        val names = p.methods.getOrElse(invariant(s"derive ${traitName.name}: the plan has no methods"))
        Seq(MethodImpl[TypeRec](names.method, None, p.rule match {
          case Rule.EqRule => equalityBody(p, env, names)
          case Rule.OrdRule => orderBody(p, env, names)
          case _ => showBody(p, env, names)
        }))
      case Rule.FunctorRule => Seq(MethodImpl[TypeRec](functorMethod, None, mapBody(p, env)))
      case Rule.TraversableRule => Seq(MethodImpl[TypeRec](traverseMethod, None, traverseBody(p, env)))
      case Rule.FoldableRule => Seq(
        MethodImpl[TypeRec](foldRightMethod, None, foldBody(p, env, rightward = true)),
        MethodImpl[TypeRec](foldLeftMethod, None, foldBody(p, env, rightward = false))
      )
    }
    topImplT(traitName, p.inst.params, p.inst.targets, p.inst.context, methods, p.superEvidence)
  }

  // ---- 型付き AST 構築ヘルパー ----

  private def valueRef(env: Env, name: Variable): TypeRec[Expr] =
    varrType(name, env.values.getOrElse(name, invariant(s"derive: ${name.name} is missing from Env")))

  // 入れ子コンテナのインスタンスに委譲するメソッド参照 method[container]（制約は検査時と同じ根拠で解決する）
  private def containerMethod(p: Plan, env: Env, method: Variable, container: TypeRec[Type]): TypeRec[Expr] = {
    val applied = mkTyApp(valueRef(env, method), container)
    val (_, to) = destructInstArrow(typeOf(applied))
      .getOrElse(invariant(s"derive: ${method.name} is not a trait method: ${typeOf(applied).show}"))
    val evidence = containerEvidence(p.inst.name, container, env)
      .fold(err => invariant(s"derive: instance resolution failed after validation: ${err.render}"), identity)
    instAppT(to, applied, evidence)
  }

  /** 導出メソッドが共有する骨格。α / β はデータ型パラメータと衝突しない fresh な型変数 */
  private final case class Scaffold(
      prefixVars: Seq[TypeVariable],
      lastVar: TypeVariable,
      alpha: TypeVariable,
      beta: TypeVariable
  ) {
    val aT: TypeRec[Type] = typeVarT(alpha)
    val bT: TypeRec[Type] = typeVarT(beta)
    def dOf(t: TypeRec[Type], target: TypeVariable): TypeRec[Type] =
      applyTypeConstructor(target, prefixVars.map(typeVarT) :+ t)
    def substLast(t: TypeRec[Type]): TypeRec[Type] = Equivalence.normalize(substType(lastVar, aT, t))
  }

  private def scaffold(p: Plan): Scaffold = {
    val used = p.dataDef.params.map(_._1).toSet
    val alpha = freshTypeVariable(TypeVariable("a"), used)
    val beta = freshTypeVariable(TypeVariable("b"), used + alpha)
    Scaffold(p.dataDef.params.init.map(_._1), p.dataDef.params.last._1, alpha, beta)
  }

  /** map = Λα. Λβ. λv. λf. letRec go = λd. match d { Kᵢ(x̄) → Kᵢ[p̄][β](φ(x̄)) } in go(v) */
  private def mapBody(p: Plan, env: Env): TypeRec[Expr] = {
    val s = scaffold(p)
    import s.{aT, bT}
    val dAlpha = s.dOf(aT, p.target)
    val dBeta = s.dOf(bT, p.target)
    val fVar = Variable("$f")
    val goVar = Variable("$go")
    val dVar = Variable("$d")
    val fType = arrowT(aT, bT)
    val goType = arrowT(dAlpha, dBeta)
    val fRef = varrType(fVar, fType)
    val goRef = varrType(goVar, goType)

    // t は α 代入済みのフィールド型。結果は t[α:=β] の値
    def transform(plan: FieldPlan, t: TypeRec[Type], x: TypeRec[Expr], depth: Int): TypeRec[Expr] = plan match {
      case FieldPlan.Const => x
      case FieldPlan.Param => mkApp(fRef, x)
      case FieldPlan.Recurse => mkApp(goRef, x)
      case FieldPlan.Func(toPlan) =>
        val (from, to) = destructArrow(t).getOrElse(invariant(s"derive map: expected an arrow field, got ${t.show}"))
        val y = Variable(s"$$y_$depth")
        mkAbs(y, from, transform(toPlan, to, mkApp(x, varrType(y, from)), depth + 1))
      case FieldPlan.Nested(innerPlan) =>
        val (head, args) = collectTypeApps(t)
        val container = args.init.foldLeft(head)(typeAppT)
        val innerA = args.last
        val innerB = Equivalence.normalize(substType(s.alpha, bT, innerA))
        val y = Variable(s"$$y_$depth")
        val innerF = mkAbs(y, innerA, transform(innerPlan, innerA, varrType(y, innerA), depth + 1))
        val mapRef = mkTyApp(mkTyApp(containerMethod(p, env, functorMethod, container), innerA), innerB)
        mkApp(mkApp(mapRef, x), innerF)
    }

    val cases = p.dataDef.constructors.zip(p.fieldPlans).map { case (ctor, plans) =>
      val binders = ctor.fields.indices.map(i => Variable(s"$$x_$i"))
      val fieldTypes = ctor.fields.map(s.substLast)
      val tyApplied = (s.prefixVars.map(typeVarT) :+ bT).foldLeft(valueRef(env, ctor.name))(mkTyApp)
      val applied = binders.lazyZip(fieldTypes).lazyZip(plans).foldLeft(tyApplied) {
        case (acc, (x, t, plan)) => mkApp(acc, transform(plan, t, varrType(x, t), 0))
      }
      MatchCase[TypeRec](ctor.name, binders, applied)
    }
    val vVar = Variable("$v")
    val goValue = mkAbs(dVar, dAlpha, matchExprT(dBeta, varrType(dVar, dAlpha), cases))
    val letR = letRecT(goVar, goType, dBeta, goValue, mkApp(goRef, varrType(vVar, dAlpha)))
    mkTyAbs(s.alpha, Kind.Star, mkTyAbs(s.beta, Kind.Star, mkAbs(vVar, dAlpha, mkAbs(fVar, fType, letR))))
  }

  /** traverse = Λα. Λγ. Λβ. λ[Applicative[γ]]. λv. λf. letRec go = λd. match d {
   *    Kᵢ(x̄) → pure(λȳ. Kᵢ[p̄][β](ȳ)) <*> τ(x₁) <*> … <*> τ(xₙ)
   *  } in go(v)
   *  τ(x) は x を γ の中へ持ち上げる: パラメータなら f(x)、再帰なら go(x)、それ以外は pure(x) */
  private def traverseBody(p: Plan, env: Env): TypeRec[Expr] = {
    val s = scaffold(p)
    import s.{aT, bT}
    val gamma = freshTypeVariable(TypeVariable("g"), p.dataDef.params.map(_._1).toSet + s.alpha + s.beta)
    val gT = typeVarT(gamma)
    def gOf(t: TypeRec[Type]): TypeRec[Type] = typeAppT(gT, t)
    def toBeta(t: TypeRec[Type]): TypeRec[Type] = Equivalence.normalize(substType(s.alpha, bT, t))
    val applicative = TypeConstraint(applicativeTrait, Seq(gT))
    val dAlpha = s.dOf(aT, p.target)
    val dBeta = s.dOf(bT, p.target)
    val fVar = Variable("$f")
    val goVar = Variable("$go")
    val dVar = Variable("$d")
    val fType = arrowT(aT, gOf(bT))
    val goType = arrowT(dAlpha, gOf(dBeta))
    val fRef = varrType(fVar, fType)
    val goRef = varrType(goVar, goType)

    // 制約付きの式 e: [Applicative[γ]] → τ に、スコープ中の Applicative[γ] を渡す
    def withApplicative(e: TypeRec[Expr]): TypeRec[Expr] = {
      val (_, to) = destructInstArrow(typeOf(e)).getOrElse(invariant(s"derive traverse: expected a constrained type, got ${typeOf(e).show}"))
      instAppT(to, e, Evidence.Given(applicative))
    }
    def pureOf(t: TypeRec[Type], x: TypeRec[Expr]): TypeRec[Expr] =
      mkApp(mkTyApp(withApplicative(mkTyApp(valueRef(env, pureMethod), gT)), t), x)
    def applyOf(from: TypeRec[Type], to: TypeRec[Type], function: TypeRec[Expr], argument: TypeRec[Expr]): TypeRec[Expr] =
      mkApp(mkApp(mkTyApp(mkTyApp(withApplicative(mkTyApp(valueRef(env, applyMethod), gT)), from), to), function), argument)

    // t は α 代入済みのフィールド型。結果は γ[t[α:=β]] の値
    def lift(plan: FieldPlan, t: TypeRec[Type], x: TypeRec[Expr], depth: Int): TypeRec[Expr] = plan match {
      case FieldPlan.Const => pureOf(t, x)
      case FieldPlan.Param => mkApp(fRef, x)
      case FieldPlan.Recurse => mkApp(goRef, x)
      case FieldPlan.Nested(innerPlan) =>
        val (head, args) = collectTypeApps(t)
        val container = args.init.foldLeft(head)(typeAppT)
        val innerA = args.last
        val y = Variable(s"$$y_$depth")
        val innerF = mkAbs(y, innerA, lift(innerPlan, innerA, varrType(y, innerA), depth + 1))
        val traverseRef = mkTyApp(mkTyApp(mkTyApp(containerMethod(p, env, traverseMethod, container), innerA), gT), toBeta(innerA))
        mkApp(mkApp(withApplicative(traverseRef), x), innerF)
      case FieldPlan.Func(_) => invariant("derive Traversable: function fields must be rejected during validation")
    }

    val cases = p.dataDef.constructors.zip(p.fieldPlans).map { case (ctor, plans) =>
      val binders = ctor.fields.indices.map(i => Variable(s"$$x_$i"))
      val rebuilt = ctor.fields.indices.map(i => Variable(s"$$r_$i"))
      val fieldTypes = ctor.fields.map(s.substLast)
      val resultTypes = fieldTypes.map(toBeta)
      val tyApplied = (s.prefixVars.map(typeVarT) :+ bT).foldLeft(valueRef(env, ctor.name))(mkTyApp)
      // λr̄. K[p̄][β](r̄): 持ち上げた各フィールドの結果から、構成子を組み立て直す関数
      val rebuild = rebuilt.zip(resultTypes).foldRight(rebuilt.zip(resultTypes).foldLeft(tyApplied) { case (acc, (r, t)) => mkApp(acc, varrType(r, t)) }) {
        case ((r, t), acc) => mkAbs(r, t, acc)
      }
      val lifted = binders.lazyZip(fieldTypes).lazyZip(plans).map { case (x, t, plan) => lift(plan, t, varrType(x, t), 0) }
      val (body, _) = lifted.zip(resultTypes).foldLeft((pureOf(typeOf(rebuild), rebuild), typeOf(rebuild))) { case ((acc, functionType), (field, fieldType)) =>
        val (_, rest) = destructArrow(functionType).getOrElse(invariant(s"derive traverse: expected a function type, got ${functionType.show}"))
        (applyOf(fieldType, rest, acc, field), rest)
      }
      MatchCase[TypeRec](ctor.name, binders, body)
    }
    val vVar = Variable("$v")
    val goValue = mkAbs(dVar, dAlpha, matchExprT(gOf(dBeta), varrType(dVar, dAlpha), cases))
    val letR = letRecT(goVar, goType, gOf(dBeta), goValue, mkApp(goRef, varrType(vVar, dAlpha)))
    val withArgs = mkAbs(vVar, dAlpha, mkAbs(fVar, fType, letR))
    val constrained = instAbsT(instArrowT(applicative, typeOf(withArgs)), applicative, withArgs)
    mkTyAbs(s.alpha, Kind.Star, mkTyAbs(gamma, hkKind, mkTyAbs(s.beta, Kind.Star, constrained)))
  }

  /** foldRight / foldLeft =
   *  Λα. Λβ. λv. λz. λf. letRec go = λd. λacc. match d { Kᵢ(x̄) → ψ(x̄, acc) } in go(v)(z) */
  private def foldBody(p: Plan, env: Env, rightward: Boolean): TypeRec[Expr] = {
    val s = scaffold(p)
    import s.{aT, bT}
    val dAlpha = s.dOf(aT, p.target)
    val fVar = Variable("$f")
    val zVar = Variable("$z")
    val goVar = Variable("$go")
    val dVar = Variable("$d")
    val accVar = Variable("$acc")
    val vVar = Variable("$v")
    val fType = if (rightward) arrowT(aT, arrowT(bT, bT)) else arrowT(bT, arrowT(aT, bT))
    val goType = arrowT(dAlpha, arrowT(bT, bT))
    val fRef = varrType(fVar, fType)
    val goRef = varrType(goVar, goType)
    val methodName = if (rightward) foldRightMethod else foldLeftMethod

    // acc: β に、フィールド x: t の畳み込みを合成する
    def combine(plan: FieldPlan, t: TypeRec[Type], x: TypeRec[Expr], acc: TypeRec[Expr], depth: Int): TypeRec[Expr] = plan match {
      case FieldPlan.Const => acc
      case FieldPlan.Param =>
        if (rightward) mkApp(mkApp(fRef, x), acc)
        else mkApp(mkApp(fRef, acc), x)
      case FieldPlan.Recurse => mkApp(mkApp(goRef, x), acc)
      case FieldPlan.Nested(innerPlan) =>
        val (head, args) = collectTypeApps(t)
        val container = args.init.foldLeft(head)(typeAppT)
        val innerA = args.last
        val y = Variable(s"$$y_$depth")
        val innerAcc = Variable(s"$$acc_$depth")
        val innerBody = combine(innerPlan, innerA, varrType(y, innerA), varrType(innerAcc, bT), depth + 1)
        val innerF =
          if (rightward) mkAbs(y, innerA, mkAbs(innerAcc, bT, innerBody))
          else mkAbs(innerAcc, bT, mkAbs(y, innerA, innerBody))
        val foldRef = mkTyApp(mkTyApp(containerMethod(p, env, methodName, container), innerA), bT)
        mkApp(mkApp(mkApp(foldRef, x), acc), innerF)
      case FieldPlan.Func(_) => invariant("derive Foldable: function fields must be rejected during validation")
    }

    val cases = p.dataDef.constructors.zip(p.fieldPlans).map { case (ctor, plans) =>
      val binders = ctor.fields.indices.map(i => Variable(s"$$x_$i"))
      val fieldTypes = ctor.fields.map(s.substLast)
      val items = binders.lazyZip(fieldTypes).lazyZip(plans).toSeq
      val ordered = if (rightward) items.reverse else items
      val body = ordered.foldLeft(varrType(accVar, bT)) {
        case (acc, (x, t, plan)) => combine(plan, t, varrType(x, t), acc, 0)
      }
      MatchCase[TypeRec](ctor.name, binders, body)
    }
    val goValue = mkAbs(dVar, dAlpha, mkAbs(accVar, bT, matchExprT(bT, varrType(dVar, dAlpha), cases)))
    val letR = letRecT(goVar, bT, goType, goValue, mkApp(mkApp(goRef, varrType(vVar, dAlpha)), varrType(zVar, bT)))
    mkTyAbs(s.alpha, Kind.Star, mkTyAbs(s.beta, Kind.Star, mkAbs(vVar, dAlpha, mkAbs(zVar, bT, mkAbs(fVar, fType, letR)))))
  }

  // ---- Eq / Ord / Show ----

  // trait のメソッド method を型 t で具体化し、根拠 evidence で制約を満たしたもの
  private def methodAt(env: Env, method: Variable, t: TypeRec[Type], evidence: Evidence[TypeRec]): TypeRec[Expr] = {
    val applied = mkTyApp(valueRef(env, method), t)
    val (_, to) = destructInstArrow(typeOf(applied))
      .getOrElse(invariant(s"derive: ${method.name} is not a trait method: ${typeOf(applied).show}"))
    instAppT(to, applied, evidence)
  }

  private def evidenceOf(field: Option[Evidence[TypeRec]]): Evidence[TypeRec] =
    field.getOrElse(invariant("derive: a non-recursive field has no evidence"))

  /** 2 つの値を比べる関数 λx. λy. letRec go = λl. λr. match l { Kᵢ(x̄) → match r { Kⱼ(ȳ) → body(i, j, x̄, ȳ) } } in go(x)(y)。
   *  body はフィールドごとの比較（FieldCompare）と、変数として参照したフィールドの値を受け取る */
  private def comparison(p: Plan)(body: (Variable, Int, Int, Seq[(FieldCompare, TypeRec[Type], TypeRec[Expr], TypeRec[Expr])]) => TypeRec[Expr]): TypeRec[Expr] = {
    val self = p.inst.targets.head
    val predicate = arrowT(self, arrowT(self, boolTypeT))
    val go = Variable("$go")
    val (l, r, x, y) = (Variable("$l"), Variable("$r"), Variable("$x"), Variable("$y"))
    val constructors = p.dataDef.constructors.zip(p.compares).zipWithIndex
    val outer = constructors.map { case ((left, compares), i) =>
      val xs = left.fields.indices.map(k => Variable(s"$$x_$k"))
      val inner = constructors.map { case ((right, _), j) =>
        val ys = right.fields.indices.map(k => Variable(s"$$y_$k"))
        // 同じ構成子どうし（i == j）のときだけ、フィールドを組にして渡す
        val fields = if (i != j) Seq.empty else compares.indices.map { k =>
          (compares(k), left.fields(k), varrType(xs(k), left.fields(k)), varrType(ys(k), right.fields(k)))
        }
        MatchCase[TypeRec](right.name, ys, body(go, i, j, fields))
      }
      MatchCase[TypeRec](left.name, xs, matchExprT(boolTypeT, varrType(r, self), inner))
    }
    val goValue = mkAbs(l, self, mkAbs(r, self, matchExprT(boolTypeT, varrType(l, self), outer)))
    val start = mkApp(mkApp(varrType(go, predicate), varrType(x, self)), varrType(y, self))
    mkAbs(x, self, mkAbs(y, self, letRecT(go, boolTypeT, predicate, goValue, start)))
  }

  private val falseT: TypeRec[Expr] = boolT(false, boolTypeT)
  private val trueT: TypeRec[Expr] = boolT(true, boolTypeT)

  /** eq: 構成子が同じで、フィールドがすべて等しい */
  private def equalityBody(p: Plan, env: Env, names: Methods): TypeRec[Expr] = comparison(p) { (go, i, j, fields) =>
    def equal(field: FieldCompare, t: TypeRec[Type], x: TypeRec[Expr], y: TypeRec[Expr]): TypeRec[Expr] =
      if (field.recursive) mkApp(mkApp(varrType(go, arrowT(t, arrowT(t, boolTypeT))), x), y)
      else mkApp(mkApp(methodAt(env, names.method, t, evidenceOf(field.evidence)), x), y)
    if (i != j) falseT
    else fields.foldRight(trueT) { case ((field, t, x, y), rest) =>
      if (rest eq trueT) equal(field, t, x, y) else ifT(boolTypeT, equal(field, t, x, y), rest, falseT)
    }
  }

  /** lt: 構成子の宣言順。同じ構成子なら、フィールドを前から比べる（辞書順） */
  private def orderBody(p: Plan, env: Env, names: Methods): TypeRec[Expr] = comparison(p) { (go, i, j, fields) =>
    val eqMethod = names.eq.getOrElse(invariant("derive Ord: the plan has no eq method"))
    def less(field: FieldCompare, t: TypeRec[Type], x: TypeRec[Expr], y: TypeRec[Expr]): TypeRec[Expr] =
      if (field.recursive) mkApp(mkApp(varrType(go, arrowT(t, arrowT(t, boolTypeT))), x), y)
      else mkApp(mkApp(methodAt(env, names.method, t, evidenceOf(field.evidence)), x), y)
    def equal(field: FieldCompare, t: TypeRec[Type], x: TypeRec[Expr], y: TypeRec[Expr]): TypeRec[Expr] =
      mkApp(mkApp(methodAt(env, eqMethod, t, evidenceOf(field.eqEvidence)), x), y)
    def lexicographic(rest: Seq[(FieldCompare, TypeRec[Type], TypeRec[Expr], TypeRec[Expr])]): TypeRec[Expr] = rest match {
      case Seq() => falseT
      case Seq((field, t, x, y)) => less(field, t, x, y)
      case (field, t, x, y) +: more => ifT(boolTypeT, less(field, t, x, y), trueT, ifT(boolTypeT, equal(field, t, x, y), lexicographic(more), falseT))
    }
    if (i < j) trueT else if (i > j) falseT else lexicographic(fields)
  }

  /** show: 構成子の名前と、フィールドの表示。名前付きのフィールドは name = 値、名前のないものは値だけ: C(x = 1, 2) */
  private def showBody(p: Plan, env: Env, names: Methods): TypeRec[Expr] = {
    val self = p.inst.targets.head
    val shower = arrowT(self, stringTypeT)
    val concatType = arrowT(stringTypeT, arrowT(stringTypeT, stringTypeT))
    def text(value: String): TypeRec[Expr] = stringLitT(value, stringTypeT)
    def concat(parts: Seq[TypeRec[Expr]]): TypeRec[Expr] =
      parts.reduceLeft((acc, part) => mkApp(mkApp(foreignT(Variable("concat"), concatType, concatType), acc), part))
    val (go, v, x) = (Variable("$go"), Variable("$v"), Variable("$x"))
    val cases = p.dataDef.constructors.zip(p.compares).map { case (constructor, compares) =>
      val xs = constructor.fields.indices.map(k => Variable(s"$$x_$k"))
      val label = shortName(constructor.name)
      val shown = compares.indices.map { k =>
        val value = varrType(xs(k), constructor.fields(k))
        val compare = compares(k)
        val rendered =
          if (compare.recursive) mkApp(varrType(go, shower), value)
          else mkApp(methodAt(env, names.method, constructor.fields(k), evidenceOf(compare.evidence)), value)
        constructor.names(k).fold(Seq(rendered))(field => Seq(text(s"$field = "), rendered))
      }
      val body =
        if (shown.isEmpty) text(label)
        else concat(text(s"$label(") +: shown.zipWithIndex.flatMap { case (part, k) => if (k == 0) part else text(", ") +: part } :+ text(")"))
      MatchCase[TypeRec](constructor.name, xs, body)
    }
    val goValue = mkAbs(v, self, matchExprT(stringTypeT, varrType(v, self), cases))
    mkAbs(x, self, letRecT(go, stringTypeT, shower, goValue, mkApp(varrType(go, shower), varrType(x, self))))
  }
}
