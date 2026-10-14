package com.yuuki14202028

import cats.syntax.all.*

/** derive 宣言（TopDerive）の検査と、型付き TopImpl への合成。
 *
 * 検査（check）は TAnalyser から宣言単位で呼ばれ、Env へ登録する InstanceDef を返す。
 * 合成（desugar）は TAnalyser の後・TraitEncoder の前のフェーズとして走り、TopDerive を
 * 手書き impl と同じ形の TopImpl へ置き換える。以降のフェーズは derive の存在を知らない。
 *
 * 導出対象は構造から一意に決まる trait（Functor / Foldable）に限る。trait は名前ではなく
 * 正準シグネチャとの α 同値で照合するため、同名の独自 trait に対して不正なコードを合成しない。
 */
object Deriver {

  private def invariant(msg: String): Nothing = sys.error(s"Compiler invariant violation: $msg")

  private enum Rule { case FunctorRule, FoldableRule, TraversableRule }

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
      superEvidence: Seq[Evidence[TypeRec]]
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
    case _ => None
  }

  // ---- 検査（TAnalyser から宣言単位で呼ばれる） ----

  /** derive が登録するインスタンス。フィールドの検査をせずに決まるので、rec の組では
   * 全員のインスタンスを先に求めて互いに見えるようにしてから、各 derive を check する */
  def instanceOf(traitName: TypeVariable, target: TypeVariable, env: Env): EitherS[InstanceDef] = for {
    dataDef <- env.dataTypes.get(target).filter(_ => !env.traits.contains(target))
      .toRight(CompileError.DeriveTargetNotData(target))
    _ <- Either.cond(dataDef.params.nonEmpty, (), CompileError.DeriveKindMismatch(traitName, target))
  } yield {
    val prefix = dataDef.params.init
    val targets = Seq(applyTypeConstructor(target, prefix.map { case (v, _) => typeVarT(v) }))
    InstanceDef(traitName, targets, Seq.empty, instanceDictionaryName(traitName, Seq(target.name)), prefix)
  }

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

  private def plan(traitName: TypeVariable, target: TypeVariable, env: Env, supers: Env): EitherS[Plan] = for {
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
      case Rule.FunctorRule => Seq(MethodImpl[TypeRec](functorMethod, None, mapBody(p, env)))
      case Rule.TraversableRule => Seq(MethodImpl[TypeRec](traverseMethod, None, traverseBody(p, env)))
      case Rule.FoldableRule => Seq(
        MethodImpl[TypeRec](foldRightMethod, None, foldBody(p, env, rightward = true)),
        MethodImpl[TypeRec](foldLeftMethod, None, foldBody(p, env, rightward = false))
      )
    }
    topImplT(traitName, p.inst.params, p.inst.targets, Seq.empty, methods, p.superEvidence)
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
}
