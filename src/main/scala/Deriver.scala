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

  private enum Rule { case FunctorRule, FoldableRule }

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
      inst: InstanceDef
  )

  private val functorMethod = Variable("map")
  private val foldRightMethod = Variable("foldRight")
  private val foldLeftMethod = Variable("foldLeft")
  private val hkKind = Kind.Arrow(Kind.Star, Kind.Star)

  // ---- 正準シグネチャ ----

  private def canonicalSigs(rule: Rule, f: TypeVariable): Seq[(Variable, TypeRec[Type])] = {
    val (a, b) = (TypeVariable("a"), TypeVariable("b"))
    val (aT, bT) = (typeVarT(a), typeVarT(b))
    def fT(x: TypeVariable) = typeAppT(typeVarT(f), typeVarT(x))
    def poly(body: TypeRec[Type]) = forallTypeT(a, Kind.Star, forallTypeT(b, Kind.Star, body))
    rule match {
      case Rule.FunctorRule => Seq(
        functorMethod -> poly(arrowT(arrowT(aT, bT), arrowT(fT(a), fT(b))))
      )
      case Rule.FoldableRule => Seq(
        foldRightMethod -> poly(arrowT(arrowT(aT, arrowT(bT, bT)), arrowT(bT, arrowT(fT(a), bT)))),
        foldLeftMethod -> poly(arrowT(arrowT(bT, arrowT(aT, bT)), arrowT(bT, arrowT(fT(a), bT))))
      )
    }
  }

  private def ruleFor(traitName: TypeVariable): Option[Rule] = traitName.name match {
    case "Functor" => Some(Rule.FunctorRule)
    case "Foldable" => Some(Rule.FoldableRule)
    case _ => None
  }

  // ---- 検査（TAnalyser から宣言単位で呼ばれる） ----

  def check(traitName: TypeVariable, target: TypeVariable, env: Env): EitherS[InstanceDef] = for {
    _ <- Either.cond(
      !env.instances.contains(instanceKey(traitName, Seq(target.name))), (),
      CompileError.OverlappingInstance(traitName, Seq(target.name))
    )
    p <- plan(traitName, target, env)
  } yield p.inst

  private def plan(traitName: TypeVariable, target: TypeVariable, env: Env): EitherS[Plan] = for {
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
    fieldPlans <- dataDef.constructors.toList.traverse { c =>
      c.fields.toList.traverse(field => classify(rule, traitName, target, dataDef, c.name, field, env))
    }
  } yield {
    val prefix = dataDef.params.init
    val targets = Seq(applyTypeConstructor(target, prefix.map { case (v, _) => typeVarT(v) }))
    val inst = InstanceDef(traitName, targets, Seq.empty, instanceDictionaryName(traitName, Seq(target.name)), prefix)
    Plan(rule, target, dataDef, fieldPlans, inst)
  }

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
            case Rule.FoldableRule => unsupported
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
              else go(args.last).map(FieldPlan.Nested.apply)
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
      val expanded = decls.map { decl =>
        decl.project match {
          case AST.TopDerive(traitName, target) => synthesize(traitName, target, env)
          case _ => decl
        }
      }
      programT(expanded, env)
  }

  private def synthesize(traitName: TypeVariable, target: TypeVariable, env: Env): TypeRec[Decl] = {
    val p = plan(traitName, target, env)
      .fold(err => invariant(s"derive ${traitName.name}[${target.name}] failed after validation: ${err.render}"), identity)
    val methods = p.rule match {
      case Rule.FunctorRule => Seq(MethodImpl[TypeRec](functorMethod, None, mapBody(p, env)))
      case Rule.FoldableRule => Seq(
        MethodImpl[TypeRec](foldRightMethod, None, foldBody(p, env, rightward = true)),
        MethodImpl[TypeRec](foldLeftMethod, None, foldBody(p, env, rightward = false))
      )
    }
    topImplT(traitName, p.inst.params, p.inst.targets, Seq.empty, methods)
  }

  // ---- 型付き AST 構築ヘルパー ----

  private def applyTypeArgI(e: TypeRec[Expr], arg: TypeRec[Type]): TypeRec[Expr] =
    destructForAllK(typeOf(e)) match {
      case Some((v, _, body)) => tyAppT(Equivalence.normalize(substType(v, arg, body)), e, arg)
      case None => invariant(s"derive: expected a polymorphic type, got ${typeOf(e).show}")
    }

  private def applyArgI(f: TypeRec[Expr], x: TypeRec[Expr]): TypeRec[Expr] =
    destructArrow(typeOf(f)) match {
      case Some((_, to)) => appT(to, f, x)
      case None => invariant(s"derive: expected a function type, got ${typeOf(f).show}")
    }

  private def valueRef(env: Env, name: Variable): TypeRec[Expr] =
    varrType(name, env.values.getOrElse(name, invariant(s"derive: ${name.name} is missing from Env")))

  private def mkAbs(v: Variable, paramType: TypeRec[Type], body: TypeRec[Expr]): TypeRec[Expr] =
    absT(v, arrowT(paramType, typeOf(body)), paramType, body)

  private def mkTyAbs(v: TypeVariable, body: TypeRec[Expr]): TypeRec[Expr] =
    tyAbsT(v, forallTypeT(v, Kind.Star, typeOf(body)), Kind.Star, body)

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

  /** map = Λα. Λβ. λf. letRec go = λd. match d { Kᵢ(x̄) → Kᵢ[p̄][β](φ(x̄)) } in go */
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
      case FieldPlan.Param => applyArgI(fRef, x)
      case FieldPlan.Recurse => applyArgI(goRef, x)
      case FieldPlan.Func(toPlan) =>
        val (from, to) = destructArrow(t).getOrElse(invariant(s"derive map: expected an arrow field, got ${t.show}"))
        val y = Variable(s"$$y_$depth")
        mkAbs(y, from, transform(toPlan, to, applyArgI(x, varrType(y, from)), depth + 1))
      case FieldPlan.Nested(innerPlan) =>
        val (head, args) = collectTypeApps(t)
        val container = args.init.foldLeft(head)(typeAppT)
        val innerA = args.last
        val innerB = Equivalence.normalize(substType(s.alpha, bT, innerA))
        val y = Variable(s"$$y_$depth")
        val innerF = mkAbs(y, innerA, transform(innerPlan, innerA, varrType(y, innerA), depth + 1))
        val mapRef = applyTypeArgI(applyTypeArgI(applyTypeArgI(valueRef(env, functorMethod), container), innerA), innerB)
        applyArgI(applyArgI(mapRef, innerF), x)
    }

    val cases = p.dataDef.constructors.zip(p.fieldPlans).map { case (ctor, plans) =>
      val binders = ctor.fields.indices.map(i => Variable(s"$$x_$i"))
      val fieldTypes = ctor.fields.map(s.substLast)
      val tyApplied = (s.prefixVars.map(typeVarT) :+ bT).foldLeft(valueRef(env, ctor.name))(applyTypeArgI)
      val applied = binders.lazyZip(fieldTypes).lazyZip(plans).foldLeft(tyApplied) {
        case (acc, (x, t, plan)) => applyArgI(acc, transform(plan, t, varrType(x, t), 0))
      }
      MatchCase[TypeRec](ctor.name, binders, applied)
    }
    val goValue = mkAbs(dVar, dAlpha, matchExprT(dBeta, varrType(dVar, dAlpha), cases))
    val letR = letRecT(goVar, goType, goType, goValue, goRef)
    mkTyAbs(s.alpha, mkTyAbs(s.beta, mkAbs(fVar, fType, letR)))
  }

  /** foldRight / foldLeft =
   *  Λα. Λβ. λf. λz. letRec go = λd. λacc. match d { Kᵢ(x̄) → ψ(x̄, acc) } in λv. go(v)(z) */
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
        if (rightward) applyArgI(applyArgI(fRef, x), acc)
        else applyArgI(applyArgI(fRef, acc), x)
      case FieldPlan.Recurse => applyArgI(applyArgI(goRef, x), acc)
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
        val foldRef = applyTypeArgI(applyTypeArgI(applyTypeArgI(valueRef(env, methodName), container), innerA), bT)
        applyArgI(applyArgI(applyArgI(foldRef, innerF), acc), x)
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
    val finalLam = mkAbs(vVar, dAlpha, applyArgI(applyArgI(goRef, varrType(vVar, dAlpha)), varrType(zVar, bT)))
    val letR = letRecT(goVar, typeOf(finalLam), goType, goValue, finalLam)
    mkTyAbs(s.alpha, mkTyAbs(s.beta, mkAbs(fVar, fType, mkAbs(zVar, bT, letR))))
  }
}
