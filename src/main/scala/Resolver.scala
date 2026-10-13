package com.yuuki14202028

import cats.syntax.all.*

import scala.annotation.tailrec

/** 制約 C[τ̄] の解決。型検査の一部として Env（スコープ中の制約・その時点までに宣言されたインスタンス）
 * だけを見て根拠を導出する。型適用が明示なので解決は決定的で、型には一切影響しない。
 *
 * 解決順: スコープ中の制約 → スコープ中の量化された制約（前提を再帰的に解決）
 *         → インスタンス（文脈を再帰的に解決）→ スコープ中の制約のスーパークラス射影
 * 量化された制約 ∀ā. P̄ ⇒ C[τ̄] を示すには、ā と P̄ をスコープに入れて C[τ̄] を解決する。
 */
object Resolver {

  private val maxResolutionDepth = 64

  private def invariant(msg: String): Nothing = sys.error(s"Compiler invariant violation: $msg")

  // 量化された制約は束縛変数の名前を除いて比べる（辞書の型の α 同値）
  def sameConstraint(left: TypeConstraint, right: TypeConstraint): Boolean =
    Equivalence.alpha(dictionaryType(left), dictionaryType(right))

  def resolve(constraint: TypeConstraint): Check[Evidence[TypeRec]] =
    Check.ask.flatMap(env => Check.lift(resolveIn(env, constraint, 0)))

  // impl C[T̄] が満たすべきスーパークラス制約: Sⱼ[p̄ := T̄]
  def superConstraints(traitDef: TraitDef, targets: Seq[TypeRec[Type]]): Seq[TypeConstraint] =
    traitDef.supers.map(instantiate(_, traitDef.paramVars, targets))

  // 制約への代入。量化された制約の束縛変数を捕獲しないよう、型 [制約] → unit への代入として行う
  private def instantiate(constraint: TypeConstraint, params: Seq[TypeVariable], args: Seq[TypeRec[Type]]): TypeConstraint =
    destructInstArrow(Equivalence.normalize(substMany(params, args, instArrowT(constraint, unitTypeT)))).map(_._1)
      .getOrElse(invariant("substitution must preserve the shape of a constraint"))

  private def resolveIn(env: Env, constraint: TypeConstraint, depth: Int): EitherS[Evidence[TypeRec]] =
    if (depth > maxResolutionDepth) Left(CompileError.ResolutionDepthExceeded(constraint.name, constraint.arg))
    else env.givens.find(sameConstraint(_, constraint)) match {
      case Some(assumed) => Right(Evidence.Given(assumed))
      case None if constraint.quantified => resolveQuantified(env, constraint, depth)
      case None => resolveViaQuantifiedGiven(env, constraint, depth)
        .orElse(resolveViaInstance(env, constraint, depth))
        .orElse(resolveViaSupers(env, constraint).map(Right(_)))
        .getOrElse(Left(CompileError.NoInstance(constraint.name, constraint.arg)))
    }

  // ∀ā. P̄ ⇒ C[τ̄] を示す: ā を型変数として、P̄ を制約としてスコープに入れ、C[τ̄] を解決する
  private def resolveQuantified(env: Env, constraint: TypeConstraint, depth: Int): EitherS[Evidence[TypeRec]] = {
    // ā がスコープ中の型変数と同じ名前だと根拠の中で取り違えるので、先に付け替える
    val taken = env.typeVars.keySet ++ env.givens.flatMap(g => freeTypeVars(dictionaryType(g))) ++ freeTypeVars(dictionaryType(constraint))
    val (fresh, _) = constraint.params.foldLeft((constraint, taken)) { case ((current, used), (param, _)) =>
      if (!used.contains(param)) (current, used + param)
      else {
        val renamed = freshTypeVariable(param, used)
        def rename(t: TypeRec[Type]): TypeRec[Type] = substType(param, typeVarT(renamed), t)
        val next = Constraint[TypeRec](
          current.name, current.arg.map(rename),
          current.params.map { case (p, k) => (if (p == param) renamed else p, k) },
          current.premises.map(p => Constraint[TypeRec](p.name, p.arg.map(rename)))
        )
        (next, used + renamed)
      }
    }
    resolveIn(env.withTypeVars(fresh.params).withGivens(fresh.premises), fresh.head, depth + 1).map(Evidence.Abs(fresh, _))
  }

  // インスタンスヘッドの一方向マッチ: pattern 中の patternVars を actual の部分型に束縛する
  private def matchTypes(
      pattern: TypeRec[Type],
      actual: TypeRec[Type],
      patternVars: Set[TypeVariable],
      acc: Map[TypeVariable, TypeRec[Type]]
  ): Option[Map[TypeVariable, TypeRec[Type]]] =
    (pattern.project, actual.project) match {
      case (AST.TypeVar(v), _) if patternVars.contains(v) =>
        acc.get(v) match {
          case Some(prev) => Option.when(Equivalence.alpha(prev, actual))(acc)
          case None => Some(acc + (v -> actual))
        }
      case (AST.TypeVar(l), AST.TypeVar(r)) => Option.when(l == r)(acc)
      case (AST.Primitive(l), AST.Primitive(r)) => Option.when(l == r)(acc)
      case (AST.TypeApp(lf, la), AST.TypeApp(rf, ra)) =>
        matchTypes(lf, rf, patternVars, acc).flatMap(matchTypes(la, ra, patternVars, _))
      case (AST.Arrow(lf, lt), AST.Arrow(rf, rt)) =>
        matchTypes(lf, rf, patternVars, acc).flatMap(matchTypes(lt, rt, patternVars, _))
      case _ => None
    }

  // スコープ中の量化された制約 ∀ā. P̄ ⇒ C[τ̄] のヘッドが合えば、前提 P̄ の解決結果を返す
  private def resolveViaQuantifiedGiven(env: Env, constraint: TypeConstraint, depth: Int): Option[EitherS[Evidence[TypeRec]]] =
    env.givens.iterator.filter(g => g.quantified && g.name == constraint.name && g.arg.length == constraint.arg.length).flatMap { assumed =>
      val paramVars = assumed.params.map(_._1)
      assumed.arg.zip(constraint.arg).toList
        .foldLeftM(Map.empty[TypeVariable, TypeRec[Type]]) { case (acc, (pattern, actual)) => matchTypes(pattern, actual, paramVars.toSet, acc) }
        .filter(subst => paramVars.forall(subst.contains))
        .map { subst =>
          val typeArgs = paramVars.map(subst)
          assumed.premises
            .traverse(premise => resolveIn(env, instantiate(premise, paramVars, typeArgs), depth + 1))
            .map(Evidence.Apply(Evidence.Given(assumed), typeArgs, _))
        }
    }.nextOption()

  // ヘッドが合うインスタンスがあれば、その文脈の解決結果を返す（文脈が解決できなければエラーで確定する）
  private def resolveViaInstance(env: Env, constraint: TypeConstraint, depth: Int): Option[EitherS[Evidence[TypeRec]]] = for {
    heads <- constraint.arg.traverse(a => typeConstructorHead(a).map(_._1))
    inst <- env.instances.get(instanceKey(constraint.name, heads))
    paramVars = inst.params.map(_._1)
    subst <- inst.targets.zip(constraint.arg).toList.foldLeftM(Map.empty[TypeVariable, TypeRec[Type]]) {
      case (acc, (pattern, actual)) => matchTypes(pattern, actual, paramVars.toSet, acc)
    }
  } yield {
    val typeArgs = paramVars.map(p => subst.getOrElse(p, invariant(s"instance ${inst.dictName.name}: parameter ${p.name} is unbound")))
    inst.context
      .traverse(c => resolveIn(env, instantiate(c, paramVars, typeArgs), depth + 1))
      .map(Evidence.Instance(constraint.name, heads, typeArgs, _))
  }

  // スコープ中の制約からスーパークラス射影をたどって目的の制約に到達できれば成功（幅優先）
  private def resolveViaSupers(env: Env, constraint: TypeConstraint): Option[Evidence[TypeRec]] = {
    type Node = (TypeConstraint, Evidence[TypeRec])

    def children(node: Node): List[Node] = {
      val (reached, evidence) = node
      env.traits.get(reached.name).fold(List.empty[Node]) { traitDef =>
        traitDef.supers.zipWithIndex.toList.map { case (sup, index) =>
          (instantiate(sup, traitDef.paramVars, reached.arg), Evidence.Super(reached.name, index, evidence))
        }
      }
    }

    @tailrec
    def search(queue: List[Node]): Option[Evidence[TypeRec]] = queue match {
      case Nil => None
      case (node @ (reached, evidence)) :: rest =>
        if (sameConstraint(reached, constraint)) Some(evidence) else search(rest ++ children(node))
    }

    // スコープ中の制約そのものは resolveIn が先に調べているので、その子から探索する
    search(env.givens.toList.filterNot(_.quantified).map(g => (g, Evidence.Given(g): Evidence[TypeRec])).flatMap(children))
  }
}
