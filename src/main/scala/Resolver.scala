package com.yuuki14202028

import cats.syntax.all.*

import scala.annotation.tailrec

/** 制約 C[τ̄] の解決。型検査の一部として Env（スコープ中の制約・その時点までに宣言されたインスタンス）
 * だけを見て根拠を導出する。型適用が明示なので解決は決定的で、型には一切影響しない。
 *
 * 解決順: スコープ中の制約 → インスタンス（文脈を再帰的に解決）→ スコープ中の制約のスーパークラス射影
 */
object Resolver {

  private val maxResolutionDepth = 64

  private def invariant(msg: String): Nothing = sys.error(s"Compiler invariant violation: $msg")

  def sameConstraint(left: TypeConstraint, right: TypeConstraint): Boolean =
    left.name == right.name && left.arg.length == right.arg.length &&
      left.arg.zip(right.arg).forall { case (l, r) => Equivalence.alpha(l, r) }

  def resolve(constraint: TypeConstraint): Check[Evidence[TypeRec]] =
    Check.ask.flatMap(env => Check.lift(resolveIn(env, constraint, 0)))

  // impl C[T̄] が満たすべきスーパークラス制約: Sⱼ[p̄ := T̄]
  def superConstraints(traitDef: TraitDef, targets: Seq[TypeRec[Type]]): Seq[TypeConstraint] =
    traitDef.supers.map(instantiate(_, traitDef.paramVars, targets))

  private def instantiate(constraint: TypeConstraint, params: Seq[TypeVariable], args: Seq[TypeRec[Type]]): TypeConstraint =
    TypeConstraint(constraint.name, constraint.arg.map(a => Equivalence.normalize(substMany(params, args, a))))

  private def resolveIn(env: Env, constraint: TypeConstraint, depth: Int): EitherS[Evidence[TypeRec]] =
    if (depth > maxResolutionDepth) Left(CompileError.ResolutionDepthExceeded(constraint.name, constraint.arg))
    else env.givens.find(sameConstraint(_, constraint)) match {
      case Some(assumed) => Right(Evidence.Given(assumed))
      case None => resolveViaInstance(env, constraint, depth)
        .orElse(resolveViaSupers(env, constraint).map(Right(_)))
        .getOrElse(Left(CompileError.NoInstance(constraint.name, constraint.arg)))
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
    search(env.givens.toList.map(g => (g, Evidence.Given(g): Evidence[TypeRec])).flatMap(children))
  }
}
