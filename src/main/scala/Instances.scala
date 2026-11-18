package com.yuuki14202028

/** 制約（辞書の型）から、その辞書を表す式を作る。
 *
 * 使える辞書は、スコープにある制約（with 節・using）と、宣言されたインスタンス。どれも型は ∀ā. [前提…] → C[T̄] の形で、
 * C[T̄] が求める制約と合えば（ā を照合で決めて）、前提を同じように解決して渡す。
 * 探す順は、スコープにある制約 → インスタンス → スコープにある制約のスーパークラス。最初に合ったものを使う。
 * 解決は型に影響しない（型引数はすでに決まっていて、辞書を選ぶだけ）。
 */
object Instances {

  private val maxDepth = 64

  /** 使える辞書の候補: ∀parameters. [premises] → head。build は、型引数と前提の辞書から辞書の式を作る */
  private final case class Candidate(parameters: List[(Symbol, Kind)], premises: List[Type], head: Type, shown: String, build: (List[Type], List[Typed]) => Typed)

  def resolve(env: Env, goal: Type, span: Span): Typed = search(env, goal, span, Nil, depth = 0)

  /** requiredBy は、この制約に至るまでの（必要とした制約, それを必要としたインスタンス）。エラーの説明に使う */
  private def search(env: Env, goal: Type, span: Span, requiredBy: List[(String, String)], depth: Int): Typed = goal match {
    case _ if depth > maxDepth => throw CompileFailure(CompileError.ResolutionTooDeep(goal, span))

    // 量化された制約 ∀a. P ⇒ C を示す: a と P をスコープに入れて C を解決する
    case Type.Forall(name, kind, body) =>
      val parameter = new Symbol(name.name)
      val inner = search(env.withTypeVariable(parameter, kind), Type.open(body, Type.Var(parameter)), span, requiredBy, depth)
      Typed.TypeLambda(parameter, kind, inner, goal)
    case Type.Given(premise, conclusion) =>
      val dictionary = new Symbol("given")
      val inner = search(env.withGiven(Evidence(premise, Typed.Variable(dictionary, premise))), conclusion, span, requiredBy, depth)
      Typed.GivenLambda(dictionary, premise, inner, goal)

    case _ =>
      val scoped = env.givens.map(candidate)
      val heads = Types.constraintHead(env, goal).map { case (traitDef, arguments) => (traitDef.symbol, arguments.flatMap(TypeHead.of), arguments.length) }
      val declared = heads.collect { case (symbol, found, count) if found.length == count => InstanceKey(symbol, found) }.flatMap(env.instances.get).map(candidate)
      val candidates = scoped.iterator ++ declared.iterator ++ scoped.iterator.flatMap(supers(env, _))
      candidates.flatMap(use(env, _, goal, span, requiredBy, depth)).nextOption().getOrElse {
        if (heads.exists { case (symbol, found, _) => env.failedInstances(InstanceKey(symbol, found)) }) throw CompileFailure(CompileError.DependsOnFailedDeclaration(span))
        val known = env.instances.collect { case (key, instance) if heads.exists(_._1 eq key.traitSymbol) => show(instance.constraint) }.toList.sorted
        // 宣言はされているが、ここからは見えないインスタンスなら、そう伝える
        val hints = env.hidden.collectFirst {
          case (constraint, later) if matches(candidate(Evidence(constraint, Typed.Constant(Literal.Unit, constraint))), goal) =>
            if (later) Hint.InstanceDeclaredLater(Type.show(goal)) else Hint.InstanceNeedsRec(Type.show(goal))
        }
        throw CompileFailure(CompileError.NoInstance(goal, requiredBy, known, span).withHints(hints.toList))
      }
  }

  private def matches(found: Candidate, goal: Type): Boolean = Types.matchType(found.head, goal, found.parameters.map(_._1).toSet, Map.empty).nonEmpty

  /** 候補の頭が goal に合えば、前提を解決して辞書を作る（前提が解決できなければ、そこでエラー） */
  private def use(env: Env, found: Candidate, goal: Type, span: Span, requiredBy: List[(String, String)], depth: Int): Option[Typed] = {
    val flexible = found.parameters.map(_._1)
    Types.matchType(found.head, goal, flexible.toSet, Map.empty).filter(solved => flexible.forall(solved.contains)).map { solved =>
      val premises = found.premises.map(premise => search(env, Type.substitute(premise, solved), span, requiredBy :+ (Type.show(goal), found.shown), depth + 1))
      found.build(flexible.map(solved), premises)
    }
  }

  private def candidate(evidence: Evidence): Candidate = {
    val (parameters, body) = Type.openForalls(evidence.constraint)
    val (premises, head) = Type.givens(body)
    Candidate(parameters, premises, head, show(evidence.constraint), (types, dictionaries) => {
      val result = Type.substitute(head, parameters.map(_._1).zip(types).toMap)
      if (types.isEmpty && dictionaries.isEmpty) evidence.term
      else Typed.Apply(evidence.term, types.map(Typed.Argument.OfType(_)) ++ dictionaries.map(Typed.Argument.OfValue(_)), result)
    })
  }

  /** found の辞書からスーパークラスの辞書を取り出して得られる候補（その先のスーパークラスも含む） */
  private def supers(env: Env, found: Candidate): List[Candidate] =
    Type.dataApplication(found.head).flatMap { case (symbol, arguments) => env.traits.get(symbol).map(_ -> arguments) }.toList.flatMap {
      case (traitDef, arguments) =>
        traitDef.supers.flatMap { case (written, projection) =>
          val superType = traitDef.instantiate(written, arguments)
          def project(typeArguments: List[Type], result: Type, dictionary: Typed): Typed = {
            val function = Typed.Variable(projection, env.values(projection))
            Typed.Apply(function, typeArguments.map(Typed.Argument.OfType(_)) :+ Typed.Argument.OfValue(dictionary), result)
          }
          val next =
            if (found.parameters.isEmpty && found.premises.isEmpty)
              Some(candidate(Evidence(superType, project(arguments, superType, found.build(Nil, Nil)))))
            // 量化された制約から得るスーパークラスは、trait を型に適用しただけのものに限る
            else if (Type.dataApplication(superType).nonEmpty)
              Some(Candidate(found.parameters, found.premises, superType, found.shown, (types, dictionaries) => {
                val solved = found.parameters.map(_._1).zip(types).toMap
                project(arguments.map(Type.substitute(_, solved)), Type.substitute(superType, solved), found.build(types, dictionaries))
              }))
            else None
          next.toList.flatMap(reached => reached :: supers(env, reached))
        }
    }

  /** 制約を、宣言の書き方で表示する: impl[a] Show[List[a]] with Show[a] */
  def show(constraint: Type): String = {
    val (parameters, body) = Type.openForalls(constraint)
    val (premises, head) = Type.givens(body)
    val binders = parameters.map { case (parameter, kind) => if (kind == Kind.Star) s"[${parameter.name}]" else s"[${parameter.name}: ${kind.show}]" }.mkString
    val context = if (premises.isEmpty) "" else premises.map(Type.show).mkString(" with ", ", ", "")
    s"impl$binders ${Type.show(head)}$context"
  }
}
