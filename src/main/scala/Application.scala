package com.yuuki14202028

import scala.collection.mutable.ListBuffer

/** 適用の並びに書かれた引数（検査済み） */
enum SpineArgument {
  case OfType(tpe: Type, kind: Kind, span: Span)
  case OfValue(value: Typed, span: Span)
  // (with d): 解決の代わりに渡す辞書
  case OfDictionary(value: Typed, span: Span)
}

/** 適用: 型引数を値から読み、制約を解決する。演算子・ドット記法・リテラルなどの糖衣も、すべてここを通る。
 *
 * 適用の並び head[T](v)… を左から順に処理する。関数の型は ∀a. / [C] → / A → の並びで、
 *
 *  - 値の引数: 先頭の ∀ と制約を外して A → … にし、A と値の型を見比べて、未決定の型パラメータを決める
 *  - 書かれた型引数 [T]: 未決定の最初の型パラメータを決める（なければ ∀ を 1 つ外す）
 *  - (with d): まだ辞書が渡されていない最初の制約に、d を渡す
 *  - 並びの終わり: 先頭に残った制約を外す。未決定の型パラメータが残っていればエラー。
 *    外した制約のうち辞書が渡されなかったものは、決まった型でインスタンスを解決する
 */
object Application {

  private def fail(error: CompileError): Nothing = throw CompileFailure(error)

  /** 関数に渡すもの。型パラメータと制約に何が入るかは、並びの終わりまでに決まる */
  private enum Step {
    case TypeParameter(parameter: Symbol)
    case Value(value: Typed)
    // at は、この制約に行き当たったときの「頭と、そこまでの引数」の位置
    case Constraint(constraint: Type, at: Span)
  }

  /** headSpan は頭の式の位置（糖衣が作った適用では、ソースに頭がないので Span.none） */
  def apply(env: Env, head: Typed, arguments: List[SpineArgument], span: Span, headSpan: Span = Span.none): Typed = {
    var current = head.tpe
    // ここまでに処理した「頭と引数」の位置（エラーで、関数の側を指すのに使う）
    var consumed = headSpan
    def functionSpan: Span = if (headSpan == Span.none) span else consumed
    var parameters = Vector.empty[(Symbol, Kind)]
    var solved = Map.empty[Symbol, Type]
    val steps = ListBuffer.empty[Step]

    def unsolved: Vector[(Symbol, Kind)] = parameters.filterNot { case (parameter, _) => solved.contains(parameter) }

    /** 先頭の ∀ か制約を 1 つ外す */
    def peel(): Boolean = current match {
      case Type.Forall(name, kind, body) =>
        val parameter = new Symbol(name.name)
        parameters :+= (parameter, kind)
        steps += Step.TypeParameter(parameter)
        current = Type.open(body, Type.Var(parameter))
        true
      case Type.Given(constraint, to) =>
        steps += Step.Constraint(constraint, functionSpan)
        current = to
        true
      case _ => false
    }

    def isForall: Boolean = current.isInstanceOf[Type.Forall]
    def isGiven: Boolean = current.isInstanceOf[Type.Given]

    /** pattern と actual を見比べて、未決定の型パラメータを決める。function は、合わなかったときに示す関数の型 */
    def read(pattern: Type, actual: Type, at: Span, function: Option[Type]): Unit = {
      val expected = Type.substitute(pattern, solved)
      val found = Types.matchType(expected, actual, unsolved.map(_._1).toSet, Map.empty).getOrElse {
        val mismatch = CompileError.TypeMismatch(expected, actual, at)
        fail(function.filter(_ => headSpan != Span.none).fold(mismatch)(tpe => mismatch.withRelated(consumed, Related.Function(tpe))))
      }
      unsolved.foreach { case (parameter, kind) =>
        found.get(parameter).map(env.kindOf).filter(_ != kind).foreach(actualKind => fail(CompileError.KindMismatch(kind, actualKind, at)))
      }
      solved ++= found
      current = Type.substitute(current, solved)
    }

    arguments.foreach {
      case SpineArgument.OfType(tpe, kind, at) =>
        if (unsolved.isEmpty) {
          while (isGiven) peel()
          if (!isForall) fail(CompileError.NotPolymorphic(current, functionSpan))
          peel()
        }
        val (parameter, expected) = unsolved.head
        if (kind != expected) fail(CompileError.KindMismatch(expected, kind, at))
        solved += parameter -> tpe
        current = Type.substitute(current, solved)
        consumed = consumed.to(at)

      case SpineArgument.OfValue(value, at) =>
        while (peel()) ()
        current match {
          case Type.Arrow(parameterType, result) =>
            val function = current
            current = result
            read(parameterType, value.tpe, at, Some(function))
            steps += Step.Value(value)
            consumed = consumed.to(at)
          case other => fail(CompileError.NotAFunction(other, functionSpan))
        }

      case SpineArgument.OfDictionary(dictionary, at) =>
        def pending: Int = steps.indexWhere(_.isInstanceOf[Step.Constraint])
        while (pending < 0 && peel()) ()
        steps.lift(pending) match {
          case Some(Step.Constraint(constraint, _)) =>
            read(constraint, dictionary.tpe, at, None)
            steps(pending) = Step.Value(dictionary)
            consumed = consumed.to(at)
          case _ => fail(CompileError.NoConstraintToSatisfy(current, functionSpan))
        }
    }

    while (isGiven) peel()
    unsolved.headOption.foreach { case (parameter, _) => fail(CompileError.TypeArgumentUndetermined(parameter.name, head.tpe, span)) }
    val passed = steps.toList.map {
      case Step.TypeParameter(parameter) => Typed.Argument.OfType(solved(parameter))
      case Step.Value(value) => Typed.Argument.OfValue(value)
      case Step.Constraint(constraint, at) => Typed.Argument.OfValue(Instances.resolve(env, Type.substitute(constraint, solved), at))
    }
    if (passed.isEmpty) head else Typed.Apply(head, passed, current)
  }
}
