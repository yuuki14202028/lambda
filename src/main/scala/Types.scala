package com.yuuki14202028

/** 書かれた型（Resolved.TypeExpr）を検査して、検査に使う型（Type）にする。型どうしの照合もここにある */
object Types {
  import Resolved as R

  private def fail(error: CompileError): Nothing = throw CompileFailure(error)

  /** 型と、そのカインド */
  def convert(env: Env, tpe: R.TypeExpr): (Type, Kind) = tpe match {
    case R.TypeExpr.Named(symbol, span) =>
      env.typeVariables.get(symbol).map(kind => (Type.Var(symbol), kind))
        .orElse(env.aliases.get(symbol))
        .orElse(env.dataTypes.get(symbol).map(data => (Type.Data(symbol), data.kind)))
        .getOrElse(fail(CompileError.DependsOnFailedDeclaration(span)))
    case R.TypeExpr.Primitive(name, _) => (Type.Primitive(name), env.kindOf(Type.Primitive(name)))
    case R.TypeExpr.Arrow(from, to, _) => (Type.Arrow(convertStar(env, from), convertStar(env, to)), Kind.Star)
    case R.TypeExpr.Given(written, to, _) => (Type.Given(constraint(env, written), convertStar(env, to)), Kind.Star)
    case R.TypeExpr.Forall(parameter, kind, body, _) =>
      (Type.forall(parameter, kind, convertStar(env.withTypeVariable(parameter, kind), body)), Kind.Star)
    case R.TypeExpr.Lambda(parameter, kind, body, _) =>
      val (converted, bodyKind) = convert(env.withTypeVariable(parameter, kind), body)
      (Type.lambda(parameter, kind, converted), Kind.Arrow(kind, bodyKind))
    case R.TypeExpr.Apply(function, argument, _) =>
      val (convertedFunction, functionKind) = convert(env, function)
      val (convertedArgument, argumentKind) = convert(env, argument)
      functionKind match {
        case Kind.Arrow(expected, result) =>
          if (expected != argumentKind) fail(CompileError.KindMismatch(expected, argumentKind, argument.span))
          (Type.apply(convertedFunction, convertedArgument), result)
        case Kind.Star => fail(CompileError.KindMismatch(Kind.Arrow(argumentKind, Kind.Star), Kind.Star, function.span))
      }
    case R.TypeExpr.Tuple(elements, function, span) => (tuple(env, elements.map(convertStar(env, _)), function, span), Kind.Star)
    case R.TypeExpr.Known(known, _) => (known, env.kindOf(known))
  }

  /** 値の型（カインド *）として書かれた型 */
  def convertStar(env: Env, tpe: R.TypeExpr): Type = {
    val (converted, kind) = convert(env, tpe)
    if (kind != Kind.Star) fail(CompileError.KindMismatch(Kind.Star, kind, tpe.span))
    converted
  }

  /** 制約として書かれた型（trait を型に適用したもの。∀ と前提が付いていてもよい） */
  def constraint(env: Env, written: R.TypeExpr): Type = {
    val converted = convertStar(env, written)
    if (constraintHead(env, converted).isEmpty) fail(CompileError.NotAConstraint(converted, written.span))
    // 量化された制約は頭との照合で使うので、∀ の変数は頭に現れなければならない
    val (parameters, body) = Type.openForalls(converted)
    val head = Type.givens(body)._2
    parameters.find { case (parameter, _) => !Type.variables(head)(parameter) }.foreach { case (parameter, _) =>
      fail(CompileError.ConstraintBinderUnused(parameter.name, written.span))
    }
    converted
  }

  /** 制約 ∀ā. [P̄] → C[T̄] の C と T̄（∀ の変数は新しい型変数で開く）。制約でなければ None */
  def constraintHead(env: Env, tpe: Type): Option[(TraitDef, List[Type])] = {
    val (_, body) = Type.openForalls(tpe)
    val (_, head) = Type.givens(body)
    Type.dataApplication(head).flatMap { case (symbol, arguments) =>
      env.traits.get(symbol).filter(_.parameters.length == arguments.length).map(_ -> arguments)
    }
  }

  /** (A, B) の型: 組に宣言された関数 ∀ā. P₁ → … → Pₙ → R の Pᵢ を要素の型と見比べて ā を決めた、R */
  private def tuple(env: Env, elements: List[Type], function: Symbol, span: Span): Type = {
    val declared = env.values.getOrElse(function, fail(CompileError.DependsOnFailedDeclaration(span)))
    val (parameters, body) = Type.openForalls(declared)
    val flexible = parameters.map(_._1).toSet
    val (found, result) = elements.foldLeft((Map.empty[Symbol, Type], body)) {
      case ((solved, Type.Arrow(from, to)), element) =>
        (matchType(from, element, flexible, solved).getOrElse(fail(CompileError.TypeMismatch(Type.substitute(from, solved), element, span))), to)
      case ((_, other), _) => fail(CompileError.NotAFunction(other, span))
    }
    parameters.find { case (parameter, _) => !found.contains(parameter) }.foreach { case (parameter, _) =>
      fail(CompileError.TypeArgumentUndetermined(parameter.name, declared, span))
    }
    Type.substitute(result, found)
  }

  /** pattern の中の型パラメータ（flexible）を、actual と一致するように決める。
   *  型の形を上から見比べるだけの一方向の照合で、actual の側は変えない。決めた対応を found に足して返す */
  def matchType(pattern: Type, actual: Type, flexible: Set[Symbol], found: Map[Symbol, Type]): Option[Map[Symbol, Type]] =
    (pattern, actual) match {
      case (Type.Var(parameter), _) if flexible(parameter) =>
        found.get(parameter) match {
          case Some(previous) => Option.when(previous == actual)(found)
          // 束縛子の内側の変数を含む型は、外へ持ち出せないので読めない
          case None => Option.when(Type.isClosed(actual))(found + (parameter -> actual))
        }
      case (Type.Arrow(patternFrom, patternTo), Type.Arrow(actualFrom, actualTo)) =>
        matchType(patternFrom, actualFrom, flexible, found).flatMap(matchType(patternTo, actualTo, flexible, _))
      case (Type.Given(patternFrom, patternTo), Type.Given(actualFrom, actualTo)) =>
        matchType(patternFrom, actualFrom, flexible, found).flatMap(matchType(patternTo, actualTo, flexible, _))
      case (Type.Apply(patternFunction, patternArgument), Type.Apply(actualFunction, actualArgument)) =>
        matchType(patternFunction, actualFunction, flexible, found).flatMap(matchType(patternArgument, actualArgument, flexible, _))
      case (Type.Forall(_, patternKind, patternBody), Type.Forall(_, actualKind, actualBody)) if patternKind == actualKind =>
        matchType(patternBody, actualBody, flexible, found)
      case (Type.Lambda(_, patternKind, patternBody), Type.Lambda(_, actualKind, actualBody)) if patternKind == actualKind =>
        matchType(patternBody, actualBody, flexible, found)
      case _ => Option.when(pattern == actual)(found)
    }
}
