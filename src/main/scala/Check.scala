package com.yuuki14202028

/** 式の型検査。式の型は、部分式の型から下から順に決める（期待される型を上から渡すことはしない）。
 *
 * ユーザーエラーは CompileFailure を投げて打ち切り、宣言の境界（Declarations）で捕まえる。
 * 糖衣（演算子・ドット記法・リテラル・context など）は、宣言された関数の適用として検査し（Application）、
 * その適用を Typed.Sugar に入れておく。
 */
object Check {
  import Resolved as R
  import Types.{convert, convertStar}

  private def fail(error: CompileError): Nothing = throw CompileFailure(error)

  def expectType(expected: Type, actual: Type, span: Span): Unit =
    if (expected != actual) fail(CompileError.TypeMismatch(expected, actual, span))

  // ---- 束縛 ----

  def signature(env: Env, parsed: R.Binding): Type = convertStar(env, parsed.tpe)

  def binding(env: Env, parsed: R.Binding, declared: Type): Typed.Binding = {
    val value = infer(env, parsed.value)
    if (declared != value.tpe) {
      val (expected, actual, at, annotation) = blame(declared, value, parsed.value, Some(parsed.tpe))
      val mismatch = CompileError.TypeMismatch(expected, actual, at)
      fail(annotation.fold(mismatch)(written => mismatch.withRelated(written.span, Related.Annotation(expected))))
    }
    Typed.Binding(parsed.symbol, declared, value)
  }

  /** 期待する型と値の型が合わない所を、λ・Λ の中へ進んで探す（引数の型は合っていて、本体の型が違う、という所まで）。
   *  返すのは、そこで期待する型・実際の型・位置と、期待する型を書いた注釈 */
  def blame(expected: Type, value: Typed, written: R.Expr, annotation: Option[R.TypeExpr]): (Type, Type, Span, Option[R.TypeExpr]) =
    (expected, value, written) match {
      case (Type.Arrow(from, to), Typed.Lambda(_, parameterType, body, _), R.Expr.Lambda(_, _, writtenBody, _)) if from == parameterType =>
        blame(to, body, writtenBody, annotation.collect { case R.TypeExpr.Arrow(_, result, _) => result })
      case (Type.Given(constraint, to), Typed.GivenLambda(_, found, body, _), R.Expr.GivenLambda(_, writtenBody, _)) if constraint == found =>
        blame(to, body, writtenBody, annotation.collect { case R.TypeExpr.Given(_, result, _) => result })
      case (Type.Forall(_, kind, expectedBody), Typed.TypeLambda(parameter, found, body, _), R.Expr.TypeLambda(_, _, writtenBody, _)) if kind == found =>
        blame(Type.open(expectedBody, Type.Var(parameter)), body, writtenBody, annotation.collect { case R.TypeExpr.Forall(_, _, inner, _) => inner })
      case _ => (expected, value.tpe, written.span, annotation)
    }

  /** body の中で起きた型の不一致に、関係する位置を付ける */
  private def relating(at: Span, reason: => Related)(body: => Typed): Typed =
    try body
    catch {
      case failure: CompileFailure => throw failure.map {
        case mismatch @ CompileError.TypeMismatch(_, _, span) if span != at => mismatch.withRelated(at, reason)
        case other => other
      }
    }

  /** rec の組。組の全員が、互いの本体と、返す環境から見える */
  def recursiveBindings(env: Env, group: List[R.Binding]): (Env, List[Typed.Binding]) = {
    val signed = group.map(parsed => parsed -> signature(env, parsed))
    val inner = env.withValues(signed.map { case (parsed, declared) => parsed.symbol -> declared })
    val symbols = group.map(_.symbol).toSet
    val checked = CompileFailure.collect(signed) { case (parsed, declared) =>
      val checked = binding(inner, parsed, declared)
      // 関数でない値はその場で評価されるので、作っている途中の組の値を読んでしまう
      if (!isFunction(checked.value) && checked.value.mentions(symbols))
        fail(CompileError.RecursiveValueNotFunction(parsed.symbol.name, parsed.span))
      checked
    }
    (inner, checked)
  }

  /** 評価しても中身が走らない値（関数）か。型の抽象は実行時には消えるので、中を見る */
  def isFunction(value: Typed): Boolean = value match {
    case Typed.Lambda(_, _, _, _) | Typed.GivenLambda(_, _, _, _) => true
    case Typed.TypeLambda(_, _, body, _) => isFunction(body)
    case _ => false
  }

  // ---- 式 ----

  def literalType(value: Literal): Type = value match {
    case Literal.Int(_, typeName) => Type.Primitive(typeName)
    case Literal.Float(_, typeName) => Type.Primitive(typeName)
    case Literal.Bool(_) => Type.bool
    case Literal.Char(_) => Type.Primitive("char")
    case Literal.Str(_) => Type.string
    case Literal.Unit => Type.unit
  }

  def inferAs(env: Env, e: R.Expr, expected: Type): Typed = {
    val typed = infer(env, e)
    expectType(expected, typed.tpe, e.span)
    typed
  }

  private def variable(env: Env, symbol: Symbol, span: Span): Typed =
    Typed.Variable(symbol, env.values.getOrElse(symbol, fail(CompileError.DependsOnFailedDeclaration(span))))

  private def valueArgument(env: Env, e: R.Expr): SpineArgument = SpineArgument.OfValue(infer(env, e), e.span)

  private def typeArgument(env: Env, tpe: Type, span: Span): SpineArgument = SpineArgument.OfType(tpe, env.kindOf(tpe), span)

  /** function(arguments…) という適用 */
  private def call(env: Env, function: Symbol, arguments: List[SpineArgument], span: Span, headSpan: Span = Span.none): Typed =
    Application(env, variable(env, function, span), arguments, span, headSpan)

  def infer(env: Env, e: R.Expr): Typed = e match {
    // 名前だけの参照も、引数のない適用（型が [C] → T なら、C を解決する）
    case R.Expr.Variable(symbol, span) => Application(env, variable(env, symbol, span), Nil, span)

    case R.Expr.Constant(value, _) => Typed.Constant(value, literalType(value))

    case R.Expr.Lambda(parameter, written, body, _) =>
      val parameterType = convertStar(env, written)
      val typedBody = infer(env.withValue(parameter, parameterType), body)
      Typed.Lambda(parameter, parameterType, typedBody, Type.Arrow(parameterType, typedBody.tpe))

    case R.Expr.TypeLambda(parameter, kind, body, _) =>
      val typedBody = infer(env.withTypeVariable(parameter, kind), body)
      Typed.TypeLambda(parameter, kind, typedBody, Type.forall(parameter, kind, typedBody.tpe))

    case R.Expr.GivenLambda(written, body, _) =>
      val constraint = Types.constraint(env, written)
      val dictionary = new Symbol("given")
      val typedBody = infer(env.withGiven(Evidence(constraint, Typed.Variable(dictionary, constraint))), body)
      Typed.GivenLambda(dictionary, constraint, typedBody, Type.Given(constraint, typedBody.tpe))

    case R.Expr.Apply(head, arguments, span) =>
      if (arguments.exists(_.isInstanceOf[R.Argument.Named])) namedConstruction(env, head, arguments, span)
      else {
        val passed = arguments.map {
          case R.Argument.OfType(written) =>
            val (tpe, kind) = convert(env, written)
            SpineArgument.OfType(tpe, kind, written.span)
          case R.Argument.OfValue(value) => valueArgument(env, value)
          case R.Argument.OfDictionary(value) => SpineArgument.OfDictionary(infer(env, value), value.span)
          case R.Argument.Named(_, namedSpan) => fail(CompileError.NamedArgumentsInvalid("named arguments must come last", namedSpan))
        }
        head match {
          case R.Expr.Select(receiver, name, candidates, selectSpan) => select(env, receiver, name, candidates, passed, selectSpan, span)
          // 名前の頭は、制約を解決せずに残す（後ろの引数が型引数や辞書を渡すので）
          case R.Expr.Variable(symbol, variableSpan) => Application(env, variable(env, symbol, variableSpan), passed, span, variableSpan)
          case _ => Application(env, infer(env, head), passed, span, head.span)
        }
      }

    case R.Expr.Select(receiver, name, candidates, span) => select(env, receiver, name, candidates, Nil, span, span)

    case R.Expr.Binary(operator, function, left, right, span) =>
      val typedLeft = infer(env, left)
      val arguments = List(SpineArgument.OfValue(typedLeft, left.span), valueArgument(env, right))
      val applied = relating(left.span, Related.LeftOperand(typedLeft.tpe))(call(env, function, arguments, span))
      Typed.Sugar(SugarForm.Operator(operator), applied, applied.tpe)

    case R.Expr.Unary(operator, function, operand, span) =>
      val applied = call(env, function, List(valueArgument(env, operand)), span)
      Typed.Sugar(SugarForm.Operator(operator), applied, applied.tpe)

    case R.Expr.Logic(isAnd, left, right, _) =>
      Typed.Logic(isAnd, inferAs(env, left, Type.bool), inferAs(env, right, Type.bool), Type.bool)

    case R.Expr.If(condition, thenBranch, elseBranch, _) =>
      val typedCondition = inferAs(env, condition, Type.bool)
      val typedThen = infer(env, thenBranch)
      val typedElse = relating(thenBranch.span, Related.ThenBranch(typedThen.tpe))(inferAs(env, elseBranch, typedThen.tpe))
      Typed.If(typedCondition, typedThen, typedElse, typedThen.tpe)

    case R.Expr.Let(parsed, body, _) =>
      val declared = signature(env, parsed)
      val checked = binding(env, parsed, declared)
      val typedBody = infer(env.withValue(parsed.symbol, declared), body)
      Typed.Let(checked, typedBody, typedBody.tpe)

    case R.Expr.LetRec(group, body, _) =>
      val (inner, checked) = recursiveBindings(env, group)
      val typedBody = infer(inner, body)
      Typed.LetRec(checked, typedBody, typedBody.tpe)

    case R.Expr.LetPattern(bound, written, value, body, _) =>
      val matched = bindPattern(env, bound, written, value, inner => infer(inner, body))
      Typed.Sugar(SugarForm.LetPattern, matched, matched.tpe)

    case R.Expr.LocalData(group, body, span) =>
      val definitions = Declarations.dataTypes(env, group)
      val typedBody = infer(env.withDataTypes(definitions), body)
      if (mentionsData(typedBody.tpe, definitions.map(_.symbol).toSet)) fail(CompileError.LocalTypeEscapes(typedBody.tpe, span))
      Typed.LocalData(definitions, typedBody, typedBody.tpe)

    case R.Expr.LocalAlias(alias, body, _) => infer(Declarations.withAlias(env, alias), body)

    case R.Expr.Match(scrutinee, cases, span) =>
      val typedScrutinee = infer(env, scrutinee)
      matchExpression(env, typedScrutinee, cases.map(checkCase(env, _, typedScrutinee.tpe)), span)

    case R.Expr.Fold(scrutinee, resultType, cases, span) => fold(env, infer(env, scrutinee), convertStar(env, resultType), cases, span)

    case R.Expr.Block(discarded, result, _) =>
      val typedResult = result.map(infer(env, _))
      Typed.Block(discarded.map(infer(env, _)), typedResult, typedResult.fold(Type.unit)(_.tpe))

    case R.Expr.Tuple(elements, function, span) =>
      val applied = call(env, function, elements.map(valueArgument(env, _)), span)
      Typed.Sugar(SugarForm.Tuple, applied, applied.tpe)

    case R.Expr.ListLiteral(elements, nil, cons, span) => listLiteral(env, elements, nil, cons, span)

    case R.Expr.Interpolation(parts, show, _) =>
      Typed.Interpolation(parts.map {
        case Left(text) => Typed.Constant(Literal.Str(text), Type.string)
        case Right(embedded) =>
          val shown = call(env, show, List(valueArgument(env, embedded)), embedded.span)
          expectType(Type.string, shown.tpe, embedded.span)
          shown
      }, Type.string)

    case R.Expr.Context(monad, steps, result, flatMap, pure, span) => context(env, monad, steps, result, flatMap, pure, span)

    case R.Expr.Summon(written, span) => Instances.resolve(env, Types.constraint(env, written), span)

    case R.Expr.Using(dictionary, body, _) =>
      val typedDictionary = infer(env, dictionary)
      if (Types.constraintHead(env, typedDictionary.tpe).isEmpty) fail(CompileError.UsingNotDictionary(typedDictionary.tpe, dictionary.span))
      val symbol = new Symbol("using")
      val typedBody = infer(env.withGiven(Evidence(typedDictionary.tpe, Typed.Variable(symbol, typedDictionary.tpe))), body)
      Typed.Sugar(SugarForm.Using, Typed.Let(Typed.Binding(symbol, typedDictionary.tpe, typedDictionary), typedBody, typedBody.tpe), typedBody.tpe)

    case R.Expr.Instance(impl, _) => Declarations.instanceExpression(env, impl)

    case R.Expr.Primitive(intrinsic, arguments, span) =>
      if (arguments.length != intrinsic.op.arity || !intrinsicAllowed(intrinsic)) fail(CompileError.IntrinsicArguments(intrinsic, span))
      val operand = Type.Primitive(intrinsic.typeName)
      val result = intrinsic.op match {
        case PrimOp.Eq | PrimOp.Neq | PrimOp.Lt | PrimOp.Leq | PrimOp.Gt | PrimOp.Geq => Type.bool
        case _ => operand
      }
      Typed.Primitive(intrinsic, arguments.map(inferAs(env, _, operand)), result)

    case R.Expr.Foreign(written, name, span) =>
      convertStar(env, written) match {
        case tpe @ Type.Arrow(_, _) => Typed.Foreign(name, tpe)
        case other => fail(CompileError.ForeignNotFunction(other, span))
      }
  }

  private def intrinsicAllowed(intrinsic: Intrinsic): Boolean = {
    val name = intrinsic.typeName
    intrinsic.op match {
      case PrimOp.Add | PrimOp.Sub | PrimOp.Mul | PrimOp.Div | PrimOp.Neg => Primitives.numbers(name)
      case PrimOp.Mod => Primitives.integers(name)
      case PrimOp.Eq | PrimOp.Neq => Primitives.numbers(name) || name == "char" || name == "bool"
      case PrimOp.Lt | PrimOp.Leq | PrimOp.Gt | PrimOp.Geq => Primitives.numbers(name)
      case PrimOp.And | PrimOp.Or | PrimOp.Xor => Primitives.integers(name) || name == "bool"
      case PrimOp.Not => name == "bool"
    }
  }

  private def mentionsData(tpe: Type, symbols: Set[Symbol]): Boolean = tpe match {
    case Type.Data(symbol) => symbols(symbol)
    case Type.Arrow(from, to) => mentionsData(from, symbols) || mentionsData(to, symbols)
    case Type.Given(constraint, to) => mentionsData(constraint, symbols) || mentionsData(to, symbols)
    case Type.Forall(_, _, body) => mentionsData(body, symbols)
    case Type.Lambda(_, _, body) => mentionsData(body, symbols)
    case Type.Apply(function, argument) => mentionsData(function, symbols) || mentionsData(argument, symbols)
    case Type.Var(_) | Type.Bound(_) | Type.Primitive(_) => false
  }

  // ---- 糖衣 ----

  /** x.name(arguments…): x の型の module の関数か、trait のメソッド name に、x を最初の引数として渡す */
  private def select(env: Env, receiver: R.Expr, name: String, candidates: R.Candidates, arguments: List[SpineArgument], selectSpan: Span, span: Span): Typed = {
    val typedReceiver = infer(env, receiver)
    val inModule: Option[R.TypeModule] = TypeHead.of(typedReceiver.tpe).flatMap {
      case TypeHead.Named(symbol) => candidates.byType.get(symbol)
      case TypeHead.Primitive(primitive) => candidates.byPrimitive.get(primitive)
      case TypeHead.Function => None
    }
    val function = (inModule.flatMap(_.members.get(name)), candidates.method) match {
      case (Some(member), Some(_)) => fail(CompileError.SelectAmbiguous(name, typedReceiver.tpe, member.name, selectSpan))
      case (Some(member), None) => member
      case (None, Some(method)) => method
      case (None, None) =>
        fail(CompileError.SelectUnknown(name, typedReceiver.tpe, inModule.map(_.name), inModule.fold(Nil)(_.members.keys.toList.sorted), selectSpan))
    }
    val applied =
      try call(env, function, SpineArgument.OfValue(typedReceiver, receiver.span) :: arguments, span, selectSpan)
      catch {
        // 受け手が関数の最初の引数に合わないなら、ドット記法では呼べない関数
        case failure: CompileFailure => throw failure.map(error => error.core match {
          case CompileError.TypeMismatch(_, _, at) if at == receiver.span =>
            CompileError.SelectNotApplicable(function.name, env.values(function), typedReceiver.tpe, selectSpan)
          case _ => error
        })
      }
    Typed.Sugar(SugarForm.Select(name), applied, applied.tpe)
  }

  /** [x, y]: cons(x)(cons(y)(nil))。型引数は最初の要素から読み、nil にも同じ型引数を渡す */
  private def listLiteral(env: Env, elements: List[R.Expr], nil: Symbol, cons: Symbol, span: Span): Typed = {
    val values = elements.map(valueArgument(env, _))
    val typeArguments = call(env, cons, List(values.head), span) match {
      case Typed.Apply(_, passed, _) => passed.collect { case Typed.Argument.OfType(tpe) => typeArgument(env, tpe, span) }
      case _ => Nil
    }
    val empty = call(env, nil, typeArguments, span)
    val first = elements.head
    val list = relating(first.span, Related.FirstElement(infer(env, first).tpe)) {
      values.foldRight(empty)((value, rest) => call(env, cons, typeArguments ++ List(value, SpineArgument.OfValue(rest, span)), span))
    }
    Typed.Sugar(SugarForm.ListLiteral, list, list.tpe)
  }

  /** C[T](x = e, …): すべてのフィールドに名前がある構成子の、名前での構築。値は書いた順に評価する */
  private def namedConstruction(env: Env, head: R.Expr, arguments: List[R.Argument], span: Span): Typed = {
    def invalid(reason: String, at: Span): Nothing = fail(CompileError.NamedArgumentsInvalid(reason, at))
    val (symbol, headSpan) = head match {
      case R.Expr.Variable(symbol, headSpan) => (symbol, headSpan)
      case _ => invalid("only a constructor can be given named arguments", head.span)
    }
    val (written, namedSpan) = arguments.last match {
      case R.Argument.Named(fields, namedSpan) => (fields, namedSpan)
      case _ => invalid("named arguments must come last", span)
    }
    val typeArguments = arguments.init.map {
      case R.Argument.OfType(tpe) =>
        val (converted, kind) = convert(env, tpe)
        SpineArgument.OfType(converted, kind, tpe.span)
      case _ => invalid("only type arguments can come before named arguments", namedSpan)
    }
    val names = env.constructors.get(symbol).map(_.fields.map(_.name)).filter(_.forall(_.nonEmpty)).map(_.flatten)
      .getOrElse(invalid(s"`${symbol.name}` is not a constructor whose fields are all named", headSpan))
    written.groupBy(_._1).values.find(_.length > 1).foreach(twice => invalid(s"the field `${twice.head._1}` is given more than once", twice.last._2))
    written.find(field => !names.contains(field._1)).foreach(unknown => fail(CompileError.UnknownField(symbol.name, unknown._1, names, unknown._2)))
    val missing = names.filterNot(name => written.exists(_._1 == name))
    if (missing.nonEmpty) fail(CompileError.MissingFields(symbol.name, missing, headSpan))

    val bound = written.map { case (name, _, value) => (name, new Symbol(name), infer(env, value), value.span) }
    val values = names.map { name =>
      val (_, temporary, value, at) = bound.find(_._1 == name).get
      SpineArgument.OfValue(Typed.Variable(temporary, value.tpe), at)
    }
    val applied = call(env, symbol, typeArguments ++ values, span)
    val meaning = bound.foldRight(applied) { case ((_, temporary, value, _), rest) =>
      Typed.Let(Typed.Binding(temporary, value.tpe, value), rest, rest.tpe)
    }
    Typed.Sugar(SugarForm.NamedArguments, meaning, meaning.tpe)
  }

  /** context[M] { x: T = e; …; r }: flatMap[M](e)(λx: T. …pure[M](r)) */
  private def context(env: Env, monad: R.TypeExpr, steps: List[R.ContextStep], result: R.Expr, flatMap: Symbol, pure: Symbol, span: Span): Typed = {
    val (monadType, monadKind) = convert(env, monad)
    val monadArgument = SpineArgument.OfType(monadType, monadKind, monad.span)

    def rest(env: Env, remaining: List[R.ContextStep]): Typed = remaining match {
      case Nil => call(env, pure, List(monadArgument, valueArgument(env, result)), result.span)

      case R.ContextStep.Bind(bound, written, value, stepSpan) :: later =>
        val computation = valueArgument(env, value)
        val parameter = new Symbol("bound")
        val parameterType = convertStar(env, written)
        val body = bindOccurrence(env, bound, Typed.Variable(parameter, parameterType), inner => rest(inner, later))
        val continuation = Typed.Lambda(parameter, parameterType, body, Type.Arrow(parameterType, body.tpe))
        call(env, flatMap, List(monadArgument, computation, SpineArgument.OfValue(continuation, stepSpan)), stepSpan)

      case R.ContextStep.Let(parsed) :: later =>
        val declared = signature(env, parsed)
        val checked = binding(env, parsed, declared)
        val body = rest(env.withValue(parsed.symbol, declared), later)
        Typed.Let(checked, body, body.tpe)

      case R.ContextStep.LetPattern(bound, written, value, _) :: later => bindPattern(env, bound, written, value, inner => rest(inner, later))

      case R.ContextStep.Rec(group) :: later =>
        val (inner, checked) = recursiveBindings(env, group)
        val body = rest(inner, later)
        Typed.LetRec(checked, body, body.tpe)
    }

    val meaning = rest(env, steps)
    Typed.Sugar(SugarForm.Context, meaning, meaning.tpe)
  }

  /** fold e as R { C(x)(rest) => … }: 再帰的なデータ型を畳み込む。自分自身の型のフィールドには、畳み込んだ結果を束縛する。
   *  意味は rec let go(v: D): R = match v { C(x)(field) => let rest: R = go(field) in … } in go(e) */
  private def fold(env: Env, scrutinee: Typed, resultType: Type, cases: List[R.Case], span: Span): Typed = {
    val self = scrutinee.tpe
    val (data, typeArguments) = Type.dataApplication(self).getOrElse(fail(CompileError.NotADataType(self, span)))
    val goSymbol = new Symbol("fold")
    val go = Typed.Variable(goSymbol, Type.Arrow(self, resultType))
    val checked = cases.map { c =>
      val (constructor, binders) = c.pattern match {
        case R.Pattern.Constructor(symbol, arguments, patternSpan) if c.guard.isEmpty =>
          val definition = env.constructors.getOrElse(symbol, fail(CompileError.DependsOnFailedDeclaration(patternSpan)))
          if (!(definition.data eq data)) fail(CompileError.PatternConstructorMismatch(symbol.name, self, patternSpan))
          if (arguments.length != definition.arity) fail(CompileError.ConstructorArity(symbol.name, definition.arity, arguments.length, patternSpan))
          (definition, arguments.map {
            case R.Pattern.Bind(binder, _) => Some(binder)
            case R.Pattern.Wildcard(_) => None
            case other => fail(CompileError.FoldPattern(other.span))
          })
        case other => fail(CompileError.FoldPattern(other.span))
      }
      // 自分自身の型のフィールドは、いったん別の変数で受けて、畳み込んだ結果を束縛子に束縛する
      val fields = constructor.fieldTypes(typeArguments).zip(binders).map { case (fieldType, binder) =>
        (fieldType, binder, if (fieldType == self) Some(new Symbol("field")) else None)
      }
      val visible = fields.flatMap { case (fieldType, binder, folded) => binder.map(_ -> (if (folded.nonEmpty) resultType else fieldType)) }
      val body = inferAs(env.withValues(visible), c.body, resultType)
      val wrapped = fields.foldRight(body) {
        case ((_, Some(binder), Some(field)), inner) =>
          val folded = Typed.Apply(go, List(Typed.Argument.OfValue(Typed.Variable(field, self))), resultType)
          Typed.Let(Typed.Binding(binder, resultType, folded), inner, inner.tpe)
        case (_, inner) => inner
      }
      val arguments = fields.map { case (_, binder, folded) => CheckedPattern.Any(folded.orElse(binder)) }
      val bound = fields.flatMap { case (fieldType, binder, folded) => folded.map(_ -> self).orElse(binder.map(_ -> fieldType)) }
      CheckedCase(CheckedPattern.Constructor(constructor, arguments), bound, None, wrapped, c.pattern.span, c.body.span)
    }
    val parameter = new Symbol("value")
    val body = matchExpression(env, Typed.Variable(parameter, self), checked, span)
    val function = Typed.Lambda(parameter, self, body, go.tpe)
    val applied = Typed.Apply(go, List(Typed.Argument.OfValue(scrutinee)), resultType)
    Typed.Sugar(SugarForm.Fold, Typed.LetRec(List(Typed.Binding(goSymbol, go.tpe, function)), applied, resultType), resultType)
  }

  // ---- match ----

  /** 検査済みの分岐 */
  final case class CheckedCase(pattern: CheckedPattern, bindings: List[(Symbol, Type)], guard: Option[Typed], body: Typed, patternSpan: Span, bodySpan: Span)

  private def checkCase(env: Env, c: R.Case, scrutineeType: Type): CheckedCase = {
    val (checked, bindings) = pattern(env, c.pattern, scrutineeType)
    val inner = env.withValues(bindings)
    CheckedCase(checked, bindings, c.guard.map(inferAs(inner, _, Type.bool)), infer(inner, c.body), c.pattern.span, c.body.span)
  }

  private def matchExpression(env: Env, scrutinee: Typed, cases: List[CheckedCase], span: Span): Typed = {
    if (cases.isEmpty) fail(CompileError.NonExhaustiveMatch("_", guarded = false, span))
    val resultType = cases.head.body.tpe
    cases.tail.find(_.body.tpe != resultType).foreach { other =>
      fail(CompileError.TypeMismatch(resultType, other.body.tpe, other.bodySpan).withRelated(cases.head.bodySpan, Related.FirstCase(resultType)))
    }

    val root = Occurrence(new Symbol("scrutinee"), scrutinee.tpe)
    val decision = Patterns.compile(env, root, cases.map(c => (c.pattern, c.guard.nonEmpty)))
    Patterns.unmatched(decision, root).foreach(example => fail(CompileError.NonExhaustiveMatch(example, cases.exists(_.guard.nonEmpty), span)))
    val reached = Patterns.reached(decision)
    cases.indices.find(!reached.contains(_)).foreach(index => fail(CompileError.UnreachableCase(index + 1, cases(index).patternSpan)))
    Typed.Match(scrutinee, root, decision, cases.map(c => Typed.Case(c.bindings, c.guard, c.body)), resultType)
  }

  /** let パターン: 型 = 値 in 本体。分岐が 1 つの match で、パターンはすべての値に当たらなければならない */
  private def bindPattern(env: Env, bound: R.Pattern, written: R.TypeExpr, value: R.Expr, body: Env => Typed): Typed =
    bindOccurrence(env, bound, inferAs(env, value, convertStar(env, written)), body)

  private def bindOccurrence(env: Env, bound: R.Pattern, value: Typed, body: Env => Typed): Typed = {
    val (checked, bindings) = pattern(env, bound, value.tpe)
    val root = Occurrence(new Symbol("scrutinee"), value.tpe)
    val decision = Patterns.compile(env, root, List((checked, false)))
    Patterns.unmatched(decision, root).foreach(example => fail(CompileError.RefutablePattern(example, bound.span)))
    val typedBody = body(env.withValues(bindings))
    Typed.Match(value, root, decision, List(Typed.Case(bindings, None, typedBody)), typedBody.tpe)
  }

  /** 型 expected の値に対するパターンと、束縛される変数 */
  def pattern(env: Env, p: R.Pattern, expected: Type): (CheckedPattern, List[(Symbol, Type)]) = p match {
    case R.Pattern.Wildcard(_) => (CheckedPattern.Any(None), Nil)
    case R.Pattern.Bind(symbol, _) => (CheckedPattern.Any(Some(symbol)), List(symbol -> expected))

    case R.Pattern.Constructor(symbol, arguments, span) =>
      val constructor = env.constructors.getOrElse(symbol, fail(CompileError.DependsOnFailedDeclaration(span)))
      val (data, typeArguments) = Type.dataApplication(expected).getOrElse(fail(CompileError.NotADataType(expected, span)))
      if (!(data eq constructor.data)) fail(CompileError.PatternConstructorMismatch(symbol.name, expected, span))
      if (arguments.length != constructor.arity) fail(CompileError.ConstructorArity(symbol.name, constructor.arity, arguments.length, span))
      val checked = arguments.zip(constructor.fieldTypes(typeArguments)).map { case (argument, fieldType) => pattern(env, argument, fieldType) }
      (CheckedPattern.Constructor(constructor, checked.map(_._1)), checked.flatMap(_._2))

    case R.Pattern.Constant(value, equals, span) =>
      expectType(expected, literalType(value), span)
      val checked = value match {
        case Literal.Bool(bool) => CheckedPattern.Bool(bool)
        case Literal.Unit => CheckedPattern.Any(None)
        // == に宣言された関数に、リテラルだけを渡したもの（値を受け取って、等しいかを返す）
        case _ =>
          val function = equals.getOrElse(fail(CompileError.UndefinedOperator("==", isPrefix = false, span)))
          val test = call(env, function, List(SpineArgument.OfValue(Typed.Constant(value, expected), span)), span)
          expectType(Type.Arrow(expected, Type.bool), test.tpe, span)
          CheckedPattern.Constant(value, test)
      }
      (checked, Nil)

    case R.Pattern.Or(alternatives, _) =>
      val checked = alternatives.map(alternative => alternative -> pattern(env, alternative, expected))
      val (_, (_, bindings)) = checked.head
      checked.tail.foreach { case (alternative, (_, others)) =>
        bindings.foreach { case (symbol, tpe) => others.find(_._1 eq symbol).foreach(other => expectType(tpe, other._2, alternative.span)) }
      }
      (CheckedPattern.Or(checked.map(_._2._1)), bindings)
  }
}
