package com.yuuki14202028

import scala.collection.mutable.ListBuffer

/** 再帰下降の構文解析。
 *
 * 改行の扱い: 式は改行で終わる。ただし、続きがあるとわかる所（二項演算子や `=` の後、括弧の中、
 * `in` / `then` / `else` や `.名前` の前など）では改行を読み飛ばす。
 */
final class Parser(tokens: Vector[Token]) {
  import TokenKind.*
  import Surface.*

  private var position = 0

  private val reservedMarks = Set("=", "=>", "->", ":")

  // ---- トークンの読み方 ----

  private def peek: Token = tokens(position)

  private def peekAt(offset: Int): Token = tokens((position + offset).min(tokens.length - 1))

  private def advance(): Token = {
    val token = peek
    if (token.kind != End) position += 1
    token
  }

  private def skipNewlines(): Unit = while (peek.kind == Newline) position += 1

  private def positionPastNewlines: Int = tokens.indexWhere(_.kind != Newline, position)

  private def peekPastNewlines: Token = tokens(positionPastNewlines)

  private def describe(token: Token): String = token.kind match {
    case Identifier(name) => s"`$name`"
    case Keyword(word) => s"`$word`"
    case IntLiteral(text, _) => s"`$text`"
    case FloatLiteral(text, _) => s"`$text`"
    case StringLiteral(_) | Interpolation(_) => "a string"
    case CharLiteral(_) => "a character"
    case Mark(text) => s"`$text`"
    case Newline => "the end of the line"
    case End => "the end of the file"
  }

  private def fail(expected: String): Nothing =
    throw CompileFailure(CompileError.Syntax(expected, describe(peek), peek.span))

  private def isMark(text: String): Boolean = peek.kind == Mark(text)
  private def isKeyword(word: String): Boolean = peek.kind == Keyword(word)

  private def expectMark(text: String): Token = if (isMark(text)) advance() else fail(s"`$text`")
  private def expectKeyword(word: String): Token = if (isKeyword(word)) advance() else fail(s"`$word`")

  /** 閉じ括弧や `in` のように、前に改行があってもよいもの */
  private def expectMarkAfterNewlines(text: String): Token = {
    if (peekPastNewlines.kind == Mark(text)) skipNewlines()
    expectMark(text)
  }

  private def expectKeywordAfterNewlines(word: String): Token = {
    if (peekPastNewlines.kind == Keyword(word)) skipNewlines()
    expectKeyword(word)
  }

  private def identifier(): (String, Span) = peek.kind match {
    case Identifier(name) => (name, advance().span)
    case _ => fail("a name")
  }

  private def isOperator: Boolean = peek.kind match {
    case Mark(text) => text.forall(Lexer.operatorCharacters.contains(_)) && !reservedMarks(text)
    case _ => false
  }

  /** 直前に空白のない `(` や `[`（適用の引数の始まり） */
  private def isAdjacent(text: String): Boolean = isMark(text) && !peek.spaceBefore

  /** 途中まで読んで失敗したら、読む前の位置に戻る */
  private def attempt[A](parse: => A): Either[CompileFailure, A] = {
    val start = position
    try Right(parse)
    catch {
      case failure: CompileFailure =>
        position = start
        Left(failure)
    }
  }

  /** open と close の間の、separator で区切った並び（中では改行できる） */
  private def separated[A](close: String, item: () => A): List[A] = {
    val items = ListBuffer.empty[A]
    skipNewlines()
    while (!isMark(close)) {
      items += item()
      skipNewlines()
      if (!isMark(close)) { expectMark(","); skipNewlines() }
    }
    items.toList
  }

  // ---- 宣言 ----

  def program(): List[Decl] = {
    skipNewlines()
    val declarations = ListBuffer.empty[Decl]
    while (peek.kind != End) {
      declarations += declaration()
      endOfDeclaration(closing = false)
    }
    declarations.toList
  }

  private def endOfDeclaration(closing: Boolean): Unit = peek.kind match {
    case Newline => skipNewlines()
    case Mark(";") => advance(); skipNewlines()
    case End => ()
    case Mark("}") if closing => ()
    case _ => fail("the end of the declaration")
  }

  /** { 宣言… } */
  private def declarationBlock[A](member: () => A): (List[A], Span) = {
    expectMark("{")
    skipNewlines()
    val members = ListBuffer.empty[A]
    while (!isMark("}")) {
      members += member()
      endOfDeclaration(closing = true)
    }
    (members.toList, advance().span)
  }

  private def declaration(): Decl = peek.kind match {
    case Keyword("let") =>
      val parsed = binding("let")
      Decl.Let(parsed, parsed.span)
    case Keyword("rec") =>
      val start = advance().span
      val members = recursiveGroup(() => declaration())
      Decl.Rec(members, start.to(members.last.span))
    case Keyword("data") =>
      val parsed = data()
      Decl.DataType(parsed, parsed.span)
    case Keyword("type") => alias()
    // prefix は宣言の先頭でだけキーワード（名前としても使える）
    case Keyword("infixl" | "infixr" | "infix") | Identifier("prefix") => operatorDeclaration()
    case Keyword("interpolate") =>
      val start = advance().span
      expectMark("=")
      val function = path()
      Decl.Interpolate(function, start.to(function.span))
    case Keyword("literal") => literalDeclaration()
    case Keyword("trait") => traitDeclaration()
    case Keyword("impl") => implDeclaration()
    case Keyword("derive") =>
      val start = advance().span
      val traitName = path()
      expectMark("[")
      val target = path()
      val end = expectMark("]").span
      Decl.Derive(traitName, target, start.to(end))
    case Keyword("module") =>
      val start = advance().span
      val (name, _) = identifier()
      val (members, end) = declarationBlock(() => declaration())
      Decl.Module(name, members, start.to(end))
    case Keyword("open") =>
      val start = advance().span
      val opened = path()
      Decl.Open(opened, start.to(opened.span))
    case Keyword("private") =>
      val start = advance().span
      val inner = declaration()
      Decl.Private(inner, start.to(inner.span))
    case Keyword("opaque") =>
      val start = advance().span
      val inner = declaration()
      Decl.Opaque(inner, start.to(inner.span))
    case Keyword("import") =>
      val start = advance().span
      peek.kind match {
        case StringLiteral(value) => Decl.Import(value, start.to(advance().span))
        case _ => fail("the path of a file")
      }
    case _ => fail("a declaration")
  }

  /** rec の後: 宣言 1 つか、{ } で囲んだ宣言の組 */
  private def recursiveGroup[A](member: () => A): List[A] =
    if (isMark("{")) {
      val (members, _) = declarationBlock(member)
      if (members.isEmpty) fail("a declaration")
      members
    } else List(member())

  /** 名前[型引数](引数): 型 with 制約 = 値。型引数・制約・引数は、型の ∀ / [C] → / → と値の Λ / λ に展開する */
  private final case class Signature(name: String, nameSpan: Span, typeParameters: List[(String, Kind)], parameters: List[(String, TypeExpr)], result: Option[TypeExpr], constraints: List[TypeExpr]) {
    def tpe: Option[TypeExpr] = result.map { resultType =>
      val function = parameters.foldRight(resultType) { case ((_, from), to) => TypeExpr.Arrow(from, to, from.span.to(to.span)) }
      val constrained = constraints.foldRight(function)((constraint, to) => TypeExpr.Given(constraint, to, to.span))
      typeParameters.foldRight(constrained) { case ((parameter, kind), inner) => TypeExpr.Forall(parameter, kind, inner, inner.span) }
    }

    def value(body: Expr): Expr = {
      val function = parameters.foldRight(body) { case ((parameter, tpe), inner) => Expr.Lambda(parameter, tpe, inner, inner.span) }
      val constrained = constraints.foldRight(function)((constraint, inner) => Expr.GivenLambda(constraint, inner, inner.span))
      typeParameters.foldRight(constrained) { case ((parameter, kind), inner) => Expr.TypeLambda(parameter, kind, inner, inner.span) }
    }
  }

  /** resultOptional: 結果の型を省略できる（impl のメソッド。型は trait のシグネチャから決まる） */
  private def signature(resultOptional: Boolean = false): Signature = {
    val (name, nameSpan) = identifier()
    val typeParameters = this.typeParameters()
    val parameters = ListBuffer.empty[(String, TypeExpr)]
    while (isMark("(")) {
      val open = advance().span
      if (isMark(")")) parameters += (("_", TypeExpr.Name(Path(List("unit"), open), open.to(advance().span))))
      else {
        val (parameter, _) = identifier()
        expectMark(":")
        parameters += ((parameter, typeExpr()))
        expectMark(")")
      }
    }
    val result = if (resultOptional && !isMark(":")) None else { expectMark(":"); Some(typeExpr()) }
    Signature(name, nameSpan, typeParameters, parameters.toList, result, withClause())
  }

  /** with C[a], D[b]（なければ空） */
  private def withClause(): List[TypeExpr] =
    if (isKeyword("with")) {
      advance()
      val constraints = ListBuffer(typeExpr())
      while (isMark(",")) { advance(); skipNewlines(); constraints += typeExpr() }
      constraints.toList
    } else Nil

  private def binding(keyword: String): Binding = {
    val start = expectKeyword(keyword).span
    val parsed = signature()
    expectMark("=")
    skipNewlines()
    val body = expr()
    Binding(parsed.name, parsed.tpe.get, parsed.value(body), start.to(body.span))
  }

  /** [a] や [f: * → *] の並び */
  private def typeParameters(): List[(String, Kind)] = {
    val parameters = ListBuffer.empty[(String, Kind)]
    while (isMark("[")) {
      advance()
      val (name, _) = identifier()
      val parameterKind = if (isMark(":")) { advance(); kind() } else Kind.Star
      expectMark("]")
      parameters += ((name, parameterKind))
    }
    parameters.toList
  }

  private def kind(): Kind = {
    val left =
      if (isMark("*")) { advance(); Kind.Star }
      else if (isMark("(")) { advance(); val inner = kind(); expectMark(")"); inner }
      else fail("a kind")
    if (isMark("→")) { advance(); Kind.Arrow(left, kind()) } else left
  }

  private def data(): Data = {
    val start = expectKeyword("data").span
    val (name, _) = identifier()
    val parameters = typeParameters()
    expectMark("=")
    skipNewlines()
    expectMark("{")
    val constructors = ListBuffer.empty[Constructor]
    skipNewlines()
    while (!isMark("}")) {
      val (constructorName, nameSpan) = identifier()
      val fields = ListBuffer.empty[Field]
      var end = nameSpan
      while (isAdjacent("(")) {
        advance()
        val fieldName = (peek.kind, peekAt(1).kind) match {
          case (Identifier(field), Mark(":")) => advance(); advance(); Some(field)
          case _ => None
        }
        fields += Field(fieldName, typeExpr())
        end = expectMark(")").span
      }
      constructors += Constructor(constructorName, fields.toList, nameSpan.to(end))
      skipNewlines()
      if (isMark(",")) { advance(); skipNewlines() }
    }
    val end = advance().span
    Data(name, parameters, constructors.toList, start.to(end))
  }

  private def alias(): Decl = {
    val start = expectKeyword("type").span
    val (name, _) = identifier()
    val parameters = typeParameters()
    expectMark("=")
    skipNewlines()
    val body = typeExpr()
    Decl.Alias(name, parameters, body, start.to(body.span))
  }

  /** infixl 6 + = add / prefix - = neg */
  private def operatorDeclaration(): Decl = {
    val keyword = advance()
    val fixity = keyword.kind match {
      case Identifier("prefix") => None
      case Keyword(word) =>
        val assoc = word match {
          case "infixl" => Assoc.Left
          case "infixr" => Assoc.Right
          case _ => Assoc.None
        }
        peek.kind match {
          case IntLiteral(text, _) if text.length == 1 => advance(); Some(Fixity(assoc, text.toInt))
          case _ => fail("a precedence from 0 to 9")
        }
      case _ => fail("an operator declaration")
    }
    if (!isOperator) fail("an operator symbol")
    val symbol = markText(advance())
    expectMark("=")
    val function = path()
    Decl.Operator(symbol, fixity, function, keyword.span.to(function.span))
  }

  /** literal [] = Nil, Cons / literal setOf = Set.empty, Set.cons / literal (,) = Pair */
  private def literalDeclaration(): Decl = {
    val start = expectKeyword("literal").span
    val kind = peek.kind match {
      case Mark("[") => advance(); expectMark("]"); LiteralKind.Brackets
      case Mark("(") =>
        advance()
        var size = 1
        while (isMark(",")) { advance(); size += 1 }
        expectMark(")")
        if (size < 2) fail("`,`")
        LiteralKind.Tuple(size)
      case Identifier(name) => advance(); LiteralKind.Named(name)
      case _ => fail("`[]`, `(,)` or a name")
    }
    expectMark("=")
    val functions = ListBuffer(path())
    if (!kind.isInstanceOf[LiteralKind.Tuple]) { expectMark(","); functions += path() }
    Decl.LiteralSyntax(kind, functions.toList, start.to(functions.last.span))
  }

  private def traitDeclaration(): Decl = {
    val start = expectKeyword("trait").span
    val (name, _) = identifier()
    val parameters = typeParameters()
    val supers = withClause()
    val (methods, end) = declarationBlock { () =>
      val defStart = expectKeyword("def").span
      val parsed = signature()
      val default = if (isMark("=")) { advance(); skipNewlines(); Some(parsed.value(expr())) } else None
      MethodSignature(parsed.name, parsed.tpe.get, default, defStart.to(default.fold(parsed.result.get.span)(_.span)))
    }
    Decl.Trait(name, parameters, supers, methods, start.to(end))
  }

  private def implDeclaration(): Decl = {
    val start = expectKeyword("impl").span
    val parameters = typeParameters()
    val traitName = path()
    val arguments = typeArguments()
    val context = withClause()
    val (methods, end) = declarationBlock(() => methodImpl())
    Decl.Impl(parameters, traitName, arguments, context, methods, start.to(end))
  }

  private def typeArguments(): List[TypeExpr] = {
    val arguments = ListBuffer.empty[TypeExpr]
    while (isAdjacent("[")) {
      advance()
      skipNewlines()
      arguments += typeExpr()
      expectMarkAfterNewlines("]")
    }
    arguments.toList
  }

  /** def m(x: T): U = e。結果の型は省略でき、def m = e とも書ける */
  private def methodImpl(): MethodImpl = {
    val start = expectKeyword("def").span
    val parsed = signature(resultOptional = true)
    expectMark("=")
    skipNewlines()
    val body = expr()
    MethodImpl(parsed.name, parsed.tpe, parsed.value(body), start.to(body.span))
  }

  /** A.B.x（間に空白は置けない）。型の名前では foreign.C.String のように foreign から始まってもよい */
  private def path(): Path = {
    val (first, firstSpan) = peek.kind match {
      case Keyword("foreign") => ("foreign", advance().span)
      case _ => identifier()
    }
    val segments = ListBuffer(first)
    var span = firstSpan
    while (isAdjacent(".") && !peekAt(1).spaceBefore && peekAt(1).kind.isInstanceOf[Identifier]) {
      advance()
      val (part, partSpan) = identifier()
      segments += part
      span = span.to(partSpan)
    }
    Path(segments.toList, span)
  }

  // ---- 型 ----

  def typeExpr(): TypeExpr = peek.kind match {
    case Mark("∀") => typeBinder(TypeExpr.Forall.apply, constraintBody = true)
    case Mark("λ") => typeBinder(TypeExpr.Lambda.apply, constraintBody = false)
    case _ =>
      val left = typeApplication()
      if (isMark("→")) {
        advance()
        skipNewlines()
        val right = typeExpr()
        TypeExpr.Arrow(left, right, left.span.to(right.span))
      } else if (isMark("⇒")) {
        advance()
        skipNewlines()
        val right = typeExpr()
        TypeExpr.Given(left, right, left.span.to(right.span))
      } else left
  }

  /** ∀a. T / ∀(f: * → *). T（λ も同じ形） */
  private def typeBinder(make: (String, Kind, TypeExpr, Span) => TypeExpr, constraintBody: Boolean): TypeExpr = {
    val start = advance().span
    val (parameter, parameterKind) = binderParameter()
    expectMark(".")
    val body = if (constraintBody) premises().getOrElse(typeExpr()) else typeExpr()
    make(parameter, parameterKind, body, start.to(body.span))
  }

  /** ∀a. P, Q ⇒ C: 量化された制約の、カンマで並べた前提（P ⇒ Q ⇒ C と同じ） */
  private def premises(): Option[TypeExpr] = attempt {
    val listed = ListBuffer(typeApplication())
    while (isMark(",")) { advance(); skipNewlines(); listed += typeApplication() }
    if (listed.length < 2) fail("`,`")
    expectMark("⇒")
    listed.toList
  }.toOption.map { listed =>
    skipNewlines()
    val conclusion = typeExpr()
    listed.foldRight(conclusion)((premise, to) => TypeExpr.Given(premise, to, premise.span.to(to.span)))
  }

  private def binderParameter(): (String, Kind) =
    if (isMark("(")) {
      advance()
      val (name, _) = identifier()
      expectMark(":")
      val parameterKind = kind()
      expectMark(")")
      (name, parameterKind)
    } else (identifier()._1, Kind.Star)

  private def typeApplication(): TypeExpr = {
    var result = typeAtom()
    while (isAdjacent("[")) {
      advance()
      skipNewlines()
      val argument = typeExpr()
      val end = expectMarkAfterNewlines("]").span
      result = TypeExpr.Apply(result, argument, result.span.to(end))
    }
    result
  }

  private def typeAtom(): TypeExpr = peek.kind match {
    case Identifier(_) | Keyword("foreign") =>
      val name = path()
      TypeExpr.Name(name, name.span)
    case Mark("(") =>
      val open = advance().span
      val elements = separated(")", () => typeExpr())
      val span = open.to(advance().span)
      elements match {
        case Nil => TypeExpr.Name(Path(List("unit"), span), span)
        case List(single) => single
        case several => TypeExpr.Tuple(several, span)
      }
    case _ => fail("a type")
  }

  // ---- 式 ----

  def expr(): Expr = peek.kind match {
    case Keyword("let") => letExpr()
    case Keyword("rec") =>
      val start = advance().span
      if (isKeyword("data") || (isMark("{") && peekPastOpenBrace.kind == Keyword("data"))) {
        val members = recursiveGroup(() => declaration())
        val body = inBody()
        Expr.Local(Decl.Rec(members, start.to(members.last.span)), body, start.to(body.span))
      } else {
        val bindings = recursiveGroup(() => binding("let"))
        val body = inBody()
        Expr.LetRec(bindings, body, start.to(body.span))
      }
    case Keyword("data" | "type") =>
      val local = declaration()
      val body = inBody()
      Expr.Local(local, body, local.span.to(body.span))
    case Keyword("if") =>
      val start = advance().span
      val condition = expr()
      expectKeywordAfterNewlines("then")
      skipNewlines()
      val thenBranch = expr()
      expectKeywordAfterNewlines("else")
      skipNewlines()
      val elseBranch = expr()
      Expr.If(condition, thenBranch, elseBranch, start.to(elseBranch.span))
    case Keyword("match") =>
      val start = advance().span
      val scrutinee = expr()
      val (parsed, end) = cases()
      Expr.Match(scrutinee, parsed, start.to(end))
    case Keyword("fold") =>
      val start = advance().span
      val scrutinee = expr()
      expectKeywordAfterNewlines("as")
      val resultType = typeExpr()
      val (parsed, end) = cases()
      Expr.Fold(scrutinee, resultType, parsed, start.to(end))
    case Keyword("using") =>
      val start = advance().span
      val dictionary = expr()
      val body = inBody()
      Expr.Using(dictionary, body, start.to(body.span))
    case Mark("λ") =>
      val start = advance().span
      val (parameter, _) = identifier()
      expectMark(":")
      val parameterType = typeExpr()
      expectMark(".")
      skipNewlines()
      val body = expr()
      Expr.Lambda(parameter, parameterType, body, start.to(body.span))
    case Mark("Λ") =>
      val start = advance().span
      val (parameter, parameterKind) = binderParameter()
      expectMark(".")
      skipNewlines()
      val body = expr()
      Expr.TypeLambda(parameter, parameterKind, body, start.to(body.span))
    case _ => operators()
  }

  private def peekPastOpenBrace: Token = tokens(tokens.indexWhere(_.kind != Newline, position + 1))

  /** let 名前… = 値 in 本体 か、let パターン: 型 = 値 in 本体 */
  private def letExpr(): Expr =
    if (letBindsPattern) {
      val start = advance().span
      val (bound, tpe, value) = patternBinding()
      val body = inBody()
      Expr.LetPattern(bound, tpe, value, body, start.to(body.span))
    } else {
      val parsed = binding("let")
      val body = inBody()
      Expr.Let(parsed, body, parsed.span.to(body.span))
    }

  /** let の後がパターンか。名前(引数: 型) や 名前() は関数の束縛、名前(パターン) は構成子のパターン */
  private def letBindsPattern: Boolean = (peekAt(1).kind, peekAt(2).kind, peekAt(3).kind, peekAt(4).kind) match {
    case (Identifier(_), Mark("("), Mark(")"), _) => false
    case (Identifier(_), Mark("("), Identifier(_), Mark(":")) => false
    case (Identifier(_), Mark("(" | "."), _, _) => true
    case (Identifier(_), _, _, _) => false
    case _ => true
  }

  /** パターン: 型 = 値 */
  private def patternBinding(): (Pattern, TypeExpr, Expr) = {
    val bound = pattern()
    expectMark(":")
    val tpe = typeExpr()
    expectMark("=")
    skipNewlines()
    (bound, tpe, expr())
  }

  private def inBody(): Expr = {
    expectKeywordAfterNewlines("in")
    skipNewlines()
    expr()
  }

  /** { パターン => 式 … } */
  private def cases(): (List[Case], Span) = {
    expectMarkAfterNewlines("{")
    skipNewlines()
    val parsed = ListBuffer.empty[Case]
    while (!isMark("}")) {
      val matched = pattern()
      val guard = if (isKeyword("if")) { advance(); Some(expr()) } else None
      expectMark("=>")
      skipNewlines()
      parsed += Case(matched, guard, expr())
      skipNewlines()
    }
    (parsed.toList, advance().span)
  }

  private def pattern(): Pattern = {
    val first = simplePattern()
    if (isMark("|")) {
      val alternatives = ListBuffer(first)
      while (isMark("|")) { advance(); skipNewlines(); alternatives += simplePattern() }
      Pattern.Or(alternatives.toList, first.span.to(alternatives.last.span))
    } else first
  }

  private def simplePattern(): Pattern = peek.kind match {
    case Identifier("_") => Pattern.Wildcard(advance().span)
    case Identifier(_) =>
      val name = path()
      val arguments = ListBuffer.empty[Pattern]
      var end = name.span
      while (isAdjacent("(")) {
        advance()
        arguments += pattern()
        end = expectMark(")").span
      }
      Pattern.Name(name, arguments.toList, name.span.to(end))
    case Mark("-") =>
      val minus = advance().span
      peek.kind match {
        case IntLiteral(text, typeName) => Pattern.Constant(Literal.Int(s"-$text", typeName), minus.to(advance().span))
        case FloatLiteral(text, typeName) => Pattern.Constant(Literal.Float(s"-$text", typeName), minus.to(advance().span))
        case _ => fail("a number")
      }
    case Mark("(") =>
      val open = advance().span
      val elements = separated(")", () => pattern())
      val span = open.to(advance().span)
      elements match {
        case Nil => Pattern.Constant(Literal.Unit, span)
        case List(single) => single
        case several => Pattern.Tuple(several, span)
      }
    case Mark("[") =>
      val open = advance().span
      val elements = separated("]", () => pattern())
      Pattern.ListLiteral(elements, open.to(advance().span))
    case _ => constant() match {
      case Some(value) => Pattern.Constant(value, advance().span)
      case None => fail("a pattern")
    }
  }

  /** 今のトークンがリテラルなら、その値 */
  private def constant(): Option[Literal] = peek.kind match {
    case IntLiteral(text, typeName) => Some(Literal.Int(text, typeName))
    case FloatLiteral(text, typeName) => Some(Literal.Float(text, typeName))
    case StringLiteral(value) => Some(Literal.Str(value))
    case CharLiteral(value) => Some(Literal.Char(value))
    case Keyword("true") => Some(Literal.Bool(true))
    case Keyword("false") => Some(Literal.Bool(false))
    case _ => None
  }

  /** a ⊕ b ⊗ c。演算子の後では改行できるが、演算子の前では改行できない（式はそこで終わる） */
  private def operators(): Expr = {
    val first = infixCalls()
    val rest = ListBuffer.empty[OperatorStep]
    while (isOperator) {
      val operator = advance()
      skipNewlines()
      rest += OperatorStep(markText(operator), operator.span, infixCalls())
    }
    if (rest.isEmpty) first else Expr.Operators(first, rest.toList, first.span.to(rest.last.operand.span))
  }

  private def markText(token: Token): String = token.kind match {
    case Mark(text) => text
    case _ => fail("an operator")
  }

  /** x f y: 2 引数の関数を引数のあいだに書く呼び出し f(x)(y)。どの演算子よりも強く結び付く */
  private def infixCalls(): Expr = {
    var left = prefixed()
    while (isInfixFunction) {
      val (name, nameSpan) = identifier()
      val right = prefixed()
      left = Expr.Apply(Expr.Variable(name, nameSpan), List(Argument.OfValue(left), Argument.OfValue(right)), left.span.to(right.span))
    }
    left
  }

  /** 空白で区切った名前の後に、空白で区切った式が続く（`C(p) => …` のような次の分岐の始まりとは読まない） */
  private def isInfixFunction: Boolean = {
    val next = peekAt(1)
    val startsOperand = next.kind match {
      case Identifier(_) | IntLiteral(_, _) | FloatLiteral(_, _) | StringLiteral(_) | CharLiteral(_) | Interpolation(_) => true
      case Keyword("true" | "false") | Mark("(" | "[" | "{") => true
      // 前置の演算子（-b）: 演算子の直後に空白がない
      case Mark(text) => text.forall(Lexer.operatorCharacters.contains(_)) && !reservedMarks(text) && !peekAt(2).spaceBefore
      case _ => false
    }
    peek.kind match {
      case Identifier(name) => name != "_" && peek.spaceBefore && next.spaceBefore && startsOperand
      case _ => false
    }
  }

  private def prefixed(): Expr =
    if (isOperator) {
      val operator = advance()
      val operand = prefixed()
      Expr.Prefix(markText(operator), operand, operator.span.to(operand.span))
    } else application()

  /** f[T](x)(y).g: 引数は、空白を置かずに続けて書く。.名前 は行を変えて書ける */
  private def application(): Expr = {
    var result = atom()
    var arguments = ListBuffer.empty[Argument]
    var end = result.span

    def flush(): Unit = if (arguments.nonEmpty) {
      result = Expr.Apply(result, arguments.toList, result.span.to(end))
      arguments = ListBuffer.empty
    }

    def continuesWithMember: Boolean = {
      val at = positionPastNewlines
      tokens(at).kind == Mark(".") && tokens.lift(at + 1).exists(next => !next.spaceBefore && next.kind.isInstanceOf[Identifier])
    }

    var continues = true
    while (continues) {
      if (isAdjacent("[")) {
        val open = advance().span
        // 名前の直後の最初の [ … ] だけは、名前付きのリテラルの要素かもしれない
        val argument = if (arguments.isEmpty && isName(result)) bracket(open) else {
          skipNewlines()
          Argument.OfType(typeExpr())
        }
        end = expectMarkAfterNewlines("]").span
        arguments += argument
      } else if (isAdjacent("(")) {
        val (argument, close) = parenthesizedArgument()
        arguments += argument
        end = close
      } else if (isAdjacent(".") && peekAt(1).kind.isInstanceOf[Identifier] && !peekAt(1).spaceBefore) {
        flush()
        advance()
        val (name, nameSpan) = identifier()
        result = Expr.Select(result, name, nameSpan, result.span.to(nameSpan))
        end = nameSpan
      } else if (peek.kind == Newline && continuesWithMember) {
        skipNewlines()
        flush()
        advance()
        val (name, nameSpan) = identifier()
        result = Expr.Select(result, name, nameSpan, result.span.to(nameSpan))
        end = nameSpan
      } else continues = false
    }
    flush()
    result
  }

  /** 名前か、名前を . でつないだもの */
  private def isName(e: Expr): Boolean = e match {
    case Expr.Variable(_, _) => true
    case Expr.Select(receiver, _, _, _) => isName(receiver)
    case _ => false
  }

  /** [ の後。型としても式の並びとしても読んでみて、読めたものを両方持つ */
  private def bracket(open: Span): Argument = {
    val start = position
    def closes: Boolean = peekPastNewlines.kind == Mark("]")
    val asType = attempt { skipNewlines(); val tpe = typeExpr(); if (!closes) fail("`]`"); tpe }
    val afterType = position
    position = start
    val asElements = attempt { val elements = separated("]", () => expr()); elements }
    (asType, asElements) match {
      case (Left(typeFailure), Left(elementsFailure)) =>
        throw (if (typeFailure.error.span.start >= elementsFailure.error.span.start) typeFailure else elementsFailure)
      case (Right(_), Left(_)) => position = afterType
      case _ => ()
    }
    Argument.Bracket(asType.toOption, asElements.toOption, open.to(peekPastNewlines.span))
  }

  /** ( ) の中の引数: 値、(with 辞書)、(名前 = 値, …) */
  private def parenthesizedArgument(): (Argument, Span) = {
    val open = expectMark("(").span
    skipNewlines()
    if (isMark(")")) {
      val close = advance().span
      (Argument.OfValue(Expr.Constant(Literal.Unit, open.to(close))), close)
    } else if (isKeyword("with")) {
      advance()
      val dictionary = expr()
      (Argument.OfDictionary(dictionary), expectMarkAfterNewlines(")").span)
    } else if (peek.kind.isInstanceOf[Identifier] && peekAt(1).kind == Mark("=")) {
      val fields = separated(")", () => {
        val (name, nameSpan) = identifier()
        expectMark("=")
        skipNewlines()
        (name, nameSpan, expr())
      })
      val close = advance().span
      (Argument.Named(fields, open.to(close)), close)
    } else {
      skipNewlines()
      val inner = expr()
      (Argument.OfValue(inner), expectMarkAfterNewlines(")").span)
    }
  }

  private def atom(): Expr = peek.kind match {
    case Identifier(name) => Expr.Variable(name, advance().span)
    case Interpolation(parts) =>
      val span = advance().span
      Expr.Interpolation(parts.map(_.map(embedded => new Parser(embedded).embeddedExpr())), span)
    case Mark("(") =>
      val open = advance().span
      val elements = separated(")", () => expr())
      val span = open.to(advance().span)
      elements match {
        case Nil => Expr.Constant(Literal.Unit, span)
        case List(single) => single
        case several => Expr.Tuple(several, span)
      }
    case Mark("[") =>
      val open = advance().span
      val elements = separated("]", () => expr())
      Expr.ListLiteral(elements, open.to(advance().span))
    case Mark("{") => block()
    case Keyword("intrinsic") =>
      val start = advance().span
      expectMark("[")
      val (name, _) = identifier()
      var end = expectMark("]").span
      val arguments = ListBuffer.empty[Expr]
      while (isAdjacent("(")) {
        advance()
        skipNewlines()
        arguments += expr()
        end = expectMarkAfterNewlines(")").span
      }
      Expr.Intrinsic(name, arguments.toList, start.to(end))
    case Keyword("foreign") =>
      val start = advance().span
      expectMark("[")
      val tpe = typeExpr()
      expectMark("]")
      val (name, end) = identifier()
      Expr.Foreign(tpe, name, start.to(end))
    case Keyword("summon") =>
      val start = advance().span
      expectMark("[")
      val constraint = typeExpr()
      Expr.Summon(constraint, start.to(expectMark("]").span))
    case Keyword("impl") =>
      val start = advance().span
      val traitName = path()
      val arguments = typeArguments()
      val (methods, end) = declarationBlock(() => methodImpl())
      Expr.Instance(traitName, arguments, methods, start.to(end))
    case Keyword("context") => context()
    case _ => constant() match {
      case Some(value) => Expr.Constant(value, advance().span)
      case None => fail("an expression")
    }
  }

  /** 文字列埋め込みの { } の中身（このパーサのトークン全体で 1 つの式） */
  private def embeddedExpr(): Expr = {
    skipNewlines()
    val inner = expr()
    skipNewlines()
    if (peek.kind != End) fail("`}`")
    inner
  }

  /** { e1; e2; 結果 }。途中の式の値は捨てる。最後が ; なら結果は unit */
  private def block(): Expr = {
    val start = expectMark("{").span
    val discarded = ListBuffer.empty[Expr]
    var result = Option.empty[Expr]
    skipNewlines()
    while (!isMark("}")) {
      result.foreach(_ => fail("`;` or `}`"))
      val item = expr()
      skipNewlines()
      if (isMark(";")) { advance(); skipNewlines(); discarded += item } else result = Some(item)
    }
    val end = advance().span
    Expr.Block(discarded.toList, result, start.to(end))
  }

  /** context[M] { 名前: 型 = 計算; let 名前: 型 = 値; 結果 } */
  private def context(): Expr = {
    val start = expectKeyword("context").span
    expectMark("[")
    val monad = typeExpr()
    expectMark("]")
    expectMarkAfterNewlines("{")
    skipNewlines()
    val steps = ListBuffer.empty[ContextStep]
    var result = Option.empty[Expr]
    while (result.isEmpty) {
      val stepStart = peek.span
      def endOfStep(): Unit = { skipNewlines(); expectMark(";"); skipNewlines() }
      if (isKeyword("rec")) {
        advance()
        val bindings = recursiveGroup(() => binding("let"))
        if (peekPastNewlines.kind == Keyword("in")) {
          val body = inBody()
          result = Some(Expr.LetRec(bindings, body, stepStart.to(body.span)))
        } else { endOfStep(); steps += ContextStep.Rec(bindings) }
      } else if (isKeyword("let") && letBindsPattern) {
        advance()
        val (bound, tpe, value) = patternBinding()
        if (peekPastNewlines.kind == Keyword("in")) {
          val body = inBody()
          result = Some(Expr.LetPattern(bound, tpe, value, body, stepStart.to(body.span)))
        } else { endOfStep(); steps += ContextStep.LetPattern(bound, tpe, value, stepStart.to(value.span)) }
      } else if (isKeyword("let")) {
        val parsed = binding("let")
        if (peekPastNewlines.kind == Keyword("in")) {
          val body = inBody()
          result = Some(Expr.Let(parsed, body, stepStart.to(body.span)))
        } else { endOfStep(); steps += ContextStep.Let(parsed) }
      } else attempt { val bound = pattern(); expectMark(":"); bound } match {
        case Right(bound) =>
          val tpe = typeExpr()
          expectMark("=")
          skipNewlines()
          val value = expr()
          endOfStep()
          steps += ContextStep.Bind(bound, tpe, value, stepStart.to(value.span))
        case Left(_) => result = Some(expr())
      }
    }
    val end = expectMarkAfterNewlines("}").span
    Expr.Context(monad, steps.toList, result.get, start.to(end))
  }
}

object Parser {
  def parse(source: String, file: String): List[Surface.Decl] = new Parser(Lexer.tokenize(source, file)).program()
}
