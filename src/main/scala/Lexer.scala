package com.yuuki14202028

enum TokenKind {
  case Identifier(name: String)
  case Keyword(word: String)
  case IntLiteral(text: String, typeName: String)
  case FloatLiteral(text: String, typeName: String)
  case StringLiteral(value: String)
  case CharLiteral(value: Char)
  // `text {式} text`: 文字列の部分と、{ } の中のトークン列
  case Interpolation(parts: List[Either[String, Vector[Token]]])
  case Mark(text: String)
  case Newline
  case End
}

/** spaceBefore: 直前に空白・改行・コメントがあったか（f(x) と f (x) を区別する） */
final case class Token(kind: TokenKind, span: Span, spaceBefore: Boolean)

object Lexer {
  val keywords: Set[String] = Set(
    "let", "rec", "data", "type", "in", "if", "then", "else", "match", "true", "false",
    "intrinsic", "foreign", "infixl", "infixr", "infix",
    "trait", "impl", "def", "derive", "module", "open", "private", "opaque", "import",
    "with", "as", "fold", "context", "summon", "using", "interpolate", "literal"
  )

  /** 演算子に使える文字。並びを最長一致で 1 つの記号として読む */
  val operatorCharacters = "+-*/%<>=!&|^~?$@:"

  private val singleMarks = "()[]{},;.λΛ∀→⇒"

  private val suffixes = List("isize", "usize", "i16", "i32", "i64", "i8", "u16", "u32", "u64", "u8", "f32", "f64")

  private def isNameStart(c: Char): Boolean = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
  private def isNamePart(c: Char): Boolean = isNameStart(c) || c.isDigit

  def tokenize(source: String, file: String): Vector[Token] = new Lexer(source, file).tokens(embedded = false)
}

final class Lexer(source: String, file: String) {
  import Lexer.*
  import TokenKind.*

  private var at = 0

  private def fail(found: String, start: Int, width: Int = 1): Nothing =
    throw CompileFailure(CompileError.Syntax("a token", found, Span(start, (start + width).min(source.length), file)))

  private def startsWith(text: String): Boolean = source.startsWith(text, at)

  /** embedded: 文字列埋め込みの { } の中。対応する } の手前で止まる */
  def tokens(embedded: Boolean): Vector[Token] = {
    val result = Vector.newBuilder[Token]
    var spaceBefore = true
    var lastWasNewline = true
    var depth = 0
    var done = false

    def emit(kind: TokenKind, start: Int): Unit = {
      result += Token(kind, Span(start, at, file), spaceBefore)
      spaceBefore = false
      lastWasNewline = kind == Newline
    }

    while (!done && at < source.length) {
      val start = at
      val c = source(at)
      if (c == '\n') {
        at += 1
        if (!lastWasNewline) emit(Newline, start)
        spaceBefore = true
      } else if (c.isWhitespace) {
        at += 1
        spaceBefore = true
      } else if (startsWith("//")) {
        while (at < source.length && source(at) != '\n') at += 1
        spaceBefore = true
      } else if (startsWith("/*")) {
        skipBlockComment(start)
        spaceBefore = true
      } else if (embedded && c == '}' && depth == 0) {
        done = true
      } else if (singleMarks.contains(c)) {
        if (c == '{') depth += 1
        if (c == '}') depth -= 1
        at += 1
        emit(Mark(c.toString), start)
      } else if (isNameStart(c)) {
        while (at < source.length && isNamePart(source(at))) at += 1
        val word = source.substring(start, at)
        emit(if (keywords(word)) Keyword(word) else Identifier(word), start)
      } else if (c.isDigit) {
        emit(number(start), start)
      } else if (c == '"') {
        emit(StringLiteral(string(start)), start)
      } else if (c == '`') {
        emit(interpolation(start), start)
      } else if (c == '\'') {
        at += 1
        if (at >= source.length) fail("an unclosed character", start)
        val value = if (source(at) == '\\') escape(start, "'") else { at += 1; source(at - 1) }
        if (at >= source.length || source(at) != '\'') fail("an unclosed character", start)
        at += 1
        emit(CharLiteral(value), start)
      } else if (operatorCharacters.contains(c)) {
        while (at < source.length && operatorCharacters.contains(source(at)) && !startsWith("//") && !startsWith("/*")) at += 1
        emit(Mark(source.substring(start, at)), start)
      } else {
        fail(s"`$c`", start)
      }
    }
    if (embedded && !done) fail("an unclosed `{` in a string", at - 1)
    result += Token(End, Span(at, at, file), spaceBefore = true)
    result.result()
  }

  private def skipBlockComment(start: Int): Unit = {
    var depth = 0
    while ({
      if (at >= source.length) fail("an unclosed comment", start, width = 2)
      if (startsWith("/*")) { depth += 1; at += 2 }
      else if (startsWith("*/")) { depth -= 1; at += 2 }
      else at += 1
      depth > 0
    }) ()
  }

  /** \ の後の文字。quotes は、その文字列で \ を付けて書ける文字 */
  private def escape(start: Int, quotes: String): Char = {
    at += 1
    if (at >= source.length) fail("the end of the file", start)
    val c = source(at)
    at += 1
    c match {
      case 'n' => '\n'
      case 'r' => '\r'
      case 't' => '\t'
      case '0' => '\u0000'
      case '\\' => c
      case _ if quotes.contains(c) => c
      case other => fail(s"the escape `\\$other`", at - 2)
    }
  }

  private def number(start: Int): TokenKind = {
    while (at < source.length && source(at).isDigit) at += 1
    val isFloat = at + 1 < source.length && source(at) == '.' && source(at + 1).isDigit
    if (isFloat) {
      at += 1
      while (at < source.length && source(at).isDigit) at += 1
      if (at < source.length && (source(at) == 'e' || source(at) == 'E')) {
        at += 1
        if (at < source.length && (source(at) == '+' || source(at) == '-')) at += 1
        while (at < source.length && source(at).isDigit) at += 1
      }
    }
    val text = source.substring(start, at)
    val suffix = suffixes.find(s => startsWith(s) && !source.lift(at + s.length).exists(isNamePart))
    suffix.foreach(s => at += s.length)
    suffix match {
      case Some(s) if Primitives.floats(s) => FloatLiteral(text, s)
      case Some(s) if !isFloat => IntLiteral(text, s)
      case Some(s) => fail(s"the suffix `$s` on a number with a fraction", start)
      case None => if (isFloat) FloatLiteral(text, "f64") else IntLiteral(text, "i32")
    }
  }

  private def string(start: Int): String = {
    val text = new StringBuilder
    at += 1
    while ({
      if (at >= source.length || source(at) == '\n') fail("an unclosed string", start)
      source(at) != '"'
    }) {
      if (source(at) == '\\') text += escape(start, "\"") else { text += source(at); at += 1 }
    }
    at += 1
    text.toString
  }

  private def interpolation(start: Int): TokenKind = {
    val parts = List.newBuilder[Either[String, Vector[Token]]]
    val text = new StringBuilder
    at += 1
    while ({
      if (at >= source.length || source(at) == '\n') fail("an unclosed string", start)
      source(at) != '`'
    }) {
      if (source(at) == '\\') text += escape(start, "`{}")
      else if (source(at) == '{') {
        parts += Left(text.toString)
        text.clear()
        at += 1
        parts += Right(tokens(embedded = true))
        at += 1
      } else { text += source(at); at += 1 }
    }
    at += 1
    parts += Left(text.toString)
    Interpolation(parts.result().filter(_ != Left("")))
  }
}
