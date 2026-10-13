package com.yuuki14202028

class CharLiteralSuite extends munit.FunSuite {

  private def parseExpr(src: String): Rec[Expr] =
    ParserAST.expr.parseAll(src).fold(e => fail(e.toString), eraseIndex(_))

  private def validate(src: String): Either[String, TypeRec[AST.Program.type]] =
    ParserAST.programParser.parseAll(src).left.map(e => s"Parse error: $e").flatMap(p => TAnalyser.validate(p).left.map(_.render))

  private def charOf(src: String): scala.Char = parseExpr(src).unfix match {
    case AST.Char(c) => c
    case other => fail(s"not a char literal: $other")
  }

  test("文字リテラル: 英字以外も書ける") {
    assertEquals(charOf("'a'"), 'a')
    assertEquals(charOf("'0'"), '0')
    assertEquals(charOf("' '"), ' ')
    assertEquals(charOf("'+'"), '+')
    assertEquals(charOf("'('"), '(')
  }

  test("文字リテラル: エスケープ") {
    assertEquals(charOf("'\\n'"), '\n')
    assertEquals(charOf("'\\t'"), '\t')
    assertEquals(charOf("'\\''"), '\'')
    assertEquals(charOf("'\\\\'"), '\\')
  }

  test("文字リテラル: 空や複数文字は構文エラー") {
    assert(ParserAST.expr.parseAll("''").isLeft)
    assert(ParserAST.expr.parseAll("'ab'").isLeft)
  }

  test("パターンにも同じ文字リテラルを書ける") {
    val src =
      """let __eq_char(x: char)(y: char): bool = intrinsic[eq_char](x)(y)
        |let kind(c: char): i32 = match c {
        |  '0' | '1' => 1
        |  ' ' | '\n' => 2
        |  '(' => 3
        |  _ => 0
        |}
        |let main(): i32 = kind('(')
        |""".stripMargin
    assert(validate(src).isRight, clue(validate(src)))
  }

  test("名前に . を含む型の演算子は、. を _ にした名前の関数で定義する") {
    val src =
      """let __add_foreign_C_String(x: foreign.C.String)(y: foreign.C.String): foreign.C.String = x
        |let greeting: foreign.C.String = "a" + "b"
        |module Money {
        |  data T = { MkT(i32) }
        |}
        |let __add_Money_T(x: Money.T)(y: Money.T): Money.T = x
        |let total: Money.T = Money.MkT(1) + Money.MkT(2)
        |let main(): i32 = 0
        |""".stripMargin
    assert(validate(src).isRight, clue(validate(src)))
  }
}
