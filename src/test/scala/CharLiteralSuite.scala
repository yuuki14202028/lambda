package com.yuuki14202028

class CharLiteralSuite extends munit.FunSuite {

  /** 文字リテラルの文字コード */
  private def codeOf(literal: String)(using munit.Location): Int =
    Lam.exitCode(s"let code: char → i32 = foreign[char → i32] char_code\nlet main(): i32 = code($literal)\n")

  private def assertSyntaxError(literal: String)(using munit.Location): Unit =
    Lam.errors(s"let c: char = $literal\nlet main(): i32 = 0\n") match {
      case List(_: CompileError.Syntax) => ()
      case other => fail(s"expected a syntax error: $other")
    }

  test("文字リテラル: 英字以外も書ける") {
    assertEquals(codeOf("'a'"), 'a'.toInt)
    assertEquals(codeOf("'0'"), '0'.toInt)
    assertEquals(codeOf("' '"), ' '.toInt)
    assertEquals(codeOf("'+'"), '+'.toInt)
    assertEquals(codeOf("'('"), '('.toInt)
  }

  test("文字リテラル: エスケープ") {
    assertEquals(codeOf("'\\n'"), '\n'.toInt)
    assertEquals(codeOf("'\\t'"), '\t'.toInt)
    assertEquals(codeOf("'\\''"), '\''.toInt)
    assertEquals(codeOf("'\\\\'"), '\\'.toInt)
  }

  test("文字リテラル: 空や複数文字は構文エラー") {
    assertSyntaxError("''")
    assertSyntaxError("'ab'")
  }

  test("パターンにも同じ文字リテラルを書ける") {
    val source =
      """trait Eq[a] { def eq(x: a)(y: a): bool }; infix 4 == = eq; impl Eq[char] { def eq(x: char)(y: char): bool = intrinsic[eq_char](x)(y) }
        |let kind(c: char): i32 = match c {
        |  '0' | '1' => 1
        |  ' ' | '\n' => 2
        |  '(' => 3
        |  _ => 0
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(source + "let main(): i32 = kind('(')\n"), 3)
    assertEquals(Lam.exitCode(source + "let main(): i32 = kind('1')\n"), 1)
    assertEquals(Lam.exitCode(source + "let main(): i32 = kind('\\n')\n"), 2)
    assertEquals(Lam.exitCode(source + "let main(): i32 = kind('x')\n"), 0)
  }

  test("名前に . を含む型にも、演算子の trait を実装できる") {
    val source =
      """trait Add[a] { def add(x: a)(y: a): a }
        |infixl 6 + = add
        |impl Add[foreign.C.String] { def add(x: foreign.C.String)(y: foreign.C.String): foreign.C.String = x }
        |let greeting: foreign.C.String = "a" + "b"
        |module Money {
        |  data T = { MkT(i32) }
        |}
        |impl Add[Money.T] { def add(x: Money.T)(y: Money.T): Money.T = x }
        |let total: Money.T = Money.MkT(1) + Money.MkT(2)
        |let print: foreign.C.String → i32 = foreign[foreign.C.String → i32] print_string_inline
        |let main(): i32 = {
        |  print(greeting);
        |  match total { Money.MkT(n) => n }
        |}
        |""".stripMargin
    assertEquals(Lam.run(source), Interpreter.Result(1, "a"))
  }
}
