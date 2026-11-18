package com.yuuki14202028

/** 文字列埋め込み `a{x}`: { } の中の式を、interpolate に宣言した関数で文字列にして埋める */
class StringInterpSuite extends munit.FunSuite {

  private val io =
    """type String = foreign.C.String
      |let print: String → i32 = foreign[String → i32] print_string_inline
      |""".stripMargin

  private val showing = io +
    """trait Show[a] {
      |  def show(x: a): String
      |}
      |
      |interpolate = show
      |
      |impl Show[i32] {
      |  def show = foreign[i32 → String] int_to_string
      |}
      |impl Show[bool] {
      |  def show(x: bool): String = if x then "T" else "F"
      |}
      |""".stripMargin

  /** text（バッククォート文字列）を表示した結果 */
  private def printed(text: String, declarations: String = "")(using munit.Location): String =
    Lam.output(s"$showing${declarations}let main(): i32 = print($text)\n")

  /** expected に当たるエラーが報告される */
  private def assertError(source: String)(expected: PartialFunction[CompileError, Any])(using munit.Location): Unit = {
    val found = Lam.errors(source)
    assert(found.exists(expected.isDefinedAt), clue(found))
  }

  private def assertSyntaxError(text: String)(using munit.Location): Unit =
    assertError(s"${showing}let x: i32 = 1\nlet main(): i32 = print($text)\n") { case _: CompileError.Syntax => }

  // ---- 読み方 ----

  test("埋め込みのないバッククォート文字列は、ふつうの文字列（interpolate の宣言は要らない）") {
    assertEquals(Lam.output(io + "let main(): i32 = print(`hello\\n`)\n"), "hello\n")
  }

  test("空のバッククォート文字列は空文字列になる") {
    assertEquals(Lam.output(io + "let main(): i32 = print(``)\n"), "")
  }

  test("埋め込み式を含むと、文字列と式が順につながる") {
    assertEquals(printed("`a = {x}\\n`", "let x: i32 = 42\n"), "a = 42\n")
  }

  test("先頭・末尾が埋め込み式でも書ける") {
    assertEquals(printed("`{x} end`", "let x: i32 = 1\n"), "1 end")
    assertEquals(printed("`start {x}`", "let x: i32 = 1\n"), "start 1")
  }

  test("埋め込みは続けて書ける") {
    assertEquals(printed("`{a}{b}`", "let a: i32 = 1\nlet b: bool = true\n"), "1T")
  }

  test("式のみのバッククォート文字列") {
    assertEquals(printed("`{x}`", "let x: i32 = 7\n"), "7")
  }

  test("エスケープ: バッククォート・波括弧・標準エスケープ") {
    assertEquals(printed("`a\\`b`"), "a`b")
    assertEquals(printed("`\\{not embed\\}`"), "{not embed}")
    assertEquals(printed("`tab\\there`"), "tab\there")
    assertEquals(printed("`back\\\\slash`"), "back\\slash")
  }

  test("埋め込み外の閉じ波括弧は平文として扱う") {
    assertEquals(printed("`a}b`"), "a}b")
  }

  test("埋め込み式の中にネストした構文を書ける") {
    val declarations = "let f(x: i32)(y: i32): i32 = y\nlet c: bool = true\n"
    assertEquals(printed("`{f(1)(2)}`", declarations), "2")
    assertEquals(printed("`{if c then 3 else 4}`", declarations), "3")
    assertEquals(printed("`{{ print(\"x\"); 5 }}`", declarations), "x5")
  }

  test("構文エラー: 閉じバッククォートなし・閉じ波括弧なし・生改行") {
    assertSyntaxError("`abc")
    assertSyntaxError("`{x`")
    assertSyntaxError("`a\nb`")
  }

  // ---- 型検査 ----

  test("型検査: String 型の埋め込みも、宣言された関数を通す") {
    val source = showing +
      """let s(): String = `v = {"a"}!`
        |let main(): i32 = 0
        |""".stripMargin
    assertError(source) { case error: CompileError.NoInstance if error.message.contains("Show[foreign.C.String]") => }
  }

  test("型検査: i32 埋め込みは show が自動挿入され、辞書解決まで通る") {
    val source = showing +
      """let s(): String = `v = {42}\n`
        |let main(): i32 = print(s())
        |""".stripMargin
    assertEquals(Lam.output(source), "v = 42\n")
  }

  test("型エラー: interpolate が宣言されていなければ埋め込めない") {
    val source =
      """let s(): foreign.C.String = `v = {42}`
        |let main(): i32 = 0
        |""".stripMargin
    assertError(source) { case CompileError.SyntaxNotDeclared(_, "interpolate", _) => }
  }

  test("宣言: interpolate には ∀a. … → a → String の関数だけを宣言できる") {
    val shown = io +
      """let render[a](x: a): String = "n"
        |interpolate = render
        |let s: String = `{1}{true}`
        |let main(): i32 = print(s)
        |""".stripMargin
    assertEquals(Lam.output(shown), "nn")
    // String を返さない関数も宣言はできて、埋め込みを書いた所でエラーになる
    val source = io +
      """let render[a](x: a): i32 = 0
        |interpolate = render
        |""".stripMargin
    assertEquals(Lam.errors(source + "let main(): i32 = 0\n"), Nil)
    assert(Lam.errors(source + "let s: String = `a{1}`\nlet main(): i32 = 0\n").exists { case _: CompileError.TypeMismatch => true; case _ => false })
  }

  test("宣言: interpolate はスコープに従う（module の中の宣言は open で見える）") {
    val source = io +
      """module Text {
        |  let render[a](x: a): String = "n"
        |  interpolate = render
        |}
        |let a: String = `{1}`
        |let main(): i32 = print(a)
        |""".stripMargin
    assertError(source) { case CompileError.SyntaxNotDeclared(_, "interpolate", _) => }
    assertEquals(Lam.output(source.replace("let a:", "open Text\nlet a:")), "n")
  }

  test("型エラー: Show trait はあるがインスタンスがない") {
    val source =
      """trait Show[a] {
        |  def show(x: a): foreign.C.String
        |}
        |interpolate = show
        |let s(): foreign.C.String = `v = {42}`
        |let main(): i32 = 0
        |""".stripMargin
    assertError(source) { case _: CompileError.NoInstance => }
  }

  test("with 節の局所辞書で埋め込みの Show[a] が解決される") {
    val source = showing +
      """let render[a](x: a): String with Show[a] = `v = {x}`
        |let main(): i32 = print(render(true))
        |""".stripMargin
    assertEquals(Lam.output(source), "v = T")
  }

  test("型エラー: バッククォート文字列全体は常に foreign.C.String 型") {
    val source = showing +
      """let n(): i32 = `{42}`
        |let main(): i32 = 0
        |""".stripMargin
    assertError(source) { case _: CompileError.TypeMismatch => }
  }
}
