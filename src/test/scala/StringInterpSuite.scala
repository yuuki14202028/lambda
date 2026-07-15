package com.yuuki14202028

class StringInterpSuite extends munit.FunSuite {

  private def parseExpr(src: String): Rec[Expr] =
    ParserAST.expr.parseAll(src) match {
      case Right(e) => eraseIndex(e)
      case Left(err) => fail(s"parse failed: $err")
    }

  private def validate(src: String): Either[String, TypeRec[AST.Program.type]] =
    ParserAST.programParser.parseAll(src).left.map(e => s"Parse error: $e").flatMap(p => TAnalyser.validate(p).left.map(_.render))

  private def desugar(src: String): Either[String, TypeRec[AST.Program.type]] =
    validate(src).flatMap(p => TraitEncoder.encode(p).left.map(_.render))

  private def church(src: String): Either[String, TypeRec[AST.Program.type]] =
    desugar(src).map(ChurchEncoder.encode)

  private def assertRight(result: Either[String, ?]): Unit = result match {
    case Right(_)  => ()
    case Left(err) => fail(err)
  }

  private def assertLeftContains(result: Either[String, ?], expected: String): Unit = result match {
    case Left(err) => assert(err.contains(expected), clue(err))
    case Right(_)  => fail(s"expected error containing '$expected', but succeeded")
  }

  private def parts(src: String): Seq[Rec[Expr]] =
    parseExpr(src).unfix match {
      case AST.StrInterp(ps) => ps
      case other => fail(s"not StrInterp: $other")
    }

  // ---- パーサー ----

  test("リテラルのみのバッククォート文字列は StringLit に縮退する") {
    assertEquals(parseExpr("`hello\\n`").show, parseExpr("\"hello\\n\"").show)
  }

  test("空のバッククォート文字列は空文字列リテラルになる") {
    assertEquals(parseExpr("``").show, parseExpr("\"\"").show)
  }

  test("埋め込み式を含むとリテラルと式のパーツ列になる") {
    val ps = parts("`a = {x}\\n`")
    assertEquals(ps.length, 3)
    assertEquals(ps(0).show, "\"a = \"")
    assertEquals(ps(1).show, "x")
    assertEquals(ps(2).show, "\"\\n\"")
  }

  test("先頭・末尾が埋め込み式でもパースできる") {
    assertEquals(parts("`{x} end`").map(_.show), Seq("x", "\" end\""))
    assertEquals(parts("`start {x}`").map(_.show), Seq("\"start \"", "x"))
  }

  test("連続した埋め込みの間に空リテラルは入らない") {
    assertEquals(parts("`{a}{b}`").map(_.show), Seq("a", "b"))
  }

  test("式のみのバッククォート文字列") {
    assertEquals(parts("`{x}`").map(_.show), Seq("x"))
  }

  test("エスケープ: バッククォート・波括弧・標準エスケープ") {
    assertEquals(parseExpr("`a\\`b`").show, parseExpr("\"a`b\"").show)
    assertEquals(parseExpr("`\\{not embed\\}`").show, parseExpr("\"{not embed}\"").show)
    assertEquals(parseExpr("`tab\\there`").show, parseExpr("\"tab\\there\"").show)
    assertEquals(parseExpr("`back\\\\slash`").show, parseExpr("\"back\\\\slash\"").show)
  }

  test("埋め込み外の閉じ波括弧は平文として扱う") {
    assertEquals(parseExpr("`a}b`").show, parseExpr("\"a}b\"").show)
  }

  test("埋め込み式の中にネストした構文を書ける") {
    assertEquals(parts("`{f(x)(y)}`").map(_.show), Seq("f(x)(y)"))
    parts("`{if c then a else b}`").head.unfix match {
      case AST.If(_, _, _) => ()
      case other => fail(s"not If: $other")
    }
    parts("`{{ x }}`").head.unfix match {
      case AST.Block(_, _) => ()
      case other => fail(s"not Block: $other")
    }
  }

  test("パース失敗: 閉じバッククォートなし・閉じ波括弧なし・生改行") {
    assert(ParserAST.expr.parseAll("`abc").isLeft)
    assert(ParserAST.expr.parseAll("`{x`").isLeft)
    assert(ParserAST.expr.parseAll("`a\nb`").isLeft)
  }

  // ---- 型検査+脱糖 ----

  private val showI32 =
    """trait Show[a] {
      |  def show(x: a): foreign.C.String
      |}
      |
      |impl Show[i32] {
      |  def show = λx: i32. "num"
      |}
      |""".stripMargin

  test("型検査: String 型の埋め込みは Show 不要で通る") {
    val src =
      """let s(): foreign.C.String = `v = {"a"}!`
        |let main(): i32 = 0
        |""".stripMargin
    assertRight(desugar(src))
  }

  test("型検査: i32 埋め込みは show が自動挿入され、辞書解決まで通る") {
    val src = showI32 +
      """let s(): foreign.C.String = `v = {42}\n`
        |let main(): i32 = 0
        |""".stripMargin
    desugar(src) match {
      case Right(p) =>
        val shown = p.show
        assert(shown.contains("concat"), clue(shown))
        assert(shown.contains("$inst_Show_i32"), clue(shown))
      case Left(err) => fail(err)
    }
  }

  test("型検査: Church encoding まで通る") {
    val src = showI32 +
      """let s(): foreign.C.String = `v = {42}\n`
        |let main(): i32 = 0
        |""".stripMargin
    assertRight(church(src))
  }

  test("型エラー: show が未定義のまま非 String を埋め込む") {
    val src =
      """let s(): foreign.C.String = `v = {42}`
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "show")
  }

  test("辞書解決エラー: Show trait はあるがインスタンスがない") {
    val src =
      """trait Show[a] {
        |  def show(x: a): foreign.C.String
        |}
        |let s(): foreign.C.String = `v = {42}`
        |let main(): i32 = 0
        |""".stripMargin
    assertRight(validate(src))
    assertLeftContains(desugar(src), "No instance")
  }

  test("with 節の局所辞書で埋め込みの Show[a] が解決される") {
    val src =
      """trait Show[a] {
        |  def show(x: a): foreign.C.String
        |}
        |let render[a](x: a): foreign.C.String with Show[a] = `v = {x}`
        |let main(): i32 = 0
        |""".stripMargin
    assertRight(desugar(src))
  }

  test("型エラー: バッククォート文字列全体は常に foreign.C.String 型") {
    val src = showI32 +
      """let n(): i32 = `{42}`
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "Type mismatch")
  }
}
