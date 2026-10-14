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
    validate(src).map(ModuleDesugar.desugar).map(PatternDesugar.desugar).map(OperatorDesugar.desugar).map(TraitEncoder.encode)

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

  // 最初の埋め込みの式
  private def embedded(src: String): Rec[Expr] =
    parseExpr(src).unfix match {
      case AST.StrInterp(ps) => ps.collectFirst { case InterpPart.Embed(value, _) => value }.getOrElse(fail("no embedded expression"))
      case other => fail(s"not StrInterp: $other")
    }

  // 文字列の部分は "…"、埋め込みは式として表示する
  private def parts(src: String): Seq[String] =
    parseExpr(src).unfix match {
      case AST.StrInterp(ps) => ps.map {
        case InterpPart.Text(value) => "\"" + value.replace("\n", "\\n") + "\""
        case InterpPart.Embed(value, _) => value.show
      }
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
    assertEquals(ps(0), "\"a = \"")
    assertEquals(ps(1), "x")
    assertEquals(ps(2), "\"\\n\"")
  }

  test("先頭・末尾が埋め込み式でもパースできる") {
    assertEquals(parts("`{x} end`"), Seq("x", "\" end\""))
    assertEquals(parts("`start {x}`"), Seq("\"start \"", "x"))
  }

  test("連続した埋め込みの間に空リテラルは入らない") {
    assertEquals(parts("`{a}{b}`"), Seq("a", "b"))
  }

  test("式のみのバッククォート文字列") {
    assertEquals(parts("`{x}`"), Seq("x"))
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
    assertEquals(parts("`{f(x)(y)}`"), Seq("f(x)(y)"))
    embedded("`{if c then a else b}`").unfix match {
      case AST.If(_, _, _) => ()
      case other => fail(s"not If: $other")
    }
    embedded("`{{ x }}`").unfix match {
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
      |interpolate = show
      |
      |impl Show[i32] {
      |  def show = λx: i32. "num"
      |}
      |""".stripMargin

  test("型検査: String 型の埋め込みも、宣言された関数を通す") {
    val src = showI32 +
      """let s(): foreign.C.String = `v = {"a"}!`
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "No instance for Show[foreign.C.String]")
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

  test("型エラー: interpolate が宣言されていなければ埋め込めない") {
    val src =
      """let s(): foreign.C.String = `v = {42}`
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "interpolate is not declared")
  }

  test("宣言: interpolate には ∀a. … → a → String の関数だけを宣言できる") {
    val src =
      """let render[a](x: a): i32 = 0
        |interpolate = render
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "cannot be declared for interpolate")
    val shown =
      """let render[a](x: a): foreign.C.String = "n"
        |interpolate = render
        |let s: foreign.C.String = `{1}{true}`
        |let main(): i32 = 0
        |""".stripMargin
    assertRight(desugar(shown))
  }

  test("宣言: interpolate はスコープに従う（module の中の宣言は open で見える）") {
    val src =
      """module Text {
        |  let render[a](x: a): foreign.C.String = "n"
        |  interpolate = render
        |}
        |let a: foreign.C.String = `{1}`
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "interpolate is not declared")
    assertRight(desugar(src.replace("let a:", "open Text\nlet a:")))
  }

  test("型エラー: Show trait はあるがインスタンスがない") {
    val src =
      """trait Show[a] {
        |  def show(x: a): foreign.C.String
        |}
        |interpolate = show
        |let s(): foreign.C.String = `v = {42}`
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "No instance")
  }

  test("with 節の局所辞書で埋め込みの Show[a] が解決される") {
    val src =
      """trait Show[a] {
        |  def show(x: a): foreign.C.String
        |}
        |interpolate = show
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
