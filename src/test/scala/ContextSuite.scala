package com.yuuki14202028

class ContextSuite extends munit.FunSuite {

  private def parseExpr(src: String): Rec[Expr] =
    ParserAST.expr.parseAll(src) match {
      case Right(e) => eraseIndex(e)
      case Left(err) => fail(s"parse failed: $err")
    }

  private def validate(src: String): Either[String, TypeRec[AST.Program.type]] =
    ParserAST.programParser.parseAll(src).left.map(e => s"Parse error: $e").flatMap(p => TAnalyser.validate(p).left.map(_.render))

  private def desugarContext(src: String): Either[String, TypeRec[AST.Program.type]] =
    validate(src).flatMap(p => ContextDesugar.desugar(p).left.map(_.render))

  private def desugar(src: String): Either[String, TypeRec[AST.Program.type]] =
    desugarContext(src).flatMap(p => TraitEncoder.encode(p).left.map(_.render))

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

  private def shownOrFail(result: Either[String, TypeRec[AST.Program.type]]): String = result match {
    case Right(p)  => p.show
    case Left(err) => fail(err)
  }

  // ---- パーサー ----

  test("パース: context ブロックは Context ノードになる") {
    parseExpr("context[Maybe] { x: i32 = m; x + 1 }").unfix match {
      case AST.Context(monad, bindings, result) =>
        assertEquals(monad.show, "Maybe")
        assertEquals(bindings.map(b => (b.name.name, b.monadic)), Seq(("x", true)))
        assertEquals(result.show, "(x + 1)")
      case other => fail(s"not Context: $other")
    }
  }

  test("パース: 純粋 let / let rec をモナド束縛と混在できる") {
    val src = "context[Maybe] { let y: i32 = 1; x: i32 = m; let rec f(n: i32): i32 = f(n); x + y }"
    parseExpr(src).unfix match {
      case AST.Context(_, bindings, _) =>
        assertEquals(bindings.map(b => (b.name.name, b.monadic, b.recursive)),
          Seq(("y", false, false), ("x", true, false), ("f", false, true)))
      case other => fail(s"not Context: $other")
    }
  }

  test("パース: 束縛なしの context ブロック") {
    parseExpr("context[Maybe] { 42 }").unfix match {
      case AST.Context(_, bindings, result) =>
        assertEquals(bindings.length, 0)
        assertEquals(result.show, "42")
      case other => fail(s"not Context: $other")
    }
  }

  test("パース失敗: 最終式のない context ブロック") {
    assert(ParserAST.expr.parseAll("context[Maybe] { x: i32 = m; }").isLeft)
  }

  // ---- 型検査+脱糖 ----

  private val monadMaybe =
    """let __add_i32(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y)
      |let __div_i32(x: i32)(y: i32): i32 = intrinsic[div_i32](x)(y)
      |let __eq_i32(x: i32)(y: i32): bool = intrinsic[eq_i32](x)(y)
      |
      |data Maybe[a] = { Nothing, Just(a) }
      |
      |trait Monad[m: * → *] {
      |  def pure[a](x: a): m[a]
      |  def flatMap[a][b](v: m[a])(f: a → m[b]): m[b]
      |}
      |
      |impl Monad[Maybe] {
      |  def pure[a](x:a): m[a] = Just[a](x)
      |  def flatMap[a][b](m: Maybe[a])(f: a → Maybe[b]): Maybe[b] = match m {
      |    Nothing => Nothing[b]
      |    Just(x) => f(x)
      |  }
      |}
      |
      |let safeDiv(x: i32)(y: i32): Maybe[i32] =
      |  if y == 0 then Nothing[i32] else Just[i32](x / y)
      |""".stripMargin

  test("型検査: context ブロックは展開されず Context ノードのまま M[R] 型が付く") {
    val src = monadMaybe +
      """let compute(): Maybe[i32] = context[Maybe] {
        |  step1: i32 = 120 safeDiv 5;
        |  step1 + 1
        |}
        |let main(): i32 = 0
        |""".stripMargin
    val shown = shownOrFail(validate(src))
    assert(shown.contains("context[Maybe]"), clue(shown))
    assert(shown.contains(")[Maybe[i32]]"), clue(shown))
  }

  test("脱糖: context ブロックは手書きの bind/ret チェーンと同じ形になる") {
    val contextVersion = monadMaybe +
      """let compute(): Maybe[i32] = context[Maybe] {
        |  step1: i32 = 120 safeDiv 5;
        |  step2: i32 = step1 safeDiv 3;
        |  step2 + 1
        |}
        |let main(): i32 = 0
        |""".stripMargin
    val manualVersion = monadMaybe +
      """let compute(): Maybe[i32] =
        |  flatMap[Maybe][i32][i32](safeDiv(120)(5))(λstep1: i32.
        |    flatMap[Maybe][i32][i32](safeDiv(step1)(3))(λstep2: i32.
        |      pure[Maybe][i32](step2 + 1)))
        |let main(): i32 = 0
        |""".stripMargin
    assertEquals(shownOrFail(desugarContext(contextVersion)), shownOrFail(desugarContext(manualVersion)))
  }

  test("脱糖: 束縛なしの context は ret になる") {
    val contextVersion = monadMaybe +
      """let r(): Maybe[i32] = context[Maybe] { 42 }
        |let main(): i32 = 0
        |""".stripMargin
    val manualVersion = monadMaybe +
      """let r(): Maybe[i32] = pure[Maybe][i32](42)
        |let main(): i32 = 0
        |""".stripMargin
    assertEquals(shownOrFail(desugarContext(contextVersion)), shownOrFail(desugarContext(manualVersion)))
  }

  test("脱糖: 純粋 let は通常の let になり、後続の束縛から参照できる") {
    val contextVersion = monadMaybe +
      """let compute(): Maybe[i32] = context[Maybe] {
        |  let denom: i32 = 5;
        |  step1: i32 = 120 safeDiv denom;
        |  step1 + 1
        |}
        |let main(): i32 = 0
        |""".stripMargin
    val manualVersion = monadMaybe +
      """let compute(): Maybe[i32] =
        |  let denom: i32 = 5 in
        |    flatMap[Maybe][i32][i32](safeDiv(120)(denom))(λstep1: i32.
        |      pure[Maybe][i32](step1 + 1))
        |let main(): i32 = 0
        |""".stripMargin
    assertEquals(shownOrFail(desugarContext(contextVersion)), shownOrFail(desugarContext(manualVersion)))
  }

  test("辞書解決: 脱糖後の bind/ret は TraitEncoder がインスタンス辞書を挿入する") {
    val src = monadMaybe +
      """let compute(): Maybe[i32] = context[Maybe] {
        |  step1: i32 = 120 safeDiv 5;
        |  step1 + 1
        |}
        |let main(): i32 = 0
        |""".stripMargin
    desugar(src) match {
      case Right(p) =>
        val shown = p.show
        assert(shown.contains("$inst_Monad_Maybe"), clue(shown))
      case Left(err) => fail(err)
    }
  }

  test("型検査: Church encoding まで通る") {
    val src = monadMaybe +
      """let compute(): Maybe[i32] = context[Maybe] {
        |  step1: i32 = 120 safeDiv 5;
        |  step1 + 1
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertRight(church(src))
  }

  test("型エラー: モナド束縛の右辺が M[T] でない") {
    val src = monadMaybe +
      """let compute(): Maybe[i32] = context[Maybe] {
        |  step1: i32 = 42;
        |  step1 + 1
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "Type mismatch")
  }

  test("型エラー: bind/ret が未定義のまま context を使う") {
    val src =
      """data Maybe[a] = { Nothing, Just(a) }
        |let r(): Maybe[i32] = context[Maybe] { 42 }
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "is not defined")
  }

  test("型エラー: カインドが * → * でない型を指定する") {
    val src = monadMaybe +
      """let r(): Maybe[i32] = context[i32] { 42 }
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "context")
  }
}
