package com.yuuki14202028

class MultiParamTraitSuite extends munit.FunSuite {

  private def parseDecl(src: String): AST[Rec, Decl] =
    ParserAST.programParser.parseAll(src) match {
      case Right(p) =>
        val decls = eraseIndex(p).unfix match { case AST.Program(decls) => decls }
        assertEquals(decls.size, 1, clue(decls.map(_.show)))
        decls.head.unfix
      case Left(e) => fail(s"parse failed: $e")
    }

  private def validate(src: String): Either[String, TypeRec[AST.Program.type]] =
    ParserAST.programParser.parseAll(src).left.map(e => s"Parse error: $e").flatMap(p => TAnalyser.validate(p).left.map(_.render))

  private def desugar(src: String): Either[String, TypeRec[AST.Program.type]] =
    validate(src).flatMap(p => TraitEncoder.encode(p).left.map(_.render))

  private def church(src: String): Either[String, TypeRec[AST.Program.type]] =
    desugar(src).map(ChurchEncoder.encode)

  private def assertLeftContains(result: Either[String, ?], expected: String): Unit = result match {
    case Left(err) => assert(err.contains(expected), clue(err))
    case Right(_)  => fail(s"expected error containing '$expected', but succeeded")
  }

  private def assertRight(result: Either[String, ?]): Unit = result match {
    case Right(_)  => ()
    case Left(err) => fail(err)
  }

  private val conversionTrait =
    """trait Conversion[a][b] {
      |  def convert(x: a): b
      |}
      |""".stripMargin

  private val conversionProgram = conversionTrait +
    """impl Conversion[i32][bool] {
      |  def convert = λx: i32. true
      |}
      |
      |impl Conversion[bool][i32] {
      |  def convert = λx: bool. 42
      |}
      |
      |let main(): i32 = convert[bool][i32](convert[i32][bool](0))
      |""".stripMargin

  // ---- パース ----

  test("パース: 2 パラメータ trait（既定カインド *）") {
    parseDecl(conversionTrait) match {
      case AST.TopTrait(v, params, supers, methods) =>
        assertEquals(v, TypeVariable("Conversion"))
        assertEquals(params, Seq((TypeVariable("a"), Kind.Star), (TypeVariable("b"), Kind.Star)))
        assert(supers.isEmpty)
        assertEquals(methods.map(_.name), Seq(Variable("convert")))
      case other => fail(s"not TopTrait: $other")
    }
  }

  test("パース: カインド注釈付きの混在パラメータ") {
    parseDecl("trait Collect[f: * → *][a] {\n  def collect(x: a): f[a]\n}\n") match {
      case AST.TopTrait(v, params, _, _) =>
        assertEquals(v, TypeVariable("Collect"))
        assertEquals(
          params,
          Seq((TypeVariable("f"), Kind.Arrow(Kind.Star, Kind.Star)), (TypeVariable("a"), Kind.Star))
        )
      case other => fail(s"not TopTrait: $other")
    }
  }

  test("パース: 複数ターゲットの impl") {
    parseDecl("impl Conversion[i32][bool] {\n  def convert = λx: i32. true\n}\n") match {
      case AST.TopImpl(v, params, targets, context, methods) =>
        assertEquals(v, TypeVariable("Conversion"))
        assert(params.isEmpty)
        assertEquals(targets.map(_.show), Seq("i32", "bool"))
        assert(context.isEmpty)
        assertEquals(methods.map(_.name), Seq(Variable("convert")))
      case other => fail(s"not TopImpl: $other")
    }
  }

  test("パース: 複数引数の with 制約") {
    parseDecl("let f[a][b](x: a): i32 with Conversion[a][b] = 0\n") match {
      case AST.TopLetWith(v, params, constraints, _, _, _) =>
        assertEquals(v, Variable("f"))
        assertEquals(params.map(_._1), Seq(TypeVariable("a"), TypeVariable("b")))
        assertEquals(constraints.map(_.name), Seq(TypeVariable("Conversion")))
        assertEquals(constraints.head.arg.map(_.show), Seq("a", "b"))
      case other => fail(s"not TopLetWith: $other")
    }
  }

  // ---- 成功パス ----

  test("成功: trait/impl/メソッド利用が型検査・脱糖・Church encoding を通る") {
    assertRight(church(conversionProgram))
  }

  test("成功: 脱糖結果にヘッド対ごとの辞書値が現れる") {
    desugar(conversionProgram) match {
      case Right(p) =>
        val shown = p.show
        assert(shown.contains("$inst_Conversion_i32_bool"), clue(shown))
        assert(shown.contains("$inst_Conversion_bool_i32"), clue(shown))
        assert(shown.contains("MkConversion"), clue(shown))
      case Left(err) => fail(err)
    }
  }

  test("成功: 複数引数制約付き関数の辞書渡し") {
    val src = conversionTrait +
      """impl Conversion[bool][i32] {
        |  def convert = λx: bool. 42
        |}
        |
        |let through[a][b](x: a): b with Conversion[a][b] = convert[a][b](x)
        |
        |let main(): i32 = through[bool][i32](true)
        |""".stripMargin
    assertRight(church(src))
  }

  test("成功: ヘッドの組が異なるインスタンスは重複にならない") {
    val src = conversionTrait +
      """impl Conversion[i32][bool] {
        |  def convert = λx: i32. true
        |}
        |
        |impl Conversion[i32][i32] {
        |  def convert = λx: i32. x
        |}
        |
        |let main(): i32 = convert[i32][i32](0)
        |""".stripMargin
    assertRight(church(src))
  }

  test("成功: 引数の数が合う with 制約は受理される") {
    val src = conversionTrait +
      "let f[a][b](x: a): i32 with Conversion[a][b] = 0\nlet main(): i32 = 0\n"
    assertRight(validate(src))
  }

  // ---- エラーパス ----

  test("検査失敗: impl の型引数の数が trait と合わない") {
    val src = conversionTrait +
      """impl Conversion[i32] {
        |  def convert = λx: i32. x
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "trait expects 2 type argument(s), got 1")
  }

  test("検査失敗: 同一のヘッドの組は Overlapping instance") {
    val src = conversionTrait +
      """impl Conversion[i32][bool] {
        |  def convert = λx: i32. true
        |}
        |impl Conversion[i32][bool] {
        |  def convert = λx: i32. false
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "Overlapping instance")
  }

  test("検査失敗: メソッド本体の型が特殊化済み署名と合わない") {
    val src = conversionTrait +
      """impl Conversion[i32][bool] {
        |  def convert = λx: i32. 0
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "Method type mismatch")
  }

  test("検査失敗: with 制約の引数の数が trait と合わない") {
    val src = conversionTrait +
      "let f[a](x: a): i32 with Conversion[a] = 0\nlet main(): i32 = 0\n"
    assertLeftContains(validate(src), "expects 2 type arguments, got 1")
  }

  test("検査失敗: with 制約の引数のカインドが trait と合わない") {
    val src =
      """data Maybe[a] = { Nothing, Just(a) }
        |""".stripMargin + conversionTrait +
        "let f[a](x: a): i32 with Conversion[a][Maybe] = 0\nlet main(): i32 = 0\n"
    assertLeftContains(validate(src), "has kind * → *, expected *")
  }

  test("検査失敗: impl ターゲットのカインドが trait パラメータと合わない") {
    val src =
      """data Maybe[a] = { Nothing, Just(a) }
        |""".stripMargin + conversionTrait +
        """impl Conversion[i32][Maybe] {
          |  def convert = λx: i32. x
          |}
          |let main(): i32 = 0
          |""".stripMargin
    assertLeftContains(validate(src), "Kind mismatch")
  }

  test("検査失敗: パラメータ名の重複は拒否") {
    val src = "trait Conversion[a][a] {\n  def convert(x: a): a\n}\nlet main(): i32 = 0\n"
    assertLeftContains(validate(src), "duplicate parameters")
  }

  test("検査失敗: スーパークラス制約でも引数の数を検査する") {
    val src = conversionTrait +
      "trait Into[a] with Conversion[a] {\n  def into(x: a): i32\n}\nlet main(): i32 = 0\n"
    assertLeftContains(validate(src), "expects 2 type arguments, got 1")
  }

  test("NoInstance: 引数の組に合うインスタンスがなければ脱糖で拒否") {
    val src = conversionTrait +
      """impl Conversion[i32][bool] {
        |  def convert = λx: i32. true
        |}
        |let main(): i32 = convert[bool][i32](true)
        |""".stripMargin
    assertLeftContains(desugar(src), "No instance")
  }

  test("AmbiguousConstraint: 型適用が trait パラメータ数に満たない裸参照は拒否") {
    val src = conversionTrait +
      """impl Conversion[i32][bool] {
        |  def convert = λx: i32. true
        |}
        |let f: ∀b. i32 → b = convert[i32]
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(desugar(src), "Ambiguous constraint")
  }
}
