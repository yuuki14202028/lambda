package com.yuuki14202028

class MethodConstraintSuite extends munit.FunSuite {

  private def parseDecl(src: String): AST[Rec, Decl] =
    ParserAST.programParser.parseAll(src) match {
      case Right(p) => eraseIndex(p).unfix match {
        case AST.Program(decls) => decls.head.unfix
      }
      case Left(err) => fail(s"parse failed: $err")
    }

  private def validate(src: String): Either[String, TypeRec[AST.Program.type]] =
    ParserAST.programParser.parseAll(src).left.map(e => s"Parse error: $e").flatMap(p => TAnalyser.validate(p).left.map(_.render))

  private def desugar(src: String): Either[String, TypeRec[AST.Program.type]] =
    validate(src).map(TraitEncoder.encode)

  private def church(src: String): Either[String, TypeRec[AST.Program.type]] =
    desugar(src).map(ChurchEncoder.encode)

  private def assertRight(result: Either[String, ?]): Unit = result match {
    case Right(_) => ()
    case Left(err) => fail(err)
  }

  private def assertLeftContains(result: Either[String, ?], expected: String): Unit = result match {
    case Left(err) => assert(err.contains(expected), clue(err))
    case Right(_) => fail(s"expected failure containing: $expected")
  }

  // メソッド自身の型変数 g に制約を付ける（trait のパラメータ t とは別）
  private val prelude =
    """data Box[a] = { MkBox(a) }
      |data Id[a] = { MkId(a) }
      |
      |trait Pointed[g: * → *] {
      |  def point[a](x: a): g[a]
      |}
      |
      |impl Pointed[Id] {
      |  def point[a](x: a): Id[a] = MkId[a](x)
      |}
      |
      |trait Wrap[t: * → *] {
      |  def wrapIn[g: * → *][a](x: t[a]): g[t[a]] with Pointed[g]
      |}
      |""".stripMargin

  private val wrapBox =
    """impl Wrap[Box] {
      |  def wrapIn[g: * → *][a](x: Box[a]): g[Box[a]] with Pointed[g] = point[g][Box[a]](x)
      |}
      |""".stripMargin

  test("パース: trait メソッドの with 節はメソッドの型パラメータ直下の暗黙引数になる") {
    parseDecl("trait Wrap[t: * → *] {\n  def wrapIn[g: * → *][a](x: t[a]): g[t[a]] with Pointed[g]\n}\n") match {
      case AST.TopTrait(_, _, _, methods) =>
        assertEquals(methods.map(_.sig.show), Seq("∀(g: * → *). ∀a. [Pointed[g]] → t[a] → g[t[a]]"))
      case other => fail(s"not TopTrait: $other")
    }
  }

  test("パース: impl メソッドの with 節は本体の暗黙引数の抽象になる") {
    parseDecl("impl Wrap[Box] {\n  def wrapIn[g: * → *][a](x: Box[a]) with Pointed[g] = point[g][Box[a]](x)\n}\n") match {
      case AST.TopImpl(_, _, _, _, methods, _) =>
        assertEquals(methods.map(_.body.show), Seq("Λ(g: * → *). Λa. λ[Pointed[g]]. λx: Box[a]. point[g][Box[a]](x)"))
      case other => fail(s"not TopImpl: $other")
    }
  }

  test("成功: 制約付きメソッドの呼び出しは、メソッドの型引数の後でも辞書が解決される") {
    val src = prelude + wrapBox +
      """let main(): i32 = match wrapIn[Box][Id][i32](MkBox[i32](7)) {
        |  MkId(b) => match b {
        |    MkBox(n) => n
        |  }
        |}
        |""".stripMargin
    assertRight(church(src))
    desugar(src) match {
      case Right(p) =>
        val shown = p.show
        assert(shown.contains("$inst_Wrap_Box"), clue(shown))
        assert(shown.contains("$inst_Pointed_Id"), clue(shown))
      case Left(err) => fail(err)
    }
  }

  test("成功: 制約付き関数の中から、スコープ中の制約で制約付きメソッドを呼べる") {
    val src = prelude + wrapBox +
      """let wrapAny[t: * → *][g: * → *][a](x: t[a]): g[t[a]] with Wrap[t], Pointed[g] = wrapIn[t][g][a](x)
        |let main(): i32 = 0
        |""".stripMargin
    assertRight(church(src))
  }

  test("検査失敗: メソッドの制約を満たすインスタンスがなければ No instance") {
    val src = prelude + wrapBox +
      """let main(): i32 = match wrapIn[Box][Box][i32](MkBox[i32](7)) {
        |  MkBox(b) => 0
        |}
        |""".stripMargin
    assertLeftContains(validate(src), "No instance for Pointed[Box]")
  }

  test("検査失敗: impl が with 節を省くとメソッドの型が合わない") {
    val src = prelude +
      """impl Wrap[Box] {
        |  def wrapIn[g: * → *][a](x: Box[a]): g[Box[a]] = point[g][Box[a]](x)
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "No instance for Pointed[g]")
  }

  test("検査失敗: 本体が制約を使わなくても、with 節のない impl は型が合わない") {
    val src =
      """data Box[a] = { MkBox(a) }
        |trait Pointed[g: * → *] {
        |  def point[a](x: a): g[a]
        |}
        |trait Keep[t: * → *] {
        |  def keep[g: * → *][a](x: t[a]): t[a] with Pointed[g]
        |}
        |impl Keep[Box] {
        |  def keep[g: * → *][a](x: Box[a]): Box[a] = x
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "Method type mismatch")
  }
}
