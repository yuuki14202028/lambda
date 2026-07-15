package com.yuuki14202028

class TraitCheckSuite extends munit.FunSuite {

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

  private val monadProgram =
    """data Maybe[a] = { Nothing, Just(a) }
      |
      |trait Monad[m: * → *] {
      |  def ret[a](x: a): m[a]
      |  def bind[a][b](v: m[a])(f: a → m[b]): m[b]
      |}
      |
      |impl Monad[Maybe] {
      |  def ret[a](x: a): m[a] = Just[a](x)
      |  def bind[a][b](v: Maybe[a])(f: a → Maybe[b]): Maybe[b] = match v {
      |    Nothing => Nothing[b]
      |    Just(x) => f(x)
      |  }
      |}
      |
      |let extract(v: Maybe[i32]): i32 = match v {
      |  Nothing => 0
      |  Just(x) => x
      |}
      |
      |let main(): i32 =
      |  extract(bind[Maybe][i32][i32](ret[Maybe][i32](41))(λx: i32. Just[i32](42)))
      |""".stripMargin

  private val showTrait =
    """trait Show[a] {
      |  def show: a → i32
      |}
      |""".stripMargin

  test("成功: trait/impl プログラムが型検査・脱糖・Church encoding を通る") {
    church(monadProgram) match {
      case Right(_)  => ()
      case Left(err) => fail(err)
    }
  }

  test("成功: 脱糖結果に辞書値と辞書型が現れる") {
    desugar(monadProgram) match {
      case Right(p) =>
        val shown = p.show
        assert(shown.contains("$inst_Monad_Maybe"), clue(shown))
        assert(shown.contains("MkMonad"), clue(shown))
        assert(shown.contains("$dict"), clue(shown))
      case Left(err) => fail(err)
    }
  }

  test("OverlappingInstance: 同一 (trait, head) の impl は拒否") {
    val src = showTrait +
      """impl Show[i32] {
        |  def show = λx: i32. x
        |}
        |impl Show[i32] {
        |  def show = λx: i32. 0
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "Overlapping instance")
  }

  test("MissingMethod: trait の全メソッドを与えない impl は拒否") {
    val src =
      """trait Pair[a] {
        |  def first: a → i32
        |  def second: a → i32
        |}
        |impl Pair[i32] {
        |  def first = λx: i32. x
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "Missing method")
  }

  test("ExtraMethod: trait にないメソッドを与える impl は拒否") {
    val src = showTrait +
      """impl Show[i32] {
        |  def show = λx: i32. x
        |  def extra = λx: i32. x
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "Extra method")
  }

  test("MethodTypeMismatch: 本体の型が特殊化済み署名と一致しない impl は拒否") {
    val src = showTrait +
      """impl Show[i32] {
        |  def show = λx: bool. 0
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "Method type mismatch")
  }

  test("カインド不一致: trait パラメータと合わない impl 対象は拒否") {
    val src =
      """data Maybe[a] = { Nothing, Just(a) }
        |""".stripMargin + showTrait +
      """impl Show[Maybe] {
        |  def show = λx: i32. x
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "Kind mismatch")
  }

  test("NoInstance: インスタンスのない型でのメソッド利用は脱糖で拒否") {
    val src = showTrait +
      """impl Show[i32] {
        |  def show = λx: i32. x
        |}
        |let main(): i32 = show[bool](true)
        |""".stripMargin
    assertLeftContains(desugar(src), "No instance")
  }

  test("AmbiguousConstraint: 型適用なしの裸参照は脱糖で拒否") {
    val src = showTrait +
      """impl Show[i32] {
        |  def show = λx: i32. x
        |}
        |let f: ∀a. a → i32 = show
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(desugar(src), "Ambiguous constraint")
  }

  test("シャドーイング: メソッド名を束縛し直した場合は辞書を挿入しない") {
    val src = showTrait +
      """impl Show[i32] {
        |  def show = λx: i32. x
        |}
        |let main(): i32 = (λshow: i32 → i32. show(1))(λx: i32. x)
        |""".stripMargin
    desugar(src) match {
      case Right(_)  => ()
      case Left(err) => fail(err)
    }
  }
}
