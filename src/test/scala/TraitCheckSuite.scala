package com.yuuki14202028

class TraitCheckSuite extends munit.FunSuite {

  private def assertError(source: String)(expected: PartialFunction[CompileError, Unit])(using munit.Location): Unit = {
    val found = Lam.errors(source)
    assert(found.exists(expected.isDefinedAt), clue(found.map(_.message)))
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

  test("成功: trait/impl プログラムがコンパイルでき、メソッドがインスタンスの実装を呼ぶ") {
    assertEquals(Lam.exitCode(monadProgram), 42)
  }

  test("成功: 辞書は構成子 MkMonad の値として組み立てられる") {
    val src = monadProgram.replace("let main", "let unused") +
      """let identity: Monad[Maybe] = MkMonad[Maybe](Λa. λx: a. Just[a](x))(Λa. Λb. λv: Maybe[a]. λf: a → Maybe[b]. Nothing[b])
        |let main(): i32 = extract(bind[Maybe](with identity)[i32][i32](Just[i32](1))(λx: i32. Just[i32](x)))
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 0)
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
    assertError(src) { case _: CompileError.OverlappingInstance => }
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
    assertError(src) { case CompileError.MissingMethods("impl Pair[i32]", List("second"), _) => }
  }

  test("ExtraMethod: trait にないメソッドを与える impl は拒否") {
    val src = showTrait +
      """impl Show[i32] {
        |  def show = λx: i32. x
        |  def extra = λx: i32. x
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertError(src) { case CompileError.ExtraMethod("Show", "extra", _) => }
  }

  test("MethodTypeMismatch: 本体の型が特殊化済み署名と一致しない impl は拒否") {
    val src = showTrait +
      """impl Show[i32] {
        |  def show = λx: bool. 0
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertError(src) { case CompileError.MethodTypeMismatch("impl Show[i32]", "show", _, _, _) => }
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
    assertError(src) { case _: CompileError.KindMismatch => }
  }

  test("NoInstance: インスタンスのない型でのメソッド利用は型検査で拒否") {
    val src = showTrait +
      """impl Show[i32] {
        |  def show = λx: i32. x
        |}
        |let main(): i32 = show[bool](true)
        |""".stripMargin
    assertError(src) { case error: CompileError.NoInstance if error.message.contains("Show[bool]") => }
  }

  test("型適用なしの裸参照は、制約が残った型になり型検査で拒否") {
    val src = showTrait +
      """impl Show[i32] {
        |  def show = λx: i32. x
        |}
        |let f: ∀a. a → i32 = show
        |let main(): i32 = 0
        |""".stripMargin
    assertError(src) {
      case error: CompileError.TypeMismatch if error.message.contains("expected `∀a. a → i32`, found `∀a. [Show[a]] → a → i32`") =>
    }
  }

  test("シャドーイング: メソッド名を束縛し直した場合は辞書を挿入しない") {
    val src = showTrait +
      """impl Show[i32] {
        |  def show = λx: i32. 0
        |}
        |let main(): i32 = (λshow: i32 → i32. show(1))(λx: i32. x)
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 1)
  }

  test("impl の型パラメータが trait やメソッドの型パラメータと同じ名前でも、メソッドの型を正しく求める") {
    val src =
      """trait Functor[f: * → *] { def map[a][b](x: f[a])(g: a → b): f[b] }
        |data Pair[a][b] = { MkPair(a)(b) }
        |impl[a] Functor[Pair[a]] { def map[c][d](x: Pair[a][c])(g: c → d): Pair[a][d] = match x { MkPair(p)(q) => MkPair[a][d](p)(g(q)) } }
        |trait Conv[a][b] { def conv(x: a): b }
        |impl[b][a] Conv[Pair[a][b]][Pair[b][a]] { def conv(x: Pair[a][b]): Pair[b][a] = match x { MkPair(p)(q) => MkPair[b][a](q)(p) } }
        |trait Semigroup[a] { def append(x: a)(y: a): a }
        |data Box[a] = { MkBox(a) }
        |impl[a] Semigroup[Box[a]] { def append(x: Box[a])(y: Box[a]): Box[a] = x }
        |let main(): i32 =
        |  match conv[Pair[bool][i32]][Pair[i32][bool]](map[Pair[bool]][i32][i32](MkPair[bool][i32](true)(6))(λn: i32. 7)) {
        |    MkPair(n)(_) => match append[Box[i32]](MkBox[i32](n))(MkBox[i32](0)) { MkBox(m) => m }
        |  }
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 7)
  }
}
