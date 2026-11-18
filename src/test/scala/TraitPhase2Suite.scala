package com.yuuki14202028

/** スーパークラス・文脈付きインスタンス・with 制約・rec impl */
class TraitPhase2Suite extends munit.FunSuite {

  private def assertError(source: String)(expected: PartialFunction[CompileError, Unit])(using munit.Location): Unit = {
    val found = Lam.errors(source)
    assert(found.exists(expected.isDefinedAt), clue(found.map(_.message)))
  }

  private def assertNoInstance(source: String, goal: String)(using munit.Location): Unit =
    assertError(source) { case error: CompileError.NoInstance if Type.show(error.goal) == goal => }

  private val eqOrd =
    """trait Eq[a] {
      |  def eq(x: a)(y: a): bool
      |}
      |
      |trait Ord[a] with Eq[a] {
      |  def lt(x: a)(y: a): bool
      |}
      |
      |impl Eq[i32] {
      |  def eq = λx: i32. λy: i32. true
      |}
      |
      |impl Ord[i32] {
      |  def lt = λx: i32. λy: i32. false
      |}
      |""".stripMargin

  private val showMaybe =
    """data Maybe[a] = { Nothing, Just(a) }
      |
      |trait Show[a] {
      |  def show(x: a): i32
      |}
      |
      |impl Show[i32] {
      |  def show = λx: i32. x
      |}
      |
      |impl[a] Show[Maybe[a]] with Show[a] {
      |  def show = λm: Maybe[a]. match m {
      |    Nothing => 0
      |    Just(x) => show[a](x)
      |  }
      |}
      |""".stripMargin

  test("スーパークラス: trait/impl がコンパイルでき、メソッドを呼べる") {
    val src = eqOrd + "let main(): i32 = if lt[i32](1)(2) then 1 else 0\n"
    assertEquals(Lam.exitCode(src), 0)
  }

  test("スーパークラス: 辞書に上位辞書が埋め込まれる（構成子の最初のフィールド）") {
    val src = eqOrd +
      """let custom: Ord[i32] = MkOrd[i32](MkEq[i32](λx: i32. λy: i32. false))(λx: i32. λy: i32. true)
        |let same[a](x: a): bool with Ord[a] = eq[a](x)(x)
        |let main(): i32 = if same[i32](with custom)(1) then 1 else 2
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 2)
  }

  test("スーパークラス経由の解決: with Ord[a] の局所辞書から Eq[a] を射影する") {
    val src = eqOrd +
      """let same[a](x: a): bool with Ord[a] = eq[a](x)(x)
        |let main(): i32 = if same[i32](1) then 1 else 0
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 1)
  }

  test("スーパークラス: 上位インスタンスがない impl は型検査で No instance") {
    val src = eqOrd +
      """impl Ord[bool] {
        |  def lt = λx: bool. λy: bool. true
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertNoInstance(src, "Eq[bool]")
  }

  test("文脈付きインスタンス: Show[Maybe[a]] with Show[a] が解決される") {
    val src = showMaybe + "let main(): i32 = show[Maybe[i32]](Just[i32](41))\n"
    assertEquals(Lam.exitCode(src), 41)
  }

  test("文脈付きインスタンス: ネストした解決 Show[Box[Maybe[i32]]]") {
    val src = showMaybe +
      """data Box[a] = { MkBox(a) }
        |impl[a] Show[Box[a]] with Show[a] {
        |  def show = λb: Box[a]. match b {
        |    MkBox(x) => show[a](x)
        |  }
        |}
        |let main(): i32 = show[Box[Maybe[i32]]](MkBox[Maybe[i32]](Just[i32](1)))
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 1)
  }

  test("文脈付きインスタンス: 文脈を満たせない場合は No instance") {
    val src = showMaybe + "let main(): i32 = show[Maybe[bool]](Nothing[bool])\n"
    assertNoInstance(src, "Show[bool]")
  }

  test("文脈付きインスタンス: 同一 head は Overlapping instance") {
    val src = showMaybe +
      """impl Show[Maybe[i32]] {
        |  def show = λm: Maybe[i32]. 0
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertError(src) { case _: CompileError.OverlappingInstance => }
  }

  test("impl の型パラメータが対象に現れない場合は拒否") {
    val src = showMaybe.replace(
      "impl[a] Show[Maybe[a]] with Show[a]",
      "impl[a][b] Show[Maybe[a]] with Show[a]"
    ) + "let main(): i32 = 0\n"
    assertError(src) { case CompileError.ImplParameterUnused("b", _) => }
  }

  test("with 制約付き関数: 呼び出し側で辞書が挿入される") {
    val src = showMaybe +
      """let render[a](x: a): i32 with Show[a] = show[a](x)
        |let main(): i32 = render[Maybe[i32]](Just[i32](7))
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 7)
  }

  test("with 制約付き再帰関数: 再帰呼び出しにも辞書が挿入される") {
    val src = showMaybe +
      """rec let render[a](x: a)(again: bool): i32 with Show[a] =
        |  if again then render[a](x)(false) else show[a](x)
        |let main(): i32 = render[i32](7)(true)
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 7)
  }

  test("with 制約付き関数: インスタンスのない型での呼び出しは No instance") {
    val src = showMaybe +
      """let render[a](x: a): i32 with Show[a] = show[a](x)
        |let main(): i32 = render[bool](true)
        |""".stripMargin
    assertNoInstance(src, "Show[bool]")
  }

  test("with 制約付き関数: 宣言した型パラメータの先でさらに多相でもよい") {
    val src = showMaybe +
      """let f[a]: ∀b. b → i32 with Show[a] = Λb. λx: b. 3
        |let main(): i32 = f[i32][bool](true)
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 3)
  }

  test("No instance は使用箇所の位置付きで報告される") {
    val src = showMaybe +
      """let main(): i32 =
        |  show[bool](true)
        |""".stripMargin
    val rendered = Diagnostics.render(Lam.errors(src), "test.lam", src)
    val line = src.linesIterator.indexWhere(_.contains("show[bool]")) + 1
    assert(rendered.contains("no instance for `Show[bool]`"), clue(rendered))
    assert(rendered.contains(s"--> test.lam:$line:3"), clue(rendered))
  }

  test("インスタンスは宣言してから使う: impl より前の使用は No instance") {
    val src =
      """trait Show[a] {
        |  def show(x: a): i32
        |}
        |let early(): i32 = show[i32](1)
        |impl Show[i32] {
        |  def show = λx: i32. x
        |}
        |let main(): i32 = early()
        |""".stripMargin
    assertNoInstance(src, "Show[i32]")
  }

  test("インスタンスは自身のメソッド本体からはまだ見えない") {
    val src = showMaybe +
      """rec data L[a] = { N, C(a)(L[a]) }
        |impl[a] Show[L[a]] with Show[a] {
        |  def show = λl: L[a]. match l {
        |    N => 0
        |    C(x)(rest) => show[L[a]](rest)
        |  }
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertNoInstance(src, "Show[L[a]]")
  }

  test("rec impl: メソッド本体から自身のインスタンスを使える") {
    val src = showMaybe +
      """let add(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y)
        |rec data L[a] = { N, C(a)(L[a]) }
        |rec impl[a] Show[L[a]] with Show[a] {
        |  def show = λl: L[a]. match l {
        |    N => 0
        |    C(x)(rest) => add(show[a](x))(show[L[a]](rest))
        |  }
        |}
        |let main(): i32 = show[L[i32]](C[i32](1)(C[i32](2)(N[i32])))
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 3)
  }

  test("rec impl: 型パラメータも文脈もない形でパースできる") {
    val src =
      """let add(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y)
        |rec data Nat = { Z, S(Nat) }
        |trait Size[a] {
        |  def size(x: a): i32
        |}
        |rec impl Size[Nat] {
        |  def size = λn: Nat. match n {
        |    Z => 0
        |    S(m) => add(1)(size[Nat](m))
        |  }
        |}
        |let main(): i32 = size[Nat](S(S(Z)))
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 2)
  }

  test("impl の型パラメータが trait のパラメータと同名でも、宣言シグネチャは impl 側を指す") {
    val src =
      """data Maybe[a] = { Nothing, Just(a) }
        |trait Show[a] {
        |  def show(x: a): i32
        |}
        |impl[a] Show[Maybe[a]] {
        |  def show(m: Maybe[a]): i32 = 5
        |}
        |let main(): i32 = show[Maybe[bool]](Nothing[bool])
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 5)
  }

  test("宣言シグネチャの返り値型では trait のパラメータ名をターゲットとして使える") {
    val src =
      """data Maybe[a] = { Nothing, Just(a) }
        |trait Empty[s] {
        |  def empty(u: unit): s
        |}
        |impl[a] Empty[Maybe[a]] {
        |  def empty(u: unit): s = Nothing[a]
        |}
        |let main(): i32 = match empty[Maybe[i32]](()) {
        |  Nothing => 1
        |  Just(_) => 0
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 1)
  }

  test("宣言シグネチャが trait と合わなければ拒否") {
    val src =
      """data Maybe[a] = { Nothing, Just(a) }
        |trait Show[a] {
        |  def show(x: a): i32
        |}
        |impl[a] Show[Maybe[a]] {
        |  def show(m: Maybe[a]): bool = true
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertError(src) { case CompileError.MethodTypeMismatch("impl Show[Maybe[a]]", "show", _, _, _) => }
  }

  test("with 制約は in 式の let でも使える") {
    val src = showMaybe +
      """let main(): i32 =
        |  let twice[a](x: a): i32 with Show[a] = show[a](x) in twice[i32](3)
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 3)
  }

  test("制約のカインド不一致は拒否") {
    val src =
      """trait Functor[f: * → *] {
        |  def fmap[a][b](g: a → b)(v: f[a]): f[b]
        |}
        |let f[a](x: a): i32 with Functor[a] = 0
        |let main(): i32 = 0
        |""".stripMargin
    assertError(src) { case _: CompileError.KindMismatch => }
  }
}
