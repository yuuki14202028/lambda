package com.yuuki14202028

class TraitPhase2Suite extends munit.FunSuite {

  private def validate(src: String): Either[String, TypeRec[AST.Program.type]] =
    ParserAST.programParser.parseAll(src).left.map(e => s"Parse error: $e").flatMap(p => TAnalyser.validate(p).left.map(_.render))

  private def desugar(src: String): Either[String, TypeRec[AST.Program.type]] =
    validate(src).map(TraitEncoder.encode)

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

  test("スーパークラス: trait/impl が型検査・脱糖・Church encoding を通る") {
    val src = eqOrd + "let main(): i32 = if lt[i32](1)(2) then 1 else 0\n"
    assertRight(church(src))
  }

  test("スーパークラス: 辞書に上位辞書が埋め込まれ、射影 let が生成される") {
    val src = eqOrd + "let main(): i32 = 0\n"
    desugar(src) match {
      case Right(p) =>
        val shown = p.show
        assert(shown.contains("$super_Ord_0"), clue(shown))
        assert(shown.contains("$inst_Eq_i32"), clue(shown))
        assert(shown.contains("$inst_Ord_i32"), clue(shown))
      case Left(err) => fail(err)
    }
  }

  test("スーパークラス経由の解決: with Ord[a] の局所辞書から Eq[a] を射影する") {
    val src = eqOrd +
      """let same[a](x: a): bool with Ord[a] = eq[a](x)(x)
        |let main(): i32 = if same[i32](1) then 1 else 0
        |""".stripMargin
    assertRight(church(src))
    desugar(src) match {
      case Right(p) =>
        val shown = p.show
        assert(shown.contains("$super_Ord_0"), clue(shown))
        assert(shown.contains("$with_0"), clue(shown))
      case Left(err) => fail(err)
    }
  }

  test("スーパークラス: 上位インスタンスがない impl は型検査で No instance") {
    val src = eqOrd +
      """impl Ord[bool] {
        |  def lt = λx: bool. λy: bool. true
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "No instance for Eq[bool]")
  }

  test("文脈付きインスタンス: Show[Maybe[a]] with Show[a] が解決される") {
    val src = showMaybe + "let main(): i32 = show[Maybe[i32]](Just[i32](41))\n"
    assertRight(church(src))
    desugar(src) match {
      case Right(p) =>
        val shown = p.show
        assert(shown.contains("$inst_Show_Maybe"), clue(shown))
        assert(shown.contains("$inst_Show_i32"), clue(shown))
        assert(shown.contains("$ctx_0"), clue(shown))
      case Left(err) => fail(err)
    }
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
    assertRight(church(src))
  }

  test("文脈付きインスタンス: 文脈を満たせない場合は No instance") {
    val src = showMaybe + "let main(): i32 = show[Maybe[bool]](Nothing[bool])\n"
    assertLeftContains(validate(src), "No instance for Show[bool]")
  }

  test("文脈付きインスタンス: 同一 head は Overlapping instance") {
    val src = showMaybe +
      """impl Show[Maybe[i32]] {
        |  def show = λm: Maybe[i32]. 0
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "Overlapping instance")
  }

  test("impl の型パラメータが対象に現れない場合は拒否") {
    val src = showMaybe.replace(
      "impl[a] Show[Maybe[a]] with Show[a]",
      "impl[a][b] Show[Maybe[a]] with Show[a]"
    ) + "let main(): i32 = 0\n"
    assertLeftContains(validate(src), "must occur in the instance target")
  }

  test("with 制約付き関数: 呼び出し側で辞書が挿入される") {
    val src = showMaybe +
      """let render[a](x: a): i32 with Show[a] = show[a](x)
        |let main(): i32 = render[Maybe[i32]](Just[i32](7))
        |""".stripMargin
    assertRight(church(src))
    desugar(src) match {
      case Right(p) =>
        val shown = p.show
        assert(shown.contains("$with_0"), clue(shown))
      case Left(err) => fail(err)
    }
  }

  test("with 制約付き再帰関数: 再帰呼び出しにも辞書が挿入される") {
    val src = showMaybe +
      """rec let render[a](x: a)(n: i32): i32 with Show[a] =
        |  if true then show[a](x) else render[a](x)(n)
        |let main(): i32 = render[i32](7)(1)
        |""".stripMargin
    assertRight(church(src))
  }

  test("with 制約付き関数: インスタンスのない型での呼び出しは No instance") {
    val src = showMaybe +
      """let render[a](x: a): i32 with Show[a] = show[a](x)
        |let main(): i32 = render[bool](true)
        |""".stripMargin
    assertLeftContains(validate(src), "No instance for Show[bool]")
  }

  test("with 制約付き関数: 宣言した型パラメータの先でさらに多相でもよい") {
    val src = showMaybe +
      """let f[a]: ∀b. b → i32 with Show[a] = Λb. λx: b. 0
        |let main(): i32 = f[i32][bool](true)
        |""".stripMargin
    assertRight(church(src))
  }

  test("No instance は使用箇所の位置付きで報告される") {
    val src = showMaybe +
      """let main(): i32 =
        |  show[bool](true)
        |""".stripMargin
    val rendered = ParserAST.programParser.parseAll(src).toOption
      .flatMap(p => TAnalyser.validate(p).left.toOption)
      .map(_.render("t.lam", cats.parse.LocationMap(src)))
    val line = src.linesIterator.indexWhere(_.contains("show[bool]")) + 1
    assert(rendered.exists(_.startsWith(s"t.lam:$line:3: No instance for Show[bool]")), clue(rendered))
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
    assertLeftContains(validate(src), "No instance for Show[i32]")
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
    assertLeftContains(validate(src), "No instance for Show[L[a]]")
  }

  test("rec impl: メソッド本体から自身のインスタンスを使える") {
    val src = showMaybe +
      """rec data L[a] = { N, C(a)(L[a]) }
        |rec impl[a] Show[L[a]] with Show[a] {
        |  def show = λl: L[a]. match l {
        |    N => 0
        |    C(x)(rest) => show[L[a]](rest)
        |  }
        |}
        |let main(): i32 = show[L[i32]](C[i32](1)(N[i32]))
        |""".stripMargin
    assertRight(church(src))
    desugar(src) match {
      case Right(p) =>
        val shown = p.show
        assert(shown.contains("rec let $inst_Show_L"), clue(shown))
        assert(!shown.contains("rec let $inst_Show_Maybe"), clue(shown))
      case Left(err) => fail(err)
    }
  }

  test("rec impl: 型パラメータも文脈もない形でパースできる") {
    val src =
      """rec data Nat = { Z, S(Nat) }
        |trait Size[a] {
        |  def size(x: a): i32
        |}
        |rec impl Size[Nat] {
        |  def size = λn: Nat. match n {
        |    Z => 0
        |    S(m) => size[Nat](m)
        |  }
        |}
        |let main(): i32 = size[Nat](S(Z))
        |""".stripMargin
    assertRight(church(src))
  }

  test("impl の型パラメータが trait のパラメータと同名でも、宣言シグネチャは impl 側を指す") {
    val src =
      """data Maybe[a] = { Nothing, Just(a) }
        |trait Show[a] {
        |  def show(x: a): i32
        |}
        |impl[a] Show[Maybe[a]] {
        |  def show(m: Maybe[a]): i32 = 0
        |}
        |let main(): i32 = show[Maybe[bool]](Nothing[bool])
        |""".stripMargin
    assertRight(church(src))
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
        |let main(): i32 = 0
        |""".stripMargin
    assertRight(church(src))
  }

  test("宣言シグネチャが trait と合わなければ拒否") {
    val src =
      """data Maybe[a] = { Nothing, Just(a) }
        |trait Show[a] {
        |  def show(x: a): i32
        |}
        |impl[a] Show[Maybe[a]] {
        |  def show(m: Maybe[a]): bool = 0
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "Method type mismatch")
  }

  test("with 制約は in 式の let でも使える") {
    val src = showMaybe +
      """let main(): i32 =
        |  let twice[a](x: a): i32 with Show[a] = show[a](x) in twice[i32](3)
        |""".stripMargin
    assertRight(church(src))
  }

  test("制約のカインド不一致は拒否") {
    val src =
      """trait Functor[f: * → *] {
        |  def fmap[a][b](g: a → b)(v: f[a]): f[b]
        |}
        |let f[a](x: a): i32 with Functor[a] = 0
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "kind")
  }
}
