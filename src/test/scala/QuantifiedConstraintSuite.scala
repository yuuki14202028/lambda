package com.yuuki14202028

class QuantifiedConstraintSuite extends munit.FunSuite {

  private def validate(src: String): Either[String, TypeRec[AST.Program.type]] =
    ParserAST.programParser.parseAll(src).left.map(e => s"Parse error: $e").flatMap(p => TAnalyser.validate(p).left.map(_.render))

  private def encode(src: String): Either[String, TypeRec[AST.Program.type]] =
    validate(src).map(p => ChurchEncoder.encode(TraitEncoder.encode(ContextDesugar.desugar(Deriver.desugar(PatternDesugar.desugar(ModuleDesugar.desugar(p)))))))

  private def assertRight(result: Either[String, ?]): Unit = result match {
    case Right(_) => ()
    case Left(err) => fail(err)
  }

  private def assertLeftContains(result: Either[String, ?], expected: String): Unit = result match {
    case Left(err) => assert(err.contains(expected), clue(err))
    case Right(_) => fail(s"expected failure containing: $expected")
  }

  private val prelude =
    """trait Size[a] { def size(x: a): i32 }
      |impl Size[i32] { def size = λx: i32. x }
      |data Box[a] = { MkBox(a) }
      |impl[a] Size[Box[a]] with Size[a] { def size = λb: Box[a]. match b { MkBox(x) => size[a](x) } }
      |data Bare[a] = { MkBare(a) }
      |let twice[f: * → *](x: f[i32]): i32 with ∀a. Size[a] ⇒ Size[f[a]] = size[f[i32]](x)
      |""".stripMargin

  private val main = "let main(): i32 = 0\n"

  test("量化された制約は型の中で [∀a. P ⇒ C] → … と表示される") {
    val env = validate(prelude + main).fold(fail(_), _.extract match { case ProgramAnn(e) => e })
    assertEquals(env.values(Variable("twice")).show, "∀(f: * → *). [∀a. Size[a] ⇒ Size[f[a]]] → f[i32] → i32")
  }

  test("本体の中では、量化された制約をインスタンスのように使える") {
    assertRight(encode(prelude + main))
  }

  test("前提が満たせなければ、本体の中でも解決できない") {
    val src = prelude + "let bad[f: * → *](x: f[bool]): i32 with ∀a. Size[a] ⇒ Size[f[a]] = size[f[bool]](x)\n"
    assertLeftContains(validate(src + main), "No instance")
  }

  test("呼び出し側では、量化された制約をインスタンスから組み立てる") {
    val shown = encode(prelude + "let n: i32 = twice[Box](MkBox[i32](1))\n" + main).fold(fail(_), _.show)
    assert(shown.contains("$inst_Size_Box"), clue(shown))
  }

  test("呼び出し側でインスタンスがなければエラー") {
    assertLeftContains(validate(prelude + "let n: i32 = twice[Bare](MkBare[i32](1))\n" + main), "No instance")
  }

  test("呼び出し側のインスタンスが別の前提を要求していればエラー") {
    val src = prelude +
      """trait Other[a] { def other(x: a): i32 }
        |data Needy[a] = { MkNeedy(a) }
        |impl[a] Size[Needy[a]] with Other[a] { def size = λn: Needy[a]. 0 }
        |let n: i32 = twice[Needy](MkNeedy[i32](1))
        |""".stripMargin
    assertLeftContains(validate(src + main), "No instance")
  }

  test("同じ量化された制約は、束縛変数の名前が違ってもそのまま渡せる") {
    val src = prelude + "let again[g: * → *](x: g[i32]): i32 with ∀b. Size[b] ⇒ Size[g[b]] = twice[g](x)\n"
    val shown = validate(src + main).fold(fail(_), _.show)
    assert(shown.contains("given ∀b. Size[b] ⇒ Size[g[b]]"), clue(shown))
  }

  test("型引数の代入で、量化された制約の束縛変数は捕獲されない") {
    val src = prelude +
      """data Pair[x][y] = { MkPair(x)(y) }
        |impl[x][y] Size[Pair[x][y]] with Size[x], Size[y] { def size = λp: Pair[x][y]. 0 }
        |let sameName[a](x: Pair[a][i32]): i32 with Size[a] = twice[Pair[a]](x)
        |""".stripMargin
    assertRight(encode(src + main))
  }

  test("impl の文脈に書ける（高階の不動点型のインスタンス）") {
    val src = prelude +
      """rec data Fix[f: * → *] = { In(f[Fix[f]]) }
        |rec impl[f: * → *] Size[Fix[f]] with ∀a. Size[a] ⇒ Size[f[a]] {
        |  def size = λx: Fix[f]. match x { In(inner) => size[f[Fix[f]]](inner) }
        |}
        |let measure(x: Fix[Box]): i32 = size[Fix[Box]](x)
        |""".stripMargin
    assertRight(encode(src + main))
  }

  test("前提のない量化された制約と、複数の前提") {
    val src = prelude +
      """trait Other[a] { def other(x: a): i32 }
        |let always[f: * → *](x: f[bool]): i32 with ∀a. Size[f[a]] = size[f[bool]](x)
        |let both[f: * → *](x: f[i32]): i32 with ∀a. Size[a], Other[a] ⇒ Size[f[a]], Other[i32] = size[f[i32]](x)
        |""".stripMargin
    assertRight(encode(src + main))
  }

  test("trait のメソッドにも書ける") {
    val src = prelude +
      """trait Measure[t] { def measure[f: * → *](x: t)(y: f[i32]): i32 with ∀a. Size[a] ⇒ Size[f[a]] }
        |impl Measure[i32] { def measure[f: * → *](x: i32)(y: f[i32]): i32 with ∀a. Size[a] ⇒ Size[f[a]] = size[f[i32]](y) }
        |let n: i32 = measure[i32][Box](1)(MkBox[i32](2))
        |""".stripMargin
    assertRight(encode(src + main))
  }

  test("束縛変数はヘッドの引数に現れなければならない") {
    val src = prelude + "let bad[f: * → *](x: f[i32]): i32 with ∀a. Size[a] ⇒ Size[f[i32]] = 0\n"
    assertLeftContains(validate(src + main), "does not appear")
  }

  test("束縛変数はスコープ中の型変数と同じ名前にできない") {
    val src = prelude + "let bad[f: * → *][a](x: f[a]): i32 with ∀a. Size[a] ⇒ Size[f[a]] = 0\n"
    assertLeftContains(validate(src + main), "already defined")
  }

  // ---- スーパークラス ----

  private val deep = prelude +
    """trait Deep[f: * → *] with ∀a. Size[a] ⇒ Size[f[a]] { def deep(x: f[i32]): i32 }
      |""".stripMargin

  test("スーパークラスに書ける: インスタンスは、その量化された制約を満たさなければならない") {
    assertRight(encode(deep + "impl Deep[Box] { def deep = λb: Box[i32]. size[Box[i32]](b) }\n" + main))
    assertLeftContains(validate(deep + "impl Deep[Bare] { def deep = λb: Bare[i32]. 0 }\n" + main), "No instance")
  }

  test("スーパークラスの量化された制約は、その trait の制約からインスタンスのように使える") {
    val src = deep +
      """let viaSuper[f: * → *](x: f[i32]): i32 with Deep[f] = size[f[i32]](x)
        |let twiceVia[f: * → *](x: f[f[i32]]): i32 with Deep[f] = size[f[f[i32]]](x)
        |impl Deep[Box] { def deep = λb: Box[i32]. 0 }
        |let n: i32 = viaSuper[Box](MkBox[i32](1))
        |""".stripMargin
    val shown = encode(src + main).fold(fail(_), _.show)
    assert(shown.contains("$super_Deep_0"), clue(shown))
  }

  test("スーパークラスの量化された制約の前提が満たせなければエラー") {
    val src = deep + "let bad[f: * → *](x: f[bool]): i32 with Deep[f] = size[f[bool]](x)\n"
    assertLeftContains(validate(src + main), "No instance")
  }

  test("スーパークラスの量化された制約を、量化された制約を要求する関数へ渡せる") {
    assertRight(encode(deep + "let pass[f: * → *](x: f[i32]): i32 with Deep[f] = twice[f](x)\n" + main))
  }

  test("デフォルトメソッドの本体でも、量化されたスーパークラスを使える") {
    val src = prelude +
      """trait Deep[f: * → *] with ∀a. Size[a] ⇒ Size[f[a]] {
        |  def start(x: f[i32]): f[i32]
        |  def measure(x: f[i32]): i32 = size[f[i32]](start(x))
        |}
        |impl Deep[Box] { def start = λb: Box[i32]. b }
        |let n: i32 = measure[Box](MkBox[i32](1))
        |""".stripMargin
    assertRight(encode(src + main))
  }

  test("量化されたスーパークラスから得た制約の、さらにスーパークラスも使える") {
    val src =
      """trait Base[f: * → *] { def base[a](x: a): f[a] }
        |trait Derived[f: * → *] with Base[f] { def derived[a](x: f[a]): f[a] }
        |trait Lifting[t: (* → *) → * → *] with ∀(m: * → *). Derived[m] ⇒ Derived[t[m]] {
        |  def lift[m: * → *][a](x: m[a]): t[m][a] with Derived[m]
        |}
        |let viaBase[t: (* → *) → * → *][m: * → *](x: i32): t[m][i32] with Lifting[t], Derived[m] = base[t[m]][i32](x)
        |let viaDerived[t: (* → *) → * → *][m: * → *](x: t[m][i32]): t[m][i32] with Lifting[t], Derived[m] = derived[t[m]][i32](x)
        |""".stripMargin
    val shown = encode(src + main).fold(fail(_), _.show)
    assert(shown.contains("$super_Derived_0"), clue(shown))
  }

  // ---- 前提の入れ子 ----

  private val compose = prelude +
    """data Compose[f: * → *][g: * → *][a] = { MkCompose(f[g[a]]) }
      |impl[f: * → *][g: * → *][a] Size[Compose[f][g][a]] with ∀b. Size[b] ⇒ Size[f[b]], ∀b. Size[b] ⇒ Size[g[b]], Size[a] {
      |  def size = λc: Compose[f][g][a]. match c { MkCompose(inner) => size[f[g[a]]](inner) }
      |}
      |let nested[h: (* → *) → * → *](x: h[Box][i32]): i32 with ∀(f: * → *). (∀b. Size[b] ⇒ Size[f[b]]) ⇒ Size[h[f][i32]] =
      |  size[h[Box][i32]](x)
      |""".stripMargin

  test("前提に量化された制約を書ける（括弧で囲む）") {
    val env = validate(compose + main).fold(fail(_), _.extract match { case ProgramAnn(e) => e })
    assertEquals(
      env.values(Variable("nested")).show,
      "∀(h: (* → *) → * → *). [∀(f: * → *). (∀b. Size[b] ⇒ Size[f[b]]) ⇒ Size[h[f][i32]]] → h[Box][i32] → i32"
    )
    assertRight(encode(compose + main))
  }

  test("前提が量化された制約は、インスタンスから組み立てられる") {
    assertRight(encode(compose + "let n: i32 = nested[Compose[Box]](MkCompose[Box][Box][i32](MkBox[Box[i32]](MkBox[i32](1))))\n" + main))
  }

  test("量化された前提が満たせなければエラー") {
    val src = compose + "let bad[h: (* → *) → * → *](x: h[Bare][i32]): i32 with ∀(f: * → *). (∀b. Size[b] ⇒ Size[f[b]]) ⇒ Size[h[f][i32]] = size[h[Bare][i32]](x)\n"
    assertLeftContains(validate(src + main), "No instance")
  }

  test("入れ子の束縛変数も、型引数の代入で捕獲されない") {
    val src = compose +
      """let outer[b](x: Compose[Box][Box][b]): i32 with Size[b] = size[Compose[Box][Box][b]](x)
        |let renamed[h: (* → *) → * → *][f: * → *](x: h[Box][i32]): i32 with ∀(g: * → *). (∀c. Size[c] ⇒ Size[g[c]]) ⇒ Size[h[g][i32]] =
        |  nested[h](x)
        |""".stripMargin
    assertRight(encode(src + main))
  }

  test("入れ子にした非再帰のデータ型を符号化できる") {
    val src =
      """data Option[a] = { None, Some(a) }
        |let x: Option[Option[i32]] = Some[Option[i32]](None[i32])
        |let n: i32 = match x { None => 1 Some(inner) => 0 }
        |""".stripMargin
    assertRight(encode(src + main))
  }
}
