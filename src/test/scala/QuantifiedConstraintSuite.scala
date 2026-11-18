package com.yuuki14202028

/** 量化された制約: with ∀a. Size[a] ⇒ Size[f[a]] */
class QuantifiedConstraintSuite extends munit.FunSuite {

  private def assertError(source: String)(expected: PartialFunction[CompileError, Unit])(using munit.Location): Unit = {
    val found = Lam.errors(source)
    assert(found.exists(expected.isDefinedAt), clue(found.map(_.message)))
  }

  private def assertNoInstance(source: String, goal: String)(using munit.Location): Unit =
    assertError(source) { case error: CompileError.NoInstance if Type.show(error.goal) == goal => }

  private val prelude =
    """let add(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y)
      |trait Size[a] { def size(x: a): i32 }
      |impl Size[i32] { def size = λx: i32. x }
      |data Box[a] = { MkBox(a) }
      |impl[a] Size[Box[a]] with Size[a] { def size = λb: Box[a]. match b { MkBox(x) => add(1)(size[a](x)) } }
      |data Bare[a] = { MkBare(a) }
      |let twice[f: * → *](x: f[i32]): i32 with ∀a. Size[a] ⇒ Size[f[a]] = size[f[i32]](x)
      |""".stripMargin

  private val main = "let main(): i32 = 0\n"

  private def mainOf(result: String): String = s"let main(): i32 = $result\n"

  test("量化された制約は、型の中では辞書を受け取る多相な引数 [∀a. [P] → C] → … になる") {
    // 型は、不一致のエラーの表示から読む
    assertError(prelude + "let bad: i32 = twice\n" + main) {
      case error: CompileError.TypeMismatch if error.message.contains("found `∀(f: * → *). [∀a. [Size[a]] → Size[f[a]]] → f[i32] → i32`") =>
    }
  }

  test("本体の中では、量化された制約をインスタンスのように使える") {
    assertEquals(Lam.exitCode(prelude + mainOf("twice[Box](MkBox[i32](1))")), 2)
  }

  test("前提が満たせなければ、本体の中でも解決できない") {
    val src = prelude + "let bad[f: * → *](x: f[bool]): i32 with ∀a. Size[a] ⇒ Size[f[a]] = size[f[bool]](x)\n"
    assertNoInstance(src + main, "Size[bool]")
  }

  test("呼び出し側では、量化された制約をインスタンスから組み立てる") {
    assertEquals(Lam.exitCode(prelude + "let n: i32 = twice[Box](MkBox[i32](5))\n" + mainOf("n")), 6)
  }

  test("呼び出し側でインスタンスがなければエラー") {
    assertError(prelude + "let n: i32 = twice[Bare](MkBare[i32](1))\n" + main) { case _: CompileError.NoInstance => }
  }

  test("呼び出し側のインスタンスが別の前提を要求していればエラー") {
    val src = prelude +
      """trait Other[a] { def other(x: a): i32 }
        |data Needy[a] = { MkNeedy(a) }
        |impl[a] Size[Needy[a]] with Other[a] { def size = λn: Needy[a]. 0 }
        |let n: i32 = twice[Needy](MkNeedy[i32](1))
        |""".stripMargin
    assertError(src + main) { case _: CompileError.NoInstance => }
  }

  test("同じ量化された制約は、束縛変数の名前が違ってもそのまま渡せる") {
    val src = prelude + "let again[g: * → *](x: g[i32]): i32 with ∀b. Size[b] ⇒ Size[g[b]] = twice[g](x)\n"
    assertEquals(Lam.exitCode(src + mainOf("again[Box](MkBox[i32](3))")), 4)
  }

  test("型引数の代入で、量化された制約の束縛変数は捕獲されない") {
    val src = prelude +
      """data Pair[x][y] = { MkPair(x)(y) }
        |impl[x][y] Size[Pair[x][y]] with Size[x], Size[y] {
        |  def size = λp: Pair[x][y]. match p { MkPair(l)(r) => add(size[x](l))(size[y](r)) }
        |}
        |let sameName[a](x: Pair[a][i32]): i32 with Size[a] = twice[Pair[a]](x)
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainOf("sameName[Box[i32]](MkPair[Box[i32]][i32](MkBox[i32](10))(5))")), 16)
  }

  test("impl の文脈に書ける（高階の不動点型のインスタンス）") {
    val src = prelude +
      """data Step[a] = { Stop, More(a) }
        |impl[a] Size[Step[a]] with Size[a] { def size = λs: Step[a]. match s {
        |  Stop => 1
        |  More(x) => add(1)(size[a](x))
        |} }
        |rec data Fix[f: * → *] = { In(f[Fix[f]]) }
        |rec impl[f: * → *] Size[Fix[f]] with ∀a. Size[a] ⇒ Size[f[a]] {
        |  def size = λx: Fix[f]. match x { In(inner) => size[f[Fix[f]]](inner) }
        |}
        |let measure(x: Fix[Step]): i32 = size[Fix[Step]](x)
        |let two: Fix[Step] = In[Step](More[Fix[Step]](In[Step](More[Fix[Step]](In[Step](Stop[Fix[Step]])))))
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainOf("measure(two)")), 3)
  }

  test("前提のない量化された制約と、複数の前提") {
    // 複数の前提は ⇒ を重ねて書く（, は with 節の制約の区切り）
    val src = prelude +
      """trait Other[a] { def other(x: a): i32 }
        |impl Other[i32] { def other = λx: i32. x }
        |data Phantom[a] = { MkPhantom }
        |impl[a] Size[Phantom[a]] { def size = λp: Phantom[a]. 5 }
        |let always[f: * → *](x: f[bool]): i32 with ∀a. Size[f[a]] = size[f[bool]](x)
        |let both[f: * → *](x: f[i32]): i32 with ∀a. Size[a] ⇒ Other[a] ⇒ Size[f[a]], Other[i32] = size[f[i32]](x)
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainOf("always[Phantom](MkPhantom[bool])")), 5)
    assertEquals(Lam.exitCode(src + mainOf("both[Box](MkBox[i32](2))")), 3)
  }

  test("trait のメソッドにも書ける") {
    val src = prelude +
      """trait Measure[t] { def measure[f: * → *](x: t)(y: f[i32]): i32 with ∀a. Size[a] ⇒ Size[f[a]] }
        |impl Measure[i32] { def measure[f: * → *](x: i32)(y: f[i32]): i32 with ∀a. Size[a] ⇒ Size[f[a]] = size[f[i32]](y) }
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainOf("measure[i32][Box](1)(MkBox[i32](2))")), 3)
  }

  test("束縛変数はヘッドの引数に現れなければならない") {
    val src = prelude + "let bad[f: * → *](x: f[i32]): i32 with ∀a. Size[a] ⇒ Size[f[i32]] = 0\n"
    assert(Lam.errors(src + main).nonEmpty)
  }

  test("束縛変数はスコープ中の型変数と同じ名前にできる（内側が隠す）") {
    val src = prelude + "let shadow[f: * → *][a](x: f[a]): i32 with ∀a. Size[a] ⇒ Size[f[a]] = 0\n"
    assertEquals(Lam.errors(src + main), Nil)
  }

  // ---- スーパークラス ----

  private val deep = prelude +
    """trait Deep[f: * → *] with ∀a. Size[a] ⇒ Size[f[a]] { def deep(x: f[i32]): i32 }
      |""".stripMargin

  test("スーパークラスに書ける: インスタンスは、その量化された制約を満たさなければならない") {
    val src = deep + "impl Deep[Box] { def deep = λb: Box[i32]. size[Box[i32]](b) }\n"
    assertEquals(Lam.exitCode(src + mainOf("deep[Box](MkBox[i32](1))")), 2)
    assertError(deep + "impl Deep[Bare] { def deep = λb: Bare[i32]. 0 }\n" + main) { case _: CompileError.NoInstance => }
  }

  test("スーパークラスの量化された制約は、その trait の制約からインスタンスのように使える") {
    val src = deep +
      """let viaSuper[f: * → *](x: f[i32]): i32 with Deep[f] = size[f[i32]](x)
        |let twiceVia[f: * → *](x: f[f[i32]]): i32 with Deep[f] = size[f[f[i32]]](x)
        |impl Deep[Box] { def deep = λb: Box[i32]. 0 }
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainOf("viaSuper[Box](MkBox[i32](1))")), 2)
    assertEquals(Lam.exitCode(src + mainOf("twiceVia[Box](MkBox[Box[i32]](MkBox[i32](1)))")), 3)
  }

  test("スーパークラスの量化された制約の前提が満たせなければエラー") {
    val src = deep + "let bad[f: * → *](x: f[bool]): i32 with Deep[f] = size[f[bool]](x)\n"
    assertError(src + main) { case _: CompileError.NoInstance => }
  }

  test("スーパークラスの量化された制約を、量化された制約を要求する関数へ渡せる") {
    val src = deep +
      """let pass[f: * → *](x: f[i32]): i32 with Deep[f] = twice[f](x)
        |impl Deep[Box] { def deep = λb: Box[i32]. 0 }
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainOf("pass[Box](MkBox[i32](4))")), 5)
  }

  test("デフォルトメソッドの本体でも、量化されたスーパークラスを使える") {
    val src = prelude +
      """trait Deep[f: * → *] with ∀a. Size[a] ⇒ Size[f[a]] {
        |  def start(x: f[i32]): f[i32]
        |  def measure(x: f[i32]): i32 = size[f[i32]](start(x))
        |}
        |impl Deep[Box] { def start = λb: Box[i32]. b }
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainOf("measure[Box](MkBox[i32](1))")), 2)
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
        |
        |data Id[a] = { MkId(a) }
        |impl Base[Id] { def base[a](x: a): Id[a] = MkId[a](x) }
        |impl Derived[Id] { def derived[a](x: Id[a]): Id[a] = x }
        |data Wrap[m: * → *][a] = { MkWrap(m[a]) }
        |impl[m: * → *] Base[Wrap[m]] with Base[m] { def base[a](x: a): Wrap[m][a] = MkWrap[m][a](base[m][a](x)) }
        |impl[m: * → *] Derived[Wrap[m]] with Derived[m] { def derived[a](x: Wrap[m][a]): Wrap[m][a] = x }
        |impl Lifting[Wrap] { def lift[m: * → *][a](x: m[a]): Wrap[m][a] with Derived[m] = MkWrap[m][a](x) }
        |let main(): i32 = match viaDerived[Wrap][Id](viaBase[Wrap][Id](7)) {
        |  MkWrap(inner) => match inner { MkId(n) => n }
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 7)
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
    assertError(compose + "let bad: i32 = nested\n" + main) {
      case error: CompileError.TypeMismatch if error.message.contains(
        "found `∀(h: (* → *) → * → *). [∀(f: * → *). [∀b. [Size[b]] → Size[f[b]]] → Size[h[f][i32]]] → h[Box][i32] → i32`") =>
    }
    assertEquals(Lam.exitCode(compose + main), 0)
  }

  test("前提が量化された制約は、インスタンスから組み立てられる") {
    val src = compose + "let n: i32 = nested[Compose[Box]](MkCompose[Box][Box][i32](MkBox[Box[i32]](MkBox[i32](1))))\n"
    assertEquals(Lam.exitCode(src + mainOf("n")), 3)
  }

  test("量化された前提が満たせなければエラー") {
    val src = compose + "let bad[h: (* → *) → * → *](x: h[Bare][i32]): i32 with ∀(f: * → *). (∀b. Size[b] ⇒ Size[f[b]]) ⇒ Size[h[f][i32]] = size[h[Bare][i32]](x)\n"
    assertError(src + main) { case _: CompileError.NoInstance => }
  }

  test("入れ子の束縛変数も、型引数の代入で捕獲されない") {
    val src = compose +
      """let outer[b](x: Compose[Box][Box][b]): i32 with Size[b] = size[Compose[Box][Box][b]](x)
        |let renamed[h: (* → *) → * → *][f: * → *](x: h[Box][i32]): i32 with ∀(g: * → *). (∀c. Size[c] ⇒ Size[g[c]]) ⇒ Size[h[g][i32]] =
        |  nested[h](x)
        |let value: Compose[Box][Box][i32] = MkCompose[Box][Box][i32](MkBox[Box[i32]](MkBox[i32](1)))
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainOf("outer[i32](value)")), 3)
    assertEquals(Lam.exitCode(src + mainOf("renamed[Compose[Box]][Bare](value)")), 3)
  }

  test("入れ子にした非再帰のデータ型を符号化できる") {
    val src =
      """data Option[a] = { None, Some(a) }
        |let x: Option[Option[i32]] = Some[Option[i32]](None[i32])
        |let n: i32 = match x {
        |  None => 1
        |  Some(inner) => match inner {
        |    None => 2
        |    Some(_) => 3
        |  }
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainOf("n")), 2)
  }
}
