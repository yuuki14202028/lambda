package com.yuuki14202028

/** インスタンスを明示的に扱う: summon / (with d) / using / impl の式 */
class ExplicitInstanceSuite extends munit.FunSuite {

  private def assertError(source: String)(expected: PartialFunction[CompileError, Unit])(using munit.Location): Unit = {
    val found = Lam.errors(source)
    assert(found.exists(expected.isDefinedAt), clue(found.map(_.message)))
  }

  private def assertNoInstance(source: String, goal: String)(using munit.Location): Unit =
    assertError(source) { case error: CompileError.NoInstance if Type.show(error.goal) == goal => }

  // インスタンスは小さい順、other は大きい順。first はインスタンスなら小さい方、other なら大きい方を返す
  private val prelude =
    """trait Order[a] { def before(x: a)(y: a): bool }
      |impl Order[i32] { def before = λx: i32. λy: i32. intrinsic[lt_i32](x)(y) }
      |let other: Order[i32] = MkOrder[i32](λx: i32. λy: i32. intrinsic[lt_i32](y)(x))
      |let first[a](x: a)(y: a): a with Order[a] = if before[a](x)(y) then x else y
      |""".stripMargin

  private val main = "let main(): i32 = 0\n"

  private def mainOf(result: String): String = s"let main(): i32 = $result\n"

  private def mainIf(condition: String): String = s"let main(): i32 = if $condition then 1 else 0\n"

  // ---- summon ----

  test("summon[C[τ]] は解決されたインスタンスの辞書で、型は C[τ]") {
    val src = prelude + "let d: Order[i32] = summon[Order[i32]]\n"
    assertEquals(Lam.exitCode(src + mainIf("before[i32](with d)(1)(2)")), 1)
  }

  test("summon はスコープ中の制約も取り出せる") {
    val src = prelude + "let get[a](x: a): Order[a] with Order[a] = summon[Order[a]]\n"
    assertEquals(Lam.exitCode(src + mainOf("first[i32](with get[i32](with other)(0))(1)(2)")), 2)
  }

  test("summon はインスタンスがなければエラー") {
    assertNoInstance(prelude + "let d: Order[bool] = summon[Order[bool]]\n" + main, "Order[bool]")
  }

  test("summon で量化された制約の辞書も取り出せる") {
    val src = prelude +
      """data Box[a] = { MkBox(a) }
        |impl[a] Order[Box[a]] with Order[a] { def before = λx: Box[a]. λy: Box[a]. match x { MkBox(p) => match y { MkBox(q) => before[a](p)(q) } } }
        |let d: ∀a. Order[a] ⇒ Order[Box[a]] = summon[∀a. Order[a] ⇒ Order[Box[a]]]
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainIf("before[Box[i32]](with d[i32](with other))(MkBox[i32](2))(MkBox[i32](1))")), 1)
  }

  // ---- (with d) ----

  test("(with d) で、解決の代わりに辞書の値を渡せる") {
    assertEquals(Lam.exitCode(prelude + mainOf("first[i32](with other)(1)(2)")), 2)
  }

  test("(with d) を付けなければ、従来どおり解決される") {
    assertEquals(Lam.exitCode(prelude + mainOf("first[i32](1)(2)")), 1)
  }

  test("インスタンスのない型でも、辞書を渡せば使える") {
    val src = prelude + "let b: bool = first[bool](with MkOrder[bool](λx: bool. λy: bool. true))(true)(false)\n"
    assertEquals(Lam.exitCode(src + mainIf("b")), 1)
  }

  test("trait のメソッドにも辞書を渡せる") {
    assertEquals(Lam.exitCode(prelude + mainIf("before[i32](with other)(1)(2)")), 0)
  }

  test("渡す値の型は、要求される辞書の型と一致しなければならない") {
    assertError(prelude + "let n: i32 = first[i32](with 1)(1)(2)\n" + main) { case _: CompileError.TypeMismatch => }
    assertError(prelude + "let n: i32 = first[i32](with MkOrder[bool](λx: bool. λy: bool. true))(1)(2)\n" + main) {
      case _: CompileError.TypeMismatch =>
    }
  }

  test("制約を取らない式には辞書を渡せない") {
    assertError(prelude + "let id(x: i32): i32 = x\nlet n: i32 = id(with other)(1)\n" + main) { case _: CompileError.NoConstraintToSatisfy => }
    assertError(prelude + "let n: i32 = first[i32](with other)(with other)(1)(2)\n" + main) { case _: CompileError.NoConstraintToSatisfy => }
  }

  test("制約が複数あるとき、先頭から順に渡し、残りは解決される") {
    val src = prelude +
      """trait Label[a] { def label(x: a): i32 }
        |impl Label[i32] { def label = λx: i32. x }
        |let both[a](x: a)(y: a): i32 with Order[a], Label[a] = if before[a](x)(y) then label[a](x) else 0
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainOf("both[i32](5)(1)")), 0)
    assertEquals(Lam.exitCode(src + mainOf("both[i32](with other)(5)(1)")), 5)
    assertEquals(Lam.exitCode(src + mainOf("both[i32](with other)(with MkLabel[i32](λx: i32. 9))(5)(1)")), 9)
  }

  test("辞書の式の中では、制約は従来どおり解決される") {
    val src = prelude + "let flip[a](x: a): Order[a] with Order[a] = MkOrder[a](λl: a. λr: a. before[a](r)(l))\n"
    assertEquals(Lam.exitCode(src + mainOf("first[i32](with flip[i32](0))(1)(2)")), 2)
  }

  test("スーパークラスを持つ trait の辞書を組み立てて渡せる") {
    val src = prelude +
      """trait Named[a] with Order[a] { def weight(x: a): i32 }
        |let heavy[a](x: a)(y: a): i32 with Named[a] = weight[a](first[a](x)(y))
        |let custom: Named[i32] = MkNamed[i32](other)(λx: i32. x)
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainOf("heavy[i32](with custom)(1)(2)")), 2)
  }

  // ---- using ----

  test("using d in e: e の中では d がインスタンスとして解決に使われる") {
    assertEquals(Lam.exitCode(prelude + mainOf("using other in first[i32](1)(first[i32](2)(3))")), 3)
  }

  test("using の外では、従来どおりインスタンスが使われる") {
    assertEquals(Lam.exitCode(prelude + mainOf("first[i32](using other in 1)(2)")), 1)
  }

  test("using はインスタンスのない型にも使える") {
    val src = prelude + "let b: bool = using MkOrder[bool](λx: bool. λy: bool. true) in first[bool](true)(false)\n"
    assertEquals(Lam.exitCode(src + mainIf("b")), 1)
    assertNoInstance(prelude + "let b: bool = first[bool](true)(false)\n" + main, "Order[bool]")
  }

  test("using は外側の同じ制約より優先される") {
    val src = prelude +
      """let flip[a](order: Order[a]): Order[a] = MkOrder[a](λx: a. λy: a. before[a](with order)(y)(x))
        |let flipped[a](x: a)(y: a): a with Order[a] = using flip[a](summon[Order[a]]) in first[a](x)(y)
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainOf("flipped[i32](1)(2)")), 2)
  }

  test("using には量化された制約の辞書（文脈付きの多相な辞書）も渡せる") {
    val src = prelude +
      """data Box[a] = { MkBox(a) }
        |let unbox(b: Box[i32]): i32 = match b { MkBox(n) => n }
        |let boxed[a]: Order[Box[a]] with Order[a] =
        |  MkOrder[Box[a]](λx: Box[a]. λy: Box[a]. match x { MkBox(p) => match y { MkBox(q) => before[a](p)(q) } })
        |let b: Box[i32] = using boxed in first[Box[i32]](MkBox[i32](1))(MkBox[i32](2))
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainOf("unbox(b)")), 1)
  }

  test("using に辞書でない値は渡せない") {
    assertError(prelude + "let n: i32 = using 1 in 2\n" + main) { case _: CompileError.UsingNotDictionary => }
  }

  // ---- impl の式 ----

  private val traits =
    """trait Not[a] { def not(x: a): a }; prefix ! = not; impl Not[bool] { def not(a: bool): bool = intrinsic[not_bool](a) }
      |trait Same[a] {
      |  def same(x: a)(y: a): bool
      |  def differ(x: a)(y: a): bool = !same(x)(y)
      |}
      |trait Ranked[a] with Same[a] {
      |  def before(x: a)(y: a): bool
      |  def after(x: a)(y: a): bool = before(y)(x)
      |}
      |impl Same[i32] { def same = λx: i32. λy: i32. true }
      |""".stripMargin

  test("impl の式は辞書の値で、型は trait の型") {
    val src = traits + "let d: Same[bool] = impl Same[bool] { def same(x: bool)(y: bool): bool = x }\n"
    assertEquals(Lam.exitCode(src + mainIf("same[bool](with d)(true)(false)")), 1)
  }

  test("impl の式: 省略したメソッドはデフォルトで埋まる") {
    val src = traits + "let d: Same[bool] = impl Same[bool] { def same = λx: bool. λy: bool. false }\n"
    assertEquals(Lam.exitCode(src + mainIf("differ[bool](with d)(true)(true)")), 1)
  }

  test("impl の式: デフォルトのないメソッドは省略できない") {
    assertError(traits + "let d: Same[bool] = impl Same[bool] { def differ = λx: bool. λy: bool. true }\n" + main) {
      case CompileError.MissingMethods("impl Same[bool]", List("same"), _) =>
    }
  }

  test("impl の式: メソッドの型は trait のシグネチャと一致しなければならない") {
    assertError(traits + "let d: Same[bool] = impl Same[bool] { def same = λx: bool. x }\n" + main) {
      case CompileError.MethodTypeMismatch("impl Same[bool]", "same", _, _, _) =>
    }
  }

  test("impl の式はインスタンスとして登録されない") {
    val src = traits + "let d: Same[bool] = impl Same[bool] { def same = λx: bool. λy: bool. true }\n"
    assertNoInstance(src + "let b: bool = same[bool](true)(false)\n" + main, "Same[bool]")
    assertEquals(Lam.exitCode(src + mainIf("same[bool](with d)(true)(false)")), 1)
  }

  test("impl の式: スーパークラスの辞書は、その位置で解決される") {
    val src = traits +
      """let d: Ranked[i32] = impl Ranked[i32] { def before = λx: i32. λy: i32. false }
        |let viaSuper[a](x: a): bool with Ranked[a] = same[a](x)(x)
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainIf("viaSuper[i32](with d)(1)")), 1)
  }

  test("impl の式: スーパークラスのインスタンスがなければエラー。using で入れれば作れる") {
    assertNoInstance(traits + "let d: Ranked[bool] = impl Ranked[bool] { def before = λx: bool. λy: bool. false }\n" + main, "Same[bool]")
    val src = traits +
      """let d: Ranked[bool] =
        |  using (impl Same[bool] { def same = λx: bool. λy: bool. true }) in
        |  impl Ranked[bool] { def before = λx: bool. λy: bool. x }
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainIf("after[bool](with d)(false)(true)")), 1)
  }

  test("impl の式は、型変数や関数型に対しても作れる") {
    val src = traits +
      """let always[a]: Same[a] = impl Same[a] { def same = λx: a. λy: a. true }
        |let onFunctions: Same[i32 → i32] = impl Same[i32 → i32] { def same = λf: i32 → i32. λg: i32 → i32. false }
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainIf("differ[bool](with always[bool])(true)(false)")), 0)
    assertEquals(Lam.exitCode(src + mainIf("differ[i32 → i32](with onFunctions)(λx: i32. x)(λx: i32. x)")), 1)
  }

  test("impl の式: 外側の制約と引数を本体で使える") {
    val src = traits +
      """let lifted[a][b](project: b → a): Same[b] with Same[a] =
        |  impl Same[b] { def same(x: b)(y: b): bool = same[a](project(x))(project(y)) }
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainIf("same[bool](with lifted[i32][bool](λv: bool. 1))(true)(false)")), 1)
  }

  test("impl の式: 型引数の数とカインドの検査") {
    assertError(traits + "let d: Same[bool] = impl Same[bool][bool] { def same = λx: bool. λy: bool. true }\n" + main) {
      case CompileError.ImplTargetArity("Same", 1, 2, _) =>
    }
    assertError(traits + "data Box[a] = { MkBox(a) }\nlet d: i32 = impl Same[Box] { def same = λx: i32. λy: i32. true }\n" + main) {
      case _: CompileError.KindMismatch =>
    }
  }
}
