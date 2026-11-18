package com.yuuki14202028

/** context[M] { … }: モナドの計算を順に並べる書き方。flatMap と pure の呼び出しと同じ意味になる */
class ContextSuite extends munit.FunSuite {

  private val monadMaybe =
    """trait Add[a] { def add(x: a)(y: a): a }; infixl 6 + = add; impl Add[i32] { def add(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y) }
      |trait Div[a] { def div(x: a)(y: a): a }; infixl 7 / = div; impl Div[i32] { def div(x: i32)(y: i32): i32 = intrinsic[div_i32](x)(y) }
      |trait Eq[a] { def eq(x: a)(y: a): bool }; infix 4 == = eq; impl Eq[i32] { def eq(x: i32)(y: i32): bool = intrinsic[eq_i32](x)(y) }
      |
      |data Maybe[a] = { Nothing, Just(a) }
      |
      |trait Monad[m: * → *] {
      |  def pure[a](x: a): m[a]
      |  def flatMap[a][b](v: m[a])(f: a → m[b]): m[b]
      |}
      |
      |impl Monad[Maybe] {
      |  def pure[a](x: a): Maybe[a] = Just[a](x)
      |  def flatMap[a][b](m: Maybe[a])(f: a → Maybe[b]): Maybe[b] = match m {
      |    Nothing => Nothing[b]
      |    Just(x) => f(x)
      |  }
      |}
      |
      |let safeDiv(x: i32)(y: i32): Maybe[i32] =
      |  if y == 0 then Nothing[i32] else Just[i32](x / y)
      |
      |// Nothing は 255
      |let orFail(m: Maybe[i32]): i32 = match m {
      |  Nothing => 255
      |  Just(x) => x
      |}
      |""".stripMargin

  /** compute(): Maybe[i32] の定義を受け取り、その結果（Nothing は 255） */
  private def result(compute: String)(using munit.Location): Int =
    Lam.exitCode(monadMaybe + compute + "\nlet main(): i32 = orFail(compute())\n")

  private def assertError(source: String)(expected: PartialFunction[CompileError, Unit])(using munit.Location): Unit = {
    val found = Lam.errors(source)
    assert(found.exists(expected.isDefinedAt), clue(found.map(_.message)))
  }

  // ---- 書き方 ----

  test("context ブロック: 束縛を順に実行し、最後の式を pure で包む") {
    assertEquals(result("let compute(): Maybe[i32] = context[Maybe] { x: i32 = safeDiv(120)(5); x + 1 }"), 25)
  }

  test("純粋 let / rec let をモナド束縛と混在できる") {
    val compute =
      """let compute(): Maybe[i32] = context[Maybe] {
        |  let y: i32 = 1;
        |  x: i32 = safeDiv(120)(5);
        |  rec let f(n: i32): i32 = if n == 0 then y else f(0);
        |  x + f(x)
        |}""".stripMargin
    assertEquals(result(compute), 25)
  }

  test("束縛なしの context ブロック") {
    assertEquals(result("let compute(): Maybe[i32] = context[Maybe] { 42 }"), 42)
  }

  test("パース失敗: 最終式のない context ブロック") {
    assertError(monadMaybe + "let compute(): Maybe[i32] = context[Maybe] { x: i32 = safeDiv(120)(5); }\nlet main(): i32 = 0\n") {
      case _: CompileError.Syntax =>
    }
  }

  // ---- 型検査と意味 ----

  test("context ブロックの型は M[R]") {
    val compute =
      """let compute(): Maybe[i32] = context[Maybe] {
        |  step1: i32 = 120 safeDiv 5;
        |  step1 + 1
        |}""".stripMargin
    assertEquals(result(compute), 25)
    val wrong = compute.replace("let compute(): Maybe[i32]", "let compute(): i32")
    assertError(monadMaybe + wrong + "\nlet main(): i32 = 0\n") {
      case error: CompileError.TypeMismatch if error.message.contains("Maybe[i32]") =>
    }
  }

  test("context ブロックは手書きの flatMap / pure の並びと同じ結果になる") {
    def context(divisor: Int): String =
      s"""let compute(): Maybe[i32] = context[Maybe] {
         |  step1: i32 = 120 safeDiv 5;
         |  step2: i32 = step1 safeDiv $divisor;
         |  step2 + 1
         |}""".stripMargin
    def manual(divisor: Int): String =
      s"""let compute(): Maybe[i32] =
         |  flatMap[Maybe][i32][i32](safeDiv(120)(5))(λstep1: i32.
         |    flatMap[Maybe][i32][i32](safeDiv(step1)($divisor))(λstep2: i32.
         |      pure[Maybe][i32](step2 + 1)))""".stripMargin
    assertEquals(result(context(3)), 9)
    assertEquals(result(manual(3)), 9)
    // 途中が Nothing なら、全体が Nothing
    assertEquals(result(context(0)), 255)
    assertEquals(result(manual(0)), 255)
  }

  test("純粋 let は通常の let になり、後続の束縛から参照できる") {
    val compute =
      """let compute(): Maybe[i32] = context[Maybe] {
        |  let denom: i32 = 5;
        |  step1: i32 = 120 safeDiv denom;
        |  step1 + 1
        |}""".stripMargin
    assertEquals(result(compute), 25)
  }

  test("辞書解決: flatMap / pure は context[M] の M のインスタンスを使う") {
    // Maybe とは別のモナド: 計算の回数を数える
    val counted =
      """data Counted[a] = { MkCounted(i32)(a) }
        |impl Monad[Counted] {
        |  def pure[a](x: a): Counted[a] = MkCounted[a](0)(x)
        |  def flatMap[a][b](m: Counted[a])(f: a → Counted[b]): Counted[b] = match m {
        |    MkCounted(count)(x) => match f(x) { MkCounted(more)(y) => MkCounted[b](count + more)(y) }
        |  }
        |}
        |let tick(x: i32): Counted[i32] = MkCounted(1)(x)
        |let counted: Counted[i32] = context[Counted] {
        |  a: i32 = tick(10);
        |  b: i32 = tick(20);
        |  a + b
        |}
        |let main(): i32 = match counted { MkCounted(count)(sum) => count + sum }
        |""".stripMargin
    assertEquals(Lam.exitCode(monadMaybe + counted), 32)
  }

  test("型エラー: モナド束縛の右辺が M[T] でない") {
    val source = monadMaybe +
      """let compute(): Maybe[i32] = context[Maybe] {
        |  step1: i32 = 42;
        |  step1 + 1
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertError(source) { case _: CompileError.TypeMismatch => }
  }

  test("型エラー: bind/ret が未定義のまま context を使う") {
    val source =
      """data Maybe[a] = { Nothing, Just(a) }
        |let r(): Maybe[i32] = context[Maybe] { 42 }
        |let main(): i32 = 0
        |""".stripMargin
    assertError(source) { case CompileError.UndefinedVariable("flatMap" | "pure", _) => }
  }

  test("型エラー: カインドが * → * でない型を指定する") {
    val source = monadMaybe +
      """let r(): Maybe[i32] = context[i32] { 42 }
        |let main(): i32 = 0
        |""".stripMargin
    assertError(source) { case _: CompileError.KindMismatch => }
  }
}
