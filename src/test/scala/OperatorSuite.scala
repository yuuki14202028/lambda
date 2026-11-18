package com.yuuki14202028

/** 演算子の宣言（infixl / infixr / infix / prefix）と、そのスコープ・優先順位・結合性 */
class OperatorSuite extends munit.FunSuite {

  private val add =
    """trait Add[a] { def add(x: a)(y: a): a }
      |impl Add[i32] { def add(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y) }
      |""".stripMargin

  private val main = "let main(): i32 = 0\n"

  private def assertError(source: String)(expected: PartialFunction[CompileError, Unit])(using munit.Location): Unit = {
    val found = Lam.errors(source)
    assert(found.exists(expected.isDefinedAt), clue(found.map(_.message)))
  }

  private def assertNotDeclared(source: String, operator: String)(using munit.Location): Unit =
    assertError(source) { case CompileError.UndefinedOperator(`operator`, false, _) => }

  private def assertMismatch(source: String)(using munit.Location): Unit =
    assertError(source) { case _: CompileError.TypeMismatch => }

  private def assertSyntaxError(source: String)(using munit.Location): Unit =
    assertError(source) { case _: CompileError.Syntax => }

  // ---- 宣言 ----

  test("operator 宣言: 演算子は宣言された関数の呼び出しになる") {
    assertEquals(Lam.exitCode(add + "infixl 6 + = add\nlet main(): i32 = 1 + 2\n"), 3)
  }

  test("宣言のない演算子は使えない") {
    assertNotDeclared(add + "let three: i32 = 1 + 2\n" + main, "+")
  }

  test("演算子は宣言より後でだけ見える") {
    assertNotDeclared(add + "let three: i32 = 1 + 2\ninfixl 6 + = add\n" + main, "+")
  }

  test("宣言する関数が未定義ならエラー") {
    assertError("infixl 6 + = plus\n" + main) { case CompileError.UndefinedVariable("plus", _) => }
  }

  test("trait のメソッドでなくても、多相関数なら宣言できる") {
    val source =
      """let first[a](x: a)(y: a): a = x
        |infixl 6 + = first
        |let main(): i32 = if true + false then 1 else 0
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 1)
  }

  test("左右で型の違う演算子を宣言できる") {
    val source =
      """data Pair[a][b] = { MkPair(a)(b) }
        |let pair[a](x: a)(y: bool): Pair[a][bool] = MkPair[a][bool](x)(y)
        |infixl 7 * = pair
        |let p: Pair[i32][bool] = 1 * true
        |""".stripMargin
    assertEquals(Lam.exitCode(source + "let main(): i32 = match p { MkPair(n)(flag) => if flag then n else 0 }\n"), 1)
    assertMismatch(source + "let q: Pair[i32][bool] = 1 * 2\n" + main)
  }

  // 以前は「∀a. … → a → …」の形の関数だけを宣言できた
  test("多相でない関数も宣言できる。引数の足りない関数は、使う所でエラー") {
    val source =
      """let plus(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y)
        |infixl 6 + = plus
        |let second[a](x: i32)(y: a): a = y
        |infixl 5 +> = second
        |let main(): i32 = if 1 + 2 +> true then 1 + 2 else 0
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 3)
    assertError("let one[a](x: a): a = x\ninfixl 6 + = one\nlet n: i32 = 1 + 2\n" + main) { case _: CompileError.NotAFunction => }
  }

  test("前置の演算子は prefix operator で宣言する") {
    val source =
      """trait Neg[a] { def neg(x: a): a }
        |impl Neg[i32] { def neg(x: i32): i32 = intrinsic[neg_i32](x) }
        |prefix - = neg
        |let f(n: i32): i32 = -n
        |let main(): i32 = f(f(5))
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 5)
    assertError("let f(n: i32): i32 = -n\n" + main) { case CompileError.UndefinedOperator("-", true, _) => }
  }

  // ---- 型から決まるインスタンス ----

  test("インスタンスのない型には使えない") {
    assertError(add + "infixl 6 + = add\nlet b: bool = true + false\n" + main) {
      case error: CompileError.NoInstance if error.message.contains("Add[bool]") =>
    }
  }

  test("型変数には、制約があれば使える") {
    val source = add + "infixl 6 + = add\n"
    assertEquals(Lam.exitCode(source + "let double[a](x: a): a with Add[a] = x + x\nlet main(): i32 = double[i32](2)\n"), 4)
    assertError(source + "let double[a](x: a): a = x + x\n" + main) {
      case error: CompileError.NoInstance if error.message.contains("Add[a]") =>
    }
  }

  test("右の値の型は、関数の型に合わせて検査する") {
    assertMismatch(add + "infixl 6 + = add\nlet n: i32 = 1 + true\n" + main)
  }

  test("using で渡した辞書が演算子にも使われる") {
    val source = add +
      """infixl 6 + = add
        |let left: Add[i32] = impl Add[i32] { def add(x: i32)(y: i32): i32 = x }
        |let main(): i32 = using left in 1 + 2
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 1)
  }

  // ---- スコープ ----

  test("後の宣言が前の宣言を隠す") {
    val source = add +
      """infixl 6 + = add
        |let first[a](x: a)(y: a): a = x
        |infixl 6 + = first
        |let main(): i32 = 1 + 2
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 1)
  }

  test("module の中の宣言は外から見えず、open で持ち込める") {
    val module = "module Ops {\n" + add + "infixl 6 + = add\nlet inside: i32 = 1 + 2\n}\n"
    assertNotDeclared(module + "let n: i32 = 1 + 2\n" + main, "+")
    assertEquals(Lam.exitCode(module + "open Ops\nlet main(): i32 = inside + 4\n"), 7)
  }

  test("private な宣言は open しても見えない") {
    val module = "module Ops {\n" + add + "private infixl 6 + = add\nlet inside: i32 = 1 + 2\n}\n"
    assertEquals(Lam.exitCode(module + "let main(): i32 = Ops.inside\n"), 3)
    assertNotDeclared(module + "open Ops\nlet n: i32 = 1 + 2\n" + main, "+")
  }

  // ---- 組み込みの && と || ----

  test("&& と || は宣言なしで使える bool 専用の演算子") {
    val source =
      """let f(a: bool)(b: bool): bool = a && b || a
        |let main(): i32 = if f(true)(false) then 1 else 0
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 1)
    assertMismatch("let n: i32 = 1 && 2\n" + main)
    assertMismatch("let b: bool = true || 1\n" + main)
  }

  test("&& と || は if に脱糖され、右辺は必要なときだけ評価される") {
    val source =
      """rec let loop(b: bool): bool = loop(b)
        |let main(): i32 = if (false && loop(true)) || (true || loop(true)) then 1 else 0
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 1)
  }

  // ---- 優先順位と結合性 ----

  private val arithmetic =
    """trait Num[a] {
      |  def plus(x: a)(y: a): a
      |  def minus(x: a)(y: a): a
      |  def times(x: a)(y: a): a
      |  def less(x: a)(y: a): bool
      |}
      |impl Num[i32] {
      |  def plus(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y)
      |  def minus(x: i32)(y: i32): i32 = intrinsic[sub_i32](x)(y)
      |  def times(x: i32)(y: i32): i32 = intrinsic[mul_i32](x)(y)
      |  def less(x: i32)(y: i32): bool = intrinsic[lt_i32](x)(y)
      |}
      |infixl 6 + = plus
      |infixl 6 - = minus
      |infixl 7 * = times
      |infix 4 < = less
      |""".stripMargin

  // 式の値で、どう結び付いたかを確かめる
  private def value(declarations: String, expression: String)(using munit.Location): Int =
    Lam.exitCode(s"${declarations}let main(): i32 = $expression\n")

  private def holds(declarations: String, expression: String)(using munit.Location): Boolean =
    value(declarations, s"if $expression then 1 else 0") == 1

  test("優先順位の高い演算子が先に結び付く") {
    assertEquals(value(arithmetic, "1 + 2 * 3"), 7)
    assertEquals(value(arithmetic, "2 * 3 + 4"), 10)
    // (1 + (2 * 3)) < ((4 * 5) + 6)
    assert(holds(arithmetic, "1 + 2 * 3 < 4 * 5 + 6"))
  }

  test("同じ優先順位: infixl は左から、infixr は右から") {
    val power = arithmetic + "infixr 8 ** = minus\n"
    assertEquals(value(arithmetic, "10 - 3 - 2"), 5)
    assertEquals(value(power, "10 ** 3 ** 2"), 9)
    // (5 ** 2) * (7 ** 4)
    assertEquals(value(power, "5 ** 2 * 7 ** 4"), 9)
  }

  test("同じ優先順位で結合性が合わない並びは、括弧が要る") {
    assertError(arithmetic + "let b: bool = 1 < 2 < 3\n" + main) { case CompileError.OperatorsNotAssociative("<", _, "<", _, _) => }
    assertError(arithmetic + "infixr 6 +> = plus\nlet n: i32 = 1 + 2 +> 3\n" + main) {
      case CompileError.OperatorsNotAssociative("+", _, "+>", _, _) =>
    }
    assertEquals(value(arithmetic + "infixr 6 +> = plus\n", "(1 + 2) +> 3"), 6)
  }

  test("前置の演算子は、二項のどの演算子よりも強い") {
    val negation = arithmetic + "let negate(x: i32): i32 = intrinsic[neg_i32](x)\nprefix - = negate\n"
    assertEquals(value(negation, "-1 + 2"), 1)
    assertEquals(value(negation, "10 + -2 * 3"), 4)
  }

  test("&& と || の優先順位は決まっている（|| < && < 宣言できる演算子の 4 以上）") {
    assert(holds(arithmetic, "1 < 2 && 2 < 3 || false"))
    // && が先: (false && true) || true、true || (false && false)
    assert(holds(arithmetic, "false && true || true"))
    assert(holds(arithmetic, "true || false && false"))
    assertError("let both[a](x: a)(y: a): a = x\ninfixl 3 && = both\n" + main) { case CompileError.OperatorBuiltin("&&", _) => }
  }

  test("優先順位はスコープに従う: 後の宣言・module の中の宣言で変わる") {
    assertEquals(value(arithmetic + "infixl 9 + = plus\n", "1 + 2 * 3"), 9)
    val inside = arithmetic + "module M {\n  infixl 9 + = plus\n  let inner: i32 = 1 + 2 * 3\n}\n"
    assertEquals(value(inside, "1 + 2 * 3"), 7)
    assertEquals(value(inside, "M.inner"), 9)
    assertEquals(value(inside + "open M\n", "1 + 2 * 3"), 9)
  }

  // ---- 独自の記号 ----

  test("記号の並びは、最長一致で 1 つの演算子") {
    val source = arithmetic + "infixr 5 <> = plus\ninfixl 1 |> = minus\ninfixl 2 >>= = plus\n"
    // (10 <> 2) |> (3 >>= 4)
    assertEquals(value(source, "10 <> 2 |> 3 >>= 4"), 5)
    assertNotDeclared(arithmetic + "let n: i32 = 1 <> 2\n" + main, "<>")
  }

  test("構文に使う記号（= => ->）は演算子にできない") {
    assertSyntaxError(arithmetic + "infixl 6 = = plus\n" + main)
    assertSyntaxError(arithmetic + "infixl 6 => = plus\n" + main)
    assertSyntaxError(arithmetic + "infixl 6 -> = plus\n" + main)
  }

  test("優先順位は 0〜9 の 1 桁") {
    assertSyntaxError(arithmetic + "infixl 10 + = plus\n" + main)
    assertSyntaxError(arithmetic + "infixl + = plus\n" + main)
  }

  test("match の分岐の中でも、次の分岐の負のリテラルを引き算と読まない") {
    val source = arithmetic +
      """let same[a](x: a)(y: a): bool = true
        |infix 4 == = same
        |let f(n: i32): i32 = match n {
        |  0 => n
        |  -1 => n + 1
        |  _ => 2
        |}
        |let main(): i32 = f(5)
        |""".stripMargin
    // == はいつも真なので、最初の分岐に当たる
    assertEquals(Lam.exitCode(source), 5)
  }
}
