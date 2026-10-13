package com.yuuki14202028

import munit.FunSuite

class OperatorSuite extends FunSuite {

  private def validate(src: String): Either[String, TypeRec[AST.Program.type]] =
    ParserAST.programParser.parseAll(src).left.map(e => s"Parse error: $e").flatMap(p => TAnalyser.validate(p).left.map(_.render))

  private def encode(src: String): Either[String, TypeRec[AST.Program.type]] =
    validate(src).map(Compiler.desugar)

  private def assertRight(result: Either[String, ?]): Unit = result match {
    case Right(_)  => ()
    case Left(err) => fail(err)
  }

  private def assertLeftContains(result: Either[String, ?], expected: String): Unit = result match {
    case Left(err) => assert(err.contains(expected), clue(err))
    case Right(_)  => fail(s"expected failure containing: $expected")
  }

  private val add =
    """trait Add[a] { def add(x: a)(y: a): a }
      |impl Add[i32] { def add(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y) }
      |""".stripMargin

  private val main = "let main(): i32 = 0\n"

  // ---- 宣言 ----

  test("operator 宣言: 演算子は宣言された関数の呼び出しになる") {
    val src = add + "infixl 6 + = add\nlet three: i32 = 1 + 2\n" + main
    val shown = encode(src).fold(fail(_), _.show)
    assert(shown.linesIterator.exists(l => l.startsWith("let three") && l.contains("$inst_Add_i32")), clue(shown))
  }

  test("宣言のない演算子は使えない") {
    assertLeftContains(validate(add + "let three: i32 = 1 + 2\n" + main), "Operator + is not declared")
  }

  test("演算子は宣言より後でだけ見える") {
    assertLeftContains(validate(add + "let three: i32 = 1 + 2\ninfixl 6 + = add\n" + main), "Operator + is not declared")
  }

  test("宣言する関数が未定義ならエラー") {
    assertLeftContains(validate("infixl 6 + = plus\n" + main), "Variable plus is not defined")
  }

  test("trait のメソッドでなくても、多相関数なら宣言できる") {
    val src =
      """let first[a](x: a)(y: a): a = x
        |infixl 6 + = first
        |let b: bool = true + false
        |""".stripMargin + main
    assertRight(encode(src))
  }

  test("左右で型の違う演算子を宣言できる") {
    val src =
      """data Pair[a][b] = { MkPair(a)(b) }
        |let pair[a](x: a)(y: bool): Pair[a][bool] = MkPair[a][bool](x)(y)
        |infixl 7 * = pair
        |let p: Pair[i32][bool] = 1 * true
        |""".stripMargin + main
    assertRight(encode(src))
    assertLeftContains(validate(src + "let q: Pair[i32][bool] = 1 * 2\n"), "Type mismatch")
  }

  test("多相でない関数・引数の足りない関数は宣言できない") {
    assertLeftContains(validate("let plus(x: i32)(y: i32): i32 = x\ninfixl 6 + = plus\n" + main), "Operator + cannot be")
    assertLeftContains(validate("let one[a](x: a): a = x\ninfixl 6 + = one\n" + main), "Operator + cannot be")
    assertLeftContains(validate("let wrong[a](x: i32)(y: a): a = y\ninfixl 6 + = wrong\n" + main), "Operator + cannot be")
  }

  test("前置の演算子は prefix operator で宣言する") {
    val src =
      """trait Neg[a] { def neg(x: a): a }
        |impl Neg[i32] { def neg(x: i32): i32 = intrinsic[neg_i32](x) }
        |prefix - = neg
        |let f(n: i32): i32 = -n
        |""".stripMargin + main
    assertRight(encode(src))
    assertLeftContains(validate("let f(n: i32): i32 = -n\n" + main), "Operator prefix - is not declared")
  }

  // ---- 型から決まるインスタンス ----

  test("インスタンスのない型には使えない") {
    assertLeftContains(validate(add + "infixl 6 + = add\nlet b: bool = true + false\n" + main), "No instance for Add[bool]")
  }

  test("型変数には、制約があれば使える") {
    val src = add + "infixl 6 + = add\n"
    assertRight(encode(src + "let double[a](x: a): a with Add[a] = x + x\nlet four: i32 = double[i32](2)\n" + main))
    assertLeftContains(validate(src + "let double[a](x: a): a = x + x\n" + main), "No instance for Add[a]")
  }

  test("右の値の型は、関数の型に合わせて検査する") {
    assertLeftContains(validate(add + "infixl 6 + = add\nlet n: i32 = 1 + true\n" + main), "Type mismatch")
  }

  test("using で渡した辞書が演算子にも使われる") {
    val src = add +
      """infixl 6 + = add
        |let left: Add[i32] = impl Add[i32] { def add(x: i32)(y: i32): i32 = x }
        |let n: i32 = using left in 1 + 2
        |""".stripMargin + main
    assertRight(encode(src))
  }

  // ---- スコープ ----

  test("後の宣言が前の宣言を隠す") {
    val src = add +
      """infixl 6 + = add
        |let first[a](x: a)(y: a): a = x
        |infixl 6 + = first
        |let b: bool = true + false
        |""".stripMargin + main
    assertRight(encode(src))
  }

  test("module の中の宣言は外から見えず、open で持ち込める") {
    val module = "module Ops {\n" + add + "infixl 6 + = add\nlet inside: i32 = 1 + 2\n}\n"
    assertLeftContains(validate(module + "let n: i32 = 1 + 2\n" + main), "Operator + is not declared")
    assertRight(encode(module + "open Ops\nlet n: i32 = 1 + 2\n" + main))
  }

  test("private な宣言は open しても見えない") {
    val module = "module Ops {\n" + add + "private infixl 6 + = add\nlet inside: i32 = 1 + 2\n}\n"
    assertRight(encode(module + main))
    assertLeftContains(validate(module + "open Ops\nlet n: i32 = 1 + 2\n" + main), "Operator + is not declared")
  }

  // ---- 組み込みの && と || ----

  test("&& と || は宣言なしで使える bool 専用の演算子") {
    assertRight(encode("let f(a: bool)(b: bool): bool = a && b || a\n" + main))
    assertLeftContains(validate("let n: i32 = 1 && 2\n" + main), "Type mismatch")
    assertLeftContains(validate("let b: bool = true || 1\n" + main), "Type mismatch")
  }

  test("&& と || は if に脱糖され、右辺は必要なときだけ評価される") {
    val shown = encode("let f(a: bool)(b: bool): bool = a && b\n" + main).fold(fail(_), _.show)
    assert(shown.contains("if"), clue(shown))
  }

  // ---- 優先順位と結合性 ----

  private val arithmetic =
    """trait Num[a] { def plus(x: a)(y: a): a  def times(x: a)(y: a): a  def less(x: a)(y: a): bool }
      |impl Num[i32] {
      |  def plus(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y)
      |  def times(x: i32)(y: i32): i32 = intrinsic[mul_i32](x)(y)
      |  def less(x: i32)(y: i32): bool = intrinsic[lt_i32](x)(y)
      |}
      |infixl 6 + = plus
      |infixl 7 * = times
      |infix 4 < = less
      |""".stripMargin

  // 型検査が組んだ木の形（型の注釈を除いたもの）
  private def shape(e: TypeRec[Expr]): String = e.project match {
    case AST.BinOp(op, left, right, _) => s"(${shape(left)} $op ${shape(right)})"
    case AST.UnaryOp(op, body, _) => s"$op${shape(body)}"
    case AST.Num(value, _) => value
    case AST.Bool(value) => value.toString
    case other => fail(s"unexpected operand: $other")
  }

  private def grouping(prelude: String, expression: String, typed: String = "i32"): String = {
    val checked = validate(prelude + s"let it: $typed = $expression\n" + main).fold(fail(_), identity)
    checked.project match {
      case AST.Program(decls) => decls.map(_.project).collectFirst { case AST.TopLet(Variable("it"), _, value) => shape(value) }.getOrElse(fail("no binding"))
    }
  }

  test("優先順位の高い演算子が先に結び付く") {
    assertEquals(grouping(arithmetic, "1 + 2 * 3"), "(1 + (2 * 3))")
    assertEquals(grouping(arithmetic, "1 * 2 + 3"), "((1 * 2) + 3)")
    assertEquals(grouping(arithmetic, "1 + 2 * 3 < 4 * 5 + 6", "bool"), "((1 + (2 * 3)) < ((4 * 5) + 6))")
  }

  test("同じ優先順位: infixl は左から、infixr は右から") {
    assertEquals(grouping(arithmetic, "1 + 2 + 3"), "((1 + 2) + 3)")
    assertEquals(grouping(arithmetic + "infixr 8 ** = times\n", "1 ** 2 ** 3"), "(1 ** (2 ** 3))")
    assertEquals(grouping(arithmetic + "infixr 8 ** = times\n", "1 ** 2 * 3 ** 4"), "((1 ** 2) * (3 ** 4))")
  }

  test("同じ優先順位で結合性が合わない並びは、括弧が要る") {
    assertLeftContains(validate(arithmetic + "let b: bool = 1 < 2 < 3\n" + main), "cannot be mixed without parentheses")
    assertLeftContains(validate(arithmetic + "infixr 6 +> = plus\nlet n: i32 = 1 + 2 +> 3\n" + main), "Operators + (infixl 6) and +> (infixr 6) cannot be mixed")
    assertEquals(grouping(arithmetic + "infixr 6 +> = plus\n", "(1 + 2) +> 3"), "((1 + 2) +> 3)")
  }

  test("前置の演算子は、二項のどの演算子よりも強い") {
    val negation = arithmetic + "prefix - = negate\n"
    val src = "let negate[a](x: a): a = x\n" + negation
    assertEquals(grouping(src, "-1 + 2"), "(-1 + 2)")
    assertEquals(grouping(src, "1 + -2 * 3"), "(1 + (-2 * 3))")
  }

  test("&& と || の優先順位は決まっている（|| < && < 宣言できる演算子の 4 以上）") {
    assertEquals(grouping(arithmetic, "1 < 2 && 2 < 3 || true", "bool"), "(((1 < 2) && (2 < 3)) || true)")
    assertEquals(grouping(arithmetic, "true || false && true || false", "bool"), "(true || ((false && true) || false))")
    assertLeftContains(validate("let both[a](x: a)(y: a): a = x\ninfixl 3 && = both\n" + main), "Operator && is built in")
  }

  test("優先順位はスコープに従う: 後の宣言・module の中の宣言で変わる") {
    assertEquals(grouping(arithmetic + "infixl 9 + = plus\n", "1 + 2 * 3"), "((1 + 2) * 3)")
    val inside = arithmetic + "module M {\n  infixl 9 + = plus\n  let inner: i32 = 1 + 2 * 3\n}\n"
    assertEquals(grouping(inside, "1 + 2 * 3"), "(1 + (2 * 3))")
    assertEquals(grouping(inside + "open M\n", "1 + 2 * 3"), "((1 + 2) * 3)")
  }

  // ---- 独自の記号 ----

  test("記号の並びは、最長一致で 1 つの演算子") {
    val src = arithmetic + "infixr 5 <> = plus\ninfixl 1 |> = plus\ninfixl 2 >>= = plus\n"
    assertEquals(grouping(src, "1 <> 2 |> 3 >>= 4"), "((1 <> 2) |> (3 >>= 4))")
    assertLeftContains(validate(arithmetic + "let n: i32 = 1 <> 2\n" + main), "Operator <> is not declared")
  }

  test("構文に使う記号（= => ->）は演算子にできない") {
    assert(ParserAST.programParser.parseAll("infixl 6 = = plus\n").isLeft)
    assert(ParserAST.programParser.parseAll("infixl 6 => = plus\n").isLeft)
    assert(ParserAST.programParser.parseAll("infixl 6 -> = plus\n").isLeft)
  }

  test("優先順位は 0〜9 の 1 桁") {
    assert(ParserAST.programParser.parseAll("infixl 10 + = plus\n").isLeft)
    assert(ParserAST.programParser.parseAll("infixl + = plus\n").isLeft)
  }

  test("match の分岐の中でも、次の分岐の負のリテラルを引き算と読まない") {
    val src = arithmetic + "infix 4 == = same\n"
    val same = "let same[a](x: a)(y: a): bool = true\n"
    assertRight(encode(same.replace("let same", "let same") + arithmetic.replace("infix 4 < = less\n", "infix 4 < = less\ninfix 4 == = same\n") +
      "let f(n: i32): i32 = match n {\n  0 => n\n  -1 => n + 1\n  _ => 2\n}\n" + main))
  }
}
