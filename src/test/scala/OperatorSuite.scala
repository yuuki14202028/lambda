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
    val src = add + "operator + = add\nlet three: i32 = 1 + 2\n" + main
    val shown = encode(src).fold(fail(_), _.show)
    assert(shown.linesIterator.exists(l => l.startsWith("let three") && l.contains("$inst_Add_i32")), clue(shown))
  }

  test("宣言のない演算子は使えない") {
    assertLeftContains(validate(add + "let three: i32 = 1 + 2\n" + main), "Operator + is not declared")
  }

  test("演算子は宣言より後でだけ見える") {
    assertLeftContains(validate(add + "let three: i32 = 1 + 2\noperator + = add\n" + main), "Operator + is not declared")
  }

  test("宣言する関数が未定義ならエラー") {
    assertLeftContains(validate("operator + = plus\n" + main), "Variable plus is not defined")
  }

  test("trait のメソッドでなくても、多相関数なら宣言できる") {
    val src =
      """let first[a](x: a)(y: a): a = x
        |operator + = first
        |let b: bool = true + false
        |""".stripMargin + main
    assertRight(encode(src))
  }

  test("左右で型の違う演算子を宣言できる") {
    val src =
      """data Pair[a][b] = { MkPair(a)(b) }
        |let pair[a](x: a)(y: bool): Pair[a][bool] = MkPair[a][bool](x)(y)
        |operator * = pair
        |let p: Pair[i32][bool] = 1 * true
        |""".stripMargin + main
    assertRight(encode(src))
    assertLeftContains(validate(src + "let q: Pair[i32][bool] = 1 * 2\n"), "Type mismatch")
  }

  test("多相でない関数・引数の足りない関数は宣言できない") {
    assertLeftContains(validate("let plus(x: i32)(y: i32): i32 = x\noperator + = plus\n" + main), "Operator + cannot be")
    assertLeftContains(validate("let one[a](x: a): a = x\noperator + = one\n" + main), "Operator + cannot be")
    assertLeftContains(validate("let wrong[a](x: i32)(y: a): a = y\noperator + = wrong\n" + main), "Operator + cannot be")
  }

  test("前置の演算子は prefix operator で宣言する") {
    val src =
      """trait Neg[a] { def neg(x: a): a }
        |impl Neg[i32] { def neg(x: i32): i32 = intrinsic[neg_i32](x) }
        |prefix operator - = neg
        |let f(n: i32): i32 = -n
        |""".stripMargin + main
    assertRight(encode(src))
    assertLeftContains(validate("let f(n: i32): i32 = -n\n" + main), "Operator prefix - is not declared")
  }

  // ---- 型から決まるインスタンス ----

  test("インスタンスのない型には使えない") {
    assertLeftContains(validate(add + "operator + = add\nlet b: bool = true + false\n" + main), "No instance for Add[bool]")
  }

  test("型変数には、制約があれば使える") {
    val src = add + "operator + = add\n"
    assertRight(encode(src + "let double[a](x: a): a with Add[a] = x + x\nlet four: i32 = double[i32](2)\n" + main))
    assertLeftContains(validate(src + "let double[a](x: a): a = x + x\n" + main), "No instance for Add[a]")
  }

  test("右の値の型は、関数の型に合わせて検査する") {
    assertLeftContains(validate(add + "operator + = add\nlet n: i32 = 1 + true\n" + main), "Type mismatch")
  }

  test("using で渡した辞書が演算子にも使われる") {
    val src = add +
      """operator + = add
        |let left: Add[i32] = impl Add[i32] { def add(x: i32)(y: i32): i32 = x }
        |let n: i32 = using left in 1 + 2
        |""".stripMargin + main
    assertRight(encode(src))
  }

  // ---- スコープ ----

  test("後の宣言が前の宣言を隠す") {
    val src = add +
      """operator + = add
        |let first[a](x: a)(y: a): a = x
        |operator + = first
        |let b: bool = true + false
        |""".stripMargin + main
    assertRight(encode(src))
  }

  test("module の中の宣言は外から見えず、open で持ち込める") {
    val module = "module Ops {\n" + add + "operator + = add\nlet inside: i32 = 1 + 2\n}\n"
    assertLeftContains(validate(module + "let n: i32 = 1 + 2\n" + main), "Operator + is not declared")
    assertRight(encode(module + "open Ops\nlet n: i32 = 1 + 2\n" + main))
  }

  test("private な宣言は open しても見えない") {
    val module = "module Ops {\n" + add + "private operator + = add\nlet inside: i32 = 1 + 2\n}\n"
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
}
