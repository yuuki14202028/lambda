package com.yuuki14202028

/** x f y: 2 引数の関数を、引数のあいだに書く呼び出し */
class InfixSuite extends munit.FunSuite {

  private val prelude =
    """let add(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y)
      |let sub(x: i32)(y: i32): i32 = intrinsic[sub_i32](x)(y)
      |let mul(x: i32)(y: i32): i32 = intrinsic[mul_i32](x)(y)
      |let neg(x: i32): i32 = intrinsic[neg_i32](x)
      |let gt(x: i32)(y: i32): bool = intrinsic[gt_i32](x)(y)
      |infixl 6 + = add
      |infixl 7 * = mul
      |infix 4 > = gt
      |prefix - = neg
      |let max(x: i32)(y: i32): i32 = if x > y then x else y
      |let inc(x: i32): i32 = x + 1
      |let a: i32 = 2
      |let b: i32 = 5
      |let c: i32 = 3
      |""".stripMargin

  private def value(expression: String)(using munit.Location): Int = Lam.exitCode(s"${prelude}let main(): i32 = $expression\n")

  private def rejected(expression: String)(using munit.Location): Unit = {
    val found = Lam.errors(s"${prelude}let main(): i32 = $expression\n")
    assert(found.nonEmpty)
  }

  test("識別子 infix は関数適用になる") {
    assertEquals(value("a max b"), 5)
    assertEquals(value("a sub b + 10"), 7)
  }

  test("識別子 infix は左結合") {
    // (2 - 5) - 3 = -6。右結合なら 2 - (5 - 3) = 0
    assertEquals(value("a sub b sub c + 10"), 4)
  }

  test("識別子 infix は記号演算子より強く結合する") {
    assertEquals(value("a max b + c"), 8)
    assertEquals(value("a max b * c"), 15)
    assertEquals(value("1 + 2 max 3"), 4)
  }

  test("識別子 infix の右辺に単項マイナス") {
    assertEquals(value("a max -b"), 2)
  }

  test("識別子 infix の右辺に関数適用") {
    assertEquals(value("a max inc(b)"), 6)
  }

  test("キーワードは infix にならない: let-in") {
    assertEquals(value("let x: i32 = 1 in x"), 1)
  }

  test("キーワードは infix にならない: if-then-else") {
    assertEquals(value("if a > b then a else b"), 5)
  }

  test("キーワードは infix にならない: match") {
    val source = prelude +
      """data Maybe = { Just(i32), Nothing }
        |let get(e: Maybe): i32 = match e { Just(x) => x Nothing => b }
        |""".stripMargin
    assertEquals(Lam.exitCode(source + "let main(): i32 = get(Just(4))\n"), 4)
    assertEquals(Lam.exitCode(source + "let main(): i32 = get(Nothing)\n"), 5)
  }

  test("キーワードは infix にならない: fold-as") {
    val source = prelude +
      """rec data Numbers = { Empty, More(i32)(Numbers) }
        |let main(): i32 = fold More(1)(More(2)(Empty)) as i32 { More(x)(rest) => x + rest Empty => a }
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 5)
  }

  test("with 節付きの let は従来どおり読める") {
    val source =
      """trait Show[a] { def show(x: a): i32 }
        |impl Show[i32] { def show(x: i32): i32 = x }
        |let f[a](x: a): i32 with Show[a] = show[a](x)
        |let main(): i32 = f(3)
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 3)
  }

  test("型チェック: ユーザー定義2引数関数の infix 使用") {
    val source =
      """trait Gt[a] { def gt(x: a)(y: a): bool }; infix 4 > = gt; impl Gt[i32] { def gt(x: i32)(y: i32): bool = intrinsic[gt_i32](x)(y) }
        |let max(x: i32)(y: i32): i32 = if x > y then x else y
        |let main(): i32 = 2 max 5
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 5)
  }

  test("型チェック: infix と記号演算子の混在") {
    val source =
      """trait Add[a] { def add(x: a)(y: a): a }; infixl 6 + = add; impl Add[i32] { def add(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y) }
        |trait Gt[a] { def gt(x: a)(y: a): bool }; infix 4 > = gt; impl Gt[i32] { def gt(x: i32)(y: i32): bool = intrinsic[gt_i32](x)(y) }
        |let max(x: i32)(y: i32): i32 = if x > y then x else y
        |let main(): i32 = 1 + 2 max 3
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 4)
  }

  test("型エラー: 未定義識別子の infix 使用") {
    Lam.errors("let main(): i32 = 2 undefinedFn 5\n") match {
      case List(CompileError.UndefinedVariable("undefinedFn", _)) => ()
      case other => fail(s"expected an undefined variable: $other")
    }
  }

  test("型エラー: 1引数関数の infix 使用") {
    val source =
      """let inc(x: i32): i32 = intrinsic[add_i32](x)(1)
        |let main(): i32 = 2 inc 5
        |""".stripMargin
    Lam.errors(source) match {
      case List(_: CompileError.NotAFunction) => ()
      case other => fail(s"expected a not-a-function error: $other")
    }
  }
}
