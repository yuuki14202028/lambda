package com.yuuki14202028

/** ソースを最後まで通し、インタプリタで動かして確かめる */
class PipelineSuite extends munit.FunSuite {

  // 演算子は言語に埋め込まれていないので、テストの先頭で宣言する
  private val prelude =
    """let addInt(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y)
      |let subInt(x: i32)(y: i32): i32 = intrinsic[sub_i32](x)(y)
      |let mulInt(x: i32)(y: i32): i32 = intrinsic[mul_i32](x)(y)
      |let eqInt(x: i32)(y: i32): bool = intrinsic[eq_i32](x)(y)
      |let ltInt(x: i32)(y: i32): bool = intrinsic[lt_i32](x)(y)
      |let negInt(x: i32): i32 = intrinsic[neg_i32](x)
      |infixl 6 + = addInt
      |infixl 6 - = subInt
      |infixl 7 * = mulInt
      |infix 4 == = eqInt
      |infix 4 < = ltInt
      |prefix - = negInt
      |
      |type String = foreign.C.String
      |let print: String → i32 = foreign[String → i32] print_string_inline
      |let showInt: i32 → String = foreign[i32 → String] int_to_string
      |
      |data Option[a] = { Some(a), None }
      |data Pair[a][b] = { Pair(a)(b) }
      |rec data List[a] = { Nil, Cons(a)(List[a]) }
      |""".stripMargin

  private def run(body: String): Interpreter.Result =
    Compiler.compile(prelude + body) match {
      case Right(program) => Interpreter.run(program)
      case Left(errors) => fail(Diagnostics.render(errors, "test.lam", prelude + body))
    }

  private def exitCode(body: String): Int = run(body).exitCode

  private def errors(body: String): List[String] =
    Compiler.compile(prelude + body).fold(_.map(_.message), _ => Nil)

  private def assertError(body: String, expected: String)(using munit.Location): Unit = {
    val found = errors(body)
    assert(found.exists(_.contains(expected)), clue(found))
  }

  test("main の結果が終了コードになる") {
    assertEquals(exitCode("let main(): i32 = 42"), 42)
  }

  test("演算子は宣言された優先順位と結合性で結び付く") {
    assertEquals(exitCode("let main(): i32 = 1 + 2 * 3 - 4"), 3)
    assertEquals(exitCode("let main(): i32 = (1 + 2) * 3"), 9)
    assertEquals(exitCode("let main(): i32 = 10 - 3 - 2"), 5)
    assertEquals(exitCode("let main(): i32 = 1 + -2 * -3"), 7)
  }

  test("結合しない演算子は並べて書けない") {
    assertError("let main(): i32 = if 1 < 2 < 3 then 0 else 1", "cannot be mixed")
  }

  test("宣言のない演算子は使えない") {
    assertError("let main(): i32 = 1 / 2", "`/` is not declared")
  }

  test("演算子には、多相でない関数も多相な関数も宣言できる") {
    val source =
      """let pipe[a][b](x: a)(f: a → b): b = f(x)
        |infixl 1 |> = pipe
        |let main(): i32 = 20 |> (λn: i32. n + 1) |> (λn: i32. n * 2)
        |""".stripMargin
    assertEquals(exitCode(source), 42)
  }

  test("&& と || は右を必要なときだけ評価する") {
    val source =
      """rec let loop(n: i32): bool = loop(n)
        |let main(): i32 = if (1 == 2 && loop(0)) || 1 == 1 then 7 else 8
        |""".stripMargin
    assertEquals(exitCode(source), 7)
  }

  test("関数・let・if・ブロック") {
    val source =
      """let twice(f: i32 → i32)(x: i32): i32 = f(f(x))
        |let main(): i32 = {
        |  print("a");
        |  print("b");
        |  let step: i32 = 3 in twice(λn: i32. n + step)(if 1 < 2 then 10 else 20)
        |}
        |""".stripMargin
    assertEquals(run(source), Interpreter.Result(16, "ab"))
  }

  test("rec let: 再帰と相互再帰") {
    val source =
      """rec let factorial(n: i32): i32 = if n == 0 then 1 else n * factorial(n - 1)
        |rec {
        |  let isEven(n: i32): bool = if n == 0 then true else isOdd(n - 1)
        |  let isOdd(n: i32): bool = if n == 0 then false else isEven(n - 1)
        |}
        |let main(): i32 = if isEven(10) then factorial(5) else 0
        |""".stripMargin
    assertEquals(exitCode(source), 120)
  }

  test("rec でなければ自分自身は見えない") {
    assertError("let f(n: i32): i32 = f(n)\nlet main(): i32 = 0", "cannot find value `f`")
  }

  test("rec の値が関数でなければ、組の値を参照できない") {
    assertError("rec let x: i32 = x\nlet main(): i32 = x", "before it is initialized")
  }

  test("深い再帰でも動く") {
    val source =
      """rec let count(n: i32)(acc: i32): i32 = if n == 0 then acc else count(n - 1)(acc + 1)
        |let main(): i32 = if count(200000)(0) == 200000 then 0 else 1
        |""".stripMargin
    assertEquals(exitCode(source), 0)
  }

  test("data と match: 入れ子のパターン") {
    val source =
      """rec let sum(xs: List[i32]): i32 = match xs {
        |  Nil => 0
        |  Cons(x)(rest) => x + sum(rest)
        |}
        |let firstTwo(xs: List[Option[i32]]): i32 = match xs {
        |  Cons(Some(a))(Cons(Some(b))(_)) => a + b
        |  Cons(Some(a))(_) => a
        |  _ => 0
        |}
        |let main(): i32 =
        |  sum(Cons(1)(Cons(2)(Cons(3)(Nil[i32])))) * 10 + firstTwo(Cons(Some(4))(Cons(Some(5))(Nil[Option[i32]])))
        |""".stripMargin
    assertEquals(exitCode(source), 69)
  }

  test("match は網羅していなければエラーで、当たらない値の例を示す") {
    val source =
      """let f(xs: List[Option[i32]]): i32 = match xs {
        |  Nil => 0
        |  Cons(Some(x))(_) => x
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertError(source, "`Cons(None)(_)` is not matched")
  }

  test("到達できない分岐はエラー") {
    val source =
      """let f(o: Option[i32]): i32 = match o {
        |  _ => 0
        |  Some(x) => x
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertError(source, "case 2 of this match can never be reached")
  }

  test("引数のない名前のパターンは、構成子がなければ変数") {
    val source =
      """let orZero(o: Option[i32]): i32 = match o {
        |  None => 0
        |  other => match other {
        |    Some(x) => x
        |    None => 0
        |  }
        |}
        |let main(): i32 = orZero(Some(5))
        |""".stripMargin
    assertEquals(exitCode(source), 5)
  }

  test("相互再帰のデータ型") {
    val source =
      """rec {
        |  data Tree = { Node(i32)(Forest) }
        |  data Forest = { Empty, More(Tree)(Forest) }
        |}
        |rec {
        |  let sizeTree(t: Tree): i32 = match t {
        |    Node(_)(children) => 1 + sizeForest(children)
        |  }
        |  let sizeForest(f: Forest): i32 = match f {
        |    Empty => 0
        |    More(t)(rest) => sizeTree(t) + sizeForest(rest)
        |  }
        |}
        |let main(): i32 = sizeTree(Node(1)(More(Node(2)(Empty))(More(Node(3)(Empty))(Empty))))
        |""".stripMargin
    assertEquals(exitCode(source), 3)
  }

  test("rec でないデータ型は自分自身を参照できない") {
    assertError("data Nat = { Zero, Succ(Nat) }\nlet main(): i32 = 0", "cannot find type `Nat`")
  }

  test("型の別名・型構成子の別名・カインド") {
    val source =
      """type Transform[a] = a → a
        |type Maybe = Option
        |let wrapTwice[f: * → *](wrap: ∀a. a → f[a])(x: i32): f[f[i32]] = wrap[f[i32]](wrap[i32](x))
        |let increment: Transform[i32] = λn: i32. n + 1
        |let main(): i32 = match wrapTwice[Maybe](Λa. λx: a. Some(x))(increment(1)) {
        |  Some(Some(n)) => n
        |  _ => 0
        |}
        |""".stripMargin
    assertEquals(exitCode(source), 2)
  }

  test("カインドが合わない型はエラー") {
    assertError("let x: Option = None[i32]\nlet main(): i32 = 0", "mismatched kinds: expected `*`, found `* → *`")
    assertError("let x: i32[i32] = 0\nlet main(): i32 = 0", "mismatched kinds")
  }

  test("互いに無関係な宣言のエラーは、まとめて報告する") {
    assertEquals(errors("let a: i32 = true\nlet b: bool = 1\nlet main(): i32 = 0").length, 2)
  }

  test("本体が失敗した宣言も、注釈の型で後続の宣言から使える") {
    val found = errors("let a: i32 = true\nlet b: bool = a\nlet main(): i32 = 0")
    assertEquals(found.length, 2, clue(found))
  }

  test("main がない・main の型が違う") {
    assertError("let x: i32 = 0", "the program has no `main`")
    assertError("let main: i32 = 0", "`main` must have type `unit → i32`")
  }

  test("未知の intrinsic はエラーとして報告する") {
    assertError("let f(x: i32): i32 = intrinsic[foo_i32](x)(x)\nlet main(): i32 = 0", "unknown intrinsic `foo_i32`")
  }

  test("コメントと、宣言の途中の改行") {
    val source =
      """// 行コメント
        |let main(): i32 = /* 途中 /* 入れ子 */ */
        |  1 +
        |  2
        |""".stripMargin
    assertEquals(exitCode(source), 3)
  }
}
