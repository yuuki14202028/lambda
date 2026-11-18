package com.yuuki14202028

/** 型引数を値から読む規則: 適用の並びを左から処理し、値の引数は未決定の型パラメータを決め、
 * 書かれた [T] は未決定の最初の型パラメータを決める。並びの終わりで決まっていなければエラー。
 */
class TypeArgumentSuite extends munit.FunSuite {

  private val prelude =
    """let addInt(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y)
      |infixl 6 + = addInt
      |
      |data Option[a] = { Some(a), None }
      |data Pair[a][b] = { Pair(a)(b) }
      |data Either[a][b] = { Left(a), Right(b) }
      |rec data List[a] = { Nil, Cons(a)(List[a]) }
      |
      |let identity[a](x: a): a = x
      |let first[a][b](p: Pair[a][b]): a = match p {
      |  Pair(x)(_) => x
      |}
      |rec let map[a][b](xs: List[a])(f: a → b): List[b] = match xs {
      |  Nil => Nil[b]
      |  Cons(x)(rest) => Cons(f(x))(map(rest)(f))
      |}
      |rec let length[a](xs: List[a]): i32 = match xs {
      |  Nil => 0
      |  Cons(_)(rest) => 1 + length(rest)
      |}
      |""".stripMargin

  private def exitCode(body: String): Int =
    Compiler.compile(prelude + body) match {
      case Right(program) => Interpreter.run(program).exitCode
      case Left(errors) => fail(Diagnostics.render(errors, "test.lam", prelude + body))
    }

  private def assertError(body: String, expected: String)(using munit.Location): Unit = {
    val found = Compiler.compile(prelude + body).fold(_.map(_.message), _ => Nil)
    assert(found.exists(_.contains(expected)), clue(found))
  }

  test("値の引数から読む") {
    assertEquals(exitCode("let main(): i32 = identity(41) + 1"), 42)
    assertEquals(exitCode("let main(): i32 = first(Pair(7)(true))"), 7)
  }

  test("関数の引数の型からも読む（map の b）") {
    assertEquals(exitCode("let main(): i32 = length(map(Cons(1)(Cons(2)(Nil[i32])))(λn: i32. Some(n)))"), 2)
  }

  test("型引数をすべて書く今までの書き方も通る") {
    assertEquals(exitCode("let main(): i32 = first[i32][bool](Pair[i32][bool](7)(true))"), 7)
  }

  test("書かれた型引数は、未決定の最初の型パラメータを決める") {
    // xs で a が決まり、[Option[i32]] は残りの b
    assertEquals(exitCode("let main(): i32 = length(map(Cons(1)(Nil[i32]))[Option[i32]](λn: i32. Some(n)))"), 1)
    // 先に書けば a
    assertEquals(exitCode("let main(): i32 = length(map[i32](Cons(1)(Nil[i32]))(λn: i32. n))"), 1)
  }

  test("引数の型に現れない型引数は書く") {
    assertEquals(exitCode("let main(): i32 = length(Nil[bool])"), 0)
    assertError("let main(): i32 = length(Nil)", "mismatched types")
    assertError("let x: Either[i32][bool] = Left(1)\nlet main(): i32 = 0", "the type argument `b`")
    assertEquals(exitCode("let x: Either[i32][bool] = Left(1)[bool]\nlet main(): i32 = 0"), 0)
  }

  test("並びの終わりで決まらない型引数はエラー（部分適用）") {
    assertError("let main(): i32 = length(map(Cons(1)(Nil[i32])))", "the type argument `b`")
  }

  test("引数のない参照は多相のまま、型引数だけ渡した値も多相のまま渡せる") {
    val source =
      """let applyToInt(f: ∀a. a → a): i32 = f(1)
        |let pairWith[a][b](x: a)(y: b): Pair[a][b] = Pair(x)(y)
        |let withInt: ∀b. i32 → b → Pair[i32][b] = pairWith[i32]
        |let main(): i32 = applyToInt(identity) + first(withInt(1)(true))
        |""".stripMargin
    assertEquals(exitCode(source), 2)
  }

  test("読んだ型と合わない引数はエラー") {
    assertError("let main(): i32 = first(Pair(1)(2)) + first(1)", "mismatched types: expected `Pair[a][b]`, found `i32`")
    assertError("let same[a](x: a)(y: a): a = x\nlet main(): i32 = same(1)(true)", "mismatched types: expected `i32`, found `bool`")
  }

  test("型構成子も読む") {
    val source =
      """let twice[f: * → *][a](x: f[a])(again: f[a] → f[f[a]]): f[f[a]] = again(x)
        |let main(): i32 = match twice(Some(3))(λo: Option[i32]. Some(o)) {
        |  Some(Some(n)) => n
        |  _ => 0
        |}
        |""".stripMargin
    assertEquals(exitCode(source), 3)
  }

  test("型引数を取らない値に型引数は渡せない") {
    assertError("let main(): i32 = identity(1)[i32]", "which takes none")
  }

  test("関数でない値は適用できない") {
    assertError("let main(): i32 = 1(2)", "applied like a function")
  }
}
