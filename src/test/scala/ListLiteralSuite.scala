package com.yuuki14202028

/** リストリテラル [x, y] と名前付きのリテラル name[x, y]: literal に宣言した関数で組み立てる */
class ListLiteralSuite extends munit.FunSuite {

  private val list =
    """rec data List[a] = { Nil, Cons(a)(List[a]) }
      |literal [] = Nil, Cons
      |let addInt(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y)
      |infixl 6 + = addInt
      |rec let sum(xs: List[i32]): i32 = match xs {
      |  Nil => 0
      |  Cons(x)(rest) => x + sum(rest)
      |}
      |rec let length[a](xs: List[a]): i32 = match xs {
      |  Nil => 0
      |  Cons(_)(rest) => 1 + length(rest)
      |}
      |""".stripMargin
  private val main = "let main(): i32 = 0\n"

  /** expected に当たるエラーが報告される */
  private def assertError(source: String)(expected: PartialFunction[CompileError, Any])(using munit.Location): Unit = {
    val found = Lam.errors(source)
    assert(found.exists(expected.isDefinedAt), clue(found))
  }

  private def assertMismatch(source: String)(using munit.Location): Unit =
    assertError(source) { case _: CompileError.TypeMismatch => }

  test("[x, y] は、literal [] に宣言した関数を最初の要素の型で具体化して組み立てる") {
    val source = list +
      """let xs: List[i32] = [1, 2, 3]
        |let ys: List[bool] = [true]
        |let zs: List[List[i32]] = [[1], [2, 3]]
        |""".stripMargin
    assertEquals(Lam.exitCode(source + "let main(): i32 = sum(xs)\n"), 6)
    assertEquals(Lam.exitCode(source + "let main(): i32 = length(ys) + length(zs)\n"), 3)
    // 右から組み立てる: 先頭は最初の要素
    assertEquals(Lam.exitCode(source + "let main(): i32 = match xs {\n  Cons(x)(Cons(y)(Cons(z)(Nil))) => x + y + y + z + z + z\n  _ => 0\n}\n"), 14)
  }

  test("要素の型は最初の要素に合わせる") {
    assertMismatch(list + "let xs: List[i32] = [1, true]\n" + main)
    assertMismatch(list + "let xs: List[bool] = [1, 2]\n" + main)
  }

  test("空の [] は書けない") {
    assertError(list + "let xs: List[i32] = []\n" + main) { case _: CompileError.EmptyListLiteral => }
  }

  test("literal [] が宣言されていなければ書けない") {
    assertError("let xs: i32 = [1]\n" + main) { case _: CompileError.SyntaxNotDeclared => }
  }

  test("literal [] には、nil: ∀a. R と cons: ∀a. a → R → R の組だけを宣言できる") {
    // 構成子でなくてもよい（R が同じなら）
    val custom =
      """rec data Bag[a] = { Empty, Put(a)(Bag[a]) }
        |let none[a]: Bag[a] = Empty[a]
        |let put[a](x: a)(b: Bag[a]): Bag[a] = Put[a](x)(b)
        |literal [] = none, put
        |let b: Bag[i32] = [1, 2]
        |let main(): i32 = match b {
        |  Put(x)(Put(y)(Empty)) => intrinsic[add_i32](x)(y)
        |  _ => 0
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(custom), 3)
    // 形の合わない関数も宣言はできて、リテラルを書いた所でエラーになる
    val swapped = "rec data List[a] = { Nil, Cons(a)(List[a]) }\nliteral [] = Cons, Nil\n"
    assertEquals(Lam.errors(swapped + main), Nil)
    assert(Lam.errors(swapped + "let xs: List[i32] = [1, 2]\n" + main).exists { case _: CompileError.NotAFunction => true; case _ => false })
  }

  test("宣言の後に改行して literal [] を書いても、式の続きとは読まない") {
    val source =
      """rec data List[a] = { Nil, Cons(a)(List[a]) }
        |let one: List[i32] = Cons[i32](1)(Nil[i32])
        |literal [] = Nil, Cons
        |let xs: List[i32] = [5]
        |let main(): i32 = match xs {
        |  Cons(x)(_) => x
        |  Nil => 0
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 5)
  }

  test("リストリテラルは式の中のどこにでも書け、型引数の後置とぶつからない") {
    val source = list +
      """let f(xs: List[i32]): List[i32] = xs
        |let ys: List[i32] = f([1, 2])
        |let n: List[i32] = Cons[i32](10)([1])
        |let main(): i32 = sum(ys) + sum(n)
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 14)
  }

  // ---- 名前付きのリテラル ----

  private val bag =
    """rec data Bag[a] = { Empty, Put(a)(Bag[a]) }
      |let none[a]: Bag[a] = Empty[a]
      |let put[a](x: a)(b: Bag[a]): Bag[a] = Put[a](x)(b)
      |literal bagOf = none, put
      |rec let total(b: Bag[i32]): i32 = match b {
      |  Empty => 0
      |  Put(x)(rest) => x + total(rest)
      |}
      |""".stripMargin

  test("名前付きのリテラル name[x, y] は、literal name に宣言した関数で組み立てる") {
    val source = list + bag +
      """let b: Bag[i32] = bagOf[1, 2]
        |let one(x: i32): Bag[i32] = bagOf[x]
        |let xs: List[i32] = [10]
        |let main(): i32 = total(b) + total(one(4)) + sum(xs)
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 17)
    assertMismatch(list + bag + "let b: Bag[bool] = bagOf[1, 2]\n" + main)
    assertError(list + bag + "let b: Bag[i32] = bagOf[]\n" + main) { case _: CompileError.EmptyListLiteral => }
  }

  test("名前付きのリテラルでない名前の [ … ] は型引数（中身が型でなければエラー）") {
    assertEquals(Lam.exitCode(list + "let xs: List[i32] = Cons[i32](1)(Nil[i32])\nlet main(): i32 = sum(xs)\n"), 1)
    assertError(list + "let f[a](x: a): a = x\nlet n: i32 = f[1, 2](3)\n" + main) { case _: CompileError.Syntax => }
  }

  test("名前付きのリテラルは値と同じ名前の空間にあり、同じ名前の束縛に隠される") {
    assertError(list + bag + "let f(bagOf: i32): Bag[i32] = bagOf[1, 2]\n" + main) { case _: CompileError.Syntax => }
  }

  test("型引数がいくつあっても、cons の最初の引数の型から読む") {
    val source = list +
      """data Pair[a][b] = { Pair(a)(b) }
        |literal (,) = Pair
        |rec data Table[k][v] = { Done, Row(k)(v)(Table[k][v]) }
        |let done[k][v]: Table[k][v] = Done[k][v]
        |let row[k][v](p: Pair[k][v])(t: Table[k][v]): Table[k][v] = match p { Pair(k1)(v1) => Row[k][v](k1)(v1)(t) }
        |literal tableOf = done, row
        |let t: Table[i32][bool] = tableOf[(1, true), (2, false)]
        |let main(): i32 = match t {
        |  Row(a)(true)(Row(b)(false)(Done)) => a + b
        |  _ => 0
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 3)
  }

  test("module の中で宣言した名前付きのリテラルは、修飾するか open して使う") {
    val source = list +
      """module Bags {
        |  rec data Bag[a] = { Empty, Put(a)(Bag[a]) }
        |  let none[a]: Bag[a] = Empty[a]
        |  let put[a](x: a)(b: Bag[a]): Bag[a] = Put[a](x)(b)
        |  literal bagOf = none, put
        |  let first(b: Bag[i32]): i32 = match b {
        |    Empty => 0
        |    Put(x)(_) => x
        |  }
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(source + "let b: Bags.Bag[i32] = Bags.bagOf[1, 2]\nlet main(): i32 = Bags.first(b)\n"), 1)
    assertEquals(Lam.exitCode(source + "open Bags\nlet b: Bag[i32] = bagOf[3, 2]\nlet main(): i32 = first(b)\n"), 3)
    assertError(source + "let b: Bags.Bag[i32] = bagOf[1, 2]\n" + main) { case CompileError.UndefinedVariable("bagOf", _) => }
  }
}
