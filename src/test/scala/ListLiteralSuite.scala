package com.yuuki14202028

import munit.FunSuite

class ListLiteralSuite extends FunSuite {

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

  private val list = "rec data List[a] = { Nil, Cons(a)(List[a]) }\nliteral [] = Nil, Cons\n"
  private val main = "let main(): i32 = 0\n"

  test("[x, y] は、literal [] に宣言した関数を最初の要素の型で具体化して組み立てる") {
    assertRight(encode(list + "let xs: List[i32] = [1, 2, 3]\nlet ys: List[bool] = [true]\nlet zs: List[List[i32]] = [[1], [2, 3]]\n" + main))
    val shown = validate(list + "let xs: List[i32] = [1, 2]\n" + main).map(ModuleDesugar.desugar).map(OperatorDesugar.desugar).fold(fail(_), _.show)
    assert(shown.contains("Cons") && shown.contains("Nil") && !shown.contains("[1, 2]"), clue(shown))
  }

  test("要素の型は最初の要素に合わせる") {
    assertLeftContains(validate(list + "let xs: List[i32] = [1, true]\n" + main), "Type mismatch")
    assertLeftContains(validate(list + "let xs: List[bool] = [1, 2]\n" + main), "Type mismatch")
  }

  test("空の [] は書けない") {
    assertLeftContains(validate(list + "let xs: List[i32] = []\n" + main), "An empty list literal")
  }

  test("literal [] が宣言されていなければ書けない") {
    assertLeftContains(validate("let xs: i32 = [1]\n" + main), "literal [] is not declared")
  }

  test("literal [] には、nil: ∀a. R と cons: ∀a. a → R → R の組だけを宣言できる") {
    assertLeftContains(validate("rec data List[a] = { Nil, Cons(a)(List[a]) }\nliteral [] = Cons, Nil\n" + main), "cannot be declared for literal []")
    // 構成子でなくてもよい（R が同じなら）
    val custom = "rec data Bag[a] = { Empty, Put(a)(Bag[a]) }\nlet none[a]: Bag[a] = Empty[a]\nlet put[a](x: a)(b: Bag[a]): Bag[a] = Put[a](x)(b)\nliteral [] = none, put\n"
    assertRight(encode(custom + "let b: Bag[i32] = [1, 2]\n" + main))
  }

  test("宣言の後に改行して literal [] を書いても、式の続きとは読まない") {
    assertRight(encode("rec data List[a] = { Nil, Cons(a)(List[a]) }\nlet one: List[i32] = Cons[i32](1)(Nil[i32])\nliteral [] = Nil, Cons\nlet xs: List[i32] = [1]\n" + main))
  }

  test("リストリテラルは式の中のどこにでも書け、型引数の後置とぶつからない") {
    assertRight(encode(list + "let f(xs: List[i32]): List[i32] = xs\nlet ys: List[i32] = f([1, 2])\nlet n: List[i32] = Cons[i32](0)([1])\n" + main))
  }

  // ---- 名前付きのリテラル ----

  private val bag =
    """rec data Bag[a] = { Empty, Put(a)(Bag[a]) }
      |let none[a]: Bag[a] = Empty[a]
      |let put[a](x: a)(b: Bag[a]): Bag[a] = Put[a](x)(b)
      |literal bagOf = none, put
      |""".stripMargin

  test("名前付きのリテラル name[x, y] は、literal name に宣言した関数で組み立てる") {
    assertRight(encode(list + bag + "let b: Bag[i32] = bagOf[1, 2]\nlet one(x: i32): Bag[i32] = bagOf[x]\nlet xs: List[i32] = [1]\n" + main))
    assertLeftContains(validate(list + bag + "let b: Bag[bool] = bagOf[1, 2]\n" + main), "Type mismatch")
    assertLeftContains(validate(list + bag + "let b: Bag[i32] = bagOf[]\n" + main), "An empty list literal")
  }

  test("名前付きのリテラルでない名前の [ … ] は型引数（中身が型でなければエラー）") {
    assertRight(encode(list + "let xs: List[i32] = Cons[i32](1)(Nil[i32])\n" + main))
    assertLeftContains(validate(list + "let f[a](x: a): a = x\nlet n: i32 = f[1, 2](3)\n" + main), "is not a literal")
  }

  test("名前付きのリテラルは値と同じ名前の空間にあり、同じ名前の束縛に隠される") {
    assertLeftContains(validate(list + bag + "let f(bagOf: i32): Bag[i32] = bagOf[1, 2]\n" + main), "is not a literal")
  }

  test("型引数がいくつあっても、cons の最初の引数の型から読む") {
    val src = list +
      """data Pair[a][b] = { Pair(a)(b) }
        |literal (,) = Pair
        |rec data Table[k][v] = { Done, Row(k)(v)(Table[k][v]) }
        |let done[k][v]: Table[k][v] = Done[k][v]
        |let row[k][v](p: Pair[k][v])(t: Table[k][v]): Table[k][v] = match p { Pair(k1)(v1) => Row[k][v](k1)(v1)(t) }
        |literal tableOf = done, row
        |let t: Table[i32][bool] = tableOf[(1, true), (2, false)]
        |""".stripMargin
    assertRight(encode(src + main))
  }

  test("module の中で宣言した名前付きのリテラルは、修飾するか open して使う") {
    val src = list +
      """module Bags {
        |  rec data Bag[a] = { Empty, Put(a)(Bag[a]) }
        |  let none[a]: Bag[a] = Empty[a]
        |  let put[a](x: a)(b: Bag[a]): Bag[a] = Put[a](x)(b)
        |  literal bagOf = none, put
        |}
        |""".stripMargin
    assertRight(encode(src + "let b: Bags.Bag[i32] = Bags.bagOf[1, 2]\n" + main))
    assertRight(encode(src + "open Bags\nlet b: Bag[i32] = bagOf[1, 2]\n" + main))
    assertLeftContains(validate(src + "let b: Bags.Bag[i32] = bagOf[1, 2]\n" + main), "is not a literal")
  }
}
