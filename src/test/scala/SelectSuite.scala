package com.yuuki14202028

import munit.FunSuite

class SelectSuite extends FunSuite {

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

  private val prelude =
    """rec data List[a] = { Nil, Cons(a)(List[a]) }
      |data Pair[a][b] = { MkPair(a)(b) }
      |trait Show[a] { def show(x: a): i32 }
      |impl Show[i32] { def show = λn: i32. n }
      |module List {
      |  let isEmpty[a](xs: List[a]): bool = match xs { Nil => true  Cons(_)(_) => false }
      |  let prepend[a](xs: List[a])(x: a): List[a] = Cons[a](x)(xs)
      |  let convert[a][b](xs: List[a])(f: a → b): List[b] = Nil[b]
      |  let shown[a](xs: List[a]): i32 with Show[a] = 0
      |  let ints(xs: List[i32]): i32 = 0
      |  let repeat(count: i32)(xs: List[i32]): List[i32] = xs
      |  let swapped[b][a](xs: List[a])(y: b): b = y
      |}
      |module Pair {
      |  let first[a][b](p: Pair[a][b]): a = match p { MkPair(x)(_) => x }
      |}
      |module i32 {
      |  let twice(n: i32): i32 = n
      |}
      |let numbers: List[i32] = Cons[i32](1)(Nil[i32])
      |""".stripMargin

  private val main = "let main(): i32 = 0\n"

  private def program(body: String): String = prelude + body + "\n" + main

  // ---- 解決 ----

  test("x.f は、x の型の module の関数に、型から読んだ型引数と x を渡す") {
    val shown = encode(program("let b: bool = numbers.isEmpty")).fold(fail(_), _.show)
    assert(shown.linesIterator.exists(l => l.startsWith("let b") && l.contains("List.isEmpty") && l.contains("numbers")), clue(shown))
  }

  test("残りの引数は、ふつうの適用で渡す") {
    assertRight(encode(program("let more: List[i32] = numbers.prepend(0)")))
    assertLeftContains(validate(program("let more: List[i32] = numbers.prepend(true)")), "Type mismatch")
  }

  test("受け手の型から決まらない型引数は、名前の後に書く") {
    assertRight(encode(program("let bs: List[bool] = numbers.convert[bool](λn: i32. true)")))
    assertLeftContains(validate(program("let bs: List[bool] = numbers.convert(λn: i32. true)")), "List.convert needs 1 type argument(s) after the name, got 0")
    assertLeftContains(validate(program("let b: bool = numbers.isEmpty[i32]")), "List.isEmpty needs 0 type argument(s) after the name, got 1")
  }

  test("型引数が複数ある型: すべて受け手の型から読む") {
    assertRight(encode(program("let p: Pair[i32][bool] = MkPair[i32][bool](1)(true)\nlet n: i32 = p.first")))
  }

  test("制約は、受け手の型から読んだ型引数で解決する") {
    assertRight(encode(program("let n: i32 = numbers.shown")))
    assertLeftContains(validate(program("let bs: List[bool] = Nil[bool]\nlet n: i32 = bs.shown")), "No instance for Show[bool]")
    assertRight(encode(program("let f[a](xs: List[a]): i32 with Show[a] = xs.shown")))
  }

  test("最初の引数の型が具体的な関数は、その型の値にだけ使える") {
    assertRight(encode(program("let n: i32 = numbers.ints")))
    assertLeftContains(validate(program("let bs: List[bool] = Nil[bool]\nlet n: i32 = bs.ints")), "List.ints cannot be called with dot notation on a value of type List[bool]")
  }

  test("組み込みの型の module は、その型の名前") {
    assertRight(encode(program("let n: i32 = 1.twice")))
    assertRight(encode(program("let n: i32 = numbers.ints.twice.twice")))
  }

  test("式の続きにも書ける（適用・括弧・行を変えた続き）") {
    assertRight(encode(program("let b: bool = numbers.prepend(0).isEmpty")))
    assertRight(encode(program("let b: bool = (Cons[i32](1)(Nil[i32])).isEmpty")))
    assertRight(encode(program("let b: bool = numbers\n  .prepend(0)\n  .prepend(1)\n  .isEmpty")))
    assertRight(encode(program("let f(xs: List[i32]): bool = xs.isEmpty")))
  }

  // ---- 使えない場合 ----

  test("module にない名前") {
    assertLeftContains(validate(program("let n: i32 = numbers.length")), "Module List has no member length")
  }

  test("module のない型（型変数・関数）には使えない") {
    assertLeftContains(validate(program("let f[a](x: a): i32 = x.twice")), "Cannot use .twice on a value of type a: the type has no module")
    assertLeftContains(validate(program("let f(g: i32 → i32): i32 = g.twice")), "the type has no module")
  }

  test("最初の引数が受け手の型でない関数は、ドットで呼べない") {
    assertLeftContains(validate(program("let more: List[i32] = numbers.repeat(2)")), "List.repeat cannot be called with dot notation on a value of type List[i32]")
    assertRight(encode(program("let more: List[i32] = List.repeat(2)(numbers)")))
  }

  test("受け手の型から読む型引数は、関数の型引数の先頭に並んでいなければならない") {
    assertLeftContains(validate(program("let b: bool = numbers.swapped[bool](true)")), "List.swapped cannot be called with dot notation")
    assertRight(encode(program("let b: bool = List.swapped[bool][i32](numbers)(true)")))
  }

  test("private なメンバーは、module の外からドットでも呼べない") {
    val src = "module List {\n  private let secret[a](xs: List[a]): i32 = 0\n  let open[a](xs: List[a]): i32 = xs.secret\n}\n"
    assertRight(encode(program(src.replace("let open", "let visible"))))
    assertLeftContains(validate(program(src.replace("let open", "let visible") + "let n: i32 = numbers.secret")), "Module List has no member secret")
  }

  // ---- 名前の読み方 ----

  test("値は同じ名前の module を隠す: a.b の a が値なら、ドット記法") {
    val src =
      """module M {
        |  let isEmpty: i32 = 0
        |}
        |let viaModule: i32 = M.isEmpty
        |let f(M: List[i32]): bool = M.isEmpty
        |""".stripMargin
    assertRight(encode(program(src)))
  }

  test("修飾された名前の後にも続けられる") {
    val src =
      """module Data {
        |  let items: List[i32] = Nil[i32]
        |}
        |let b: bool = Data.items.isEmpty
        |let c: bool = Data.items.prepend(1).isEmpty
        |""".stripMargin
    assertRight(encode(program(src)))
    assertLeftContains(validate(program(src + "let d: bool = Data.missing.isEmpty")), "Variable Data.missing.isEmpty is not defined")
  }

  test("module の中の型の module は、その型の完全な名前") {
    val src =
      """module Shape {
        |  data T = { Rect(i32)(i32) }
        |  module T {
        |    let width(s: T): i32 = match s { Rect(w)(_) => w }
        |  }
        |}
        |let w: i32 = Shape.Rect(2)(3).width
        |""".stripMargin
    assertRight(encode(program(src)))
  }

  test("opaque type の値にも、その型の module の関数を呼べる") {
    val src =
      """module Counter {
        |  opaque type T = i32
        |  let zero: T = 0
        |  module T {
        |    let value(c: T): i32 = 0
        |  }
        |}
        |let n: i32 = Counter.zero.value
        |""".stripMargin
    assertRight(encode(program(src)))
    assertLeftContains(validate(program(src + "let m: i32 = Counter.zero.twice")), "Module Counter.T has no member twice")
  }

  test("x.f(with d) とは書けない（制約はドットの時点で解決する）") {
    assertLeftContains(validate(program("let d: Show[i32] = summon[Show[i32]]\nlet n: i32 = numbers.shown(with d)")), "Cannot pass an instance with (with …)")
    assertRight(encode(program("let d: Show[i32] = summon[Show[i32]]\nlet n: i32 = using d in numbers.shown")))
  }
}
