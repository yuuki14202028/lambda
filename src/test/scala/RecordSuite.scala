package com.yuuki14202028

import munit.FunSuite

class RecordSuite extends FunSuite {

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

  private val point = "data Point = { MkPoint(x: i32)(y: bool) }\nlet p: Point = MkPoint(1)(true)\n"
  private val main = "let main(): i32 = 0\n"

  test("名前付きフィールドは、型の module にフィールドを読む関数を作る") {
    assertRight(encode(point + "let a: i32 = p.x\nlet b: bool = p.y\n" + main))
    assertRight(encode(point + "let a: i32 = Point.x(p)\nlet f: Point → bool = Point.y\n" + main))
    assertLeftContains(validate(point + "let a: bool = p.x\n" + main), "Type mismatch")
    assertLeftContains(validate(point + "let a: i32 = p.z\n" + main), "Module Point has no member z")
  }

  test("アクセサは、構成子を 1 段だけ分解する関数に脱糖される") {
    val shown = validate(point + main).map(typed => RecordDesugar.desugar(ModuleDesugar.desugar(typed))).fold(fail(_), _.show)
    assert(shown.linesIterator.exists(l => l.startsWith("let Point.x: Point → i32") && l.contains("MkPoint")), clue(shown))
    assert(shown.linesIterator.exists(_.startsWith("let Point.y: Point → bool")), clue(shown))
  }

  test("構築とパターンは、位置で書く（今までどおり）") {
    assertRight(encode(point + "let n: i32 = match p { MkPoint(a)(_) => a }\n" + main))
    assertRight(encode(point + "let n: i32 = let MkPoint(a)(b): Point = p in a\n" + main))
  }

  test("型引数を持つ data: フィールドの型は受け手の型から決まる") {
    val src = "data Box[a] = { MkBox(content: a)(count: i32) }\n"
    assertRight(encode(src + "let n: i32 = MkBox[bool](true)(2).count\nlet b: bool = MkBox[bool](true)(2).content\n" + main))
    assertRight(encode(src + "let f[a](box: Box[a]): a = box.content\n" + main))
    assertRight(encode(src + "let g: ∀a. Box[a] → a = Box.content\n" + main))
  }

  test("名前のないフィールドと混ぜられる") {
    val src = "data Entry = { MkEntry(key: i32)(bool)(value: i32) }\nlet e: Entry = MkEntry(1)(true)(2)\n"
    assertRight(encode(src + "let n: i32 = e.key\nlet m: i32 = e.value\n" + main))
  }

  test("型の module にメンバーを足せる。フィールドは名前だけで書ける") {
    val src = point + "module Point {\n  let double(q: Point): i32 = x(q)\n  let flipped(q: Point): Point = MkPoint(q.x)(q.y)\n}\n"
    assertRight(encode(src + "let n: i32 = p.double\nlet m: i32 = p.flipped.x\n" + main))
  }

  test("module の中の data: アクセサは、その型の module（M.T）のメンバー") {
    val src = "module Geometry {\n  data Circle = { MkCircle(radius: i32) }\n  let unit: Circle = MkCircle(1)\n  let r: i32 = unit.radius\n}\n"
    assertRight(encode(src + "let n: i32 = Geometry.unit.radius\nlet m: i32 = Geometry.Circle.radius(Geometry.unit)\n" + main))
    assertRight(encode(src + "open Geometry\nlet n: i32 = Circle.radius(unit)\n" + main))
  }

  test("rec data の組でも使える") {
    val src =
      """rec {
        |  data Tree = { Node(label: i32)(children: Forest) }
        |  data Forest = { Empty, Trees(Tree)(Forest) }
        |}
        |let t: Tree = Node(1)(Empty)
        |let n: i32 = t.label
        |let f: Forest = t.children
        |""".stripMargin
    assertRight(encode(src + main))
    assertRight(encode("rec data Chain = { Link(value: i32)(next: Chain) }\nlet f(c: Chain): i32 = c.next.next.value\n" + main))
  }

  test("opaque type をフィールドに持つ data: module の外では抽象型のまま読める") {
    val src =
      """module Bank {
        |  opaque type Money = i32
        |  data Account = { MkAccount(balance: Money) }
        |  let open: Account = MkAccount(100)
        |  let amount(m: Money): i32 = m
        |}
        |let m: Bank.Money = Bank.open.balance
        |let n: i32 = Bank.amount(Bank.open.balance)
        |""".stripMargin
    assertRight(encode(src + main))
    assertLeftContains(validate(src + "let k: i32 = Bank.open.balance\n" + main), "Type mismatch")
  }

  // ---- 書けない場合 ----

  test("フィールドの名前は、data の中で重複できない") {
    assertLeftContains(validate("data P = { MkP(x: i32)(x: i32) }\n" + main), "has duplicate fields")
  }

  test("構成子が 1 つでない data には、名前付きフィールドを書けない") {
    assertLeftContains(validate("data Shape = { Circle(radius: i32), Square(side: i32) }\n" + main),
      "Data type Shape cannot have named fields: it does not have exactly one constructor")
  }

  test("式の中の data には、名前付きフィールドを書けない") {
    assertLeftContains(validate("let n: i32 = data P = { MkP(x: i32) } in 0\n" + main), "cannot have named fields: it is declared inside an expression")
  }

  test("opaque data には、名前付きフィールドを書けない") {
    assertLeftContains(validate("module M {\n  opaque data P = { MkP(x: i32) }\n}\n" + main), "Data type M.P cannot have named fields: it is opaque")
  }
}
