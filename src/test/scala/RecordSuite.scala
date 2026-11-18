package com.yuuki14202028

/** 名前付きフィールド: 型の module にできるアクセサと、名前での構築 */
class RecordSuite extends munit.FunSuite {

  private val point = "data Point = { MkPoint(x: i32)(y: bool) }\nlet p: Point = MkPoint(1)(true)\n"
  private val main = "let main(): i32 = 0\n"

  private def exitCode(source: String, result: String)(using munit.Location): Int = Lam.exitCode(s"$source\nlet main(): i32 = $result\n")

  /** expected に当たるエラーが報告される */
  private def assertError(source: String)(expected: PartialFunction[CompileError, Any])(using munit.Location): Unit = {
    val found = Lam.errors(source)
    assert(found.exists(expected.isDefinedAt), clue(found))
  }

  private def assertNamedArgumentsInvalid(source: String, reason: String)(using munit.Location): Unit =
    assertError(source) { case CompileError.NamedArgumentsInvalid(found, _) if found.contains(reason) => }

  test("名前付きフィールドは、型の module にフィールドを読む関数を作る") {
    assertEquals(exitCode(point + "let a: i32 = p.x\nlet b: bool = p.y", "if b then a else 5"), 1)
    assertEquals(exitCode(point + "let a: i32 = Point.x(p)\nlet f: Point → bool = Point.y", "if f(p) then a else 5"), 1)
    assertError(point + "let a: bool = p.x\n" + main) { case _: CompileError.TypeMismatch => }
    assertError(point + "let a: i32 = p.z\n" + main) {
      case CompileError.SelectUnknown("z", receiver, _, _, _) if Type.show(receiver) == "Point" =>
    }
  }

  test("構築とパターンは、位置で書く（今までどおり）") {
    assertEquals(exitCode(point + "let n: i32 = match p { MkPoint(a)(_) => a }", "n"), 1)
    assertEquals(exitCode(point + "let n: i32 = let MkPoint(a)(b): Point = p in a", "n"), 1)
  }

  test("型引数を持つ data: フィールドの型は受け手の型から決まる") {
    val source = "data Box[a] = { MkBox(content: a)(count: i32) }\n"
    assertEquals(exitCode(source + "let n: i32 = MkBox[bool](true)(2).count\nlet b: bool = MkBox[bool](true)(2).content", "if b then n else 0"), 2)
    assertEquals(exitCode(source + "let f[a](box: Box[a]): a = box.content", "f(MkBox(3)(0))"), 3)
    assertEquals(exitCode(source + "let g: ∀a. Box[a] → a = Box.content", "g[i32](MkBox(4)(0))"), 4)
  }

  test("名前のないフィールドと混ぜられる") {
    val source = "data Entry = { MkEntry(key: i32)(bool)(value: i32) }\nlet e: Entry = MkEntry(1)(true)(2)\n"
    assertEquals(exitCode(source + "let n: i32 = e.key\nlet m: i32 = e.value", "intrinsic[add_i32](n)(m)"), 3)
  }

  test("型の module にメンバーを足せる。フィールドは名前だけで書ける") {
    val source = point + "module Point {\n  let double(q: Point): i32 = intrinsic[add_i32](x(q))(x(q))\n  let flipped(q: Point): Point = MkPoint(q.x)(q.y)\n}\n"
    assertEquals(exitCode(source + "let n: i32 = p.double\nlet m: i32 = p.flipped.x", "intrinsic[add_i32](n)(m)"), 3)
  }

  test("module の中の data: アクセサは、その型の module（M.T）のメンバー") {
    val source = "module Geometry {\n  data Circle = { MkCircle(radius: i32) }\n  let unit: Circle = MkCircle(1)\n  let r: i32 = unit.radius\n}\n"
    assertEquals(exitCode(source + "let n: i32 = Geometry.unit.radius\nlet m: i32 = Geometry.Circle.radius(Geometry.unit)", "intrinsic[add_i32](n)(m)"), 2)
    assertEquals(exitCode(source + "open Geometry\nlet n: i32 = Circle.radius(unit)", "n"), 1)
  }

  test("rec data の組でも使える") {
    val source =
      """rec {
        |  data Tree = { Node(label: i32)(children: Forest) }
        |  data Forest = { Empty, Trees(Tree)(Forest) }
        |}
        |let t: Tree = Node(1)(Empty)
        |let n: i32 = t.label
        |let f: Forest = t.children
        |""".stripMargin
    assertEquals(exitCode(source, "match f { Empty => n  Trees(_)(_) => 0 }"), 1)
    // 値を作れない型なので、検査が通ることだけを確かめる
    assertEquals(Lam.exitCode("rec data Chain = { Link(value: i32)(next: Chain) }\nlet f(c: Chain): i32 = c.next.next.value\n" + main), 0)
  }

  test("opaque type をフィールドに持つ data: module の外では抽象型のまま読める") {
    val source =
      """module Bank {
        |  opaque type Money = i32
        |  data Account = { MkAccount(balance: Money) }
        |  let opened: Account = MkAccount(100)
        |  let amount(m: Money): i32 = m
        |}
        |let m: Bank.Money = Bank.opened.balance
        |let n: i32 = Bank.amount(Bank.opened.balance)
        |""".stripMargin
    assertEquals(exitCode(source, "n"), 100)
    assertError(source + "let k: i32 = Bank.opened.balance\n" + main) { case _: CompileError.TypeMismatch => }
  }

  // ---- 書けない場合 ----

  test("フィールドの名前は、data の中で重複できない") {
    assertError("data P = { MkP(x: i32)(x: i32) }\n" + main) { case CompileError.AlreadyDefined("x", _) => }
  }

  test("構成子が 1 つでない data には、名前付きフィールドを書けない") {
    assertError("data Shape = { Circle(radius: i32), Square(side: i32) }\n" + main) { case _: CompileError.FieldNamesNotAllowed => }
  }

  test("式の中の data には、名前付きフィールドを書けない") {
    assertError("let n: i32 = data P = { MkP(x: i32) } in 0\n" + main) { case _: CompileError.FieldNamesNotAllowed => }
  }

  test("opaque data には、名前付きフィールドを書けない") {
    assertError("module M {\n  opaque data P = { MkP(x: i32) }\n}\n" + main) { case _: CompileError.FieldNamesNotAllowed => }
  }

  // ---- 名前付きの構築 ----

  private val tagged = "data Tagged[a] = { MkTagged(tag: i32)(value: a) }\ndata Line = { MkLine(from: Point)(Point) }\n"

  test("名前付きの構築: フィールドを名前で、好きな順に渡せる") {
    assertEquals(exitCode(point + "let q: Point = MkPoint(x = 1, y = true)\nlet r: Point = MkPoint(y = false, x = 2)", "if r.y then 0 else intrinsic[add_i32](q.x)(r.x)"), 3)
    assertEquals(exitCode(point + tagged + "let t: Tagged[bool] = MkTagged[bool](value = true, tag = 4)", "if t.value then t.tag else 0"), 4)
    assertEquals(exitCode(point + "let q: Point = MkPoint(\n  x = 5,\n  y = true\n)", "q.x"), 5)
  }

  test("名前付きの構築: 値は書いた順に評価される") {
    val source = point +
      """let print: foreign.C.String → i32 = foreign[foreign.C.String → i32] print_string_inline
        |let main(): i32 = {
        |  let r: Point = MkPoint(y = { print("y"); false }, x = { print("x"); 2 }) in
        |  let q: Point = MkPoint(x = { print("a"); 3 }, y = { print("b"); false }) in
        |  intrinsic[add_i32](r.x)(q.x)
        |}
        |""".stripMargin
    assertEquals(Lam.run(source), Interpreter.Result(5, "yxab"))
  }

  test("名前付きの構築: フィールドの型が合わなければエラー") {
    assertError(point + "let q: Point = MkPoint(x = true, y = true)\n" + main) { case _: CompileError.TypeMismatch => }
  }

  test("名前付きの構築: 構成子以外・名前のないフィールドはエラー。型引数は値から読める") {
    assertNamedArgumentsInvalid(point + "let f(x: i32): i32 = x\nlet n: i32 = f(x = 1)\n" + main, "`f`")
    assertNamedArgumentsInvalid(point + tagged + "let l: Line = MkLine(from = p)\n" + main, "`MkLine`")
    assertEquals(exitCode(point + tagged + "let t: Tagged[bool] = MkTagged(tag = 6, value = true)", "if t.value then t.tag else 0"), 6)
  }

  test("名前付きの構築: 知らない名前・重複・不足はエラー") {
    assertError(point + "let q: Point = MkPoint(x = 1, z = true)\n" + main) { case CompileError.UnknownField("MkPoint", "z", List("x", "y"), _) => }
    assertNamedArgumentsInvalid(point + "let q: Point = MkPoint(x = 1, x = 2, y = true)\n" + main, "`x`")
    assertError(point + "let q: Point = MkPoint(x = 1)\n" + main) { case CompileError.MissingFields("MkPoint", List("y"), _) => }
  }

  test("名前付きの構築: 括弧の中の == は名前付きの引数ではない") {
    val source =
      """let eq(x: i32)(y: i32): bool = intrinsic[eq_i32](x)(y)
        |infix 4 == = eq
        |let f(b: bool): i32 = if b then 1 else 2
        |let x: i32 = 1
        |let main(): i32 = f(x == 1)
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 1)
  }
}
