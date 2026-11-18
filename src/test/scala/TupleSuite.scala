package com.yuuki14202028

/** 組 (x, y): literal (,) に宣言した関数の呼び出し。型とパターンにも書ける */
class TupleSuite extends munit.FunSuite {

  private val pair =
    """data Pair[a][b] = { Pair(a)(b) }
      |data Triple[a][b][c] = { Triple(a)(b)(c) }
      |literal (,) = Pair
      |literal (,,) = Triple
      |rec data List[a] = { Nil, Cons(a)(List[a]) }
      |literal [] = Nil, Cons
      |let addInt(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y)
      |infixl 6 + = addInt
      |""".stripMargin
  private val main = "let main(): i32 = 0\n"

  /** expected に当たるエラーが報告される */
  private def assertError(source: String)(expected: PartialFunction[CompileError, Any])(using munit.Location): Unit = {
    val found = Lam.errors(source)
    assert(found.exists(expected.isDefinedAt), clue(found))
  }

  test("(x, y) は literal (,) に宣言した関数を、要素の型で具体化して適用したもの") {
    val source = pair +
      """let p: Pair[i32][bool] = (1, true)
        |let t: Triple[i32][bool][i32] = (1, false, 2)
        |let n: Pair[Pair[i32][i32]][bool] = ((3, 4), true)
        |let main(): i32 = match n {
        |  Pair(Pair(x)(y))(_) => match t {
        |    Triple(a)(_)(c) => match p {
        |      Pair(k)(_) => x + y + a + c + k
        |    }
        |  }
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 11)
    assertError(pair + "let p: Pair[i32][i32] = (1, true)\n" + main) { case _: CompileError.TypeMismatch => }
  }

  test("(x) は括弧のまま、() は unit") {
    assertEquals(Lam.exitCode(pair + "let n: i32 = (1)\nlet u: unit = ()\nlet main(): i32 = n\n"), 1)
  }

  test("組の型 (A, B) は、宣言した関数の結果の型") {
    val source = pair +
      """let p: (i32, bool) = (1, true)
        |let xs: List[(i32, (bool, i32))] = [(1, (true, 2))]
        |let f(q: (i32, i32, bool)): i32 = match q { Triple(a)(b)(_) => a + b }
        |let main(): i32 = match xs {
        |  Cons(Pair(a)(Pair(_)(b)))(_) => a + b + f((10, 20, true))
        |  Nil => 0
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 33)
    assertError(pair + "let p: (i32, bool) = (true, 1)\n" + main) { case _: CompileError.TypeMismatch => }
  }

  test("組とリストのパターン: 宣言した関数が構成子なら、そのパターンとして読む") {
    val source = pair +
      """let first(p: (i32, bool)): i32 = match p { (x, _) => x }
        |let size(xs: List[i32]): i32 = match xs {
        |  [] => 0
        |  [_] => 1
        |  [x, y] => x + y
        |  _ => 3
        |}
        |let sumPair(p: (i32, i32)): i32 = let (a, b): (i32, i32) = p in a + b
        |""".stripMargin
    assertEquals(Lam.exitCode(source + "let main(): i32 = first((7, true))\n"), 7)
    assertEquals(Lam.exitCode(source + "let main(): i32 = size(Nil[i32])\n"), 0)
    assertEquals(Lam.exitCode(source + "let main(): i32 = size([9])\n"), 1)
    assertEquals(Lam.exitCode(source + "let main(): i32 = size([10, 20])\n"), 30)
    assertEquals(Lam.exitCode(source + "let main(): i32 = size([1, 2, 4])\n"), 3)
    assertEquals(Lam.exitCode(source + "let main(): i32 = sumPair((5, 6))\n"), 11)
  }

  test("リストのパターンも網羅性を調べる") {
    assertError(pair + "let size(xs: List[i32]): i32 = match xs {\n  [] => 0\n  [_] => 1\n}\n" + main) { case _: CompileError.NonExhaustiveMatch => }
  }

  test("宣言のない要素の数の組は書けない") {
    assertError(pair + "let q: i32 = match (1, 2, 3, 4) { _ => 0 }\n" + main) {
      case CompileError.SyntaxNotDeclared(_, declaration, _) if declaration.contains("(,,,)") =>
    }
  }

  test("構成子でない関数を宣言した組は、パターンに書けない") {
    val source = "data P = { MkP(i32)(i32) }\nlet mk(x: i32)(y: i32): P = MkP(x)(y)\nliteral (,) = mk\nlet p: P = (1, 2)\n"
    assertEquals(Lam.exitCode(source + "let main(): i32 = match p { MkP(a)(_) => a }\n"), 1)
    assertError(source + "let n: i32 = match p { (a, _) => a }\n" + main) { case _: CompileError.SyntaxPatternNotConstructor => }
  }

  test("literal (,) の関数の型パラメータが引数の型に現れなければ、組を書いた所でエラー") {
    val declared = "let mk[a][b](x: i32)(y: b): b = y\nliteral (,) = mk\n"
    assertEquals(Lam.errors(declared + main), Nil)
    assertError(declared + "let p: i32 = (1, 2)\n" + main) { case CompileError.TypeArgumentUndetermined("a", _, _) => }
  }
}
