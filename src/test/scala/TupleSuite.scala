package com.yuuki14202028

import munit.FunSuite

class TupleSuite extends FunSuite {

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

  private val pair =
    """data Pair[a][b] = { Pair(a)(b) }
      |data Triple[a][b][c] = { Triple(a)(b)(c) }
      |literal (,) = Pair
      |literal (,,) = Triple
      |rec data List[a] = { Nil, Cons(a)(List[a]) }
      |literal [] = Nil, Cons
      |""".stripMargin
  private val main = "let main(): i32 = 0\n"

  test("(x, y) は literal (,) に宣言した関数を、要素の型で具体化して適用したもの") {
    assertRight(encode(pair + "let p: Pair[i32][bool] = (1, true)\nlet t: Triple[i32][bool][i32] = (1, false, 2)\nlet n: Pair[Pair[i32][i32]][bool] = ((1, 2), true)\n" + main))
    assertLeftContains(validate(pair + "let p: Pair[i32][i32] = (1, true)\n" + main), "Type mismatch")
  }

  test("(x) は括弧のまま、() は unit") {
    assertRight(encode(pair + "let n: i32 = (1)\nlet u: unit = ()\n" + main))
  }

  test("組の型 (A, B) は、宣言した関数の結果の型") {
    assertRight(encode(pair + "let p: (i32, bool) = (1, true)\nlet xs: List[(i32, (bool, i32))] = [(1, (true, 2))]\nlet f(q: (i32, i32, bool)): i32 = 0\n" + main))
    assertLeftContains(validate(pair + "let p: (i32, bool) = (true, 1)\n" + main), "Type mismatch")
  }

  test("組とリストのパターン: 宣言した関数が構成子なら、そのパターンとして読む") {
    val src = pair +
      """let first(p: (i32, bool)): i32 = match p { (x, _) => x }
        |let size(xs: List[i32]): i32 = match xs {
        |  [] => 0
        |  [_] => 1
        |  [x, y] => x + y
        |  _ => 3
        |}
        |let sumPair(p: (i32, i32)): i32 = let (a, b): (i32, i32) = p in a + b
        |""".stripMargin
    assertRight(encode("trait Add[a] { def add(x: a)(y: a): a }\ninfixl 6 + = add\nimpl Add[i32] { def add(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y) }\n" + src + main))
  }

  test("リストのパターンも網羅性を調べる") {
    assertLeftContains(validate(pair + "let size(xs: List[i32]): i32 = match xs {\n  [] => 0\n  [_] => 1\n}\n" + main), "not exhaustive")
  }

  test("宣言のない要素の数の組は書けない") {
    assertLeftContains(validate(pair + "let q: i32 = match (1, 2, 3, 4) { _ => 0 }\n" + main), "literal (,,,) is not declared")
  }

  test("構成子でない関数を宣言した組は、パターンに書けない") {
    val src = "data P = { MkP(i32)(i32) }\nlet mk(x: i32)(y: i32): P = MkP(x)(y)\nliteral (,) = mk\nlet p: P = (1, 2)\nlet n: i32 = match p { (a, _) => a }\n" + main
    assertLeftContains(validate(src), "cannot be used in a pattern")
  }

  test("literal (,) の関数の型パラメータは、どれも引数の型に現れなければならない") {
    assertLeftContains(validate("let mk[a][b](x: i32)(y: b): b = y\nliteral (,) = mk\n" + main), "cannot be declared for literal (,)")
  }
}
