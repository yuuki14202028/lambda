package com.yuuki14202028

// 型展開 = 名前解決 + β 正規化（SystemFOmega.md §9）の意味論を固定するスイート。
// エイリアスは型レベル λ なので部分適用が合法になり、アリティ過不足はカインドエラーで検出される。
class TypeExpandSuite extends munit.FunSuite {

  private def validate(src: String): Either[String, TypeRec[AST.Program.type]] =
    ParserAST.programParser.parseAll(src).left.map(e => s"Parse error: $e").flatMap(p => TAnalyser.validate(p).left.map(_.render))

  private def assertRight(result: Either[String, ?]): Unit = result match {
    case Right(_) => ()
    case Left(err) => fail(err)
  }

  private def assertLeftContains(result: Either[String, ?], expected: String): Unit = result match {
    case Left(err) => assert(err.contains(expected), clue(err))
    case Right(_) => fail(s"expected error containing '$expected', but succeeded")
  }

  test("エイリアスの完全適用は β 正規化で展開される") {
    val src =
      """type Fn2[a][b] = a → b
        |let g: Fn2[i32][bool] = λx: i32. true
        |let main(): i32 = 0
        |""".stripMargin
    assertRight(validate(src))
  }

  test("エイリアスの部分適用は高カインド位置で合法") {
    val src =
      """type Fn2[a][b] = a → b
        |type Apply[f: * → *][a] = f[a]
        |let g: Apply[Fn2[i32]][bool] = λx: i32. true
        |let main(): i32 = 0
        |""".stripMargin
    assertRight(validate(src))
  }

  test("部分適用されたエイリアスを * 位置に書くとカインドエラーになる") {
    val src =
      """type Fn2[a][b] = a → b
        |let g: Fn2[i32] = λx: i32. true
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "has kind")
  }

  test("エイリアスの過剰適用はカインドエラーになる") {
    val src =
      """type Id[a] = a
        |let g: Id[i32][bool] = λx: i32. true
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "Cannot apply")
  }

  test("未定義の型名は名前解決エラーになる") {
    val src =
      """let g: Unknown[i32] = λx: i32. true
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "is not defined")
  }
}
