package com.yuuki14202028

class CommentSuite extends munit.FunSuite {

  private def validate(src: String): Either[String, TypeRec[AST.Program.type]] =
    ParserAST.programParser.parseAll(src).left.map(e => s"Parse error: $e").flatMap(p => TAnalyser.validate(p).left.map(_.render))

  private def shown(src: String): String =
    ParserAST.programParser.parseAll(src).fold(e => fail(e.toString), p => eraseIndex(p).show)

  private val plain =
    """let add(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y)
      |data Option[a] = { None, Some(a) }
      |let f(o: Option[i32]): i32 = match o {
      |  Some(x) => add(x)(1)
      |  None => 0
      |}
      |let main(): i32 = f(Some[i32](1))
      |""".stripMargin

  test("行コメント: // から行末まで") {
    val src =
      """// 足し算
        |let add(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y) // 組み込み
        |
        |// 値があるかもしれない型
        |data Option[a] = { None, Some(a) } // 2 つの構成子
        |let f(o: Option[i32]): i32 = match o { // 分解する
        |  // 値があるとき
        |  Some(x) => add(x)(1) // 1 を足す
        |  // ないとき
        |  None => 0
        |} // ここまで
        |let main(): i32 = f(Some[i32](1))
        |// 終わり""".stripMargin
    assertEquals(shown(src), shown(plain))
    assert(validate(src).isRight)
  }

  test("ブロックコメント: /* から */ まで。式や型の途中にも書ける") {
    val src =
      """/* 足し算
        |   2 行にわたる説明 */
        |let add(x: /* 左 */ i32)(y: i32 /* 右 */): i32 = intrinsic[add_i32](x)(y)
        |data Option[a] = { None, /* または */ Some(a) }
        |let f(o: Option[i32]): i32 = match /* 対象 */ o {
        |  Some(x) => add(x)(/* 定数 */ 1)
        |  None => 0
        |}
        |let main(): i32 = f(Some[i32](1))
        |""".stripMargin
    assertEquals(shown(src), shown(plain))
  }

  test("ブロックコメントは入れ子にできる") {
    val src = "/* 外 /* 内 */ まだ外 */\n" + plain
    assertEquals(shown(src), shown(plain))
  }

  test("コメントだけの行は宣言を区切らない・つなげない") {
    val src =
      """let x: i32 = 1
        |// あいだ
        |
        |/* あいだ */
        |let main(): i32 = x
        |""".stripMargin
    assert(validate(src).isRight)
  }

  test("文字列の中の // はコメントではない") {
    val src = "let s: foreign.C.String = \"a // b /* c */\"\nlet main(): i32 = 0\n"
    assert(shown(src).contains("a // b /* c */"))
  }

  test("割り算はコメントにならない") {
    val src =
      """let __div_i32(a: i32)(b: i32): i32 = intrinsic[div_i32](a)(b)
        |let half(n: i32): i32 = n / 2 // 半分
        |let quarter(n: i32): i32 = n / 2 / 2
        |let main(): i32 = 0
        |""".stripMargin
    assert(validate(src).isRight)
  }

  test("閉じていないブロックコメントは構文エラー") {
    assert(ParserAST.programParser.parseAll("let x: i32 = 1 /* 閉じない\nlet main(): i32 = 0\n").isLeft)
  }
}
