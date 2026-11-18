package com.yuuki14202028

class CommentSuite extends munit.FunSuite {

  // コメントのないプログラム。コメントを足しても結果は変わらない
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
    val source =
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
    assertEquals(Lam.exitCode(plain), 2)
    assertEquals(Lam.exitCode(source), 2)
  }

  test("ブロックコメント: /* から */ まで。式や型の途中にも書ける") {
    val source =
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
    assertEquals(Lam.exitCode(source), 2)
  }

  test("ブロックコメントは入れ子にできる") {
    assertEquals(Lam.exitCode("/* 外 /* 内 */ まだ外 */\n" + plain), 2)
  }

  test("コメントだけの行は宣言を区切らない・つなげない") {
    val source =
      """let x: i32 = 1
        |// あいだ
        |
        |/* あいだ */
        |let main(): i32 = x
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 1)
  }

  test("文字列の中の // はコメントではない") {
    val source =
      """let print: foreign.C.String → i32 = foreign[foreign.C.String → i32] print_string_inline
        |let s: foreign.C.String = "a // b /* c */"
        |let main(): i32 = print(s)
        |""".stripMargin
    assertEquals(Lam.output(source), "a // b /* c */")
  }

  test("割り算はコメントにならない") {
    val source =
      """trait Div[a] { def div(x: a)(y: a): a }; infixl 7 / = div; impl Div[i32] { def div(a: i32)(b: i32): i32 = intrinsic[div_i32](a)(b) }
        |let half(n: i32): i32 = n / 2 // 半分
        |let quarter(n: i32): i32 = n / 2 / 2
        |let main(): i32 = intrinsic[add_i32](half(20))(quarter(20))
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 15)
  }

  test("閉じていないブロックコメントは構文エラー") {
    Lam.errors("let x: i32 = 1 /* 閉じない\nlet main(): i32 = 0\n") match {
      case List(_: CompileError.Syntax) => ()
      case other => fail(s"expected a syntax error: $other")
    }
  }
}
