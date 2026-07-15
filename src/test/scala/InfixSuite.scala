package com.yuuki14202028

class InfixSuite extends munit.FunSuite {

  private def parseExpr(src: String): Rec[Expr] =
    ParserAST.expr.parseAll(src) match {
      case Right(e) => eraseIndex(e)
      case Left(err) => fail(s"parse failed: $err")
    }

  private def validate(src: String): Either[String, TypeRec[AST.Program.type]] =
    ParserAST.programParser.parseAll(src).left.map(e => s"Parse error: $e").flatMap(p => TAnalyser.validate(p).left.map(_.render))

  private def assertLeft(result: Either[String, ?]): Unit = result match {
    case Left(_)  => ()
    case Right(_) => fail("expected type error, but succeeded")
  }

  test("識別子 infix は関数適用に脱糖される") {
    assertEquals(parseExpr("a max b").show, parseExpr("max(a)(b)").show)
  }

  test("識別子 infix は左結合") {
    assertEquals(parseExpr("a max b max c").show, parseExpr("max(max(a)(b))(c)").show)
  }

  test("識別子 infix は記号演算子より強く結合する") {
    assertEquals(parseExpr("a max b + c").show, parseExpr("(a max b) + c").show)
    assertEquals(parseExpr("a max b * c").show, parseExpr("(a max b) * c").show)
    assertEquals(parseExpr("1 + 2 max 3").show, parseExpr("1 + (2 max 3)").show)
  }

  test("識別子 infix の右辺に単項マイナス") {
    assertEquals(parseExpr("a max -b").show, parseExpr("max(a)(-b)").show)
  }

  test("識別子 infix の右辺に関数適用") {
    assertEquals(parseExpr("a max f(b)").show, parseExpr("max(a)(f(b))").show)
  }

  test("キーワードは infix にならない: let-in") {
    assertEquals(parseExpr("let x: i32 = 1 in x").show, parseExpr("let x: i32 = 1 in x").show)
    parseExpr("let x: i32 = 1 in x").unfix match {
      case AST.Let(_, _, _, _) => ()
      case other => fail(s"not Let: $other")
    }
  }

  test("キーワードは infix にならない: if-then-else") {
    parseExpr("if c then a else b").unfix match {
      case AST.If(_, _, _) => ()
      case other => fail(s"not If: $other")
    }
  }

  test("キーワードは infix にならない: match") {
    parseExpr("match e { Just(x) => x Nothing => y }").unfix match {
      case AST.Match(_, _) => ()
      case other => fail(s"not Match: $other")
    }
  }

  test("キーワードは infix にならない: fold-as-with") {
    parseExpr("fold e as i32 with Just(x) => x").unfix match {
      case AST.Fold(_, _, _) => ()
      case other => fail(s"not Fold: $other")
    }
  }

  test("with 節付き topLet は従来どおりパースされる") {
    ParserAST.programParser.parseAll("let f[a](x: a): i32 with Show[a] = 0\n") match {
      case Right(_) => ()
      case Left(err) => fail(s"parse failed: $err")
    }
  }

  test("型チェック: ユーザー定義2引数関数の infix 使用") {
    val ok =
      """let __gt_i32(x: i32)(y: i32): bool = intrinsic[gt_i32](x)(y)
        |let max(x: i32)(y: i32): i32 = if x > y then x else y
        |let main(): i32 = 2 max 5
        |""".stripMargin
    validate(ok) match {
      case Right(_)  => ()
      case Left(err) => fail(s"type check failed: $err")
    }
  }

  test("型チェック: infix と記号演算子の混在") {
    val src =
      """let __add_i32(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y)
        |let __gt_i32(x: i32)(y: i32): bool = intrinsic[gt_i32](x)(y)
        |let max(x: i32)(y: i32): i32 = if x > y then x else y
        |let main(): i32 = 1 + 2 max 3
        |""".stripMargin
    validate(src) match {
      case Right(_)  => ()
      case Left(err) => fail(s"type check failed: $err")
    }
  }

  test("型エラー: 未定義識別子の infix 使用") {
    assertLeft(validate("let main(): i32 = 2 undefinedFn 5\n"))
  }

  test("型エラー: 1引数関数の infix 使用") {
    val src =
      """let inc(x: i32): i32 = intrinsic[add_i32](x)(1)
        |let main(): i32 = 2 inc 5
        |""".stripMargin
    assertLeft(validate(src))
  }
}
