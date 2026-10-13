package com.yuuki14202028

class RecursionSuite extends munit.FunSuite {

  private def validate(src: String): Either[String, TypeRec[AST.Program.type]] =
    ParserAST.programParser.parseAll(src).left.map(e => s"Parse error: $e").flatMap(p => TAnalyser.validate(p).left.map(_.render))

  private def assertRight(result: Either[String, ?]): Unit = result match {
    case Right(_) => ()
    case Left(err) => fail(err)
  }

  private def assertLeftContains(result: Either[String, ?], expected: String): Unit = result match {
    case Left(err) => assert(err.contains(expected), clue(err))
    case Right(_) => fail(s"expected failure containing: $expected")
  }

  private val notFunction = "must be a function"

  test("let rec: 関数は自身を参照できる") {
    assertRight(validate("let rec f(x: i32): i32 = f(x)\nlet main(): i32 = 0\n"))
  }

  test("let rec: 型パラメータ付きの関数も関数として扱う") {
    assertRight(validate("let rec f[a](x: a): a = f[a](x)\nlet main(): i32 = 0\n"))
  }

  test("let rec: 関数でない値が自身を参照するのは拒否") {
    assertLeftContains(validate("let rec x: i32 = x\nlet main(): i32 = 0\n"), notFunction)
  }

  test("let rec: 関数の適用結果として自身を参照するのも拒否") {
    val src =
      """let apply(f: unit → i32): unit → i32 = f
        |let rec g: unit → i32 = apply(λu: unit. g(u))
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), notFunction)
  }

  test("let rec: 自身を参照しない値は関数でなくてもよい") {
    assertRight(validate("let rec x: i32 = 1\nlet main(): i32 = x\n"))
  }

  test("let rec: in 式でも拒否") {
    assertLeftContains(validate("let main(): i32 = let rec x: i32 = x in x\n"), notFunction)
  }

  test("let rec: context ブロックの let rec でも拒否") {
    val src =
      """data Maybe[a] = { Nothing, Just(a) }
        |trait Monad[m: * → *] {
        |  def pure[a](x: a): m[a]
        |  def flatMap[a][b](v: m[a])(f: a → m[b]): m[b]
        |}
        |impl Monad[Maybe] {
        |  def pure[a](x: a): m[a] = Just[a](x)
        |  def flatMap[a][b](v: Maybe[a])(f: a → Maybe[b]): Maybe[b] = match v {
        |    Nothing => Nothing[b]
        |    Just(x) => f(x)
        |  }
        |}
        |let r(): Maybe[i32] = context[Maybe] {
        |  let rec x: i32 = x;
        |  x
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), notFunction)
  }

  private val defaultTrait =
    """trait Default[a] {
      |  def default: a
      |  def orDefault(x: a): a
      |}
      |""".stripMargin

  test("impl rec: 関数でないメソッドが自身のインスタンスを使うのは拒否") {
    val src = defaultTrait +
      """impl rec Default[i32] {
        |  def default = default[i32]
        |  def orDefault = λx: i32. x
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "method default uses its own instance")
  }

  test("impl rec: 関数のメソッドからは自身のインスタンスを使える") {
    val src = defaultTrait +
      """impl rec Default[i32] {
        |  def default = 0
        |  def orDefault = λx: i32. default[i32]
        |}
        |let main(): i32 = orDefault[i32](1)
        |""".stripMargin
    assertRight(validate(src))
  }

  test("impl rec: 文脈のあるインスタンスは辞書自体が関数なので、値のメソッドでもよい") {
    val src = defaultTrait +
      """data Box[a] = { MkBox(a) }
        |impl rec[a] Default[Box[a]] with Default[a] {
        |  def default = orDefault[Box[a]](MkBox[a](default[a]))
        |  def orDefault = λx: Box[a]. x
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertRight(validate(src))
  }
}
