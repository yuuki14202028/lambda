package com.yuuki14202028

class DefaultMethodSuite extends munit.FunSuite {

  private def validate(src: String): Either[String, TypeRec[AST.Program.type]] =
    ParserAST.programParser.parseAll(src).left.map(e => s"Parse error: $e").flatMap(p => TAnalyser.validate(p).left.map(_.render))

  private def encode(src: String): Either[String, TypeRec[AST.Program.type]] =
    validate(src).map(p => Compiler.desugar(p))

  private def assertRight(result: Either[String, ?]): Unit = result match {
    case Right(_) => ()
    case Left(err) => fail(err)
  }

  private def assertLeftContains(result: Either[String, ?], expected: String): Unit = result match {
    case Left(err) => assert(err.contains(expected), clue(err))
    case Right(_) => fail(s"expected failure containing: $expected")
  }

  private val not = "let __not_bool(a: bool): bool = intrinsic[not_bool](a)\n"

  private val equal = not +
    """trait Equal[a] {
      |  def same(x: a)(y: a): bool
      |  def differ(x: a)(y: a): bool = !same(x)(y)
      |}
      |""".stripMargin

  private val main = "let main(): i32 = 0\n"

  test("デフォルトを持つメソッドは impl で省略できる") {
    val shown = encode(equal + "impl Equal[bool] { def same = λx: bool. λy: bool. true }\nlet b: bool = differ[bool](true)(false)\n" + main)
      .fold(fail(_), _.show)
    assert(shown.contains("$default_Equal_differ"), clue(shown))
    assert(shown.contains("$method_0"), clue(shown))
  }

  test("デフォルトは impl で上書きできる") {
    val src = equal + "impl Equal[bool] { def same = λx: bool. λy: bool. true  def differ = λx: bool. λy: bool. false }\n"
    val shown = encode(src + main).fold(fail(_), _.show)
    assert(!shown.contains("$method_0"), clue(shown))
  }

  test("デフォルトを持たないメソッドは省略できない") {
    assertLeftContains(validate(equal + "impl Equal[bool] { def differ = λx: bool. λy: bool. false }\n" + main), "same")
  }

  test("デフォルトの本体の型はシグネチャと一致しなければならない") {
    val src =
      """trait Equal[a] {
        |  def same(x: a)(y: a): bool
        |  def differ(x: a)(y: a): bool = x
        |}
        |""".stripMargin
    assertLeftContains(validate(src + main), "the default of method differ")
  }

  test("デフォルトの本体からは必須メソッドが普通の関数として見える（型適用は付けない）") {
    val src = not +
      """trait Equal[a] {
        |  def same(x: a)(y: a): bool
        |  def differ(x: a)(y: a): bool = !same[a](x)(y)
        |}
        |""".stripMargin
    assertLeftContains(validate(src + main), "Not a polymorphic function")
  }

  test("デフォルトから別のデフォルトは呼べない") {
    val src = not +
      """trait Equal[a] {
        |  def same(x: a)(y: a): bool
        |  def differ(x: a)(y: a): bool = !same(x)(y)
        |  def reallyDiffer(x: a)(y: a): bool = differ(x)(y)
        |}
        |""".stripMargin
    assertLeftContains(validate(src + main), "Variable differ is not defined")
  }

  test("デフォルトは自分自身を呼べない（再帰にならない）") {
    val src =
      """trait Loop[a] {
        |  def start(x: a): a
        |  def spin(x: a): a = spin(x)
        |}
        |""".stripMargin
    assertLeftContains(validate(src + main), "Variable spin is not defined")
  }

  test("デフォルトの本体から自身のインスタンスは使えない") {
    val src = equal +
      """let viaInstance[a](x: a)(y: a): bool with Equal[a] = same[a](x)(y)
        |trait Later[a] with Equal[a] {
        |  def start(x: a): a
        |  def ok(x: a): bool = viaInstance[a](x)(x)
        |}
        |let needsLater[a](x: a): a with Later[a] = start[a](x)
        |trait Self[a] {
        |  def begin(x: a): a
        |  def bad(x: a): a = needsLater[a](x)
        |}
        |""".stripMargin
    assertLeftContains(validate(src + main), "No instance")
  }

  test("デフォルトの本体ではスーパークラスの制約が使える") {
    val src = equal +
      """trait Compare[a] with Equal[a] {
        |  def less(x: a)(y: a): bool
        |  def lessOrSame(x: a)(y: a): bool = if less(x)(y) then true else same[a](x)(y)
        |}
        |impl Equal[bool] { def same = λx: bool. λy: bool. true }
        |impl Compare[bool] { def less = λx: bool. λy: bool. false }
        |let b: bool = lessOrSame[bool](true)(false)
        |""".stripMargin
    assertRight(encode(src + main))
  }

  test("メソッド自身の型パラメータと制約を持つデフォルト") {
    val src = equal +
      """trait Container[f: * → *] {
        |  def first[a](xs: f[a])(fallback: a): a
        |  def firstDiffers[a](xs: f[a])(x: a): bool with Equal[a] = differ[a](first[a](xs)(x))(x)
        |}
        |data Box[a] = { MkBox(a) }
        |impl Container[Box] { def first[a](xs: Box[a])(fallback: a): a = match xs { MkBox(x) => x } }
        |impl Equal[bool] { def same = λx: bool. λy: bool. true }
        |let b: bool = firstDiffers[Box][bool](MkBox[bool](true))(false)
        |""".stripMargin
    assertRight(encode(src + main))
  }

  test("文脈付きの impl と rec impl でもデフォルトで埋められる") {
    val src = equal +
      """rec data List[a] = { Nil, Cons(a)(List[a]) }
        |rec impl[a] Equal[List[a]] with Equal[a] {
        |  def same = λxs: List[a]. λys: List[a]. match xs {
        |    Nil => true
        |    Cons(x)(xt) => match ys {
        |      Nil => false
        |      Cons(y)(yt) => if same[a](x)(y) then same[List[a]](xt)(yt) else false
        |    }
        |  }
        |}
        |impl Equal[bool] { def same = λx: bool. λy: bool. true }
        |let b: bool = differ[List[bool]](Nil[bool])(Nil[bool])
        |""".stripMargin
    assertRight(encode(src + main))
  }

  test("デフォルトを使わない impl の辞書は従来と同じ形") {
    val shown = encode(equal + "impl Equal[bool] { def same = λx: bool. λy: bool. true  def differ = λx: bool. λy: bool. false }\n" + main)
      .fold(fail(_), _.show)
    assert(!shown.contains("$method_"), clue(shown))
  }

  test("module の中の trait でもデフォルトを使える") {
    val src = not +
      """module M {
        |  trait Equal[a] {
        |    def same(x: a)(y: a): bool
        |    def differ(x: a)(y: a): bool = !same(x)(y)
        |  }
        |}
        |impl M.Equal[bool] { def same = λx: bool. λy: bool. true }
        |let b: bool = M.differ[bool](true)(false)
        |""".stripMargin
    assertRight(encode(src + main))
  }
}
