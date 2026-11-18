package com.yuuki14202028

class DefaultMethodSuite extends munit.FunSuite {

  private def assertError(source: String)(expected: PartialFunction[CompileError, Unit])(using munit.Location): Unit = {
    val found = Lam.errors(source)
    assert(found.exists(expected.isDefinedAt), clue(found.map(_.message)))
  }

  private val not = "trait Not[a] { def not(x: a): a }; prefix ! = not; impl Not[bool] { def not(a: bool): bool = intrinsic[not_bool](a) }\n"

  private val equal = not +
    """trait Equal[a] {
      |  def same(x: a)(y: a): bool
      |  def differ(x: a)(y: a): bool = !same(x)(y)
      |}
      |""".stripMargin

  private val main = "let main(): i32 = 0\n"

  private def mainOf(condition: String): String = s"let main(): i32 = if $condition then 1 else 0\n"

  test("デフォルトを持つメソッドは impl で省略できる") {
    val src = equal + "impl Equal[bool] { def same = λx: bool. λy: bool. false }\n"
    assertEquals(Lam.exitCode(src + mainOf("differ[bool](true)(false)")), 1)
    assertEquals(Lam.exitCode(src + mainOf("same[bool](true)(false)")), 0)
  }

  test("デフォルトは impl で上書きできる") {
    val src = equal + "impl Equal[bool] { def same = λx: bool. λy: bool. false; def differ = λx: bool. λy: bool. false }\n"
    assertEquals(Lam.exitCode(src + mainOf("differ[bool](true)(false)")), 0)
  }

  test("デフォルトを持たないメソッドは省略できない") {
    assertError(equal + "impl Equal[bool] { def differ = λx: bool. λy: bool. false }\n" + main) {
      case CompileError.MissingMethods("impl Equal[bool]", List("same"), _) =>
    }
  }

  test("デフォルトの本体の型はシグネチャと一致しなければならない") {
    val src =
      """trait Equal[a] {
        |  def same(x: a)(y: a): bool
        |  def differ(x: a)(y: a): bool = x
        |}
        |""".stripMargin
    assertError(src + main) { case CompileError.TypeMismatch(expected, actual, _) if Type.show(expected) == "bool" && Type.show(actual) == "a" => }
  }

  test("デフォルトの本体からは必須メソッドが普通の関数として見える（型適用は付けない）") {
    val src = not +
      """trait Equal[a] {
        |  def same(x: a)(y: a): bool
        |  def differ(x: a)(y: a): bool = !same[a](x)(y)
        |}
        |""".stripMargin
    assertError(src + main) { case _: CompileError.NotPolymorphic => }
  }

  test("デフォルトから別のデフォルトは呼べない") {
    val src = not +
      """trait Equal[a] {
        |  def same(x: a)(y: a): bool
        |  def differ(x: a)(y: a): bool = !same(x)(y)
        |  def reallyDiffer(x: a)(y: a): bool = differ(x)(y)
        |}
        |""".stripMargin
    assertError(src + main) { case CompileError.UndefinedVariable("differ", _) => }
  }

  test("デフォルトは自分自身を呼べない（再帰にならない）") {
    val src =
      """trait Loop[a] {
        |  def start(x: a): a
        |  def spin(x: a): a = spin(x)
        |}
        |""".stripMargin
    assertError(src + main) { case CompileError.UndefinedVariable("spin", _) => }
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
    Lam.errors(src + main) match {
      case List(error: CompileError.NoInstance) => assertEquals(Type.show(error.goal), "Later[a]")
      case other => fail(other.map(_.message).mkString("\n"))
    }
  }

  test("デフォルトの本体ではスーパークラスの制約が使える") {
    val src = equal +
      """trait Compare[a] with Equal[a] {
        |  def less(x: a)(y: a): bool
        |  def lessOrSame(x: a)(y: a): bool = if less(x)(y) then true else same[a](x)(y)
        |}
        |impl Equal[bool] { def same = λx: bool. λy: bool. true }
        |impl Compare[bool] { def less = λx: bool. λy: bool. false }
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainOf("lessOrSame[bool](true)(false)")), 1)
  }

  test("メソッド自身の型パラメータと制約を持つデフォルト") {
    val src = equal +
      """trait Container[f: * → *] {
        |  def first[a](xs: f[a])(fallback: a): a
        |  def firstDiffers[a](xs: f[a])(x: a): bool with Equal[a] = differ[a](first[a](xs)(x))(x)
        |}
        |data Box[a] = { MkBox(a) }
        |impl Container[Box] { def first[a](xs: Box[a])(fallback: a): a = match xs { MkBox(x) => x } }
        |impl Equal[bool] { def same = λx: bool. λy: bool. false }
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainOf("firstDiffers[Box][bool](MkBox[bool](true))(false)")), 1)
  }

  test("文脈付きの impl と rec impl でもデフォルトで埋められる") {
    val src = equal +
      """rec data List[a] = { Nil, Cons(a)(List[a]) }
        |rec impl[a] Equal[List[a]] with Equal[a] {
        |  def same = λxs: List[a]. λys: List[a]. match xs {
        |    Nil => match ys {
        |      Nil => true
        |      Cons(y)(yt) => false
        |    }
        |    Cons(x)(xt) => match ys {
        |      Nil => false
        |      Cons(y)(yt) => if same[a](x)(y) then same[List[a]](xt)(yt) else false
        |    }
        |  }
        |}
        |impl Equal[bool] { def same = λx: bool. λy: bool. true }
        |let one: List[bool] = Cons[bool](true)(Nil[bool])
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainOf("differ[List[bool]](Nil[bool])(Nil[bool])")), 0)
    assertEquals(Lam.exitCode(src + mainOf("differ[List[bool]](one)(Cons[bool](true)(one))")), 1)
  }

  test("module の中の trait でもデフォルトを使える") {
    val src = not +
      """module M {
        |  trait Equal[a] {
        |    def same(x: a)(y: a): bool
        |    def differ(x: a)(y: a): bool = !same(x)(y)
        |  }
        |}
        |impl M.Equal[bool] { def same = λx: bool. λy: bool. false }
        |""".stripMargin
    assertEquals(Lam.exitCode(src + mainOf("M.differ[bool](true)(false)")), 1)
  }
}
