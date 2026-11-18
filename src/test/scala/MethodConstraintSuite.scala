package com.yuuki14202028

/** メソッド自身の型パラメータに付く制約（def m[g](…): T with C[g]） */
class MethodConstraintSuite extends munit.FunSuite {

  private def assertError(source: String)(expected: PartialFunction[CompileError, Unit])(using munit.Location): Unit = {
    val found = Lam.errors(source)
    assert(found.exists(expected.isDefinedAt), clue(found.map(_.message)))
  }

  // メソッド自身の型変数 g に制約を付ける（trait のパラメータ t とは別）
  private val prelude =
    """data Box[a] = { MkBox(a) }
      |data Id[a] = { MkId(a) }
      |
      |trait Pointed[g: * → *] {
      |  def point[a](x: a): g[a]
      |}
      |
      |impl Pointed[Id] {
      |  def point[a](x: a): Id[a] = MkId[a](x)
      |}
      |
      |trait Wrap[t: * → *] {
      |  def wrapIn[g: * → *][a](x: t[a]): g[t[a]] with Pointed[g]
      |}
      |""".stripMargin

  private val wrapBox =
    """impl Wrap[Box] {
      |  def wrapIn[g: * → *][a](x: Box[a]): g[Box[a]] with Pointed[g] = point[g][Box[a]](x)
      |}
      |""".stripMargin

  private val unwrap =
    """let main(): i32 = match wrapIn[Box][Id][i32](MkBox[i32](7)) {
      |  MkId(b) => match b {
      |    MkBox(n) => n
      |  }
      |}
      |""".stripMargin

  test("trait メソッドの with 節はメソッドの型パラメータ直下の暗黙引数になる") {
    val src = prelude + wrapBox + "let bad: i32 = wrapIn[Box]\nlet main(): i32 = 0\n"
    // 型は、不一致のエラーの表示から読む
    assertError(src) {
      case error: CompileError.TypeMismatch if error.message.contains("found `∀(g: * → *). ∀a. [Pointed[g]] → Box[a] → g[Box[a]]`") =>
    }
  }

  test("成功: 制約付きメソッドの呼び出しは、メソッドの型引数の後でも辞書が解決される") {
    assertEquals(Lam.exitCode(prelude + wrapBox + unwrap), 7)
  }

  test("成功: 制約付き関数の中から、スコープ中の制約で制約付きメソッドを呼べる") {
    val src = prelude + wrapBox +
      """let wrapAny[t: * → *][g: * → *][a](x: t[a]): g[t[a]] with Wrap[t], Pointed[g] = wrapIn[t][g][a](x)
        |let main(): i32 = match wrapAny[Box][Id][i32](MkBox[i32](9)) {
        |  MkId(b) => match b {
        |    MkBox(n) => n
        |  }
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 9)
  }

  test("検査失敗: メソッドの制約を満たすインスタンスがなければ No instance") {
    val src = prelude + wrapBox +
      """let main(): i32 = match wrapIn[Box][Box][i32](MkBox[i32](7)) {
        |  MkBox(b) => 0
        |}
        |""".stripMargin
    assertError(src) { case error: CompileError.NoInstance if Type.show(error.goal) == "Pointed[Box]" => }
  }

  test("検査失敗: impl が with 節を省くとメソッドの型が合わない") {
    val src = prelude +
      """impl Wrap[Box] {
        |  def wrapIn[g: * → *][a](x: Box[a]): g[Box[a]] = point[g][Box[a]](x)
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertError(src) { case error: CompileError.NoInstance if Type.show(error.goal) == "Pointed[g]" => }
  }

  test("検査失敗: 本体が制約を使わなくても、with 節のない impl は型が合わない") {
    val src =
      """data Box[a] = { MkBox(a) }
        |trait Pointed[g: * → *] {
        |  def point[a](x: a): g[a]
        |}
        |trait Keep[t: * → *] {
        |  def keep[g: * → *][a](x: t[a]): t[a] with Pointed[g]
        |}
        |impl Keep[Box] {
        |  def keep[g: * → *][a](x: Box[a]): Box[a] = x
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertError(src) { case CompileError.MethodTypeMismatch("impl Keep[Box]", "keep", _, _, _) => }
  }
}
