package com.yuuki14202028

class MultiParamTraitSuite extends munit.FunSuite {

  private def assertError(source: String)(expected: PartialFunction[CompileError, Unit])(using munit.Location): Unit = {
    val found = Lam.errors(source)
    assert(found.exists(expected.isDefinedAt), clue(found.map(_.message)))
  }

  private val conversionTrait =
    """trait Conversion[a][b] {
      |  def convert(x: a): b
      |}
      |""".stripMargin

  private val conversionProgram = conversionTrait +
    """impl Conversion[i32][bool] {
      |  def convert = λx: i32. true
      |}
      |
      |impl Conversion[bool][i32] {
      |  def convert = λx: bool. if x then 42 else 0
      |}
      |
      |let main(): i32 = convert[bool][i32](convert[i32][bool](0))
      |""".stripMargin

  // ---- 構文 ----

  test("パース: カインド注釈付きの混在パラメータ") {
    val src =
      """data Box[a] = { MkBox(a) }
        |trait Collect[f: * → *][a] {
        |  def collect(x: a): f[a]
        |}
        |impl Collect[Box][i32] {
        |  def collect(x: i32): Box[i32] = MkBox[i32](x)
        |}
        |let main(): i32 = match collect[Box][i32](3) { MkBox(n) => n }
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 3)
  }

  test("パース: 複数引数の with 制約") {
    val src = conversionTrait + "let f[a][b](x: a): i32 with Conversion[a][b] = 0\nlet g: i32 = f\nlet main(): i32 = 0\n"
    // 型は、不一致のエラーの表示から読む
    assertError(src) { case error: CompileError.TypeMismatch if error.message.contains("found `∀a. ∀b. [Conversion[a][b]] → a → i32`") => }
  }

  // ---- 成功パス ----

  test("成功: 2 パラメータの trait / 複数ターゲットの impl / メソッド利用") {
    assertEquals(Lam.exitCode(conversionProgram), 42)
  }

  test("成功: 辞書は構成子 MkConversion の値で、型は Conversion[a][b]") {
    val src = conversionTrait +
      """let custom: Conversion[i32][bool] = MkConversion[i32][bool](λx: i32. false)
        |let main(): i32 = if convert[i32][bool](with custom)(0) then 1 else 2
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 2)
  }

  test("成功: 複数引数制約付き関数の辞書渡し") {
    val src = conversionTrait +
      """impl Conversion[bool][i32] {
        |  def convert = λx: bool. 42
        |}
        |
        |let through[a][b](x: a): b with Conversion[a][b] = convert[a][b](x)
        |
        |let main(): i32 = through[bool][i32](true)
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 42)
  }

  test("成功: ヘッドの組が異なるインスタンスは重複にならない") {
    val src = conversionTrait +
      """impl Conversion[i32][bool] {
        |  def convert = λx: i32. true
        |}
        |
        |impl Conversion[i32][i32] {
        |  def convert = λx: i32. x
        |}
        |
        |let main(): i32 = if convert[i32][bool](0) then convert[i32][i32](5) else 0
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 5)
  }

  test("成功: 引数の数が合う with 制約は受理される") {
    val src = conversionTrait +
      "let f[a][b](x: a): i32 with Conversion[a][b] = 0\nlet main(): i32 = 0\n"
    assertEquals(Lam.exitCode(src), 0)
  }

  // ---- エラーパス ----

  test("検査失敗: impl の型引数の数が trait と合わない") {
    val src = conversionTrait +
      """impl Conversion[i32] {
        |  def convert = λx: i32. x
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertError(src) { case CompileError.ImplTargetArity("Conversion", 2, 1, _) => }
  }

  test("検査失敗: 同一のヘッドの組は Overlapping instance") {
    val src = conversionTrait +
      """impl Conversion[i32][bool] {
        |  def convert = λx: i32. true
        |}
        |impl Conversion[i32][bool] {
        |  def convert = λx: i32. false
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertError(src) { case _: CompileError.OverlappingInstance => }
  }

  test("検査失敗: メソッド本体の型が特殊化済み署名と合わない") {
    val src = conversionTrait +
      """impl Conversion[i32][bool] {
        |  def convert = λx: i32. 0
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertError(src) {
      case CompileError.MethodTypeMismatch("impl Conversion[i32][bool]", "convert", expected, actual, _)
          if Type.show(expected) == "bool" && Type.show(actual) == "i32" =>
    }
  }

  test("検査失敗: with 制約の引数の数が trait と合わない") {
    val src = conversionTrait +
      "let f[a](x: a): i32 with Conversion[a] = 0\nlet main(): i32 = 0\n"
    assertError(src) { case _: CompileError.KindMismatch => }
  }

  test("検査失敗: with 制約の引数のカインドが trait と合わない") {
    val src =
      """data Maybe[a] = { Nothing, Just(a) }
        |""".stripMargin + conversionTrait +
        "let f[a](x: a): i32 with Conversion[a][Maybe] = 0\nlet main(): i32 = 0\n"
    assertError(src) { case CompileError.KindMismatch(Kind.Star, Kind.Arrow(Kind.Star, Kind.Star), _) => }
  }

  test("検査失敗: impl ターゲットのカインドが trait パラメータと合わない") {
    val src =
      """data Maybe[a] = { Nothing, Just(a) }
        |""".stripMargin + conversionTrait +
        """impl Conversion[i32][Maybe] {
          |  def convert = λx: i32. x
          |}
          |let main(): i32 = 0
          |""".stripMargin
    assertError(src) { case _: CompileError.KindMismatch => }
  }

  test("検査失敗: パラメータ名の重複は拒否") {
    val src = "trait Conversion[a][a] {\n  def convert(x: a): a\n}\nlet main(): i32 = 0\n"
    assertError(src) { case CompileError.AlreadyDefined("a", _) => }
  }

  test("検査失敗: スーパークラス制約でも引数の数を検査する") {
    val src = conversionTrait +
      "trait Into[a] with Conversion[a] {\n  def into(x: a): i32\n}\nlet main(): i32 = 0\n"
    assertError(src) { case _: CompileError.KindMismatch => }
  }

  test("NoInstance: 引数の組に合うインスタンスがなければ型検査で拒否") {
    val src = conversionTrait +
      """impl Conversion[i32][bool] {
        |  def convert = λx: i32. true
        |}
        |let main(): i32 = convert[bool][i32](true)
        |""".stripMargin
    assertError(src) { case error: CompileError.NoInstance if Type.show(error.goal) == "Conversion[bool][i32]" => }
  }

  test("型適用が trait パラメータ数に満たない参照は、制約が残った型になり型検査で拒否") {
    val src = conversionTrait +
      """impl Conversion[i32][bool] {
        |  def convert = λx: i32. true
        |}
        |let f: ∀b. i32 → b = convert[i32]
        |let main(): i32 = 0
        |""".stripMargin
    assertError(src) {
      case error: CompileError.TypeMismatch if error.message.contains("expected `∀b. i32 → b`, found `∀b. [Conversion[i32][b]] → i32 → b`") =>
    }
  }
}
