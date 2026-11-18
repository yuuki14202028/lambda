package com.yuuki14202028

/** trait / impl / with の構文が読めることを、プログラムを動かして確かめる */
class TraitParseSuite extends munit.FunSuite {

  private def syntaxErrors(source: String): List[CompileError] =
    Lam.errors(source).collect { case error: CompileError.Syntax => error }

  private val maybe = "data Maybe[a] = { Nothing, Just(a) }\n"

  test("trait: 単一メソッド（注釈なしパラメータ）") {
    val src =
      """trait Show[a] {
        |  def show: a → foreign.C.String
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 0)
  }

  test("trait: 高階カインドパラメータと型パラメータ付きメソッド") {
    val src = maybe +
      """trait Functor[f: * → *] {
        |  def fmap[a][b](ab: a → b): f[a] → f[b]
        |}
        |impl Functor[Maybe] {
        |  def fmap[a][b](ab: a → b): Maybe[a] → Maybe[b] = λm: Maybe[a]. match m {
        |    Nothing => Nothing[b]
        |    Just(x) => Just[b](ab(x))
        |  }
        |}
        |let main(): i32 = match fmap[Maybe][bool][i32](λb: bool. 5)(Just[bool](true)) {
        |  Nothing => 0
        |  Just(n) => n
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 5)
  }

  test("trait: 複数メソッド") {
    val src = maybe +
      """trait Monad[m: * → *] {
        |  def ret: ∀a. a → m[a]
        |  def bind: ∀a. ∀b. m[a] → (a → m[b]) → m[b]
        |}
        |impl Monad[Maybe] {
        |  def ret = Λa. λx: a. Just[a](x)
        |  def bind = Λa. Λb. λv: Maybe[a]. λf: a → Maybe[b]. match v {
        |    Nothing => Nothing[b]
        |    Just(x) => f(x)
        |  }
        |}
        |let main(): i32 = match bind[Maybe][i32][i32](ret[Maybe][i32](2))(λn: i32. Just[i32](3)) {
        |  Nothing => 0
        |  Just(n) => n
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 3)
  }

  test("trait: スーパークラス制約 with") {
    val src =
      """trait Eq[a] {
        |  def same(x: a)(y: a): bool
        |}
        |trait Ord[a] with Eq[a] {
        |  def cmp(x: a)(y: a): i32
        |}
        |impl Eq[i32] { def same(x: i32)(y: i32): bool = true }
        |impl Ord[i32] { def cmp(x: i32)(y: i32): i32 = 4 }
        |let viaSuper[a](x: a): i32 with Ord[a] = if same[a](x)(x) then cmp[a](x)(x) else 0
        |let main(): i32 = viaSuper[i32](1)
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 4)
  }

  test("impl: 注釈なしメソッド") {
    val src =
      """trait Show[a] {
        |  def show: a → i32
        |}
        |impl Show[i32] {
        |  def show = λx: i32. x
        |}
        |let main(): i32 = show[i32](6)
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 6)
  }

  test("impl: 型パラメータと with 文脈") {
    val src = maybe +
      """trait Show[a] {
        |  def show: a → i32
        |}
        |impl Show[i32] {
        |  def show = λx: i32. x
        |}
        |impl[a] Show[Maybe[a]] with Show[a] {
        |  def show = λx: Maybe[a]. match x {
        |    Nothing => 0
        |    Just(v) => show[a](v)
        |  }
        |}
        |let main(): i32 = show[Maybe[i32]](Just[i32](8))
        |""".stripMargin
    assertEquals(Lam.exitCode(src), 8)
  }

  test("let: with 制約は型パラメータ直下の暗黙引数になる") {
    val src =
      """trait Show[a] {
        |  def show: a → i32
        |}
        |let f[a](x: a): i32 with Show[a] = 0
        |let g: i32 = f
        |let main(): i32 = 0
        |""".stripMargin
    // 型は、不一致のエラーの表示から読む
    assert(Lam.messages(src).exists(_.contains("found `∀a. [Show[a]] → a → i32`")), clue(Lam.messages(src)))
  }

  test("let: with なしは従来どおり") {
    val src =
      """let f[a](x: a): i32 = 0
        |let g: i32 = f
        |let main(): i32 = 0
        |""".stripMargin
    assert(Lam.messages(src).exists(_.contains("found `∀a. a → i32`")), clue(Lam.messages(src)))
  }

  test("trait と impl を同一プログラムで混在") {
    val src =
      """trait Show[a] {
        |  def show: a → foreign.C.String
        |}
        |
        |impl Show[i32] {
        |  def show = foreign[i32 → foreign.C.String] int_to_string
        |}
        |let print: foreign.C.String → i32 = foreign[foreign.C.String → i32] print_string_inline
        |let main(): i32 = print(show[i32](12))
        |""".stripMargin
    assertEquals(Lam.output(src), "12")
  }

  test("構文エラー: with の後には制約が要る") {
    assert(syntaxErrors("let f[a](x: a): i32 with = 0\nlet main(): i32 = 0\n").nonEmpty)
    assert(syntaxErrors("trait Ord[a] with {\n  def cmp(x: a)(y: a): i32\n}\nlet main(): i32 = 0\n").nonEmpty)
  }

  test("構文エラー: trait / impl の本体は def の並び") {
    assert(syntaxErrors("trait Show[a] {\n  let show: a → i32\n}\nlet main(): i32 = 0\n").nonEmpty)
    assert(syntaxErrors("trait Show[a] { def show: a → i32 }\nimpl Show[i32] {\n  show = λx: i32. x\n}\nlet main(): i32 = 0\n").nonEmpty)
  }
}
