package com.yuuki14202028

/** derive: データ型の形から Functor / Foldable / Traversable / Eq / Ord / Show のインスタンスを作る */
class DeriveSuite extends munit.FunSuite {

  private val arithmetic =
    """let addInt(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y)
      |let mulInt(x: i32)(y: i32): i32 = intrinsic[mul_i32](x)(y)
      |infixl 6 + = addInt
      |infixl 7 * = mulInt
      |""".stripMargin

  private val functorTrait =
    """trait Functor[f: * → *] {
      |  def map[a][b](x: f[a])(g: a → b): f[b]
      |}
      |""".stripMargin

  private val foldableTrait =
    """trait Foldable[f: * → *] {
      |  def foldRight[a][b](x: f[a])(z: b)(g: a → b → b): b
      |  def foldLeft[a][b](x: f[a])(z: b)(g: b → a → b): b
      |}
      |""".stripMargin

  private val optionData = "data Option[a] = { None, Some(a) }\n"
  private val listData = "rec data List[a] = { Nil, Cons(a)(List[a]) }\n"
  private val eitherData = "data Either[e][a] = { Left(e), Right(a) }\n"
  private val roseData = "rec data Rose[a] = { Node(a)(List[Rose[a]]) }\n"
  private val mainDecl = "let main(): i32 = 0\n"

  private val sumList =
    """rec let sum(xs: List[i32]): i32 = match xs {
      |  Nil => 0
      |  Cons(x)(rest) => x + sum(rest)
      |}
      |""".stripMargin

  // Node(1) の下に Node(2) と Node(3)
  private val sampleRose =
    "let rose: Rose[i32] = Node(1)(Cons(Node(2)(Nil[Rose[i32]]))(Cons(Node(3)(Nil[Rose[i32]]))(Nil[Rose[i32]])))\n"

  private def assertError(source: String)(expected: PartialFunction[CompileError, Unit])(using munit.Location): Unit = {
    val found = Lam.errors(source)
    assert(found.exists(expected.isDefinedAt), clue(found.map(_.message)))
  }

  private def assertUnsupported(source: String, traitName: String, target: String)(using munit.Location): Unit =
    assertError(source) { case CompileError.DeriveUnsupported(`traitName`, `target`, _, _) => }

  private def assertNoInstance(source: String, goal: String)(using munit.Location): Unit =
    assertError(source) { case error: CompileError.NoInstance if error.message.contains(goal) => }

  private def assertDeriveNeeds(source: String, traitName: String, target: String, goal: String)(using munit.Location): Unit =
    assertError(source) { case CompileError.DeriveNeeds(`traitName`, `target`, needed, _) if Type.show(needed) == goal => }

  // ---- Functor 導出 ----

  test("Functor[Option]: 導出したインスタンスは、手書きの impl と同じように解決される") {
    val source = arithmetic + functorTrait + optionData +
      """derive Functor[Option]
        |let inc(x: Option[i32]): Option[i32] = map[Option][i32][i32](x)(λn: i32. n + 1)
        |let main(): i32 = match inc(Some(1)) {
        |  Some(n) => n
        |  None => 0
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 2)
  }

  test("Functor[List]: 再帰データ型は自身のインスタンスで再帰する") {
    val source = arithmetic + functorTrait + listData + sumList +
      """derive Functor[List]
        |let main(): i32 = sum(map[List][i32][i32](Cons(1)(Cons(2)(Nil[i32])))(λn: i32. n * 10))
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 30)
  }

  test("Functor[Either]: 先行パラメータはインスタンスパラメータになる") {
    val source = arithmetic + functorTrait + eitherData +
      """derive Functor[Either]
        |let get(x: Either[bool][i32]): i32 = match map[Either[bool]][i32][i32](x)(λn: i32. n + 1) {
        |  Left(flag) => if flag then 10 else 20
        |  Right(n) => n
        |}
        |let main(): i32 = get(Right[bool][i32](1)) + get(Left[bool][i32](true))
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 12)
  }

  test("Functor[Rose]: ネストした関手フィールドは先行インスタンスへ委譲する") {
    val source = arithmetic + functorTrait + listData + roseData + sampleRose +
      """derive Functor[List]
        |derive Functor[Rose]
        |rec {
        |  let total(r: Rose[i32]): i32 = match r { Node(x)(children) => x + totalAll(children) }
        |  let totalAll(rs: List[Rose[i32]]): i32 = match rs {
        |    Nil => 0
        |    Cons(r)(rest) => total(r) + totalAll(rest)
        |  }
        |}
        |let main(): i32 = total(map[Rose][i32][i32](rose)(λn: i32. n * 2))
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 12)
  }

  test("Functor: 共変位置の関数フィールドも導出できる") {
    val source = arithmetic + functorTrait +
      """data Reader[a] = { MkReader(i32 → a) }
        |derive Functor[Reader]
        |let main(): i32 = match map[Reader][i32][i32](MkReader(λn: i32. n + 1))(λn: i32. n * 2) {
        |  MkReader(run) => run(3)
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 8)
  }

  // ---- Foldable 導出 ----

  private val digits =
    """let leftDigits[f: * → *](xs: f[i32]): i32 with Foldable[f] = foldLeft[f][i32][i32](xs)(0)(λacc: i32. λx: i32. acc * 10 + x)
      |let rightDigits[f: * → *](xs: f[i32]): i32 with Foldable[f] = foldRight[f][i32][i32](xs)(0)(λx: i32. λacc: i32. acc * 10 + x)
      |""".stripMargin

  test("Foldable[Option] / Foldable[List]: 導出が通る") {
    val source = arithmetic + foldableTrait + optionData + listData + digits +
      """derive Foldable[Option]
        |derive Foldable[List]
        |let list: List[i32] = Cons(1)(Cons(2)(Nil[i32]))
        |let main(): i32 = leftDigits[List](list) + rightDigits[List](list) + leftDigits[Option](Some(5)) + rightDigits[Option](None[i32])
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 12 + 21 + 5)
  }

  test("Foldable[Rose]: ネストしたフィールドは先行インスタンスへ委譲する") {
    val source = arithmetic + foldableTrait + listData + roseData + sampleRose + digits +
      """derive Foldable[List]
        |derive Foldable[Rose]
        |let main(): i32 = leftDigits[Rose](rose)
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 123)
  }

  test("Foldable[Either]: a を含まないコンストラクタは畳み込みに影響しない") {
    val source = arithmetic + foldableTrait + eitherData + digits +
      """derive Foldable[Either]
        |let main(): i32 = leftDigits[Either[i32]](Left[i32][i32](9)) + leftDigits[Either[i32]](Right[i32][i32](4))
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 4)
  }

  // ---- エラー ----

  test("エラー: 未対応の trait は導出できない") {
    val source = "trait Pretty[a] {\n  def pretty(x: a): i32\n}\n" + optionData + "derive Pretty[Option]\n" + mainDecl
    assertUnsupported(source, "Pretty", "Option")
  }

  test("エラー: 同名でもシグネチャが正準形でない trait は導出できない") {
    val flipped =
      """trait Functor[f: * → *] {
        |  def map[a][b](g: a → b): f[b] → f[a]
        |}
        |""".stripMargin
    assertUnsupported(flipped + optionData + "derive Functor[Option]\n" + mainDecl, "Functor", "Option")
  }

  test("エラー: データ型でない対象は導出できない") {
    assertUnsupported(functorTrait + "type Alias[a] = a\nderive Functor[Alias]\n" + mainDecl, "Functor", "Alias")
    assertUnsupported(functorTrait + "derive Functor[Functor]\n" + mainDecl, "Functor", "Functor")
  }

  test("エラー: 型パラメータのないデータ型は導出できない") {
    assertUnsupported(functorTrait + "data Wrap = { MkWrap(i32) }\nderive Functor[Wrap]\n" + mainDecl, "Functor", "Wrap")
  }

  test("エラー: 負の位置の出現は導出できない") {
    assertUnsupported(functorTrait + "data Pred[a] = { MkPred(a → bool) }\nderive Functor[Pred]\n" + mainDecl, "Functor", "Pred")
  }

  test("エラー: Foldable は関数フィールドを畳み込めない") {
    assertUnsupported(foldableTrait + "data Reader[a] = { MkReader(i32 → a) }\nderive Foldable[Reader]\n" + mainDecl, "Foldable", "Reader")
  }

  test("エラー: ネストしたフィールドのインスタンスが先行して存在しない") {
    val source = functorTrait + optionData + "data Boxed[a] = { MkBoxed(Option[a]) }\nderive Functor[Boxed]\n" + mainDecl
    assertDeriveNeeds(source, "Functor", "Boxed", "Functor[Option]")
  }

  test("エラー: 手書き impl と重複する derive は Overlapping instance") {
    val source = functorTrait + optionData +
      """impl Functor[Option] {
        |  def map[a][b](o: Option[a])(g: a → b): Option[b] = match o {
        |    None => None[b]
        |    Some(x) => Some[b](g(x))
        |  }
        |}
        |derive Functor[Option]
        |""".stripMargin + mainDecl
    assertError(source) { case _: CompileError.OverlappingInstance => }
  }

  test("エラー: 同じ derive を二度書くと Overlapping instance") {
    val source = functorTrait + optionData + "derive Functor[Option]\nderive Functor[Option]\n" + mainDecl
    assertError(source) { case _: CompileError.OverlappingInstance => }
  }

  test("パラメータを含まない自己適用フィールドはそのまま保持される") {
    val source = arithmetic + functorTrait +
      """rec data Weird[a] = { MkWeird(Weird[i32])(a), WNil }
        |derive Functor[Weird]
        |let main(): i32 = match map[Weird][bool][i32](MkWeird(MkWeird(WNil[i32])(7))(true))(λb: bool. 1) {
        |  MkWeird(MkWeird(_)(inner))(outer) => inner + outer
        |  _ => 0
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 8)
  }

  // 導出したインスタンスは普通の impl として検査されるので、非正則な再帰も、型が合えば通る
  test("非正則な自己適用も、自身のインスタンスで導出できる") {
    val source = arithmetic + functorTrait +
      """rec data Weird[a] = { MkWeird(Weird[Weird[a]])(a), WNil }
        |derive Functor[Weird]
        |let main(): i32 = match map[Weird][i32][i32](MkWeird(WNil[Weird[i32]])(4))(λn: i32. n + 1) {
        |  MkWeird(_)(n) => n
        |  WNil => 0
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 5)
  }

  // ---- Traversable ----

  private val traversablePrelude =
    """trait Functor[f: * → *] { def map[a][b](x: f[a])(g: a → b): f[b] }
      |trait Applicative[f: * → *] with Functor[f] {
      |  def pure[a](x: a): f[a]
      |  def apply[a][b](g: f[a → b]): f[a] → f[b]
      |}
      |trait Traversable[t: * → *] {
      |  def traverse[a][g: * → *][b](xs: t[a])(f: a → g[b]): g[t[b]] with Applicative[g]
      |}
      |""".stripMargin

  private val optionApplicative = optionData +
    """impl Functor[Option] {
      |  def map[a][b](x: Option[a])(g: a → b): Option[b] = match x {
      |    None => None[b]
      |    Some(v) => Some[b](g(v))
      |  }
      |}
      |impl Applicative[Option] {
      |  def pure[a](x: a): Option[a] = Some[a](x)
      |  def apply[a][b](g: Option[a → b]): Option[a] → Option[b] = λx: Option[a]. match g {
      |    None => None[b]
      |    Some(h) => match x {
      |      None => None[b]
      |      Some(v) => Some[b](h(v))
      |    }
      |  }
      |}
      |let eqInt(x: i32)(y: i32): bool = intrinsic[eq_i32](x)(y)
      |infix 4 == = eqInt
      |// 0 があれば全体が None
      |let nonZero(n: i32): Option[i32] = if n == 0 then None[i32] else Some(n * 2)
      |""".stripMargin

  test("derive Traversable: 再帰・パラメータ・定数のフィールド") {
    val source = arithmetic + traversablePrelude + optionApplicative + listData + sumList +
      """data Tagged[a] = { Tag(i32)(a) }
        |derive Traversable[List]
        |derive Traversable[Tagged]
        |let total(xs: List[i32]): i32 = match traverse[List][i32][Option][i32](xs)(nonZero) {
        |  Some(doubled) => sum(doubled)
        |  None => 100
        |}
        |let tagged(t: Tagged[i32]): i32 = match traverse[Tagged][i32][Option][i32](t)(nonZero) {
        |  Some(Tag(tag)(value)) => tag + value
        |  None => 100
        |}
        |let main(): i32 = total(Cons(1)(Cons(2)(Nil[i32]))) + total(Cons(1)(Cons(0)(Nil[i32]))) + tagged(Tag(1)(2))
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 6 + 100 + 5)
  }

  test("derive Traversable: 入れ子のコンテナは、そのコンテナの traverse に委譲する") {
    val source = arithmetic + traversablePrelude + optionApplicative + listData + roseData + sampleRose +
      """derive Traversable[List]
        |derive Traversable[Rose]
        |let main(): i32 = match traverse[Rose][i32][Option][i32](rose)(nonZero) {
        |  Some(Node(x)(Cons(Node(y)(_))(Cons(Node(z)(_))(_)))) => x + y + z
        |  _ => 0
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 12)
  }

  test("derive Traversable: 入れ子のコンテナにインスタンスがなければエラー") {
    val source = traversablePrelude + listData + roseData + "derive Traversable[Rose]\n" + mainDecl
    assertDeriveNeeds(source, "Traversable", "Rose", "Traversable[List]")
  }

  test("derive Traversable: 関数のフィールドは導出できない") {
    val source = traversablePrelude + "data Reader[a] = { MkReader(i32 → a) }\nderive Traversable[Reader]\n" + mainDecl
    assertUnsupported(source, "Traversable", "Reader")
  }

  test("derive Traversable: traverse のシグネチャが正準形と違えばエラー") {
    val source =
      """trait Functor[f: * → *] { def map[a][b](x: f[a])(g: a → b): f[b] }
        |trait Applicative[f: * → *] with Functor[f] {
        |  def pure[a](x: a): f[a]
        |  def apply[a][b](g: f[a → b]): f[a] → f[b]
        |}
        |trait Traversable[t: * → *] {
        |  def traverse[g: * → *][a][b](f: a → g[b])(xs: t[a]): g[t[a]] with Applicative[g]
        |}
        |data Box[a] = { MkBox(a) }
        |derive Traversable[Box]
        |""".stripMargin + mainDecl
    assertUnsupported(source, "Traversable", "Box")
  }

  test("derive Traversable: Applicative が定義されていなければエラー") {
    val source =
      """trait Applicative2[f: * → *] { def pure2[a](x: a): f[a] }
        |trait Traversable[t: * → *] {
        |  def traverse[a][g: * → *][b](xs: t[a])(f: a → g[b]): g[t[b]] with Applicative2[g]
        |}
        |data Box[a] = { MkBox(a) }
        |derive Traversable[Box]
        |""".stripMargin + mainDecl
    assertUnsupported(source, "Traversable", "Box")
  }

  test("rec { derive }: 相互再帰のデータ型にも Traversable を導出できる") {
    val source = arithmetic + traversablePrelude + optionApplicative +
      """rec {
        |  data Tree[a] = { Node(a)(Forest[a]) }
        |  data Forest[a] = { Empty, Trees(Tree[a])(Forest[a]) }
        |}
        |rec {
        |  derive Traversable[Tree]
        |  derive Traversable[Forest]
        |}
        |let tree: Tree[i32] = Node(1)(Trees(Node(2)(Empty[i32]))(Empty[i32]))
        |let main(): i32 = match traverse[Tree][i32][Option][i32](tree)(nonZero) {
        |  Some(Node(x)(Trees(Node(y)(_))(_))) => x + y
        |  _ => 0
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 6)
  }

  // ---- Eq / Ord / Show ----

  private val valueTraits =
    """trait Eq[a] { def eq(x: a)(y: a): bool }
      |trait Ord[a] with Eq[a] { def lt(x: a)(y: a): bool }
      |trait Show[a] { def show(x: a): foreign.C.String }
      |impl Eq[i32] { def eq(x: i32)(y: i32): bool = intrinsic[eq_i32](x)(y) }
      |impl Ord[i32] { def lt(x: i32)(y: i32): bool = intrinsic[lt_i32](x)(y) }
      |impl Show[i32] { def show = foreign[i32 → foreign.C.String] int_to_string }
      |let print: foreign.C.String → i32 = foreign[foreign.C.String → i32] print_string_inline
      |""".stripMargin

  private def holds(declarations: String, condition: String)(using munit.Location): Boolean =
    Lam.exitCode(s"${declarations}let main(): i32 = if $condition then 1 else 0\n") == 1

  test("derive Eq / Ord / Show: 型パラメータごとに同じ trait の文脈を持つインスタンスになる") {
    val source = valueTraits + listData +
      """derive Eq[List]
        |derive Ord[List]
        |derive Show[List]
        |let one: List[i32] = Cons(1)(Nil[i32])
        |let oneTwo: List[i32] = Cons(1)(Cons(2)(Nil[i32]))
        |""".stripMargin
    assert(holds(source, "eq[List[i32]](oneTwo)(oneTwo)"))
    assert(!holds(source, "eq[List[i32]](one)(oneTwo)"))
    // 構成子は宣言順（Nil < Cons）、同じ構成子ならフィールドの辞書順
    assert(holds(source, "lt[List[i32]](one)(oneTwo)"))
    assert(!holds(source, "lt[List[i32]](oneTwo)(one)"))
    assert(holds(source, "lt[List[i32]](oneTwo)(Cons(2)(Nil[i32]))"))
    assertEquals(Lam.output(source + "let main(): i32 = print(show[List[i32]](oneTwo))\n"), "Cons(1, Cons(2, Nil))")
    // 文脈 Eq[a] は、要素の型のインスタンスで満たす
    assertNoInstance(source + "let same: bool = eq[List[bool]](Nil[bool])(Nil[bool])\n" + mainDecl, "Eq[bool]")
  }

  test("derive Eq / Ord / Show: 型パラメータのない data・フィールドのない構成子") {
    val source = valueTraits + "data Color = { Red, Green }\nderive Eq[Color]\nderive Ord[Color]\nderive Show[Color]\n"
    assert(holds(source, "eq[Color](Red)(Red) && lt[Color](Red)(Green)"))
    assert(!holds(source, "eq[Color](Red)(Green) || lt[Color](Green)(Red) || lt[Color](Red)(Red)"))
    assertEquals(Lam.output(source + "let main(): i32 = print(show[Color](Green))\n"), "Green")
  }

  test("derive Ord: スーパークラスの Eq が先に要る") {
    assertDeriveNeeds(valueTraits + "data Color = { Red, Green }\nderive Ord[Color]\n" + mainDecl, "Ord", "Color", "Eq[Color]")
  }

  test("derive Eq: インスタンスのないフィールドはエラー") {
    assertDeriveNeeds(valueTraits + "data F = { MkF(i32 → i32) }\nderive Eq[F]\n" + mainDecl, "Eq", "F", "Eq[i32 → i32]")
  }

  test("derive Eq: 型パラメータはカインド * に限る") {
    val source = valueTraits + "data Apply[f: * → *] = { MkApply(f[i32]) }\nderive Eq[Apply]\n" + mainDecl
    assertUnsupported(source, "Eq", "Apply")
  }

  test("derive Show: 正準シグネチャと違えばエラー") {
    val source = "trait Show[a] { def show(x: a): i32 }\ndata Color = { Red }\nderive Show[Color]\n" + mainDecl
    assertUnsupported(source, "Show", "Color")
  }
}
