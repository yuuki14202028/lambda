package com.yuuki14202028

/** ドット記法 x.f: x の型の module の関数か、スコープにある trait のメソッドの呼び出し */
class SelectSuite extends munit.FunSuite {

  private val prelude =
    """rec data List[a] = { Nil, Cons(a)(List[a]) }
      |data Pair[a][b] = { MkPair(a)(b) }
      |trait Show[a] { def show(x: a): i32 }
      |impl Show[i32] { def show = λn: i32. n }
      |module List {
      |  let isEmpty[a](xs: List[a]): bool = match xs { Nil => true  Cons(_)(_) => false }
      |  let prepend[a](xs: List[a])(x: a): List[a] = Cons[a](x)(xs)
      |  let convert[a][b](xs: List[a])(f: a → b): List[b] = match xs { Nil => Nil[b]  Cons(x)(_) => Cons[b](f(x))(Nil[b]) }
      |  let shown[a](xs: List[a]): i32 with Show[a] = match xs { Nil => 0  Cons(x)(_) => show[a](x) }
      |  let ints(xs: List[i32]): i32 = match xs { Nil => 0  Cons(x)(_) => x }
      |  let repeat(count: i32)(xs: List[i32]): List[i32] = xs
      |  let swapped[b][a](xs: List[a])(y: b): b = y
      |}
      |module Pair {
      |  let first[a][b](p: Pair[a][b]): a = match p { MkPair(x)(_) => x }
      |}
      |module i32 {
      |  let twice(n: i32): i32 = intrinsic[add_i32](n)(n)
      |}
      |let numbers: List[i32] = Cons[i32](1)(Nil[i32])
      |""".stripMargin

  private def program(body: String, main: String = "0"): String = s"$prelude$body\nlet main(): i32 = $main\n"

  /** expected に当たるエラーが報告される */
  private def assertError(source: String)(expected: PartialFunction[CompileError, Any])(using munit.Location): Unit = {
    val found = Lam.errors(source)
    assert(found.exists(expected.isDefinedAt), clue(found))
  }

  // ---- 解決 ----

  test("x.f は、x の型の module の関数に、型から読んだ型引数と x を渡す") {
    assertEquals(Lam.exitCode(program("let b: bool = numbers.isEmpty", "if b then 1 else 2")), 2)
    assertEquals(Lam.exitCode(program("let b: bool = Nil[i32].isEmpty", "if b then 1 else 2")), 1)
  }

  test("残りの引数は、ふつうの適用で渡す") {
    assertEquals(Lam.exitCode(program("let more: List[i32] = numbers.prepend(5)", "more.ints")), 5)
    assertError(program("let more: List[i32] = numbers.prepend(true)")) { case _: CompileError.TypeMismatch => }
  }

  test("受け手の型から決まらない型引数は、名前の後に書くか、後の引数から読む") {
    assertEquals(Lam.exitCode(program("let bs: List[bool] = numbers.convert[bool](λn: i32. true)", "if bs.isEmpty then 1 else 2")), 2)
    assertEquals(Lam.exitCode(program("let bs: List[bool] = numbers.convert(λn: i32. true)", "if bs.isEmpty then 1 else 2")), 2)
    assertError(program("let b: bool = numbers.isEmpty[i32]")) { case _: CompileError.NotPolymorphic => }
  }

  test("型引数が複数ある型: すべて受け手の型から読む") {
    assertEquals(Lam.exitCode(program("let p: Pair[i32][bool] = MkPair[i32][bool](1)(true)\nlet n: i32 = p.first", "n")), 1)
  }

  test("制約は、受け手の型から読んだ型引数で解決する") {
    assertEquals(Lam.exitCode(program("let n: i32 = numbers.shown", "n")), 1)
    assertError(program("let bs: List[bool] = Nil[bool]\nlet n: i32 = bs.shown")) {
      case error: CompileError.NoInstance if error.message.contains("Show[bool]") =>
    }
    assertEquals(Lam.exitCode(program("let f[a](xs: List[a]): i32 with Show[a] = xs.shown", "f(numbers)")), 1)
  }

  test("最初の引数の型が具体的な関数は、その型の値にだけ使える") {
    assertEquals(Lam.exitCode(program("let n: i32 = numbers.ints", "n")), 1)
    assertError(program("let bs: List[bool] = Nil[bool]\nlet n: i32 = bs.ints")) { case CompileError.SelectNotApplicable("List.ints", _, _, _) => }
  }

  test("組み込みの型の module は、その型の名前") {
    assertEquals(Lam.exitCode(program("let n: i32 = 1.twice", "n")), 2)
    assertEquals(Lam.exitCode(program("let n: i32 = numbers.ints.twice.twice", "n")), 4)
  }

  test("式の続きにも書ける（適用・括弧・行を変えた続き）") {
    assertEquals(Lam.exitCode(program("let n: i32 = numbers.prepend(3).ints", "n")), 3)
    assertEquals(Lam.exitCode(program("let n: i32 = (Cons[i32](4)(Nil[i32])).ints", "n")), 4)
    assertEquals(Lam.exitCode(program("let n: i32 = numbers\n  .prepend(0)\n  .prepend(6)\n  .ints", "n")), 6)
    assertEquals(Lam.exitCode(program("let f(xs: List[i32]): bool = xs.isEmpty", "if f(numbers) then 1 else 2")), 2)
  }

  // ---- 使えない場合 ----

  test("module にない名前") {
    assertError(program("let n: i32 = numbers.length")) {
      case CompileError.SelectUnknown("length", receiver, _, _, _) if Type.show(receiver) == "List[i32]" =>
    }
  }

  test("module のない型（型変数・関数）には使えない") {
    assertError(program("let f[a](x: a): i32 = x.twice")) {
      case CompileError.SelectUnknown("twice", receiver, _, _, _) if Type.show(receiver) == "a" =>
    }
    assertError(program("let f(g: i32 → i32): i32 = g.twice")) { case CompileError.SelectUnknown("twice", _, _, _, _) => }
  }

  test("最初の引数が受け手の型でない関数は、ドットで呼べない") {
    assertError(program("let more: List[i32] = numbers.repeat(2)")) { case CompileError.SelectNotApplicable("List.repeat", _, _, _) => }
    assertEquals(Lam.exitCode(program("let more: List[i32] = List.repeat(2)(numbers)", "more.ints")), 1)
  }

  // 以前は「受け手の型から読む型引数は先頭」という制限があった。今は、適用の並びを左から処理するだけ
  test("受け手の型から読む型引数は、関数の型引数の先頭に並んでいなくてもよい") {
    assertEquals(Lam.exitCode(program("let b: bool = numbers.swapped[bool](true)", "if b then 1 else 2")), 1)
    assertEquals(Lam.exitCode(program("let n: i32 = numbers.swapped(7)", "n")), 7)
    assertEquals(Lam.exitCode(program("let b: bool = List.swapped[bool][i32](numbers)(true)", "if b then 1 else 2")), 1)
  }

  test("private なメンバーは、module の外からドットでも呼べない") {
    val source = "module List {\n  private let secret[a](xs: List[a]): i32 = 9\n  let visible[a](xs: List[a]): i32 = xs.secret\n}\n"
    assertEquals(Lam.exitCode(program(source, "numbers.visible")), 9)
    assertError(program(source + "let n: i32 = numbers.secret")) { case CompileError.SelectUnknown("secret", _, _, _, _) => }
  }

  // ---- 名前の読み方 ----

  test("a.b が修飾された名前（M.x）として読めるなら、それが優先される") {
    val source =
      """module M {
        |  let isEmpty: i32 = 0
        |  let other: i32 = 0
        |}
        |let f(M: List[i32]): i32 = M.isEmpty
        |let g(M: List[i32]): bool = M.prepend(1).isEmpty
        |""".stripMargin
    assertEquals(Lam.exitCode(program(source, "if g(numbers) then 1 else f(numbers)")), 0)
  }

  test("構成子と同じ名前の module のメンバーを、修飾して呼べる") {
    val source =
      """data Box[a] = { Box(a) }
        |module Box {
        |  let content[a](b: Box[a]): a = match b { Box(x) => x }
        |}
        |let n: i32 = Box.content[i32](Box[i32](1))
        |let m: i32 = Box[i32](2).content
        |""".stripMargin
    assertEquals(Lam.exitCode(program(source, "intrinsic[add_i32](n)(m)")), 3)
  }

  test("修飾された名前の後にも続けられる") {
    val source =
      """module Data {
        |  let items: List[i32] = Nil[i32]
        |}
        |let b: bool = Data.items.isEmpty
        |let c: bool = Data.items.prepend(1).isEmpty
        |""".stripMargin
    assertEquals(Lam.exitCode(program(source, "if b then (if c then 1 else 2) else 3")), 2)
    assertError(program(source + "let d: bool = Data.missing.isEmpty")) { case CompileError.UndefinedVariable("Data.missing", _) => }
  }

  test("module の中の型の module は、その型の完全な名前") {
    val source =
      """module Shape {
        |  data T = { Rect(i32)(i32) }
        |  module T {
        |    let width(s: T): i32 = match s { Rect(w)(_) => w }
        |  }
        |}
        |let w: i32 = Shape.Rect(2)(3).width
        |""".stripMargin
    assertEquals(Lam.exitCode(program(source, "w")), 2)
  }

  test("opaque type の値にも、その型の module の関数を呼べる") {
    val source =
      """module Counter {
        |  opaque type T = i32
        |  let zero: T = 0
        |  module T {
        |    let value(c: T): i32 = 8
        |  }
        |}
        |let n: i32 = Counter.zero.value
        |""".stripMargin
    assertEquals(Lam.exitCode(program(source, "n")), 8)
    assertError(program(source + "let m: i32 = Counter.zero.twice")) {
      case CompileError.SelectUnknown("twice", receiver, _, _, _) if Type.show(receiver) == "Counter.T" =>
    }
  }

  test("x.f(with d): 関数の制約を先頭から d で満たす") {
    val dictionary = "let d: Show[i32] = impl Show[i32] { def show = λn: i32. 7 }\n"
    assertEquals(Lam.exitCode(program(dictionary + "let n: i32 = numbers.shown(with d)", "n")), 7)
    // 修飾された名前の後も同じ
    assertEquals(Lam.exitCode(program(dictionary + "let n: i32 = List.shown[i32](with d)(numbers)", "n")), 7)
    // 式の続きのドットでも
    assertEquals(Lam.exitCode(program(dictionary + "let n: i32 = numbers.prepend(1).shown(with d)", "n")), 7)
    assertEquals(Lam.exitCode(program(dictionary + "let n: i32 = using d in numbers.shown", "n")), 7)
    assertEquals(Lam.exitCode(program("let d: Show[i32] = summon[Show[i32]]\nlet n: i32 = using d in numbers.shown", "n")), 1)
  }

  test("x.f(with d): 辞書の型が制約と合わなければエラー、制約がなければエラー") {
    assertError(program("let n: i32 = numbers.shown(with 1)")) { case _: CompileError.TypeMismatch => }
    assertError(program("let d: Show[i32] = summon[Show[i32]]\nlet n: bool = numbers.isEmpty(with d)")) {
      case _: CompileError.NoConstraintToSatisfy =>
    }
  }

  // ---- trait のメソッド ----

  private val traits =
    """rec data List[a] = { Nil, Cons(a)(List[a]) }
      |data Option[a] = { Some(a), None }
      |trait Functor[f: * → *] { def map[a][b](x: f[a])(g: a → b): f[b] }
      |trait Applicative[f: * → *] with Functor[f] { def pure[a](x: a): f[a] }
      |trait Traversable[t: * → *] with Functor[t] { def traverse[a][g: * → *][b](xs: t[a])(f: a → g[b]): g[t[b]] with Applicative[g] }
      |trait Show[a] { def show(x: a): i32 }
      |impl Show[i32] { def show = λn: i32. n }
      |impl Functor[Option] { def map[a][b](x: Option[a])(g: a → b): Option[b] = match x { Some(v) => Some[b](g(v))  None => None[b] } }
      |impl Applicative[Option] { def pure[a](x: a): Option[a] = Some[a](x) }
      |rec impl Functor[List] { def map[a][b](x: List[a])(g: a → b): List[b] = match x { Nil => Nil[b]  Cons(v)(rest) => Cons[b](g(v))(map[List][a][b](rest)(g)) } }
      |impl Traversable[Option] { def traverse[a][g: * → *][b](xs: Option[a])(f: a → g[b]): g[Option[b]] with Applicative[g] = match xs {
      |  Some(v) => f(v).map[Option[b]](λy: b. Some[b](y))
      |  None => pure[g][Option[b]](None[b])
      |} }
      |let numbers: List[i32] = Cons[i32](1)(Nil[i32])
      |let head(xs: List[i32]): i32 = match xs { Nil => 0  Cons(x)(_) => x }
      |""".stripMargin

  private def withTraits(body: String, main: String = "0"): String = s"$traits$body\nlet main(): i32 = $main\n"

  test("trait のメソッドも、受け手を最初の引数としてドットで呼べる") {
    assertEquals(Lam.exitCode(withTraits("let doubled: List[i32] = numbers.map[i32](λn: i32. intrinsic[add_i32](n)(n))", "head(doubled)")), 2)
    assertEquals(Lam.exitCode(withTraits("let n: i32 = (3).show", "n")), 3)
  }

  test("trait のメソッド: 受け手の型が型変数でも、スコープ中の制約で解決する") {
    val source = "let twice[f: * → *](xs: f[i32]): f[i32] with Functor[f] = xs.map[i32](λn: i32. intrinsic[add_i32](n)(1)).map[i32](λn: i32. intrinsic[add_i32](n)(1))"
    assertEquals(Lam.exitCode(withTraits(source, "head(twice[List](numbers))")), 3)
  }

  test("trait のメソッド: メソッド自身の制約は、型引数を書いた時点で解決する") {
    val main =
      """match o {
        |  Some(Some(n)) => n
        |  _ => 0
        |}""".stripMargin
    assertEquals(Lam.exitCode(withTraits("let o: Option[Option[i32]] = Some[i32](5).traverse[Option][i32](λn: i32. Some[i32](n))", main)), 5)
  }

  test("trait のメソッド: インスタンスがなければ、ふつうのインスタンスのエラー") {
    assertError(withTraits("let b: i32 = true.show")) { case _: CompileError.NoInstance => }
  }

  test("型の module の関数と trait のメソッドが同じ名前なら、曖昧さのエラー") {
    val source = "module List { let map[a][b](xs: List[a])(f: a → b): List[b] = Nil[b] }\nlet m: List[i32] = numbers.map[i32](λn: i32. n)"
    assertError(withTraits(source)) { case CompileError.SelectAmbiguous("map", _, _, _) => }
  }

  test("局所的な束縛に隠された trait のメソッドは、候補にならない") {
    assertError(withTraits("let f(show: i32 → i32): i32 = (1).show")) { case CompileError.SelectUnknown("show", _, _, _, _) => }
  }
}
