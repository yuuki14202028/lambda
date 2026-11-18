package com.yuuki14202028

/** 再帰は rec の印がある所に限る: rec let / rec data / rec impl と、相互再帰の rec { … } */
class RecursionSuite extends munit.FunSuite {

  private val arithmetic =
    """let addInt(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y)
      |let subInt(x: i32)(y: i32): i32 = intrinsic[sub_i32](x)(y)
      |let eqInt(x: i32)(y: i32): bool = intrinsic[eq_i32](x)(y)
      |infixl 6 + = addInt
      |infixl 6 - = subInt
      |infix 4 == = eqInt
      |""".stripMargin

  private val main = "let main(): i32 = 0\n"

  private def assertAccepted(source: String)(using munit.Location): Unit = assertEquals(Lam.exitCode(source), 0)

  private def assertError(source: String)(expected: PartialFunction[CompileError, Unit])(using munit.Location): Unit = {
    val found = Lam.errors(source)
    assert(found.exists(expected.isDefinedAt), clue(found.map(_.message)))
  }

  private def assertNotFunction(source: String)(using munit.Location): Unit =
    assertError(source) { case _: CompileError.RecursiveValueNotFunction => }

  private def assertSyntaxError(source: String)(using munit.Location): Unit =
    assertError(source) { case _: CompileError.Syntax => }

  test("rec let: 関数は自身を参照できる") {
    val source = arithmetic +
      """rec let sum(n: i32): i32 = if n == 0 then 0 else n + sum(n - 1)
        |let main(): i32 = sum(4)
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 10)
  }

  test("rec let: 型パラメータ付きの関数も関数として扱う") {
    val source = arithmetic +
      """rec let times[a](n: i32)(f: a → a)(x: a): a = if n == 0 then x else times[a](n - 1)(f)(f(x))
        |let main(): i32 = times[i32](3)(λk: i32. k + 2)(1)
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 7)
  }

  test("rec let: 関数でない値が自身を参照するのは拒否") {
    assertNotFunction("rec let x: i32 = x\nlet main(): i32 = 0\n")
  }

  test("rec let: 関数の適用結果として自身を参照するのも拒否") {
    val source =
      """let apply(f: unit → i32): unit → i32 = f
        |rec let g: unit → i32 = apply(λu: unit. g(u))
        |let main(): i32 = 0
        |""".stripMargin
    assertNotFunction(source)
  }

  test("rec let: 自身を参照しない値は関数でなくてもよい") {
    assertEquals(Lam.exitCode("rec let x: i32 = 1\nlet main(): i32 = x\n"), 1)
  }

  test("rec let: in 式でも拒否") {
    assertNotFunction("let main(): i32 = rec let x: i32 = x in x\n")
  }

  private val maybeMonad =
    """data Maybe[a] = { Nothing, Just(a) }
      |trait Monad[m: * → *] {
      |  def pure[a](x: a): m[a]
      |  def flatMap[a][b](v: m[a])(f: a → m[b]): m[b]
      |}
      |impl Monad[Maybe] {
      |  def pure[a](x: a): Maybe[a] = Just[a](x)
      |  def flatMap[a][b](v: Maybe[a])(f: a → Maybe[b]): Maybe[b] = match v {
      |    Nothing => Nothing[b]
      |    Just(x) => f(x)
      |  }
      |}
      |""".stripMargin

  test("rec let: context ブロックの rec let でも拒否") {
    val source = maybeMonad +
      """let r(): Maybe[i32] = context[Maybe] {
        |  rec let x: i32 = x;
        |  x
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertNotFunction(source)
  }

  private val defaultTrait =
    """trait Default[a] {
      |  def default: a
      |  def orDefault(x: a): a
      |}
      |""".stripMargin

  test("rec impl: 関数でないメソッドが自身のインスタンスを使うのは拒否") {
    val source = defaultTrait +
      """rec impl Default[i32] {
        |  def default = default[i32]
        |  def orDefault = λx: i32. x
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertError(source) { case CompileError.RecursiveInstanceMethodNotFunction("default", _) => }
  }

  test("rec impl: 関数のメソッドからは自身のインスタンスを使える") {
    val source = defaultTrait +
      """rec impl Default[i32] {
        |  def default = 5
        |  def orDefault = λx: i32. default[i32]
        |}
        |let main(): i32 = orDefault[i32](1)
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 5)
  }

  test("rec impl: 文脈のあるインスタンスは辞書自体が関数なので、値のメソッドでもよい") {
    val source = defaultTrait +
      """data Box[a] = { MkBox(a) }
        |impl Default[i32] {
        |  def default = 5
        |  def orDefault = λx: i32. x
        |}
        |rec impl[a] Default[Box[a]] with Default[a] {
        |  def default = orDefault[Box[a]](MkBox[a](default[a]))
        |  def orDefault = λx: Box[a]. x
        |}
        |let main(): i32 = orDefault[i32](0)
        |""".stripMargin
    // 検査は通る。default[Box[i32]] を実際に読むと止まらない（rec の停止性は保証しない）
    assertEquals(Lam.exitCode(source), 0)
  }

  // ---- rec { … }: 相互再帰 ----

  private val evenOdd =
    """rec {
      |  let isEven(n: i32): bool = if n == 0 then true else isOdd(n - 1)
      |  let isOdd(n: i32): bool = if n == 0 then false else isEven(n - 1)
      |}
      |""".stripMargin

  test("rec { let }: 組のメンバーは互いに参照できる") {
    val source = arithmetic + evenOdd +
      """let both(n: i32): bool = isEven(n) && isOdd(n + 1)
        |let main(): i32 = if both(10) then 1 else 0
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 1)
  }

  test("rec { let }: 区切りは改行でもセミコロンでもよい") {
    assertAccepted("rec { let f(x: i32): i32 = g(x); let g(x: i32): i32 = f(x) }\n" + main)
  }

  test("rec let は宣言 1 つの rec { } と同じ") {
    val single = arithmetic +
      """rec let sum(n: i32): i32 = if n == 0 then 0 else n + sum(n - 1)
        |rec data L = { N, C(L) }
        |rec let length(xs: L): i32 = match xs {
        |  N => 0
        |  C(rest) => 1 + length(rest)
        |}
        |let main(): i32 = sum(3) + length(C(C(N)))
        |""".stripMargin
    val grouped = arithmetic +
      """rec { let sum(n: i32): i32 = if n == 0 then 0 else n + sum(n - 1) }
        |rec { data L = { N, C(L) } }
        |rec {
        |  let length(xs: L): i32 = match xs {
        |    N => 0
        |    C(rest) => 1 + length(rest)
        |  }
        |}
        |let main(): i32 = sum(3) + length(C(C(N)))
        |""".stripMargin
    assertEquals(Lam.exitCode(single), 8)
    assertEquals(Lam.exitCode(grouped), 8)
  }

  test("rec でなければ後ろの宣言は見えない") {
    val source =
      """let f(x: i32): i32 = g(x)
        |let g(x: i32): i32 = x
        |""".stripMargin
    assertError(source + main) { case CompileError.UndefinedVariable("g", _) => }
    assertError("let f(x: i32): i32 = f(x)\n" + main) { case CompileError.UndefinedVariable("f", _) => }
  }

  test("rec { let }: 関数でない値は組のメンバーを参照できない") {
    val source =
      """rec {
        |  let f(x: i32): i32 = x
        |  let y: i32 = f(1)
        |}
        |""".stripMargin
    assertNotFunction(source + main)
  }

  test("rec { let }: 組のメンバーを参照しない値は関数でなくてもよい") {
    val source =
      """rec {
        |  let base: i32 = 3
        |  let f(stop: bool): i32 = if stop then base else f(true)
        |}
        |let main(): i32 = f(false)
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 3)
  }

  test("rec { let }: 同じ名前を二度定義できない") {
    assertError("rec { let f(x: i32): i32 = x; let f(x: i32): i32 = x }\n" + main) { case CompileError.AlreadyDefined("f", _) => }
  }

  test("rec { let } in: 式の中でも相互再帰できる") {
    val source = arithmetic +
      """let parity(n: i32): bool =
        |  rec {
        |    let isEven(k: i32): bool = if k == 0 then true else isOdd(k - 1)
        |    let isOdd(k: i32): bool = if k == 0 then false else isEven(k - 1)
        |  } in isEven(n)
        |let main(): i32 = if parity(6) then 1 else 0
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 1)
    assertNotFunction("let h: i32 = rec { let f(x: i32): i32 = x; let y: i32 = f(1) } in y\n" + main)
  }

  private val forest =
    """rec {
      |  data Tree[a] = { Node(a)(Forest[a]) }
      |  data Forest[a] = { Empty, Trees(Tree[a])(Forest[a]) }
      |}
      |""".stripMargin

  // Node(1) の下に葉が 2 つ
  private val sampleTree =
    "let sample: Tree[i32] = Node[i32](1)(Trees[i32](Node[i32](2)(Empty[i32]))(Trees[i32](Node[i32](3)(Empty[i32]))(Empty[i32])))\n"

  test("rec { data }: 相互再帰のデータ型を定義して match できる") {
    val source = arithmetic + forest + sampleTree +
      """rec {
        |  let sum(t: Tree[i32]): i32 = match t { Node(x)(children) => x + sumAll(children) }
        |  let sumAll(f: Forest[i32]): i32 = match f {
        |    Empty => 0
        |    Trees(t)(rest) => sum(t) + sumAll(rest)
        |  }
        |}
        |let main(): i32 = sum(sample)
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 6)
  }

  test("rec でなければ後ろのデータ型は見えない") {
    val source =
      """data Tree = { Node(Forest) }
        |data Forest = { Empty, Trees(Tree)(Forest) }
        |""".stripMargin
    assertError(source + main) { case CompileError.UndefinedType("Forest", _) => }
    assertError("data L = { N, C(L) }\n" + main) { case CompileError.UndefinedType("L", _) => }
  }

  test("rec { data }: 型名・構成子名の重複は拒否") {
    assertError("rec { data A = { MkA(A) }; data A = { MkB(A) } }\n" + main) { case CompileError.AlreadyDefined("A", _) => }
    assertError("rec { data A = { Mk(B) }; data B = { Mk(A) } }\n" + main) { case CompileError.AlreadyDefined("Mk", _) => }
  }

  test("rec { data }: fold は自身の型の出現だけを畳み込む") {
    // rest は畳み込んだ結果（i32）、t は組の別の型なので Tree[a] のまま
    val source = arithmetic + forest + sampleTree +
      """let count[a](f: Forest[a]): i32 = fold f as i32 {
        |  Empty => 0
        |  Trees(t)(rest) => match t { Node(_)(_) => 1 + rest }
        |}
        |let main(): i32 = match sample { Node(_)(children) => count(children) }
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 2)
  }

  test("fold の後に続けて宣言を書ける") {
    val source = arithmetic +
      """rec data List = { Nil, Cons(i32)(List) }
        |let length(xs: List): i32 = fold xs as i32 {
        |  Nil => 0
        |  Cons(x)(acc) => 1 + acc
        |}
        |let after: i32 = length(Cons(7)(Cons(8)(Nil)))
        |let main(): i32 = after
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 2)
  }

  test("古い fold の書き方（with）は構文エラー") {
    val source =
      """rec data List = { Nil, Cons(i32)(List) }
        |let length(xs: List): i32 = fold xs as i32 with Nil => 0
        |""".stripMargin
    assertSyntaxError(source + main)
  }

  private val sizeTrait = "trait Size[a] { def size(x: a): i32 }\n"

  test("rec { impl }: 組のインスタンスは互いのメソッド本体から見える") {
    val source = arithmetic + forest + sampleTree + sizeTrait +
      """rec {
        |  impl[a] Size[Tree[a]] {
        |    def size = λt: Tree[a]. match t { Node(x)(children) => 1 + size[Forest[a]](children) }
        |  }
        |  impl[a] Size[Forest[a]] {
        |    def size = λf: Forest[a]. match f {
        |      Empty => 0
        |      Trees(t)(rest) => size[Tree[a]](t) + size[Forest[a]](rest)
        |    }
        |  }
        |}
        |let main(): i32 = size[Tree[i32]](sample)
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 3)
  }

  test("rec でなければ後ろのインスタンスは見えない") {
    val source = forest + sizeTrait +
      """impl[a] Size[Tree[a]] {
        |  def size = λt: Tree[a]. match t { Node(x)(children) => size[Forest[a]](children) }
        |}
        |""".stripMargin
    assertError(source + main) { case error: CompileError.NoInstance if error.message.contains("Size[Forest[a]]") => }
  }

  test("rec { impl }: 文脈のないインスタンスで組のインスタンスを使うメソッドは関数に限る") {
    val source =
      """trait Default[a] { def default: a }
        |data A = { MkA(i32) }
        |data B = { MkB(i32) }
        |rec {
        |  impl Default[A] { def default = match default[B] { MkB(n) => MkA(n) } }
        |  impl Default[B] { def default = MkB(1) }
        |}
        |""".stripMargin
    assertError(source + main) { case CompileError.RecursiveInstanceMethodNotFunction("default", _) => }
  }

  test("rec { impl }: 同じインスタンスを二度定義できない") {
    val source = sizeTrait +
      """rec {
        |  impl Size[i32] { def size = λx: i32. 1 }
        |  impl Size[i32] { def size = λx: i32. 2 }
        |}
        |""".stripMargin
    assertError(source + main) { case error: CompileError.OverlappingInstance if error.message.contains("Size[i32]") => }
  }

  test("rec { data } in: 式の中でも相互再帰のデータ型を定義できる") {
    val source =
      """let depth: i32 =
        |  rec {
        |    data Tree = { Node(Forest) }
        |    data Forest = { Empty, Trees(Tree)(Forest) }
        |  } in match Node(Trees(Node(Empty))(Empty)) {
        |    Node(children) => match children {
        |      Empty => 0
        |      Trees(t)(rest) => 1
        |    }
        |  }
        |let main(): i32 = depth
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 1)
    assertError("let x: i32 = rec { data A = { MkA(A) }; data A = { MkB(A) } } in 0\n" + main) { case CompileError.AlreadyDefined("A", _) => }
  }

  test("context ブロックの中でも rec { let } で相互再帰できる") {
    val source = arithmetic + maybeMonad +
      """let run(m: Maybe[i32]): Maybe[bool] = context[Maybe] {
        |  x: i32 = m;
        |  rec {
        |    let isEven(n: i32): bool = if n == 0 then true else isOdd(n - 1)
        |    let isOdd(n: i32): bool = if n == 0 then false else isEven(n - 1)
        |  };
        |  isOdd(x)
        |}
        |let main(): i32 = match run(Just(7)) {
        |  Just(odd) => if odd then 1 else 0
        |  Nothing => 2
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 1)
    val notFunction = maybeMonad +
      """let run(m: Maybe[i32]): Maybe[i32] = context[Maybe] {
        |  x: i32 = m;
        |  rec {
        |    let f(n: i32): i32 = g
        |    let g: i32 = f(1)
        |  };
        |  f(x)
        |}
        |""".stripMargin
    assertNotFunction(notFunction + main)
  }

  private val functor = "trait Functor[f: * → *] { def map[a][b](x: f[a])(g: a → b): f[b] }\n"

  private val sumTree =
    """rec {
      |  let sum(t: Tree[i32]): i32 = match t { Node(x)(children) => x + sumAll(children) }
      |  let sumAll(f: Forest[i32]): i32 = match f {
      |    Empty => 0
      |    Trees(t)(rest) => sum(t) + sumAll(rest)
      |  }
      |}
      |""".stripMargin

  test("rec { derive }: 相互再帰のデータ型にまとめて derive できる") {
    val source = arithmetic + functor + forest + sampleTree + sumTree +
      """rec {
        |  derive Functor[Tree]
        |  derive Functor[Forest]
        |}
        |let main(): i32 = sum(map[Tree][i32][i32](sample)(λn: i32. n + 10))
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 36)
  }

  test("rec { derive / impl }: derive と手書きの impl を同じ組にできる") {
    val source = arithmetic + functor + forest + sampleTree + sumTree +
      """rec {
        |  derive Functor[Tree]
        |  impl Functor[Forest] {
        |    def map = Λa. Λb. λf: Forest[a]. λg: a → b. match f {
        |      Empty => Empty[b]
        |      Trees(t)(rest) => Trees[b](map[Tree][a][b](t)(g))(map[Forest][a][b](rest)(g))
        |    }
        |  }
        |}
        |let main(): i32 = sum(map[Tree][i32][i32](sample)(λn: i32. n + 10))
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 36)
  }

  test("rec の外では、相互再帰のデータ型に derive できない") {
    assertError(functor + forest + "derive Functor[Tree]\n" + main) {
      case CompileError.DeriveNeeds("Functor", "Tree", goal, _) if Type.show(goal) == "Functor[Forest]" =>
    }
  }

  test("rec { derive }: 同じ derive を二度書けない") {
    val source = functor + "rec data L[a] = { N, C(a)(L[a]) }\nrec { derive Functor[L]; derive Functor[L] }\n"
    assertError(source + main) { case error: CompileError.OverlappingInstance if error.message.contains("Functor[L]") => }
  }

  private val hierarchy =
    """trait Base[f: * → *] { def base[a](x: a): f[a] }
      |trait Derived[f: * → *] with Base[f] { def derived[a](x: f[a]): f[a] }
      |data Box[a] = { MkBox(a) }
      |""".stripMargin

  test("rec { impl }: スーパークラスのインスタンスは、組の中で先に宣言したものを使える") {
    val source = hierarchy +
      """rec {
        |  impl Base[Box] { def base[a](x: a): Box[a] = derived[Box][a](MkBox[a](x)) }
        |  impl Derived[Box] { def derived[a](x: Box[a]): Box[a] = x }
        |}
        |let main(): i32 = match base[Box][i32](4) { MkBox(n) => n }
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 4)
  }

  test("rec { impl }: 文脈のないインスタンスは、後で宣言したものをスーパークラスにできない") {
    val source = hierarchy +
      """rec {
        |  impl Derived[Box] { def derived[a](x: Box[a]): Box[a] = x }
        |  impl Base[Box] { def base[a](x: a): Box[a] = derived[Box][a](MkBox[a](x)) }
        |}
        |""".stripMargin
    assertError(source + main) { case error: CompileError.NoInstance if error.message.contains("Base[Box]") => }
  }

  test("rec { impl }: 文脈のあるインスタンスは、組のどれでもスーパークラスにできる") {
    val source = hierarchy +
      """data Wrap[f: * → *][a] = { MkWrap(f[a]) }
        |impl Base[Box] { def base[a](x: a): Box[a] = MkBox[a](x) }
        |impl Derived[Box] { def derived[a](x: Box[a]): Box[a] = x }
        |rec {
        |  impl[f: * → *] Derived[Wrap[f]] with Derived[f] { def derived[a](x: Wrap[f][a]): Wrap[f][a] = x }
        |  impl[f: * → *] Base[Wrap[f]] with Derived[f] { def base[a](x: a): Wrap[f][a] = MkWrap[f][a](base[f][a](x)) }
        |}
        |let main(): i32 = match derived[Wrap[Box]][i32](base[Wrap[Box]][i32](4)) { MkWrap(MkBox(n)) => n }
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 4)
  }

  test("rec { derive }: スーパークラスのインスタンスは、組の中で先に宣言する") {
    val traits = functor + "trait Mappable[f: * → *] with Functor[f] { def same[a](x: f[a]): f[a] }\n"
    val accepted = arithmetic + traits + forest + sampleTree + sumTree +
      """rec {
        |  derive Functor[Tree]
        |  derive Functor[Forest]
        |  impl Mappable[Tree] { def same[a](x: Tree[a]): Tree[a] = map[Tree][a][a](x)(λv: a. v) }
        |}
        |let main(): i32 = sum(same[Tree][i32](sample))
        |""".stripMargin
    assertEquals(Lam.exitCode(accepted), 6)
    val rejected = traits + forest +
      """rec {
        |  impl Mappable[Tree] { def same[a](x: Tree[a]): Tree[a] = map[Tree][a][a](x)(λv: a. v) }
        |  derive Functor[Tree]
        |  derive Functor[Forest]
        |}
        |""".stripMargin
    assertError(rejected + main) { case error: CompileError.NoInstance if error.message.contains("Functor[Tree]") => }
  }

  test("rec { }: 種類の違う宣言は混ぜられない") {
    assertError("rec { let f(x: i32): i32 = x; data A = { MkA } }\n" + main) { case _: CompileError.MixedRecursiveGroup => }
  }

  test("rec の後に書けるのは let / data / impl だけ") {
    assertError("rec type T = i32\n" + main) {
      case _: CompileError.Syntax =>
      case _: CompileError.MixedRecursiveGroup =>
    }
    assertSyntaxError("rec { }\n" + main)
  }

  test("古い書き方（let rec / data rec / impl rec）は構文エラー") {
    assertSyntaxError("let rec f(x: i32): i32 = f(x)\n" + main)
    assertSyntaxError("data rec L = { N, C(L) }\n" + main)
  }

  test("module の中の rec { } と、private / opaque との組み合わせ") {
    val source =
      """module M {
        |  opaque rec {
        |    data Tree = { Node(Forest) }
        |    data Forest = { Empty, Trees(Tree)(Forest) }
        |  }
        |  private rec {
        |    let f(stop: bool): i32 = if stop then 3 else g(true)
        |    let g(stop: bool): i32 = f(stop)
        |  }
        |  let empty: Forest = Empty
        |  let run(stop: bool): i32 = f(stop)
        |}
        |let e: M.Forest = M.empty
        |""".stripMargin
    assertEquals(Lam.exitCode(source + "let main(): i32 = M.run(false)\n"), 3)
    assertError(source + "let bad: M.Forest = M.Empty\n" + main) {
      case CompileError.PrivateName(name, _, _) if name.contains("Empty") =>
      case CompileError.UndefinedVariable(name, _) if name.contains("Empty") =>
    }
    assertError(source + "let bad: i32 = M.f(true)\n" + main) {
      case CompileError.PrivateName(name, _, _) if name.contains("f") =>
      case CompileError.UndefinedVariable(name, _) if name.contains("f") =>
    }
  }
}
