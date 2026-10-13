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

  test("rec let: 関数は自身を参照できる") {
    assertRight(validate("rec let f(x: i32): i32 = f(x)\nlet main(): i32 = 0\n"))
  }

  test("rec let: 型パラメータ付きの関数も関数として扱う") {
    assertRight(validate("rec let f[a](x: a): a = f[a](x)\nlet main(): i32 = 0\n"))
  }

  test("rec let: 関数でない値が自身を参照するのは拒否") {
    assertLeftContains(validate("rec let x: i32 = x\nlet main(): i32 = 0\n"), notFunction)
  }

  test("rec let: 関数の適用結果として自身を参照するのも拒否") {
    val src =
      """let apply(f: unit → i32): unit → i32 = f
        |rec let g: unit → i32 = apply(λu: unit. g(u))
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), notFunction)
  }

  test("rec let: 自身を参照しない値は関数でなくてもよい") {
    assertRight(validate("rec let x: i32 = 1\nlet main(): i32 = x\n"))
  }

  test("rec let: in 式でも拒否") {
    assertLeftContains(validate("let main(): i32 = rec let x: i32 = x in x\n"), notFunction)
  }

  test("rec let: context ブロックの rec let でも拒否") {
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
        |  rec let x: i32 = x;
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

  test("rec impl: 関数でないメソッドが自身のインスタンスを使うのは拒否") {
    val src = defaultTrait +
      """rec impl Default[i32] {
        |  def default = default[i32]
        |  def orDefault = λx: i32. x
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "method default uses its own instance")
  }

  test("rec impl: 関数のメソッドからは自身のインスタンスを使える") {
    val src = defaultTrait +
      """rec impl Default[i32] {
        |  def default = 0
        |  def orDefault = λx: i32. default[i32]
        |}
        |let main(): i32 = orDefault[i32](1)
        |""".stripMargin
    assertRight(validate(src))
  }

  test("rec impl: 文脈のあるインスタンスは辞書自体が関数なので、値のメソッドでもよい") {
    val src = defaultTrait +
      """data Box[a] = { MkBox(a) }
        |rec impl[a] Default[Box[a]] with Default[a] {
        |  def default = orDefault[Box[a]](MkBox[a](default[a]))
        |  def orDefault = λx: Box[a]. x
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertRight(validate(src))
  }

  // ---- rec { … }: 相互再帰 ----

  private def encode(src: String): Either[String, TypeRec[AST.Program.type]] =
    validate(src).map(p => ChurchEncoder.encode(TraitEncoder.encode(ContextDesugar.desugar(Deriver.desugar(ModuleDesugar.desugar(p))))))

  private val main = "let main(): i32 = 0\n"

  test("rec { let }: 組のメンバーは互いに参照できる") {
    val src =
      """rec {
        |  let f(x: i32): i32 = g(x)
        |  let g(x: i32): i32 = f(x)
        |}
        |let h(x: i32): i32 = f(g(x))
        |""".stripMargin
    assertRight(encode(src + main))
  }

  test("rec { let }: 区切りは改行でもセミコロンでもよい") {
    assertRight(encode("rec { let f(x: i32): i32 = g(x); let g(x: i32): i32 = f(x) }\n" + main))
  }

  test("rec let は宣言 1 つの rec { } と同じ") {
    def shown(src: String): String = ParserAST.programParser.parseAll(src).fold(e => fail(e.toString), p => eraseIndex(p).show)
    assertEquals(shown("rec let f(x: i32): i32 = f(x)"), shown("rec { let f(x: i32): i32 = f(x) }"))
    assertEquals(shown("rec data L = { N, C(L) }"), shown("rec { data L = { N, C(L) } }"))
  }

  test("rec でなければ後ろの宣言は見えない") {
    val src =
      """let f(x: i32): i32 = g(x)
        |let g(x: i32): i32 = x
        |""".stripMargin
    assertLeftContains(validate(src + main), "Variable g is not defined")
  }

  test("rec { let }: 関数でない値は組のメンバーを参照できない") {
    val src =
      """rec {
        |  let f(x: i32): i32 = x
        |  let y: i32 = f(1)
        |}
        |""".stripMargin
    assertLeftContains(validate(src + main), notFunction)
  }

  test("rec { let }: 組のメンバーを参照しない値は関数でなくてもよい") {
    val src =
      """rec {
        |  let base: i32 = 1
        |  let f(x: i32): i32 = if true then base else f(x)
        |}
        |""".stripMargin
    assertRight(encode(src + main))
  }

  test("rec { let }: 同じ名前を二度定義できない") {
    assertLeftContains(validate("rec { let f(x: i32): i32 = x; let f(x: i32): i32 = x }\n" + main), "Variable f is already defined")
  }

  test("rec { let } in: 式の中でも相互再帰できる") {
    val src =
      """let h(n: i32): i32 =
        |  rec {
        |    let f(x: i32): i32 = g(x)
        |    let g(x: i32): i32 = f(x)
        |  } in f(n)
        |""".stripMargin
    assertRight(encode(src + main))
    assertLeftContains(validate("let h: i32 = rec { let f(x: i32): i32 = x; let y: i32 = f(1) } in y\n" + main), notFunction)
  }

  private val forest =
    """rec {
      |  data Tree[a] = { Node(a)(Forest[a]) }
      |  data Forest[a] = { Empty, Trees(Tree[a])(Forest[a]) }
      |}
      |""".stripMargin

  test("rec { data }: 相互再帰のデータ型を定義して match できる") {
    val src = forest +
      """rec {
        |  let depth[a](t: Tree[a]): i32 = match t { Node(x)(children) => deepest[a](children) }
        |  let deepest[a](f: Forest[a]): i32 = match f {
        |    Empty => 0
        |    Trees(t)(rest) => depth[a](t)
        |  }
        |}
        |let leaf: Tree[i32] = Node[i32](1)(Empty[i32])
        |""".stripMargin
    assertRight(encode(src + main))
  }

  test("rec { data }: 組のデータ型は同じ組として記録される") {
    val env = validate(forest + main).fold(fail(_), _.extract match { case ProgramAnn(e) => e })
    val group = Set(TypeVariable("Tree"), TypeVariable("Forest"))
    assertEquals(env.dataTypes(TypeVariable("Tree")).group, group)
    assertEquals(env.dataTypes(TypeVariable("Forest")).group, group)
  }

  test("rec でなければ後ろのデータ型は見えない") {
    val src =
      """data Tree = { Node(Forest) }
        |data Forest = { Empty, Trees(Tree)(Forest) }
        |""".stripMargin
    assertLeftContains(validate(src + main), "Type variable Forest is not defined")
  }

  test("rec { data }: 型名・構成子名の重複は拒否") {
    assertLeftContains(validate("rec { data A = { MkA(A) }; data A = { MkB(A) } }\n" + main), "already defined")
    assertLeftContains(validate("rec { data A = { Mk(B) }; data B = { Mk(A) } }\n" + main), "already defined")
  }

  test("rec { data }: fold は自身の型の出現だけを畳み込む") {
    val src = forest +
      """let count[a](f: Forest[a]): i32 = (fold f as i32 with
        |  Empty => 0
        |  Trees(t)(rest) => rest)
        |""".stripMargin
    assertRight(encode(src + main))
  }

  test("rec { impl }: 組のインスタンスは互いのメソッド本体から見える") {
    val src = forest +
      """trait Size[a] { def size(x: a): i32 }
        |rec {
        |  impl[a] Size[Tree[a]] {
        |    def size = λt: Tree[a]. match t { Node(x)(children) => size[Forest[a]](children) }
        |  }
        |  impl[a] Size[Forest[a]] {
        |    def size = λf: Forest[a]. match f {
        |      Empty => 0
        |      Trees(t)(rest) => size[Tree[a]](t)
        |    }
        |  }
        |}
        |""".stripMargin
    val shown = encode(src + main).fold(fail(_), _.show)
    assert(shown.contains("rec {"), clue(shown))
  }

  test("rec でなければ後ろのインスタンスは見えない") {
    val src = forest +
      """trait Size[a] { def size(x: a): i32 }
        |impl[a] Size[Tree[a]] {
        |  def size = λt: Tree[a]. match t { Node(x)(children) => size[Forest[a]](children) }
        |}
        |""".stripMargin
    assertLeftContains(validate(src + main), "No instance")
  }

  test("rec { impl }: 文脈のないインスタンスで組のインスタンスを使うメソッドは関数に限る") {
    val src =
      """trait Default[a] { def default: a }
        |data A = { MkA(i32) }
        |data B = { MkB(i32) }
        |rec {
        |  impl Default[A] { def default = match default[B] { MkB(n) => MkA(n) } }
        |  impl Default[B] { def default = MkB(1) }
        |}
        |""".stripMargin
    assertLeftContains(validate(src + main), notFunction)
  }

  test("rec { impl }: 同じインスタンスを二度定義できない") {
    val src =
      """trait Size[a] { def size(x: a): i32 }
        |rec {
        |  impl Size[i32] { def size = λx: i32. 1 }
        |  impl Size[i32] { def size = λx: i32. 2 }
        |}
        |""".stripMargin
    assertLeftContains(validate(src + main), "Size")
  }

  test("rec { data } in: 式の中でも相互再帰のデータ型を定義できる") {
    val src =
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
        |""".stripMargin
    assertRight(encode(src + main))
    assertLeftContains(validate("let x: i32 = rec { data A = { MkA(A) }; data A = { MkB(A) } } in 0\n" + main), "already defined")
  }

  test("context ブロックの中でも rec { let } で相互再帰できる") {
    val src =
      """data Maybe[a] = { Nothing, Just(a) }
        |trait Functor[f: * → *] { def map[a][b](g: a → b)(x: f[a]): f[b] }
        |trait Applicative[f: * → *] with Functor[f] { def pure[a](x: a): f[a] }
        |trait Monad[m: * → *] with Applicative[m] { def flatMap[a][b](x: m[a])(g: a → m[b]): m[b] }
        |impl Functor[Maybe] { def map = Λa. Λb. λg: a → b. λx: Maybe[a]. match x { Nothing => Nothing[b] Just(v) => Just[b](g(v)) } }
        |impl Applicative[Maybe] { def pure = Λa. λx: a. Just[a](x) }
        |impl Monad[Maybe] { def flatMap = Λa. Λb. λx: Maybe[a]. λg: a → Maybe[b]. match x { Nothing => Nothing[b] Just(v) => g(v) } }
        |let run(m: Maybe[i32]): Maybe[i32] = context[Maybe] {
        |  x: i32 = m;
        |  rec {
        |    let f(n: i32): i32 = g(n)
        |    let g(n: i32): i32 = f(n)
        |  };
        |  f(x)
        |}
        |""".stripMargin
    assertRight(encode(src + main))
    assertLeftContains(
      validate(src.replace("let g(n: i32): i32 = f(n)", "let g: i32 = f(1)").replace("g(n)", "g") + main),
      notFunction
    )
  }

  private val functor = "trait Functor[f: * → *] { def map[a][b](g: a → b)(x: f[a]): f[b] }\n"

  test("rec { derive }: 相互再帰のデータ型にまとめて derive できる") {
    val src = functor + forest +
      """rec {
        |  derive Functor[Tree]
        |  derive Functor[Forest]
        |}
        |let mapped: Tree[bool] = map[Tree][i32][bool](λn: i32. true)(Node[i32](1)(Empty[i32]))
        |""".stripMargin
    val shown = encode(src + main).fold(fail(_), _.show)
    assert(shown.contains("$inst_Functor_Tree"), clue(shown))
    assert(shown.contains("$inst_Functor_Forest"), clue(shown))
  }

  test("rec { derive / impl }: derive と手書きの impl を同じ組にできる") {
    val src = functor + forest +
      """rec {
        |  derive Functor[Tree]
        |  impl Functor[Forest] {
        |    def map = Λa. Λb. λg: a → b. λf: Forest[a]. match f {
        |      Empty => Empty[b]
        |      Trees(t)(rest) => Trees[b](map[Tree][a][b](g)(t))(map[Forest][a][b](g)(rest))
        |    }
        |  }
        |}
        |""".stripMargin
    assertRight(encode(src + main))
  }

  test("rec の外では、相互再帰のデータ型に derive できない") {
    assertLeftContains(validate(functor + forest + "derive Functor[Tree]\n" + main), "Functor")
  }

  test("rec { derive }: 同じ derive を二度書けない") {
    val src = functor + "rec data L[a] = { N, C(a)(L[a]) }\nrec { derive Functor[L]; derive Functor[L] }\n"
    assertLeftContains(validate(src + main), "Functor")
  }

  test("rec { }: 種類の違う宣言は混ぜられない") {
    assertLeftContains(validate("rec { let f(x: i32): i32 = x; data A = { MkA } }\n" + main), "one kind")
  }

  test("rec の後に書けるのは let / data / impl だけ") {
    assert(ParserAST.programParser.parseAll("rec type T = i32\n" + main).isLeft)
    assert(ParserAST.programParser.parseAll("rec { }\n" + main).isLeft)
  }

  test("古い書き方（let rec / data rec / impl rec）は構文エラー") {
    assert(ParserAST.programParser.parseAll("let rec f(x: i32): i32 = f(x)\n" + main).isLeft)
    assert(ParserAST.programParser.parseAll("data rec L = { N, C(L) }\n" + main).isLeft)
  }

  test("module の中の rec { } と、private / opaque との組み合わせ") {
    val src =
      """module M {
        |  opaque rec {
        |    data Tree = { Node(Forest) }
        |    data Forest = { Empty, Trees(Tree)(Forest) }
        |  }
        |  private rec {
        |    let f(x: i32): i32 = g(x)
        |    let g(x: i32): i32 = f(x)
        |  }
        |  let empty: Forest = Empty
        |  let run(x: i32): i32 = f(x)
        |}
        |let e: M.Forest = M.empty
        |""".stripMargin
    assertRight(encode(src + main))
    assertLeftContains(validate(src + "let bad: M.Forest = M.Empty\n" + main), "Variable M.Empty is not defined")
    assertLeftContains(validate(src + "let bad: i32 = M.f(1)\n" + main), "Variable M.f is not defined")
  }
}
