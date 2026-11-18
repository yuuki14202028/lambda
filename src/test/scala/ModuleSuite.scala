package com.yuuki14202028

/** module: 名前空間・open・private・opaque・module の中の import */
class ModuleSuite extends munit.FunSuite {

  private val add = "trait Add[a] { def add(x: a)(y: a): a }; infixl 6 + = add; impl Add[i32] { def add(a: i32)(b: i32): i32 = intrinsic[add_i32](a)(b) }\n"

  private val counter = add +
    """module Counter {
      |  opaque type T = i32
      |  private let step: i32 = 1
      |  let zero: T = 0
      |  let inc(c: T): T = c + step
      |  let get(c: T): i32 = c
      |}
      |""".stripMargin

  private def withMain(source: String): String = source + "\nlet main(): i32 = 0\n"

  private def accepted(source: String)(using munit.Location): Unit = assertEquals(Lam.exitCode(source), 0)

  /** expected に当たるエラーが報告される */
  private def assertError(source: String)(expected: PartialFunction[CompileError, Any])(using munit.Location): Unit = {
    val found = Lam.errors(source)
    assert(found.exists(expected.isDefinedAt), clue(found))
  }

  private def assertMismatch(source: String)(using munit.Location): Unit =
    assertError(source) { case _: CompileError.TypeMismatch => }

  // ---- 名前空間 ----

  test("module のメンバーは修飾して参照する") {
    val source = counter + "let two: Counter.T = Counter.inc(Counter.inc(Counter.zero))\nlet main(): i32 = Counter.get(two)\n"
    assertEquals(Lam.exitCode(source), 2)
  }

  test("module のメンバーは修飾なしでは外から見えない") {
    assertError(withMain(counter + "let z: Counter.T = zero")) { case CompileError.UndefinedVariable("zero", _) => }
    assertError(withMain(counter + "let z: T = Counter.zero")) { case CompileError.UndefinedType("T", _) => }
  }

  test("同じ名前を別の module で定義できる") {
    val source =
      """module A { let x: i32 = 1 }
        |module B { let x: bool = true }
        |let a: i32 = A.x
        |let b: bool = B.x
        |let main(): i32 = if b then a else 0
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 1)
  }

  test("入れ子の module は外側の名前が見え、外からは経路で参照する") {
    val source =
      """let base: i32 = 1
        |module A {
        |  let x: i32 = base
        |  module B {
        |    data Flag = { On, Off }
        |    let y: i32 = x
        |  }
        |  let flag: B.Flag = B.On
        |}
        |let z: i32 = A.B.y
        |let f(flag: A.B.Flag): i32 = match flag {
        |  A.B.On => 1
        |  A.B.Off => 0
        |}
        |let main(): i32 = if intrinsic[eq_i32](z)(1) then f(A.flag) else 5
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 1)
  }

  test("module の中の定義は、外側の同名の定義を隠す") {
    val source =
      """let x: i32 = 1
        |module M {
        |  let x: bool = true
        |  let y: bool = x
        |}
        |let z: i32 = x
        |let main(): i32 = if M.y then z else 0
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 1)
  }

  test("let（rec なし）の値からは、定義中の名前ではなく外側の同名の定義が見える") {
    val source = add +
      """let x: i32 = 1
        |module M {
        |  let x: i32 = x + 1
        |}
        |let main(): i32 = M.x
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 2)
  }

  test("局所的な束縛は module のメンバーを隠す") {
    val source =
      """module M {
        |  let x: i32 = 1
        |  let f(x: bool): bool = x
        |  let g[T](y: T): T = y
        |}
        |let main(): i32 = if M.f(true) then M.g[i32](3) else 0
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 3)
  }

  test("式の中の type / data も module のメンバーを隠す") {
    val source =
      """module M {
        |  type T = bool
        |  data D = { A }
        |  let x: i32 = type T = i32 in data D = { A(T) } in match A(1) { A(n) => n }
        |  let y: T = true
        |  let z: D = A
        |}
        |let main(): i32 = if M.y then M.x else 0
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 1)
  }

  test("module の中の構文エラーは、その位置で報告される") {
    val source = "module M {\n  let x: i32 = 1\n  let y i32 = 2\n}\nlet main(): i32 = 0\n"
    Lam.errors(source) match {
      case List(CompileError.Syntax(_, _, span)) => assertEquals(source.take(span.start).count(_ == '\n'), 2)
      case other => fail(s"expected a syntax error: $other")
    }
  }

  test("同じ module はもう一度開ける（メンバーを足す）") {
    val source =
      """module M {
        |  let x: i32 = 1
        |  private let hidden: i32 = 2
        |}
        |module M {
        |  let y: i32 = x
        |  let z: i32 = hidden
        |}
        |let a: i32 = M.x
        |let b: i32 = M.y
        |""".stripMargin
    assertEquals(Lam.exitCode(source + "let main(): i32 = M.z\n"), 2)
    assertError(withMain(source + "let c: i32 = M.hidden")) { case CompileError.PrivateName("M.hidden", _, _) => }
  }

  test("未定義の module・メンバー") {
    assertError(withMain("let x: i32 = M.x")) { case CompileError.UndefinedVariable("M", _) => }
    assertError(withMain("module M { }\nlet x: i32 = M.x")) { case CompileError.UndefinedVariable("M.x", _) => }
    assertError(withMain("let x: M.T = 0")) { case CompileError.UndefinedType("M.T", _) => }
  }

  // ---- open ----

  test("open した module のメンバーは修飾なしで書ける") {
    val source = counter +
      """open Counter
        |let two: T = inc(inc(zero))
        |let n: i32 = get(two)
        |let main(): i32 = n
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 2)
  }

  test("open しても private なメンバーは見えない") {
    assertError(withMain(counter + "open Counter\nlet s: i32 = step")) { case CompileError.UndefinedVariable("step", _) => }
  }

  test("open は入れ子の module も経路で指定できる") {
    val source =
      """module A { module B { let y: i32 = 1 } }
        |open A.B
        |let z: i32 = y
        |let main(): i32 = z
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 1)
  }

  test("open の後の定義は、open した名前を隠す") {
    val source =
      """module M { let x: i32 = 1 }
        |open M
        |let x: bool = true
        |let y: bool = x
        |let main(): i32 = if y then 1 else 0
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 1)
  }

  test("未定義の module は open できない") {
    assertError(withMain("open M")) { case CompileError.UndefinedModule("M", _) => }
    assertError(withMain("module M { }\nopen M.N")) { case CompileError.UndefinedModule(_, _) => }
  }

  // ---- private ----

  test("private な宣言は module の外から見えない") {
    assertError(withMain(counter + "let s: i32 = Counter.step")) { case CompileError.PrivateName("Counter.step", _, _) => }
  }

  test("private な型・module も外から見えない") {
    val source =
      """module M {
        |  private type Hidden = i32
        |  private module Inner { let x: i32 = 1 }
        |  let y: Hidden = Inner.x
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(source + "let main(): i32 = M.y\n"), 1)
    assertError(withMain(source + "let z: M.Hidden = 0")) { case CompileError.PrivateName("M.Hidden", _, _) => }
    assertError(withMain(source + "let z: i32 = M.Inner.x")) { case CompileError.PrivateName(_, _, _) | CompileError.UndefinedVariable("M.Inner", _) => }
  }

  test("private / opaque は module の中でしか使えない") {
    assert(Lam.errors(withMain("private let x: i32 = 1")).nonEmpty)
    assert(Lam.errors(withMain("opaque type T = i32")).nonEmpty)
  }

  // ---- opaque type ----

  test("opaque type は module の中では表現と同じ型") {
    accepted(withMain(counter))
  }

  test("opaque type は module の外では表現と別の型") {
    assertMismatch(withMain(counter + "let n: i32 = Counter.zero"))
    assertMismatch(withMain(counter + "let c: Counter.T = 0"))
    assertMismatch(withMain(counter + "let n: Counter.T = Counter.inc(1)"))
    assertError(withMain(counter + "let n: Counter.T = Counter.zero + Counter.zero")) {
      case error: CompileError.NoInstance if error.message.contains("Add[Counter.T]") =>
    }
  }

  test("封印の後、公開メンバーの型は抽象型のまま見える") {
    accepted(withMain(counter + "let f: Counter.T → Counter.T = Counter.inc\nlet g: Counter.T → i32 = Counter.get"))
    assertMismatch(withMain(counter + "let f: i32 → i32 = Counter.inc"))
    assertMismatch(withMain(counter + "let g: i32 → i32 = Counter.get"))
  }

  test("opaque type を使う別名・データ型・trait も、外からは抽象型のまま見える") {
    val source = counter.replace("  let get(c: T): i32 = c\n", "") +
      """module Wrap {
        |  opaque type T = i32
        |  type Two = T → T
        |  data Box = { MkBox(T) }
        |  trait Make[a] { def make(x: a): T }
        |  let unbox(b: Box): T = match b { MkBox(x) => x }
        |}
        |""".stripMargin
    accepted(withMain(source))
    assertMismatch(withMain(source + "let f: Wrap.Two = λx: i32. x"))
    assertMismatch(withMain(source + "let b: Wrap.Box = Wrap.MkBox(1)"))
    assertError(withMain(source + "impl Wrap.Make[i32] { def make = λx: i32. x }")) {
      case error: CompileError.MethodTypeMismatch if error.message.contains("Wrap.T") =>
    }
  }

  test("入れ子の module は、外側の module を閉じたときにまとめて封印される") {
    val source =
      """module M {
        |  opaque type T = i32
        |  module N {
        |    let f(x: T): i32 = x
        |  }
        |  let one: T = 1
        |  let inside: i32 = N.f(1)
        |}
        |let ok: i32 = M.N.f(M.one)
        |""".stripMargin
    assertEquals(Lam.exitCode(source + "let main(): i32 = ok\n"), 1)
    assertMismatch(withMain(source + "let bad: i32 = M.N.f(1)"))
  }

  test("型パラメータを持つ opaque type") {
    val source =
      """module M {
        |  opaque type Wrapped[a] = a → a
        |  let wrap[a](f: a → a): Wrapped[a] = f
        |  let run[a](w: Wrapped[a])(x: a): a = w(x)
        |}
        |let w: M.Wrapped[i32] = M.wrap[i32](λx: i32. x)
        |let n: i32 = M.run[i32](w)(1)
        |""".stripMargin
    assertEquals(Lam.exitCode(source + "let main(): i32 = n\n"), 1)
    assertError(withMain(source + "let m: i32 = w(1)")) { case _: CompileError.NotAFunction => }
  }

  test("抽象型へのインスタンスは module の外で定義でき、表現の型のインスタンスと区別される") {
    val source = counter +
      """trait Weight[a] { def weight(x: a): i32 }
        |impl Weight[i32] { def weight = λx: i32. x }
        |impl Weight[Counter.T] { def weight = λc: Counter.T. Counter.get(c) + 10 }
        |let a: i32 = weight[i32](1)
        |let b: i32 = weight[Counter.T](Counter.zero)
        |let main(): i32 = a + b
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 11)
  }

  // ---- opaque data ----

  private val stack =
    """module Stack {
      |  opaque rec data Stack[a] = { Empty, Push(a)(Stack[a]) }
      |  let empty[a]: Stack[a] = Empty[a]
      |  let push[a](x: a)(s: Stack[a]): Stack[a] = Push[a](x)(s)
      |  let isEmpty[a](s: Stack[a]): bool = match s {
      |    Empty => true
      |    Push(x)(rest) => false
      |  }
      |}
      |""".stripMargin

  test("opaque data は型の名前だけを公開する") {
    val source = stack +
      """let s: Stack.Stack[i32] = Stack.push[i32](1)(Stack.empty[i32])
        |let main(): i32 = if Stack.isEmpty(s) then 0 else 1
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 1)
  }

  test("opaque data の構成子は module の外から使えない") {
    assertError(withMain(stack + "let s: Stack.Stack[i32] = Stack.Empty[i32]")) {
      case error @ (_: CompileError.PrivateName | _: CompileError.UndefinedVariable) if error.message.contains("Stack.Empty") =>
    }
    assertError(withMain(stack + "open Stack\nlet s: Stack[i32] = Empty[i32]")) { case CompileError.UndefinedVariable("Empty", _) => }
    val matching =
      """let f(s: Stack.Stack[i32]): i32 = match s {
        |  Stack.Empty => 0
        |  Stack.Push(x)(rest) => 1
        |}
        |""".stripMargin
    val found = Lam.errors(withMain(stack + matching))
    assert(found.exists(_.message.contains("Stack.Empty")), clue(found))
  }

  // ---- trait と module ----

  test("module の中の trait は外から修飾して実装・使用できる") {
    val source =
      """module M {
        |  trait Weight[a] { def weight(x: a): i32 }
        |  impl Weight[i32] { def weight = λx: i32. x }
        |  let twice[a](x: a): i32 with Weight[a] = weight[a](x)
        |}
        |impl M.Weight[bool] { def weight = λb: bool. 1 }
        |let a: i32 = M.weight[bool](true)
        |let b: i32 = M.twice[i32](1)
        |let c[a](x: a): i32 with M.Weight[a] = M.weight[a](x)
        |let d: M.Weight[i32] = M.MkWeight[i32](λx: i32. 0)
        |let main(): i32 = intrinsic[add_i32](intrinsic[add_i32](a)(b))(c[i32](5))
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 7)
  }

  test("インスタンスは module の中で定義しても全体で有効") {
    val source =
      """trait Weight[a] { def weight(x: a): i32 }
        |module M {
        |  data Box = { MkBox(i32) }
        |  impl Weight[Box] { def weight = λb: Box. 1 }
        |}
        |let a: i32 = weight[M.Box](M.MkBox(1))
        |""".stripMargin
    assertEquals(Lam.exitCode(source + "let main(): i32 = a\n"), 1)
    assertError(withMain(source + "impl Weight[M.Box] { def weight = λb: M.Box. 2 }")) {
      case error: CompileError.OverlappingInstance if error.message.contains("Weight") =>
    }
  }

  test("型構成子の別名（本体のカインドが * でない別名）") {
    val source =
      """data Either[e][a] = { Left(e), Right(a) }
        |trait Functor[f: * → *] { def map[a][b](x: f[a])(g: a → b): f[b] }
        |impl[e] Functor[Either[e]] { def map[a][b](x: Either[e][a])(g: a → b): Either[e][b] = match x { Left(err) => Left[e][b](err)  Right(v) => Right[e][b](g(v)) } }
        |type Result = Either[bool]
        |let ok: Result[i32] = Right[bool][i32](1)
        |let mapped: Result[i32] = map[Result][i32][i32](ok)(λn: i32. intrinsic[add_i32](n)(1))
        |""".stripMargin
    val main =
      """let main(): i32 = match mapped {
        |  Left(_) => 0
        |  Right(n) => n
        |}
        |""".stripMargin
    assertEquals(Lam.exitCode(source + main), 2)
    assertError(withMain(source + "let bad: Result = ok")) { case _: CompileError.KindMismatch => }
  }

  // ---- 検査中の module を修飾子にした参照 ----

  test("module の中から、自身を修飾子にして参照できる（private も含む）") {
    val source =
      """module M {
        |  type T = i32
        |  private let hidden: M.T = 1
        |  let shown: T = M.hidden
        |  module N {
        |    let inner: M.T = M.shown
        |    let again: i32 = M.N.inner
        |    let short: i32 = N.inner
        |  }
        |  private module P { let secret: i32 = 2 }
        |  let viaPrivate: i32 = M.P.secret
        |}
        |let main(): i32 = intrinsic[add_i32](M.N.again)(M.viaPrivate)
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 3)
  }

  test("自身を修飾子にしても、まだ定義していない名前は参照できない") {
    assertError(withMain("module M {\n  let a: i32 = M.b\n  let b: i32 = 1\n}")) { case CompileError.UndefinedVariable("M.b", _) => }
  }

  test("外からは、private な名前は修飾しても参照できない（従来どおり）") {
    assertError(withMain("module M { private let hidden: i32 = 1 }\nlet x: i32 = M.hidden")) { case CompileError.PrivateName("M.hidden", _, _) => }
  }

  // ---- module の中の import ----

  private def withFiles(files: (String, String)*)(body: java.nio.file.Path => Unit): Unit = {
    val directory = java.nio.file.Files.createTempDirectory("lambda-import")
    files.foreach { case (name, content) => java.nio.file.Files.writeString(directory.resolve(name), content) }
    body(directory)
  }

  private def compileFile(directory: java.nio.file.Path, name: String): Either[List[CompileError], Lambda.Program] = {
    val path = directory.resolve(name)
    Compiler.compile(java.nio.file.Files.readString(path), path.toString)
  }

  private def runFile(directory: java.nio.file.Path, name: String)(using munit.Location): Int =
    compileFile(directory, name) match {
      case Right(program) => Interpreter.run(program).exitCode
      case Left(found) => fail(found.map(_.message).mkString("\n"))
    }

  test("module の中の import は、その module の中に宣言を取り込む") {
    withFiles(
      "lib.lam" -> "let value: i32 = 1\ndata Flag = { On, Off }\n",
      "main.lam" -> "module A { import \"lib\" }\nmodule B { import \"lib\" }\nlet x: i32 = A.value\nlet f: B.Flag = B.On\nlet main(): i32 = match f {\n  B.On => x\n  B.Off => 0\n}\n"
    ) { directory =>
      assertEquals(runFile(directory, "main.lam"), 1)
    }
  }

  test("module の中に取り込んだ名前は、外からは修飾なしでは見えない") {
    withFiles(
      "lib.lam" -> "let value: i32 = 1\n",
      "main.lam" -> "module A { import \"lib\" }\nlet x: i32 = value\nlet main(): i32 = 0\n"
    ) { directory =>
      compileFile(directory, "main.lam").left.map(_.map(_.core)) match {
        case Left(List(CompileError.UndefinedVariable("value", _))) => ()
        case other => fail(s"expected an undefined variable: $other")
      }
    }
  }

  test("top-level と module の中の両方に、同じファイルを取り込める") {
    withFiles(
      "lib.lam" -> "let value: i32 = 1\n",
      "main.lam" -> "import \"lib\"\nmodule A { import \"lib\" }\nlet x: i32 = value\nlet y: i32 = A.value\nlet main(): i32 = intrinsic[add_i32](x)(y)\n"
    ) { directory =>
      assertEquals(runFile(directory, "main.lam"), 2)
    }
  }

  test("取り込み中のファイルを module の中から取り込むとエラー") {
    withFiles(
      "a.lam" -> "module M { import \"main\" }\n",
      "main.lam" -> "import \"a\"\nlet main(): i32 = 0\n"
    ) { directory =>
      compileFile(directory, "main.lam") match {
        case Left(List(_: CompileError.ImportCycle)) => ()
        case other => fail(s"expected an import cycle: $other")
      }
    }
  }
}
