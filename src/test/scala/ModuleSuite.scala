package com.yuuki14202028

class ModuleSuite extends munit.FunSuite {

  private def validate(src: String): Either[String, TypeRec[AST.Program.type]] =
    ParserAST.programParser.parseAll(src).left.map(e => s"Parse error: $e").flatMap(p => TAnalyser.validate(p).left.map(_.render))

  // 型検査の後の全フェーズ（コード生成の手前まで）
  private def encode(src: String): Either[String, TypeRec[AST.Program.type]] =
    validate(src).map(p => ChurchEncoder.encode(TraitEncoder.encode(ContextDesugar.desugar(Deriver.desugar(ModuleDesugar.desugar(p))))))

  private def assertRight(result: Either[String, ?]): Unit = result match {
    case Right(_)  => ()
    case Left(err) => fail(err)
  }

  private def assertLeftContains(result: Either[String, ?], expected: String): Unit = result match {
    case Left(err) => assert(err.contains(expected), clue(err))
    case Right(_)  => fail(s"expected error containing '$expected', but succeeded")
  }

  private val add = "let __add_i32(a: i32)(b: i32): i32 = intrinsic[add_i32](a)(b)\n"

  private val counter = add +
    """module Counter {
      |  opaque type T = i32
      |  private let step: i32 = 1
      |  let zero: T = 0
      |  let inc(c: T): T = c + step
      |  let get(c: T): i32 = c
      |}
      |""".stripMargin

  private def withMain(src: String): String = src + "\nlet main(): i32 = 0\n"

  // ---- 名前空間 ----

  test("module のメンバーは修飾して参照する") {
    assertRight(encode(withMain(counter + "let two: Counter.T = Counter.inc(Counter.inc(Counter.zero))")))
  }

  test("module のメンバーは修飾なしでは外から見えない") {
    assertLeftContains(validate(withMain(counter + "let z: Counter.T = zero")), "Variable zero is not defined")
    assertLeftContains(validate(withMain(counter + "let z: T = Counter.zero")), "Type variable T is not defined")
  }

  test("同じ名前を別の module で定義できる") {
    val src =
      """module A { let x: i32 = 1 }
        |module B { let x: bool = true }
        |let a: i32 = A.x
        |let b: bool = B.x
        |""".stripMargin
    assertRight(encode(withMain(src)))
  }

  test("入れ子の module は外側の名前が見え、外からは経路で参照する") {
    val src =
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
        |""".stripMargin
    assertRight(encode(withMain(src)))
  }

  test("module の中の定義は、外側の同名の定義を隠す") {
    val src =
      """let x: i32 = 1
        |module M {
        |  let x: bool = true
        |  let y: bool = x
        |}
        |let z: i32 = x
        |""".stripMargin
    assertRight(encode(withMain(src)))
  }

  test("let（rec なし）の値からは、定義中の名前ではなく外側の同名の定義が見える") {
    val src = add +
      """let x: i32 = 1
        |module M {
        |  let x: i32 = x + 1
        |}
        |""".stripMargin
    assertRight(encode(withMain(src)))
  }

  test("局所的な束縛は module のメンバーを隠す") {
    val src =
      """module M {
        |  let x: i32 = 1
        |  let f(x: bool): bool = x
        |  let g[T](y: T): T = y
        |}
        |""".stripMargin
    assertRight(encode(withMain(src)))
  }

  test("式の中の type / data も module のメンバーを隠す") {
    val src =
      """module M {
        |  type T = bool
        |  data D = { A }
        |  let x: i32 = type T = i32 in data D = { A(T) } in match A(1) { A(n) => n }
        |  let y: T = true
        |  let z: D = A
        |}
        |""".stripMargin
    assertRight(encode(withMain(src)))
  }

  test("module の中の構文エラーは、その位置で報告される") {
    val src = "module M {\n  let x: i32 = 1\n  let y i32 = 2\n}\nlet main(): i32 = 0\n"
    ParserAST.programParser.parseAll(src) match {
      case Left(err) => assertEquals(cats.parse.LocationMap(src).toLineCol(err.failedAtOffset).map(_._1), Some(2))
      case Right(_) => fail("expected a parse error")
    }
  }

  test("同じ module を二度定義できない") {
    assertLeftContains(validate(withMain("module M { }\nmodule M { }")), "Module M is already defined")
  }

  test("未定義の module・メンバー") {
    assertLeftContains(validate(withMain("let x: i32 = M.x")), "Variable M.x is not defined")
    assertLeftContains(validate(withMain("module M { }\nlet x: i32 = M.x")), "Variable M.x is not defined")
    assertLeftContains(validate(withMain("let x: M.T = 0")), "Type variable M.T is not defined")
  }

  // ---- open ----

  test("open した module のメンバーは修飾なしで書ける") {
    val src = counter +
      """open Counter
        |let two: T = inc(inc(zero))
        |let n: i32 = get(two)
        |""".stripMargin
    assertRight(encode(withMain(src)))
  }

  test("open しても private なメンバーは見えない") {
    assertLeftContains(validate(withMain(counter + "open Counter\nlet s: i32 = step")), "Variable step is not defined")
  }

  test("open は入れ子の module も経路で指定できる") {
    val src =
      """module A { module B { let y: i32 = 1 } }
        |open A.B
        |let z: i32 = y
        |""".stripMargin
    assertRight(encode(withMain(src)))
  }

  test("open の後の定義は、open した名前を隠す") {
    val src =
      """module M { let x: i32 = 1 }
        |open M
        |let x: bool = true
        |let y: bool = x
        |""".stripMargin
    assertRight(encode(withMain(src)))
  }

  test("未定義の module は open できない") {
    assertLeftContains(validate(withMain("open M")), "Module M is not defined")
    assertLeftContains(validate(withMain("module M { }\nopen M.N")), "Module M.N is not defined")
  }

  // ---- private ----

  test("private な宣言は module の外から見えない") {
    assertLeftContains(validate(withMain(counter + "let s: i32 = Counter.step")), "Variable Counter.step is not defined")
  }

  test("private な型・module も外から見えない") {
    val src =
      """module M {
        |  private type Hidden = i32
        |  private module Inner { let x: i32 = 1 }
        |  let y: Hidden = Inner.x
        |}
        |""".stripMargin
    assertRight(encode(withMain(src)))
    assertLeftContains(validate(withMain(src + "let z: M.Hidden = 0")), "Type variable M.Hidden is not defined")
    assertLeftContains(validate(withMain(src + "let z: i32 = M.Inner.x")), "Variable M.Inner.x is not defined")
  }

  test("private / opaque は module の中でしか使えない") {
    assertLeftContains(validate(withMain("private let x: i32 = 1")), "private can only be used inside a module")
    assertLeftContains(validate(withMain("opaque type T = i32")), "opaque can only be used inside a module")
  }

  // ---- opaque type ----

  test("opaque type は module の中では表現と同じ型") {
    assertRight(encode(withMain(counter)))
  }

  test("opaque type は module の外では表現と別の型") {
    assertLeftContains(validate(withMain(counter + "let n: i32 = Counter.zero")), "Type mismatch")
    assertLeftContains(validate(withMain(counter + "let c: Counter.T = 0")), "Type mismatch")
    assertLeftContains(validate(withMain(counter + "let n: i32 = Counter.zero + 1")), "Operator + is not defined for type Counter.T")
    assertLeftContains(validate(withMain(counter + "let n: Counter.T = Counter.inc(1)")), "Type mismatch")
  }

  test("封印の後、公開メンバーの型は抽象型のまま見える") {
    val typed = validate(withMain(counter)).fold(fail(_), identity)
    val env = typed.extract match {
      case ProgramAnn(e) => e
    }
    assertEquals(env.values(Variable("Counter.inc")).show, "Counter.T → Counter.T")
    assertEquals(env.values(Variable("Counter.get")).show, "Counter.T → i32")
  }

  test("opaque type を使う別名・データ型・trait も、外からは抽象型のまま見える") {
    val src = counter.replace("  let get(c: T): i32 = c\n", "") +
      """module Wrap {
        |  opaque type T = i32
        |  type Two = T → T
        |  data Box = { MkBox(T) }
        |  trait Make[a] { def make(x: a): T }
        |  let unbox(b: Box): T = match b { MkBox(x) => x }
        |}
        |""".stripMargin
    assertRight(encode(withMain(src)))
    assertLeftContains(validate(withMain(src + "let f: Wrap.Two = λx: i32. x")), "Type mismatch")
    assertLeftContains(validate(withMain(src + "let b: Wrap.Box = Wrap.MkBox(1)")), "Type mismatch")
    assertLeftContains(validate(withMain(src + "impl Wrap.Make[i32] { def make = λx: i32. x }")), "Wrap.T")
  }

  test("入れ子の module は、外側の module を閉じたときにまとめて封印される") {
    val src =
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
    assertRight(encode(withMain(src)))
    assertLeftContains(validate(withMain(src + "let bad: i32 = M.N.f(1)")), "Type mismatch")
  }

  test("型パラメータを持つ opaque type") {
    val src =
      """module M {
        |  opaque type Wrapped[a] = a → a
        |  let wrap[a](f: a → a): Wrapped[a] = f
        |  let run[a](w: Wrapped[a])(x: a): a = w(x)
        |}
        |let w: M.Wrapped[i32] = M.wrap[i32](λx: i32. x)
        |let n: i32 = M.run[i32](w)(1)
        |""".stripMargin
    assertRight(encode(withMain(src)))
    assertLeftContains(validate(withMain(src + "let m: i32 = w(1)")), "Not a function")
  }

  test("抽象型へのインスタンスは module の外で定義でき、表現の型のインスタンスと区別される") {
    val src = counter +
      """trait Weight[a] { def weight(x: a): i32 }
        |impl Weight[i32] { def weight = λx: i32. x }
        |impl Weight[Counter.T] { def weight = λc: Counter.T. Counter.get(c) }
        |let a: i32 = weight[i32](1)
        |let b: i32 = weight[Counter.T](Counter.zero)
        |""".stripMargin
    val shown = encode(withMain(src)).fold(fail(_), _.show)
    assert(shown.contains("$inst_Weight_i32"), clue(shown))
    assert(shown.contains("$inst_Weight_Counter.T"), clue(shown))
  }

  test("脱糖の後に module と抽象型は残らない") {
    val typed = validate(withMain(counter + "let two: Counter.T = Counter.inc(Counter.zero)")).fold(fail(_), identity)
    val desugared = ModuleDesugar.desugar(typed)
    val shown = desugared.show
    assert(!shown.contains("module"), clue(shown))
    assert(shown.contains("let Counter.step: i32"), clue(shown))
    assert(shown.contains("(Counter.zero)[i32]"), clue(shown))
    val env = desugared.extract match {
      case ProgramAnn(e) => e
    }
    assert(env.abstractTypes.isEmpty)
    assertEquals(env.values(Variable("Counter.inc")).show, "i32 → i32")
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
    assertRight(encode(withMain(stack + "let s: Stack.Stack[i32] = Stack.push[i32](1)(Stack.empty[i32])")))
  }

  test("opaque data の構成子は module の外から使えない") {
    assertLeftContains(validate(withMain(stack + "let s: Stack.Stack[i32] = Stack.Empty[i32]")), "Variable Stack.Empty is not defined")
    assertLeftContains(validate(withMain(stack + "open Stack\nlet s: Stack[i32] = Empty[i32]")), "Variable Empty is not defined")
    val matching =
      """let f(s: Stack.Stack[i32]): i32 = match s {
        |  Stack.Empty => 0
        |  Stack.Push(x)(rest) => 1
        |}
        |""".stripMargin
    assertLeftContains(validate(withMain(stack + matching)), "Stack.Empty")
  }

  // ---- trait と module ----

  test("module の中の trait は外から修飾して実装・使用できる") {
    val src =
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
        |""".stripMargin
    assertRight(encode(withMain(src)))
  }

  test("インスタンスは module の中で定義しても全体で有効") {
    val src =
      """trait Weight[a] { def weight(x: a): i32 }
        |module M {
        |  data Box = { MkBox(i32) }
        |  impl Weight[Box] { def weight = λb: Box. 1 }
        |}
        |let a: i32 = weight[M.Box](M.MkBox(1))
        |""".stripMargin
    assertRight(encode(withMain(src)))
    assertLeftContains(validate(withMain(src + "impl Weight[M.Box] { def weight = λb: M.Box. 2 }")), "Weight")
  }
}
