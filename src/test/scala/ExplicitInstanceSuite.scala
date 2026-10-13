package com.yuuki14202028

class ExplicitInstanceSuite extends munit.FunSuite {

  private def validate(src: String): Either[String, TypeRec[AST.Program.type]] =
    ParserAST.programParser.parseAll(src).left.map(e => s"Parse error: $e").flatMap(p => TAnalyser.validate(p).left.map(_.render))

  private def encode(src: String): Either[String, TypeRec[AST.Program.type]] =
    validate(src).map(p => ChurchEncoder.encode(TraitEncoder.encode(ContextDesugar.desugar(Deriver.desugar(PatternDesugar.desugar(ModuleDesugar.desugar(p)))))))

  private def assertRight(result: Either[String, ?]): Unit = result match {
    case Right(_) => ()
    case Left(err) => fail(err)
  }

  private def assertLeftContains(result: Either[String, ?], expected: String): Unit = result match {
    case Left(err) => assert(err.contains(expected), clue(err))
    case Right(_) => fail(s"expected failure containing: $expected")
  }

  private val prelude =
    """trait Order[a] { def before(x: a)(y: a): bool }
      |impl Order[i32] { def before = λx: i32. λy: i32. true }
      |let other: Order[i32] = MkOrder[i32](λx: i32. λy: i32. false)
      |let first[a](x: a)(y: a): a with Order[a] = if before[a](x)(y) then x else y
      |""".stripMargin

  private val main = "let main(): i32 = 0\n"

  // ---- summon ----

  test("summon[C[τ]] は解決されたインスタンスの辞書で、型は C[τ]") {
    val shown = encode(prelude + "let d: Order[i32] = summon[Order[i32]]\n" + main).fold(fail(_), _.show)
    assert(shown.contains("$inst_Order_i32"), clue(shown))
  }

  test("summon はスコープ中の制約も取り出せる") {
    assertRight(encode(prelude + "let get[a](x: a): Order[a] with Order[a] = summon[Order[a]]\n" + main))
  }

  test("summon はインスタンスがなければエラー") {
    assertLeftContains(validate(prelude + "let d: Order[bool] = summon[Order[bool]]\n" + main), "No instance")
  }

  test("summon で量化された制約の辞書も取り出せる") {
    val src = prelude +
      """data Box[a] = { MkBox(a) }
        |impl[a] Order[Box[a]] with Order[a] { def before = λx: Box[a]. λy: Box[a]. true }
        |let d: ∀a. Order[a] → Order[Box[a]] = summon[∀a. Order[a] ⇒ Order[Box[a]]]
        |""".stripMargin
    assertRight(encode(src + main))
  }

  // ---- (with d) ----

  test("(with d) で、解決の代わりに辞書の値を渡せる") {
    val shown = encode(prelude + "let n: i32 = first[i32](with other)(1)(2)\n" + main).fold(fail(_), _.show)
    val line = shown.linesIterator.find(_.startsWith("let n:")).getOrElse(fail(shown))
    assert(line.contains("other"), clue(line))
    assert(!line.contains("$inst_Order_i32"), clue(line))
  }

  test("(with d) を付けなければ、従来どおり解決される") {
    val shown = encode(prelude + "let n: i32 = first[i32](1)(2)\n" + main).fold(fail(_), _.show)
    val line = shown.linesIterator.find(_.startsWith("let n:")).getOrElse(fail(shown))
    assert(line.contains("$inst_Order_i32"), clue(line))
  }

  test("インスタンスのない型でも、辞書を渡せば使える") {
    assertRight(encode(prelude + "let b: bool = first[bool](with MkOrder[bool](λx: bool. λy: bool. true))(true)(false)\n" + main))
  }

  test("trait のメソッドにも辞書を渡せる") {
    assertRight(encode(prelude + "let b: bool = before[i32](with other)(1)(2)\n" + main))
  }

  test("渡す値の型は、要求される辞書の型と一致しなければならない") {
    assertLeftContains(validate(prelude + "let n: i32 = first[i32](with 1)(1)(2)\n" + main), "Type mismatch")
    assertLeftContains(
      validate(prelude + "let n: i32 = first[i32](with MkOrder[bool](λx: bool. λy: bool. true))(1)(2)\n" + main),
      "Type mismatch"
    )
  }

  test("制約を取らない式には辞書を渡せない") {
    assertLeftContains(validate(prelude + "let id(x: i32): i32 = x\nlet n: i32 = id(with other)(1)\n" + main), "takes no instance")
    assertLeftContains(validate(prelude + "let n: i32 = first[i32](with other)(with other)(1)(2)\n" + main), "takes no instance")
  }

  test("制約が複数あるとき、先頭から順に渡し、残りは解決される") {
    val src = prelude +
      """trait Label[a] { def label(x: a): i32 }
        |impl Label[i32] { def label = λx: i32. x }
        |let both[a](x: a): i32 with Order[a], Label[a] = if before[a](x)(x) then label[a](x) else 0
        |let one: i32 = both[i32](with other)(1)
        |let two: i32 = both[i32](with other)(with MkLabel[i32](λx: i32. 0))(1)
        |""".stripMargin
    val shown = encode(src + main).fold(fail(_), _.show)
    val one = shown.linesIterator.find(_.startsWith("let one:")).getOrElse(fail(shown))
    assert(one.contains("$inst_Label_i32"), clue(one))
    assert(!one.contains("$inst_Order_i32"), clue(one))
  }

  test("辞書の式の中では、制約は従来どおり解決される") {
    val src = prelude +
      """let flip[a](x: a): Order[a] with Order[a] = MkOrder[a](λl: a. λr: a. before[a](r)(l))
        |let n: i32 = first[i32](with flip[i32](0))(1)(2)
        |""".stripMargin
    assertRight(encode(src + main))
  }

  test("スーパークラスを持つ trait の辞書を組み立てて渡せる") {
    val src = prelude +
      """trait Named[a] with Order[a] { def weight(x: a): i32 }
        |let heavy[a](x: a)(y: a): i32 with Named[a] = weight[a](first[a](x)(y))
        |let custom: Named[i32] = MkNamed[i32](other)(λx: i32. x)
        |let n: i32 = heavy[i32](with custom)(1)(2)
        |""".stripMargin
    assertRight(encode(src + main))
  }

  // ---- using ----

  test("using d in e: e の中では d がインスタンスとして解決に使われる") {
    val shown = encode(prelude + "let n: i32 = using other in first[i32](1)(first[i32](2)(3))\n" + main).fold(fail(_), _.show)
    val line = shown.linesIterator.find(_.startsWith("let n:")).getOrElse(fail(shown))
    assert(!line.contains("$inst_Order_i32"), clue(line))
  }

  test("using の外では、従来どおりインスタンスが使われる") {
    val shown = encode(prelude + "let n: i32 = first[i32](using other in 1)(2)\n" + main).fold(fail(_), _.show)
    val line = shown.linesIterator.find(_.startsWith("let n:")).getOrElse(fail(shown))
    assert(line.contains("$inst_Order_i32"), clue(line))
  }

  test("using はインスタンスのない型にも使える") {
    assertRight(encode(prelude + "let b: bool = using MkOrder[bool](λx: bool. λy: bool. true) in first[bool](true)(false)\n" + main))
    assertLeftContains(validate(prelude + "let b: bool = first[bool](true)(false)\n" + main), "No instance")
  }

  test("using は外側の同じ制約より優先される") {
    val src = prelude +
      """let flip[a](order: Order[a]): Order[a] = MkOrder[a](λx: a. λy: a. before[a](with order)(y)(x))
        |let flipped[a](x: a)(y: a): a with Order[a] = using flip[a](summon[Order[a]]) in first[a](x)(y)
        |""".stripMargin
    val shown = validate(src + main).fold(fail(_), _.show)
    assertRight(encode(src + main))
    assert(shown.contains("using"), clue(shown))
  }

  test("using には量化された制約の辞書（多相関数）も渡せる") {
    val src = prelude +
      """data Box[a] = { MkBox(a) }
        |let boxed: ∀a. Order[a] → Order[Box[a]] = Λa. λinner: Order[a]. MkOrder[Box[a]](λx: Box[a]. λy: Box[a]. true)
        |let b: Box[i32] = using boxed in first[Box[i32]](MkBox[i32](1))(MkBox[i32](2))
        |""".stripMargin
    assertRight(encode(src + main))
  }

  test("using に辞書でない値は渡せない") {
    assertLeftContains(validate(prelude + "let n: i32 = using 1 in 2\n" + main), "using expects a dictionary")
  }
}
