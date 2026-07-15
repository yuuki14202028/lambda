package com.yuuki14202028

class DeriveSuite extends munit.FunSuite {

  private def parseDecls(src: String): Seq[Rec[Decl]] =
    ParserAST.programParser.parseAll(src) match {
      case Right(p) => eraseIndex(p).unfix match { case AST.Program(decls) => decls }
      case Left(e)  => fail(s"parse failed: $e")
    }

  private def validate(src: String): Either[String, TypeRec[AST.Program.type]] =
    ParserAST.programParser.parseAll(src).left.map(e => s"Parse error: $e").flatMap(p => TAnalyser.validate(p).left.map(_.render))

  private def desugar(src: String): Either[String, TypeRec[AST.Program.type]] =
    validate(src).map(Deriver.desugar).flatMap(p => TraitEncoder.encode(p).left.map(_.render))

  private def church(src: String): Either[String, TypeRec[AST.Program.type]] =
    desugar(src).map(ChurchEncoder.encode)

  private def assertRight(result: Either[String, ?]): Unit = result match {
    case Right(_)  => ()
    case Left(err) => fail(err)
  }

  private def assertLeftContains(result: Either[String, ?], expected: String): Unit = result match {
    case Left(err) => assert(err.contains(expected), clue(err))
    case Right(_)  => fail(s"expected error containing '$expected', but succeeded")
  }

  private def shownOrFail(result: Either[String, TypeRec[AST.Program.type]]): String = result match {
    case Right(p)  => p.show
    case Left(err) => fail(err)
  }

  private val functorTrait =
    """trait Functor[f: * → *] {
      |  def map[a][b](g: a → b): f[a] → f[b]
      |}
      |""".stripMargin

  private val foldableTrait =
    """trait Foldable[f: * → *] {
      |  def foldRight[a][b](g: a → b → b)(z: b): f[a] → b
      |  def foldLeft[a][b](g: b → a → b)(z: b): f[a] → b
      |}
      |""".stripMargin

  private val optionData = "data Option[a] = { None, Some(a) }\n"
  private val listData = "data rec List[a] = { Nil, Cons(a)(List[a]) }\n"
  private val eitherData = "data Either[e][a] = { Left(e), Right(a) }\n"
  private val mainDecl = "let main(): i32 = 0\n"

  // ---- パース ----

  test("パース: derive 宣言は TopDerive ノードになる") {
    parseDecls("derive Functor[Option]\n").head.unfix match {
      case AST.TopDerive(traitName, target) =>
        assertEquals(traitName, TypeVariable("Functor"))
        assertEquals(target, TypeVariable("Option"))
      case other => fail(s"not TopDerive: $other")
    }
  }

  // ---- Functor 導出 ----

  test("Functor[Option]: 型検査・合成・辞書化・Church encoding を通る") {
    assertRight(church(functorTrait + optionData + "derive Functor[Option]\n" + mainDecl))
  }

  test("Functor[Option]: 手書き impl と同じ辞書 let が生成される") {
    val shown = shownOrFail(desugar(functorTrait + optionData + "derive Functor[Option]\n" + mainDecl))
    assert(shown.contains("$inst_Functor_Option"), clue(shown))
    assert(shown.contains("MkFunctor"), clue(shown))
  }

  test("Functor[List]: 再帰データ型は内部 letRec で自己再帰する") {
    val src = functorTrait + listData + "derive Functor[List]\n" + mainDecl
    assertRight(church(src))
    val shown = shownOrFail(desugar(src))
    assert(shown.contains("$go"), clue(shown))
  }

  test("Functor[Either]: 先行パラメータはインスタンスパラメータになる") {
    val src = functorTrait + eitherData + "derive Functor[Either]\n" + mainDecl
    assertRight(church(src))
    assert(shownOrFail(desugar(src)).contains("$inst_Functor_Either"))
  }

  test("Functor[Rose]: ネストした関手フィールドは先行インスタンスへ委譲する") {
    val roseData = "data rec Rose[a] = { Node(a)(List[Rose[a]]) }\n"
    val src = functorTrait + listData + roseData +
      "derive Functor[List]\nderive Functor[Rose]\n" + mainDecl
    assertRight(church(src))
    val shown = shownOrFail(desugar(src))
    assert(shown.contains("$inst_Functor_Rose"), clue(shown))
    assert(shown.contains("$inst_Functor_List"), clue(shown))
  }

  test("Functor: 共変位置の関数フィールドも導出できる") {
    val src = functorTrait + "data Reader[a] = { MkReader(i32 → a) }\n" +
      "derive Functor[Reader]\n" + mainDecl
    assertRight(church(src))
  }

  test("導出インスタンスの利用: 呼び出し側で辞書が解決される") {
    val src = functorTrait + optionData + "derive Functor[Option]\n" +
      "let inc(x: Option[i32]): Option[i32] = map[Option][i32][i32](λn: i32. n)(x)\n" + mainDecl
    assertRight(church(src))
    val shown = shownOrFail(desugar(src))
    assert(shown.contains("$inst_Functor_Option"), clue(shown))
  }

  // ---- Foldable 導出 ----

  test("Foldable[Option] / Foldable[List]: 導出が通る") {
    val src = foldableTrait + optionData + listData +
      "derive Foldable[Option]\nderive Foldable[List]\n" + mainDecl
    assertRight(church(src))
    val shown = shownOrFail(desugar(src))
    assert(shown.contains("$inst_Foldable_Option"), clue(shown))
    assert(shown.contains("$inst_Foldable_List"), clue(shown))
  }

  test("Foldable[Rose]: ネストしたフィールドは先行インスタンスへ委譲する") {
    val roseData = "data rec Rose[a] = { Node(a)(List[Rose[a]]) }\n"
    val src = foldableTrait + listData + roseData +
      "derive Foldable[List]\nderive Foldable[Rose]\n" + mainDecl
    assertRight(church(src))
  }

  test("Foldable[Either]: a を含まないコンストラクタは畳み込みに影響しない") {
    assertRight(church(foldableTrait + eitherData + "derive Foldable[Either]\n" + mainDecl))
  }

  // ---- エラー ----

  test("エラー: 未対応の trait は導出できない") {
    val src = "trait Show[a] {\n  def show(x: a): i32\n}\n" + optionData +
      "derive Show[Option]\n" + mainDecl
    assertLeftContains(validate(src), "not derivable")
  }

  test("エラー: 同名でもシグネチャが正準形でない trait は導出できない") {
    val flipped =
      """trait Functor[f: * → *] {
        |  def map[a][b](g: a → b): f[b] → f[a]
        |}
        |""".stripMargin
    assertLeftContains(validate(flipped + optionData + "derive Functor[Option]\n" + mainDecl), "must have signature")
  }

  test("エラー: データ型でない対象は導出できない") {
    assertLeftContains(validate(functorTrait + "derive Functor[Functor]\n" + mainDecl), "not a data type")
  }

  test("エラー: 型パラメータのないデータ型は導出できない") {
    val src = functorTrait + "data Wrap = { MkWrap(i32) }\n" + "derive Functor[Wrap]\n" + mainDecl
    assertLeftContains(validate(src), "final type parameter")
  }

  test("エラー: 負の位置の出現は導出できない") {
    val src = functorTrait + "data Pred[a] = { MkPred(a → bool) }\n" + "derive Functor[Pred]\n" + mainDecl
    assertLeftContains(validate(src), "negative position")
  }

  test("エラー: Foldable は関数フィールドを畳み込めない") {
    val src = foldableTrait + "data Reader[a] = { MkReader(i32 → a) }\n" + "derive Foldable[Reader]\n" + mainDecl
    assertLeftContains(validate(src), "cannot derive over")
  }

  test("エラー: ネストしたフィールドのインスタンスが先行して存在しない") {
    val src = functorTrait + optionData + "data Boxed[a] = { MkBoxed(Option[a]) }\n" +
      "derive Functor[Boxed]\n" + mainDecl
    assertLeftContains(validate(src), "is not defined at this point")
  }

  test("エラー: 手書き impl と重複する derive は Overlapping instance") {
    val src = functorTrait + optionData +
      """impl Functor[Option] {
        |  def map[a][b](g: a → b): Option[a] → Option[b] = λo: Option[a]. match o {
        |    None => None[b]
        |    Some(x) => Some[b](g(x))
        |  }
        |}
        |derive Functor[Option]
        |""".stripMargin + mainDecl
    assertLeftContains(validate(src), "Overlapping instance")
  }

  test("エラー: 同じ derive を二度書くと Overlapping instance") {
    val src = functorTrait + optionData +
      "derive Functor[Option]\nderive Functor[Option]\n" + mainDecl
    assertLeftContains(validate(src), "Overlapping instance")
  }

  test("パラメータを含まない自己適用フィールドはそのまま保持される") {
    val src = functorTrait + "data rec Weird[a] = { MkWeird(Weird[i32]), WNil }\n" +
      "derive Functor[Weird]\n" + mainDecl
    assertRight(church(src))
  }

  test("エラー: 非正則な自己適用は導出できない") {
    val src = functorTrait + "data rec Weird[a] = { MkWeird(Weird[Weird[a]]), WNil }\n" +
      "derive Functor[Weird]\n" + mainDecl
    assertLeftContains(validate(src), "cannot derive over")
  }
}
