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
    validate(src).map(PatternDesugar.desugar).map(Deriver.desugar).map(TraitEncoder.encode)

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
    val roseData = "rec data Rose[a] = { Node(a)(List[Rose[a]]) }\n"
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
      "let inc(x: Option[i32]): Option[i32] = map[Option][i32][i32](x)(λn: i32. n)\n" + mainDecl
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
    val roseData = "rec data Rose[a] = { Node(a)(List[Rose[a]]) }\n"
    val src = foldableTrait + listData + roseData +
      "derive Foldable[List]\nderive Foldable[Rose]\n" + mainDecl
    assertRight(church(src))
  }

  test("Foldable[Either]: a を含まないコンストラクタは畳み込みに影響しない") {
    assertRight(church(foldableTrait + eitherData + "derive Foldable[Either]\n" + mainDecl))
  }

  // ---- エラー ----

  test("エラー: 未対応の trait は導出できない") {
    val src = "trait Pretty[a] {\n  def pretty(x: a): i32\n}\n" + optionData +
      "derive Pretty[Option]\n" + mainDecl
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
        |  def map[a][b](o: Option[a])(g: a → b): Option[b] = match o {
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
    val src = functorTrait + "rec data Weird[a] = { MkWeird(Weird[i32]), WNil }\n" +
      "derive Functor[Weird]\n" + mainDecl
    assertRight(church(src))
  }

  test("エラー: 非正則な自己適用は導出できない") {
    val src = functorTrait + "rec data Weird[a] = { MkWeird(Weird[Weird[a]]), WNil }\n" +
      "derive Functor[Weird]\n" + mainDecl
    assertLeftContains(validate(src), "cannot derive over")
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

  test("derive Traversable: 再帰・パラメータ・定数のフィールド") {
    val src = traversablePrelude +
      "rec data List[a] = { Nil, Cons(a)(List[a]) }\ndata Tagged[a] = { Tag(i32)(a) }\n" +
      "derive Traversable[List]\nderive Traversable[Tagged]\n" + mainDecl
    val shown = shownOrFail(church(src))
    assert(shown.contains("$inst_Traversable_List"), clue(shown))
    assert(shown.contains("$inst_Traversable_Tagged"), clue(shown))
  }

  test("derive Traversable: 入れ子のコンテナは、そのコンテナの traverse に委譲する") {
    val src = traversablePrelude +
      "rec data List[a] = { Nil, Cons(a)(List[a]) }\nrec data Rose[a] = { Node(a)(List[Rose[a]]) }\n" +
      "derive Traversable[List]\nderive Traversable[Rose]\n" + mainDecl
    assertRight(church(src))
  }

  test("derive Traversable: 入れ子のコンテナにインスタンスがなければエラー") {
    val src = traversablePrelude +
      "rec data List[a] = { Nil, Cons(a)(List[a]) }\nrec data Rose[a] = { Node(a)(List[Rose[a]]) }\n" +
      "derive Traversable[Rose]\n" + mainDecl
    assertLeftContains(validate(src), "Traversable")
  }

  test("derive Traversable: 関数のフィールドは導出できない") {
    val src = traversablePrelude + "data Reader[a] = { MkReader(i32 → a) }\nderive Traversable[Reader]\n" + mainDecl
    assertLeftContains(validate(src), "cannot derive over")
  }

  test("derive Traversable: traverse のシグネチャが正準形と違えばエラー") {
    val src =
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
    assertLeftContains(validate(src), "traverse")
  }

  test("derive Traversable: Applicative が定義されていなければエラー") {
    val src =
      """trait Applicative2[f: * → *] { def pure2[a](x: a): f[a] }
        |trait Traversable[t: * → *] {
        |  def traverse[g: * → *][a][b](f: a → g[b])(xs: t[a]): g[t[b]] with Applicative2[g]
        |}
        |data Box[a] = { MkBox(a) }
        |derive Traversable[Box]
        |""".stripMargin + mainDecl
    assert(validate(src).isLeft)
  }

  test("rec { derive }: 相互再帰のデータ型にも Traversable を導出できる") {
    val src = traversablePrelude +
      """rec {
        |  data Tree[a] = { Node(a)(Forest[a]) }
        |  data Forest[a] = { Empty, Trees(Tree[a])(Forest[a]) }
        |}
        |rec {
        |  derive Traversable[Tree]
        |  derive Traversable[Forest]
        |}
        |""".stripMargin + mainDecl
    assertRight(church(src))
  }

  // ---- Eq / Ord / Show ----

  private val valueTraits =
    """trait Eq[a] { def eq(x: a)(y: a): bool }
      |trait Ord[a] with Eq[a] { def lt(x: a)(y: a): bool }
      |trait Show[a] { def show(x: a): foreign.C.String }
      |impl Eq[i32] { def eq(x: i32)(y: i32): bool = intrinsic[eq_i32](x)(y) }
      |impl Ord[i32] { def lt(x: i32)(y: i32): bool = intrinsic[lt_i32](x)(y) }
      |impl Show[i32] { def show = foreign[i32 → foreign.C.String] int_to_string }
      |""".stripMargin

  test("derive Eq / Ord / Show: 型パラメータごとに同じ trait の文脈を持つインスタンスになる") {
    val src = valueTraits + listData + "derive Eq[List]\nderive Ord[List]\nderive Show[List]\n" +
      "let same: bool = eq[List[i32]](Nil[i32])(Nil[i32])\n" + mainDecl
    val typed = validate(src).fold(fail(_), identity)
    val env = typed.extract match { case ProgramAnn(e) => e }
    val inst = env.instances(instanceKey(TypeVariable("Eq"), Seq("List")))
    assertEquals(inst.context.map(showTypeConstraint), Seq("Eq[a]"))
    assertRight(church(src))
  }

  test("derive Eq / Ord / Show: 型パラメータのない data・フィールドのない構成子") {
    assertRight(church(valueTraits + "data Color = { Red, Green }\nderive Eq[Color]\nderive Ord[Color]\nderive Show[Color]\n" + mainDecl))
  }

  test("derive Ord: スーパークラスの Eq が先に要る") {
    assertLeftContains(validate(valueTraits + "data Color = { Red, Green }\nderive Ord[Color]\n" + mainDecl), "No instance for Eq[Color]")
  }

  test("derive Eq: インスタンスのないフィールドはエラー") {
    assertLeftContains(validate(valueTraits + "data F = { MkF(i32 → i32) }\nderive Eq[F]\n" + mainDecl), "field requires instance Eq[i32 → i32]")
  }

  test("derive Eq: 型パラメータはカインド * に限る") {
    assertLeftContains(validate(valueTraits + "data Apply[f: * → *] = { MkApply(f[i32]) }\nderive Eq[Apply]\n" + mainDecl), "must have kind *")
  }

  test("derive Show: 正準シグネチャと違えばエラー") {
    val src = "trait Show[a] { def show(x: a): i32 }\ndata Color = { Red }\nderive Show[Color]\n" + mainDecl
    assertLeftContains(validate(src), "must have signature")
  }
}
