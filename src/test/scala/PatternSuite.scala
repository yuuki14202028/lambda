package com.yuuki14202028

class PatternSuite extends munit.FunSuite {

  private def validate(src: String): Either[String, TypeRec[AST.Program.type]] =
    ParserAST.programParser.parseAll(src).left.map(e => s"Parse error: $e").flatMap(p => TAnalyser.validate(p).left.map(_.render))

  private def lowered(src: String): Either[String, TypeRec[AST.Program.type]] =
    validate(src).map(p => PatternDesugar.desugar(ContextDesugar.desugar(ModuleDesugar.desugar(p))))

  private def encode(src: String): Either[String, TypeRec[AST.Program.type]] =
    validate(src).map(Compiler.desugar)

  private def assertRight(result: Either[String, ?]): Unit = result match {
    case Right(_) => ()
    case Left(err) => fail(err)
  }

  private def assertLeftContains(result: Either[String, ?], expected: String): Unit = result match {
    case Left(err) => assert(err.contains(expected), clue(err))
    case Right(_) => fail(s"expected failure containing: $expected")
  }

  private val prelude =
    """trait Eq[a] { def eq(x: a)(y: a): bool }; infix 4 == = eq; impl Eq[i32] { def eq(a: i32)(b: i32): bool = intrinsic[eq_i32](a)(b) }
      |data Option[a] = { None, Some(a) }
      |rec data List[a] = { Nil, Cons(a)(List[a]) }
      |data Pair[a][b] = { MkPair(a)(b) }
      |""".stripMargin

  private val main = "let main(): i32 = 0\n"

  private def program(body: String): String = prelude + body + "\n" + main

  // ---- 書けるパターン ----

  test("入れ子の構成子パターン") {
    assertRight(encode(program(
      """let f(xs: List[i32]): i32 = match xs {
        |  Cons(x)(Cons(y)(rest)) => x
        |  Cons(x)(Nil) => x
        |  Nil => 0
        |}""".stripMargin)))
  }

  test("変数と _ はどの値にも当たる") {
    assertRight(encode(program("let f(o: Option[i32]): i32 = match o { Some(x) => x  _ => 0 }")))
    assertRight(encode(program("let f(o: Option[i32]): Option[i32] = match o { whole => whole }")))
    assertRight(encode(program("let f(n: i32): i32 = match n { m => m }")))
  }

  test("リテラルのパターン（整数・bool）") {
    assertRight(encode(program("let f(n: i32): i32 = match n { 0 => 1  1 => 2  _ => 3 }")))
    assertRight(encode(program("let f(b: bool): i32 = match b { true => 1  false => 0 }")))
    assertRight(encode(program("let f(o: Option[i32]): i32 = match o { Some(0) => 1  Some(n) => n  None => 0 }")))
  }

  test("引数のない名前は、スコープ中の構成子なら構成子、そうでなければ変数") {
    assertRight(encode(program("let f(o: Option[i32]): i32 = match o { None => 0  other => 1 }")))
    assertLeftContains(validate(program("let f(o: Option[i32]): i32 = match o { None => 0  Nil => 1 }")), "does not belong")
  }

  test("パターンの変数は本体で使える（型はその位置の型）") {
    assertRight(encode(program("let f(p: Pair[i32][Option[bool]]): bool = match p { MkPair(n)(Some(b)) => b  MkPair(n)(None) => false }")))
    assertLeftContains(validate(program("let f(p: Pair[i32][bool]): i32 = match p { MkPair(n)(b) => b }")), "Type mismatch")
  }

  test("脱糖の後は、構成子 1 段の match だけが残る") {
    val shown = lowered(program("let f(xs: List[i32]): i32 = match xs { Cons(x)(Cons(y)(_)) => x  _ => 0 }")).fold(fail(_), _.show)
    assert(!shown.contains("=>"), clue(shown))
    assert(shown.contains("match"), clue(shown))
  }

  test("複数の経路から使われる本体は複製されない") {
    val shown = lowered(program("let f(xs: List[i32]): i32 = match xs { Cons(x)(Cons(y)(_)) => x  _ => 12345 }")).fold(fail(_), _.show)
    assertEquals("12345".r.findAllIn(shown).length, 1, clue(shown))
  }

  // ---- 網羅性と到達不能 ----

  test("網羅していなければ、当たらない値の例を示す") {
    assertLeftContains(validate(program("let f(o: Option[i32]): i32 = match o { Some(x) => x }")), "no case matches None")
    assertLeftContains(
      validate(program("let f(xs: List[i32]): i32 = match xs { Cons(x)(Nil) => x  Nil => 0 }")),
      "no case matches Cons(_)(Cons(_)(_))"
    )
    assertLeftContains(validate(program("let f(n: i32): i32 = match n { 0 => 1  1 => 2 }")), "no case matches _")
    assertLeftContains(validate(program("let f(b: bool): i32 = match b { true => 1 }")), "no case matches false")
    assertLeftContains(
      validate(program("let f(p: Pair[bool][bool]): i32 = match p { MkPair(true)(true) => 1  MkPair(false)(_) => 0 }")),
      "no case matches MkPair(true)(false)"
    )
  }

  test("到達不能な分岐は拒否") {
    assertLeftContains(validate(program("let f(o: Option[i32]): i32 = match o { Some(x) => x  None => 0  None => 1 }")), "case 3 is unreachable")
    assertLeftContains(validate(program("let f(o: Option[i32]): i32 = match o { _ => 0  None => 1 }")), "case 2 is unreachable")
    assertLeftContains(validate(program("let f(n: i32): i32 = match n { 0 => 1  0 => 2  _ => 3 }")), "case 2 is unreachable")
  }

  // ---- 型エラー ----

  test("構成子の引数の数が合わない") {
    assertLeftContains(validate(program("let f(o: Option[i32]): i32 = match o { Some(x)(y) => x  None => 0 }")), "Some")
  }

  test("構成子でない名前に引数は付けられない") {
    assertLeftContains(validate(program("let f(o: Option[i32]): i32 = match o { foo(x) => x  None => 0 }")), "foo is not a constructor")
  }

  test("同じ変数を 1 つのパターンで二度束縛できない") {
    assertLeftContains(validate(program("let f(p: Pair[i32][i32]): i32 = match p { MkPair(x)(x) => x }")), "Variable x is already defined")
  }

  test("リテラルの型がスクルーティニーの型と合わない") {
    assertLeftContains(validate(program("let f(b: bool): i32 = match b { 0 => 1  _ => 2 }")), "Type mismatch")
    assertLeftContains(validate(program("let f(o: Option[i32]): i32 = match o { 0 => 1  _ => 2 }")), "Type mismatch")
  }

  test("データ型でない値を構成子で分解できない") {
    assertLeftContains(validate(program("let f(n: i32): i32 = match n { Some(x) => x  _ => 0 }")), "Not a data type")
  }

  test("リテラルのパターンは == に宣言された関数で比べる（宣言がなければエラー）") {
    val src =
      """let f(n: i32): i32 = match n { 0 => 1  _ => 2 }
        |let main(): i32 = 0
        |""".stripMargin
    assertLeftContains(validate(src), "Operator == is not declared")
  }

  // ---- 負の数・文字列・選択肢・条件 ----

  private val less = "trait Lt[a] { def lt(x: a)(y: a): bool }; infix 4 < = lt; impl Lt[i32] { def lt(a: i32)(b: i32): bool = intrinsic[lt_i32](a)(b) }\n"

  test("負の数のリテラル（直前の本体と引き算にならない）") {
    assertRight(encode(program("let f(n: i32): i32 = match n {\n  0 => n\n  -1 => 1\n  _ => 2\n}")))
  }

  test("文字列のリテラルも == で比べる（Eq[String] のインスタンスが要る）") {
    val eqString = "impl Eq[foreign.C.String] { def eq(x: foreign.C.String)(y: foreign.C.String): bool = true }\n"
    val src = "let f(s: foreign.C.String): i32 = match s { \"a\" => 1  \"b\" => 2  _ => 0 }\n"
    assertRight(encode(program(eqString + src)))
    assertLeftContains(validate(program(src)), "No instance for Eq[foreign.C.String]")
    assertLeftContains(validate(program(eqString + "let f(s: foreign.C.String): i32 = match s { \"a\" => 1 }")), "no case matches _")
  }

  test("p | q: 複数のパターンを 1 つの本体にまとめる") {
    assertRight(encode(program("let f(n: i32): i32 = match n { 0 | 1 | 2 => 1  _ => 0 }")))
    assertRight(encode(program("let f(o: Option[i32]): i32 = match o { Some(0 | 1) => 1  Some(_) | None => 0 }")))
    assertRight(encode(program("let f(p: Pair[Option[i32]][i32]): i32 = match p { MkPair(Some(x))(_) | MkPair(None)(x) => x }")))
  }

  test("p | q: 本体は複製されない") {
    val shown = lowered(program("let f(n: i32): i32 = match n { 0 | 1 | 2 => 12345  _ => 0 }")).fold(fail(_), _.show)
    assertEquals("12345".r.findAllIn(shown).length, 1, clue(shown))
  }

  test("p | q: どの選択肢も同じ変数を同じ型で束縛する") {
    assertLeftContains(validate(program("let f(o: Option[i32]): i32 = match o { Some(x) | None => 0 }")), "must bind the same variables")
    assertLeftContains(
      validate(program("let f(p: Pair[i32][bool]): i32 = match p { MkPair(x)(_) | MkPair(_)(x) => 0 }")),
      "must bind the same variables"
    )
  }

  test("p | q: 網羅性は選択肢を合わせて調べる") {
    assertLeftContains(validate(program("let f(b: Pair[bool][bool]): i32 = match b { MkPair(true)(_) | MkPair(_)(true) => 1 }")),
      "no case matches MkPair(false)(false)")
  }

  test("条件付きの分岐: 条件が偽なら後続の分岐を試す") {
    assertRight(encode(less + program("let f(o: Option[i32]): i32 = match o { Some(n) if n < 0 => 0  Some(n) => n  None => 1 }")))
  }

  test("条件付きの分岐は網羅に数えない") {
    assertLeftContains(
      validate(less + program("let f(o: Option[i32]): i32 = match o { Some(n) if n < 0 => 0  None => 1 }")),
      "no case matches Some(_)"
    )
    assertLeftContains(validate(less + program("let f(n: i32): i32 = match n { m if m < 0 => 0 }")), "no case matches _")
  }

  test("条件は bool でなければならず、パターンの変数を使える") {
    assertLeftContains(validate(program("let f(o: Option[i32]): i32 = match o { Some(n) if n => 0  _ => 1 }")), "Type mismatch")
    assertLeftContains(validate(program("let f(o: Option[i32]): i32 = match o { Some(n) if m => 0  _ => 1 }")), "Variable m is not defined")
  }

  test("条件付きの分岐の後でも、同じパターンの分岐は到達可能") {
    assertRight(encode(less + program("let f(n: i32): i32 = match n { m if m < 0 => 0  m => m }")))
    assertLeftContains(validate(less + program("let f(n: i32): i32 = match n { m => m  k if k < 0 => 0 }")), "case 2 is unreachable")
  }

  test("後続の分岐の本体は、条件付きの分岐のパターン変数を捕まえない") {
    val shown = lowered(less + program("let x: i32 = 7\nlet f(o: Option[i32]): i32 = match o { Some(x) if x < 0 => 0  _ => x }")).fold(fail(_), _.show)
    val line = shown.linesIterator.find(_.startsWith("let f:")).getOrElse(fail(shown))
    // 後続の分岐の本体（外側の x を返す）は、パターンの x を束縛する let の外に置かれる
    assert(line.contains("(x)[i32]") && line.indexOf("(x)[i32]") < line.indexOf("let x: i32"), clue(line))
  }

  // ---- 束縛でのパターン ----

  test("let パターン: 値を分解して束縛する") {
    assertRight(encode(program("let sum(p: Pair[i32][i32]): i32 = let MkPair(a)(b): Pair[i32][i32] = p in a")))
    assertRight(encode(program("let deep(p: Pair[Pair[i32][bool]][i32]): bool = let MkPair(MkPair(_)(flag))(_): Pair[Pair[i32][bool]][i32] = p in flag")))
    assertRight(encode(program("let ignore(n: i32): i32 = let _: i32 = n in 0")))
  }

  test("let パターン: 脱糖の後は match になる") {
    val shown = lowered(program("let first(p: Pair[i32][i32]): i32 = let MkPair(a)(_): Pair[i32][i32] = p in a")).fold(fail(_), _.show)
    assert(shown.linesIterator.exists(l => l.startsWith("let first:") && l.contains("match")), clue(shown))
  }

  test("let パターン: すべての値に当たらないパターンは拒否") {
    assertLeftContains(
      validate(program("let get(o: Option[i32]): i32 = let Some(x): Option[i32] = o in x")),
      "A binding pattern must match every value: None is not matched"
    )
    assertLeftContains(
      validate(program("let get(p: Pair[Option[i32]][i32]): i32 = let MkPair(Some(x))(_): Pair[Option[i32]][i32] = p in x")),
      "MkPair(None)(_) is not matched"
    )
  }

  test("let パターン: 注釈は値全体の型で、値とパターンの両方に合わなければならない") {
    assertLeftContains(validate(program("let f(p: Pair[i32][i32]): i32 = let MkPair(a)(b): Pair[i32][bool] = p in a")), "Type mismatch")
    assertLeftContains(validate(program("let f(o: Option[i32]): i32 = let MkPair(a)(b): Option[i32] = o in a")), "does not belong")
  }

  test("let の引数のない名前は、構成子と同じ名前でも変数の束縛のまま") {
    assertRight(encode(program("let f(o: Option[i32]): Option[i32] = let None: Option[i32] = o in None")))
  }

  test("関数の定義（let f(x: T): U）は、これまでどおり読める") {
    assertRight(encode(program("let g(n: i32): i32 = let double(m: i32): i32 = m in double(n)")))
  }

  private val optionMonad =
    """trait Functor[f: * → *] { def map[a][b](g: a → b)(x: f[a]): f[b] }
      |trait Applicative[f: * → *] with Functor[f] { def pure[a](x: a): f[a] }
      |trait Monad[m: * → *] with Applicative[m] { def flatMap[a][b](x: m[a])(g: a → m[b]): m[b] }
      |impl Functor[Option] { def map = Λa. Λb. λg: a → b. λx: Option[a]. match x { None => None[b]  Some(v) => Some[b](g(v)) } }
      |impl Applicative[Option] { def pure = Λa. λx: a. Some[a](x) }
      |impl Monad[Option] { def flatMap = Λa. Λb. λx: Option[a]. λg: a → Option[b]. match x { None => None[b]  Some(v) => g(v) } }
      |""".stripMargin

  test("context の中のパターン: モナドの束縛と let の両方で分解できる") {
    val src = optionMonad +
      """let f(o: Option[Pair[i32][bool]]): Option[i32] = context[Option] {
        |  MkPair(n)(flag): Pair[i32][bool] = o;
        |  let MkPair(a)(b): Pair[i32][i32] = MkPair[i32][i32](n)(n);
        |  if flag then a else b
        |}""".stripMargin
    assertRight(encode(program(src)))
  }

  test("context の中のパターンも、すべての値に当たらなければ拒否") {
    val src = optionMonad +
      """let f(o: Option[Option[i32]]): Option[i32] = context[Option] {
        |  Some(n): Option[i32] = o;
        |  n
        |}""".stripMargin
    assertLeftContains(validate(program(src)), "None is not matched")
  }

  // ---- パーサ ----

  test("キーワードで始まる識別子（letter / database / iffy / matcher / typed）を使える") {
    val src = "let letter: i32 = 1\nlet database: i32 = letter\nlet iffy: i32 = database\nlet matcher: i32 = iffy\nlet typed: i32 = matcher\n"
    assertRight(validate(src + main))
  }
}
