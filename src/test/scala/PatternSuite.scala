package com.yuuki14202028

/** match のパターン（入れ子・リテラル・選択肢・条件）、網羅性の検査、束縛でのパターン */
class PatternSuite extends munit.FunSuite {

  private val prelude =
    """trait Eq[a] { def eq(x: a)(y: a): bool }
      |infix 4 == = eq
      |impl Eq[i32] { def eq(a: i32)(b: i32): bool = intrinsic[eq_i32](a)(b) }
      |let addInt(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y)
      |let ltInt(x: i32)(y: i32): bool = intrinsic[lt_i32](x)(y)
      |let negInt(x: i32): i32 = intrinsic[neg_i32](x)
      |infixl 6 + = addInt
      |prefix - = negInt
      |infix 4 < = ltInt
      |data Option[a] = { None, Some(a) }
      |rec data List[a] = { Nil, Cons(a)(List[a]) }
      |data Pair[a][b] = { MkPair(a)(b) }
      |""".stripMargin

  /** 宣言と、main の結果になる式 */
  private def result(declarations: String, main: String)(using munit.Location): Int =
    Lam.exitCode(s"$prelude$declarations\nlet main(): i32 = $main\n")

  private def assertError(declarations: String)(expected: PartialFunction[CompileError, Unit])(using munit.Location): Unit = {
    val found = Lam.errors(s"$prelude$declarations\nlet main(): i32 = 0\n")
    assert(found.exists(expected.isDefinedAt), clue(found.map(_.message)))
  }

  private def assertNotMatched(declarations: String, example: String)(using munit.Location): Unit =
    assertError(declarations) { case CompileError.NonExhaustiveMatch(`example`, _, _) => }

  private def assertUnreachable(declarations: String, index: Int)(using munit.Location): Unit =
    assertError(declarations) { case CompileError.UnreachableCase(`index`, _) => }

  private def assertMismatch(declarations: String)(using munit.Location): Unit =
    assertError(declarations) { case _: CompileError.TypeMismatch => }

  // ---- 書けるパターン ----

  test("入れ子の構成子パターン") {
    val f =
      """let f(xs: List[i32]): i32 = match xs {
        |  Cons(x)(Cons(y)(rest)) => x + y
        |  Cons(x)(Nil) => x
        |  Nil => 0
        |}""".stripMargin
    assertEquals(result(f, "f(Cons(1)(Cons(2)(Cons(4)(Nil[i32]))))"), 3)
    assertEquals(result(f, "f(Cons(5)(Nil[i32]))"), 5)
    assertEquals(result(f, "f(Nil[i32])"), 0)
  }

  test("変数と _ はどの値にも当たる") {
    val orZero = "let f(o: Option[i32]): i32 = match o { Some(x) => x\n _ => 0 }"
    assertEquals(result(orZero, "f(Some(3)) + f(None[i32])"), 3)
    val whole = "let f(o: Option[i32]): Option[i32] = match o { whole => whole }"
    assertEquals(result(whole, "match f(Some(4)) { Some(n) => n\n None => 0 }"), 4)
    assertEquals(result("let f(n: i32): i32 = match n { m => m }", "f(6)"), 6)
  }

  test("リテラルのパターン（整数・bool）") {
    val number = "let f(n: i32): i32 = match n { 0 => 1\n 1 => 2\n _ => 3 }"
    assertEquals(result(number, "f(0) + f(1) + f(7)"), 6)
    val boolean = "let f(b: bool): i32 = match b { true => 1\n false => 0 }"
    assertEquals(result(boolean, "f(true) + f(false)"), 1)
    val nested = "let f(o: Option[i32]): i32 = match o { Some(0) => 10\n Some(n) => n\n None => 20 }"
    assertEquals(result(nested, "f(Some(0)) + f(Some(4)) + f(None[i32])"), 34)
  }

  test("引数のない名前は、スコープ中の構成子なら構成子、そうでなければ変数") {
    val f = "let f(o: Option[i32]): i32 = match o { None => 0\n other => 1 }"
    assertEquals(result(f, "f(None[i32]) + f(Some(5))"), 1)
    assertError("let f(o: Option[i32]): i32 = match o { None => 0\n Nil => 1 }") {
      case CompileError.PatternConstructorMismatch("Nil", _, _) =>
    }
  }

  test("パターンの変数は本体で使える（型はその位置の型）") {
    val f =
      """let f(p: Pair[i32][Option[bool]]): bool = match p {
        |  MkPair(n)(Some(b)) => b
        |  MkPair(n)(None) => false
        |}""".stripMargin
    assertEquals(result(f, "if f(MkPair(1)(Some(true))) then 1 else 0"), 1)
    assertMismatch("let f(p: Pair[i32][bool]): i32 = match p { MkPair(n)(b) => b }")
  }

  // ---- 網羅性と到達不能 ----

  test("網羅していなければ、当たらない値の例を示す") {
    assertNotMatched("let f(o: Option[i32]): i32 = match o { Some(x) => x }", "None")
    assertNotMatched("let f(xs: List[i32]): i32 = match xs { Cons(x)(Nil) => x\n Nil => 0 }", "Cons(_)(Cons(_)(_))")
    assertNotMatched("let f(n: i32): i32 = match n { 0 => 1\n 1 => 2 }", "_")
    assertNotMatched("let f(b: bool): i32 = match b { true => 1 }", "false")
    assertNotMatched(
      "let f(p: Pair[bool][bool]): i32 = match p { MkPair(true)(true) => 1\n MkPair(false)(_) => 0 }",
      "MkPair(true)(false)"
    )
  }

  test("到達不能な分岐は拒否") {
    assertUnreachable("let f(o: Option[i32]): i32 = match o { Some(x) => x\n None => 0\n None => 1 }", 3)
    assertUnreachable("let f(o: Option[i32]): i32 = match o { _ => 0\n None => 1 }", 2)
    assertUnreachable("let f(n: i32): i32 = match n { 0 => 1\n 0 => 2\n _ => 3 }", 2)
  }

  // ---- 型エラー ----

  test("構成子の引数の数が合わない") {
    assertError("let f(o: Option[i32]): i32 = match o { Some(x)(y) => x\n None => 0 }") {
      case CompileError.ConstructorArity("Some", 1, 2, _) =>
    }
  }

  test("構成子でない名前に引数は付けられない") {
    assertError("let f(o: Option[i32]): i32 = match o { foo(x) => x\n None => 0 }") {
      case CompileError.NotAConstructor("foo", _) =>
    }
  }

  test("同じ変数を 1 つのパターンで二度束縛できない") {
    assertError("let f(p: Pair[i32][i32]): i32 = match p { MkPair(x)(x) => x }") {
      case CompileError.PatternBindsTwice("x", _) =>
    }
  }

  test("リテラルの型がスクルーティニーの型と合わない") {
    assertMismatch("let f(b: bool): i32 = match b { 0 => 1\n _ => 2 }")
    assertMismatch("let f(o: Option[i32]): i32 = match o { 0 => 1\n _ => 2 }")
  }

  test("データ型でない値を構成子で分解できない") {
    assertError("let f(n: i32): i32 = match n { Some(x) => x\n _ => 0 }") {
      case _: CompileError.NotADataType =>
      case _: CompileError.PatternConstructorMismatch =>
    }
  }

  test("リテラルのパターンは == に宣言された関数で比べる（宣言がなければエラー）") {
    val source =
      """let f(n: i32): i32 = match n { 0 => 1
        |  _ => 2 }
        |let main(): i32 = 0
        |""".stripMargin
    Lam.errors(source) match {
      case List(CompileError.UndefinedOperator("==", false, _)) => ()
      case other => fail(other.map(_.message).toString)
    }
  }

  // ---- 負の数・文字列・選択肢・条件 ----

  test("負の数のリテラル（直前の本体と引き算にならない）") {
    val f = "let f(n: i32): i32 = match n {\n  0 => n\n  -1 => 1\n  _ => 2\n}"
    assertEquals(result(f, "f(0) + f(-1) + f(5)"), 3)
  }

  test("文字列のリテラルも == で比べる（Eq[String] のインスタンスが要る）") {
    val eqString =
      """let sameString: foreign.C.String → foreign.C.String → bool = foreign[foreign.C.String → foreign.C.String → bool] string_equal
        |impl Eq[foreign.C.String] { def eq(x: foreign.C.String)(y: foreign.C.String): bool = sameString(x)(y) }
        |""".stripMargin
    val f = "let f(s: foreign.C.String): i32 = match s { \"a\" => 1\n \"b\" => 2\n _ => 0 }\n"
    assertEquals(result(eqString + f, "f(\"a\") + f(\"b\") + f(\"b\") + f(\"c\")"), 5)
    assertError(f) { case error: CompileError.NoInstance if error.message.contains("Eq[foreign.C.String]") => }
    assertNotMatched(eqString + "let f(s: foreign.C.String): i32 = match s { \"a\" => 1 }", "_")
  }

  test("p | q: 複数のパターンを 1 つの本体にまとめる") {
    val number = "let f(n: i32): i32 = match n { 0 | 1 | 2 => 1\n _ => 0 }"
    assertEquals(result(number, "f(0) + f(1) + f(2) + f(3)"), 3)
    val nested = "let f(o: Option[i32]): i32 = match o { Some(0 | 1) => 1\n Some(_) | None => 0 }"
    assertEquals(result(nested, "f(Some(0)) + f(Some(1)) + f(Some(2)) + f(None[i32])"), 2)
    val binding = "let f(p: Pair[Option[i32]][i32]): i32 = match p { MkPair(Some(x))(_) | MkPair(None)(x) => x }"
    assertEquals(result(binding, "f(MkPair(Some(3))(10)) + f(MkPair(None[i32])(4))"), 7)
  }

  test("p | q: どの選択肢も同じ変数を同じ型で束縛する") {
    assertError("let f(o: Option[i32]): i32 = match o { Some(x) | None => 0 }") {
      case CompileError.AlternativesBindDifferently("x", _) =>
    }
    assertError("let f(p: Pair[i32][bool]): i32 = match p { MkPair(x)(_) | MkPair(_)(x) => 0 }") {
      case _: CompileError.AlternativesBindDifferently =>
      case _: CompileError.TypeMismatch =>
    }
  }

  test("p | q: 網羅性は選択肢を合わせて調べる") {
    assertNotMatched(
      "let f(b: Pair[bool][bool]): i32 = match b { MkPair(true)(_) | MkPair(_)(true) => 1 }",
      "MkPair(false)(false)"
    )
  }

  test("条件付きの分岐: 条件が偽なら後続の分岐を試す") {
    val f = "let f(o: Option[i32]): i32 = match o { Some(n) if n < 0 => 0\n Some(n) => n\n None => 1 }"
    assertEquals(result(f, "f(Some(-5)) + f(Some(5)) + f(None[i32])"), 6)
  }

  test("条件付きの分岐は網羅に数えない") {
    assertNotMatched("let f(o: Option[i32]): i32 = match o { Some(n) if n < 0 => 0\n None => 1 }", "Some(_)")
    assertNotMatched("let f(n: i32): i32 = match n { m if m < 0 => 0 }", "_")
  }

  test("条件は bool でなければならず、パターンの変数を使える") {
    assertMismatch("let f(o: Option[i32]): i32 = match o { Some(n) if n => 0\n _ => 1 }")
    assertError("let f(o: Option[i32]): i32 = match o { Some(n) if m => 0\n _ => 1 }") {
      case CompileError.UndefinedVariable("m", _) =>
    }
  }

  test("条件付きの分岐の後でも、同じパターンの分岐は到達可能") {
    val f = "let f(n: i32): i32 = match n { m if m < 0 => 0\n m => m }"
    assertEquals(result(f, "f(-3) + f(3)"), 3)
    assertUnreachable("let f(n: i32): i32 = match n { m => m\n k if k < 0 => 0 }", 2)
  }

  test("後続の分岐の本体は、条件付きの分岐のパターン変数を捕まえない") {
    // 2 つ目の分岐の x は外側の x（7）
    val f = "let x: i32 = 7\nlet f(o: Option[i32]): i32 = match o { Some(x) if x < 0 => 0\n _ => x }"
    assertEquals(result(f, "f(Some(5))"), 7)
  }

  // ---- 束縛でのパターン ----

  test("let パターン: 値を分解して束縛する") {
    val sum = "let sum(p: Pair[i32][i32]): i32 = let MkPair(a)(b): Pair[i32][i32] = p in a + b"
    assertEquals(result(sum, "sum(MkPair(3)(4))"), 7)
    val deep =
      "let deep(p: Pair[Pair[i32][bool]][i32]): bool = let MkPair(MkPair(_)(flag))(_): Pair[Pair[i32][bool]][i32] = p in flag"
    assertEquals(result(deep, "if deep(MkPair(MkPair(1)(true))(2)) then 1 else 0"), 1)
    assertEquals(result("let ignore(n: i32): i32 = let _: i32 = n in 9", "ignore(1)"), 9)
  }

  test("let パターン: すべての値に当たらないパターンは拒否") {
    assertError("let get(o: Option[i32]): i32 = let Some(x): Option[i32] = o in x") {
      case CompileError.RefutablePattern("None", _) =>
    }
    assertError("let get(p: Pair[Option[i32]][i32]): i32 = let MkPair(Some(x))(_): Pair[Option[i32]][i32] = p in x") {
      case CompileError.RefutablePattern("MkPair(None)(_)", _) =>
    }
  }

  test("let パターン: 注釈は値全体の型で、値とパターンの両方に合わなければならない") {
    assertMismatch("let f(p: Pair[i32][i32]): i32 = let MkPair(a)(b): Pair[i32][bool] = p in a")
    assertError("let f(o: Option[i32]): i32 = let MkPair(a)(b): Option[i32] = o in a") {
      case CompileError.PatternConstructorMismatch("MkPair", _, _) =>
    }
  }

  test("let の引数のない名前は、構成子と同じ名前でも変数の束縛のまま") {
    val f = "let f(o: Option[i32]): Option[i32] = let None: Option[i32] = o in None"
    assertEquals(result(f, "match f(Some(8)) { Some(n) => n\n None => 0 }"), 8)
  }

  test("関数の定義（let f(x: T): U）は、これまでどおり読める") {
    val g = "let g(n: i32): i32 = let double(m: i32): i32 = m + m in double(n)"
    assertEquals(result(g, "g(4)"), 8)
  }

  private val optionMonad =
    """trait Functor[f: * → *] { def map[a][b](x: f[a])(g: a → b): f[b] }
      |trait Applicative[f: * → *] with Functor[f] { def pure[a](x: a): f[a] }
      |trait Monad[m: * → *] with Applicative[m] { def flatMap[a][b](x: m[a])(g: a → m[b]): m[b] }
      |impl Functor[Option] {
      |  def map = Λa. Λb. λx: Option[a]. λg: a → b. match x {
      |    None => None[b]
      |    Some(v) => Some[b](g(v))
      |  }
      |}
      |impl Applicative[Option] { def pure = Λa. λx: a. Some[a](x) }
      |impl Monad[Option] {
      |  def flatMap = Λa. Λb. λx: Option[a]. λg: a → Option[b]. match x {
      |    None => None[b]
      |    Some(v) => g(v)
      |  }
      |}
      |""".stripMargin

  test("context の中のパターン: モナドの束縛と let の両方で分解できる") {
    val f = optionMonad +
      """let f(o: Option[Pair[i32][bool]]): Option[i32] = context[Option] {
        |  MkPair(n)(flag): Pair[i32][bool] = o;
        |  let MkPair(a)(b): Pair[i32][i32] = MkPair[i32][i32](n)(n + 1);
        |  if flag then a else b
        |}""".stripMargin
    assertEquals(result(f, "match f(Some(MkPair(4)(false))) { Some(n) => n\n None => 0 }"), 5)
  }

  test("context の中のパターンも、すべての値に当たらなければ拒否") {
    val f = optionMonad +
      """let f(o: Option[Option[i32]]): Option[i32] = context[Option] {
        |  Some(n): Option[i32] = o;
        |  n
        |}""".stripMargin
    assertError(f) { case CompileError.RefutablePattern("None", _) => }
  }

  // ---- パーサ ----

  test("キーワードで始まる識別子（letter / database / iffy / matcher / typed）を使える") {
    val names = "let letter: i32 = 1\nlet database: i32 = letter\nlet iffy: i32 = database\nlet matcher: i32 = iffy\nlet typed: i32 = matcher"
    assertEquals(result(names, "typed"), 1)
  }
}
