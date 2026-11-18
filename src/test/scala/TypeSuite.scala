package com.yuuki14202028

/** 型の表現（Type.scala）: 束縛変数は番号なので α 同値は ==、代入は束縛子に捕まらず、型の適用は作るときに計算される。
 * 後半は、同じ性質をソースから確かめる（型の別名の展開とカインド）。
 */
class TypeSuite extends munit.FunSuite {
  import Type.{Arrow, Var}

  private val star = Kind.Star
  private val (a, b, c, x, y) = (new Symbol("a"), new Symbol("b"), new Symbol("c"), new Symbol("x"), new Symbol("y"))

  // ---- 等しさ（α 同値） ----

  test("プリミティブ型は自身と等しく、別のプリミティブ型とは異なる") {
    assertEquals(Type.i32, Type.Primitive("i32"))
    assertNotEquals(Type.i32, Type.bool)
  }

  test("∀ は束縛変数の名前によらず等しい") {
    assertEquals(Type.forall(a, star, Arrow(Var(a), Var(a))), Type.forall(b, star, Arrow(Var(b), Var(b))))
  }

  test("自由な変数と束縛された変数を区別する") {
    // ∀b. a（a は自由）と ∀a. a
    assertNotEquals(Type.forall(b, star, Var(a)), Type.forall(a, star, Var(a)))
  }

  test("同じ名前の束縛子が入れ子になっても、どちらを指すかを区別する") {
    val inner = new Symbol("a")
    // ∀x. ∀y. x と ∀a. ∀a. a（内側の a）
    val outerBound = Type.forall(x, star, Type.forall(y, star, Var(x)))
    val innerBound = Type.forall(a, star, Type.forall(inner, star, Var(inner)))
    assertNotEquals(outerBound, innerBound)
    assertEquals(outerBound, Type.forall(a, star, Type.forall(b, star, Var(a))))
  }

  test("同じ綴りでも、別の Symbol は別の型変数") {
    assertNotEquals(Var(a): Type, Var(new Symbol("a")): Type)
  }

  // ---- 代入 ----

  test("substitute: 変数を型に置き換える") {
    assertEquals(Type.substitute(Var(a), Map(a -> Var(b))), Var(b))
    assertEquals(Type.substitute(Type.forall(c, star, Var(a)), Map(a -> Var(b))), Type.forall(c, star, Var(b)))
  }

  test("substitute: 束縛された変数は置き換えない") {
    val identity = Type.forall(a, star, Var(a))
    assertEquals(Type.substitute(identity, Map(a -> Var(b))), identity)
  }

  test("substitute: 置き換える型の変数は、束縛子に捕まらない") {
    // (∀b. a)[a := b] は ∀b'. b で、∀b. b ではない
    val result = Type.substitute(Type.forall(b, star, Var(a)), Map(a -> Var(b)))
    assertEquals(result, Type.forall(c, star, Var(b)))
    assertNotEquals(result, Type.forall(b, star, Var(b)))
    assertEquals(Type.variables(result), Set(b))
  }

  test("substitute: 入れ子の束縛子の下でも捕まらない") {
    // (∀a. ∀b. a → x)[x := a] = ∀a'. ∀b. a' → a
    val bound = new Symbol("a")
    val tpe = Type.forall(bound, star, Type.forall(b, star, Arrow(Var(bound), Var(x))))
    val expected = Type.forall(c, star, Type.forall(b, star, Arrow(Var(c), Var(a))))
    assertEquals(Type.substitute(tpe, Map(x -> Var(a))), expected)
  }

  test("substitute は同時代入: [a := b, b := c] を a → b に適用すると b → c") {
    assertEquals(Type.substitute(Arrow(Var(a), Var(b)), Map(a -> Var(b), b -> Var(c))), Arrow(Var(b), Var(c)))
  }

  test("substitute は同時代入: 置き換える型が、他の置き換えられる変数を含む場合") {
    val replacements = Map(a -> Var(b), b -> Arrow(Var(a), Var(c)))
    assertEquals(Type.substitute(Var(a), replacements), Var(b))
    assertEquals(Type.substitute(Var(b), replacements), Arrow(Var(a), Var(c)))
  }

  test("open: ∀a. body の束縛変数に型を渡す") {
    Type.forall(a, star, Arrow(Var(a), Type.forall(b, star, Arrow(Var(b), Var(a))))) match {
      case Type.Forall(_, _, body) =>
        assertEquals(Type.open(body, Type.i32), Arrow(Type.i32, Type.forall(b, star, Arrow(Var(b), Type.i32))))
      case other => fail(Type.show(other))
    }
  }

  // ---- 正規形 ----

  test("型レベルの λ の適用は、作るときに計算される") {
    val function = Type.lambda(a, star, Type.lambda(b, star, Arrow(Var(a), Var(b))))
    assertEquals(Type.apply(Type.apply(function, Type.i32), Type.bool), Arrow(Type.i32, Type.bool))
    // 部分適用は λ のまま
    assertEquals(Type.apply(function, Type.i32), Type.lambda(b, star, Arrow(Type.i32, Var(b))))
  }

  test("代入でできた λ の適用も計算される") {
    val f = new Symbol("f")
    val applied = Type.apply(Var(f), Type.i32)
    assertEquals(applied, Type.Apply(Var(f), Type.i32))
    assertEquals(Type.substitute(applied, Map(f -> Type.lambda(a, star, Arrow(Var(a), Var(a))))), Arrow(Type.i32, Type.i32))
  }

  // ---- 型の中の変数 ----

  test("variables: プリミティブ型は変数を持たず、変数はそれ自身") {
    assertEquals(Type.variables(Type.i32), Set.empty[Symbol])
    assertEquals(Type.variables(Var(a)), Set(a))
  }

  test("variables: → と適用は両側を合併する") {
    assertEquals(Type.variables(Arrow(Var(a), Var(b))), Set(a, b))
    assertEquals(Type.variables(Type.apply(Var(a), Var(b))), Set(a, b))
  }

  test("variables: ∀ と λ が束縛した変数は含まない") {
    assertEquals(Type.variables(Type.forall(a, star, Arrow(Var(a), Var(b)))), Set(b))
    assertEquals(Type.variables(Type.forall(a, star, Type.forall(b, star, Arrow(Var(a), Var(b))))), Set.empty[Symbol])
    assertEquals(Type.variables(Type.lambda(a, Kind.Arrow(star, star), Type.apply(Var(a), Var(c)))), Set(c))
  }

  test("variables: 同じ名前の束縛子の下でも、外側の自由な変数は残る") {
    val inner = new Symbol("a")
    val tpe = Type.forall(a, star, Arrow(Var(a), Type.forall(inner, star, Arrow(Var(inner), Var(b)))))
    assertEquals(Type.variables(tpe), Set(b))
  }

  // ---- ソースから: 注釈の等しさ ----

  private def assertError(source: String)(expected: PartialFunction[CompileError, Unit])(using munit.Location): Unit = {
    val found = Lam.errors(source)
    assert(found.exists(expected.isDefinedAt), clue(found.map(_.message)))
  }

  test("束縛変数の名前だけが違う注釈は、同じ型") {
    val source =
      """let first: ∀x. ∀y. x → y → x = Λa. Λb. λleft: a. λright: b. left
        |let renamed: ∀p. ∀q. p → q → p = first
        |let main(): i32 = renamed(7)(true)
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 7)
  }

  test("束縛変数の指す先が違う注釈は、違う型") {
    val source =
      """let first: ∀x. ∀y. x → y → x = Λa. Λb. λleft: a. λright: b. left
        |let second: ∀a. ∀b. a → b → b = first
        |let main(): i32 = 0
        |""".stripMargin
    assertError(source) { case _: CompileError.TypeMismatch => }
  }

  // ---- ソースから: 型の別名の展開（別名は型レベルの λ） ----

  test("エイリアスの完全適用は β 正規化で展開される") {
    val source =
      """type Fn2[a][b] = a → b
        |let g: Fn2[i32][bool] = λx: i32. true
        |let h: i32 → bool = g
        |let main(): i32 = if h(1) then 1 else 0
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 1)
  }

  test("エイリアスの部分適用は高カインド位置で合法") {
    val source =
      """type Fn2[a][b] = a → b
        |type Apply[f: * → *][a] = f[a]
        |let g: Apply[Fn2[i32]][bool] = λx: i32. true
        |let main(): i32 = if g(1) then 1 else 0
        |""".stripMargin
    assertEquals(Lam.exitCode(source), 1)
  }

  test("部分適用されたエイリアスを * 位置に書くとカインドエラーになる") {
    val source =
      """type Fn2[a][b] = a → b
        |let g: Fn2[i32] = λx: i32. true
        |let main(): i32 = 0
        |""".stripMargin
    assertError(source) { case CompileError.KindMismatch(Kind.Star, Kind.Arrow(Kind.Star, Kind.Star), _) => }
  }

  test("エイリアスの過剰適用はカインドエラーになる") {
    val source =
      """type Id[a] = a
        |let g: Id[i32][bool] = λx: i32. true
        |let main(): i32 = 0
        |""".stripMargin
    assertError(source) { case _: CompileError.KindMismatch => }
  }

  test("未定義の型名は名前解決エラーになる") {
    val source =
      """let g: Unknown[i32] = λx: i32. true
        |let main(): i32 = 0
        |""".stripMargin
    assertError(source) { case CompileError.UndefinedType("Unknown", _) => }
  }
}
