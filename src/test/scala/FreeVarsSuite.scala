package com.yuuki14202028

class FreeVarsSuite extends munit.FunSuite {

  private val x = Variable("x")
  private val y = Variable("y")
  private val z = Variable("z")
  private val a = TypeVariable("a")
  private val b = TypeVariable("b")
  private val c = TypeVariable("c")
  private val k = Kind.Star

  // ---------- freeTypeVars ----------

  test("freeTypeVars(i32): プリミティブ型は自由型変数を持たない") {
    assertEquals(freeTypeVars(intTypeT), Set.empty[TypeVariable])
  }

  test("freeTypeVars(a): 単独の TypeVar はそれ自身が自由") {
    assertEquals(freeTypeVars(typeVarT(a)), Set(a))
  }

  test("freeTypeVars(a -> b): Arrow は両側の自由型変数を合併する") {
    val t = arrowT(typeVarT(a), typeVarT(b))
    assertEquals(freeTypeVars(t), Set(a, b))
  }

  test("freeTypeVars(∀a: *. a -> b): ForAll は束縛した型変数を除外する") {
    val t = forallTypeT(a, k, arrowT(typeVarT(a), typeVarT(b)))
    assertEquals(freeTypeVars(t), Set(b))
  }

  test("freeTypeVars(∀a: *. ∀b: *. a -> b): ForAll の入れ子で完全に閉じている") {
    val t = forallTypeT(a, k, forallTypeT(b, k, arrowT(typeVarT(a), typeVarT(b))))
    assertEquals(freeTypeVars(t), Set.empty[TypeVariable])
  }

  test("freeTypeVars(λa: *. a[c]): TypeAbs も束縛する") {
    val t = typeAbsT(a, k, typeAppT(typeVarT(a), typeVarT(c)))
    assertEquals(freeTypeVars(t), Set(c))
  }

  test("freeTypeVars(a[b]): TypeApp は関数側と引数側を合併する") {
    val t = typeAppT(typeVarT(a), typeVarT(b))
    assertEquals(freeTypeVars(t), Set(a, b))
  }

  test("freeTypeVars(∀a: *. a -> ∀a: *. a -> b): シャドウィング下でも外側の自由型変数は残る") {
    val inner = forallTypeT(a, k, arrowT(typeVarT(a), typeVarT(b)))
    val outer = forallTypeT(a, k, arrowT(typeVarT(a), inner))
    assertEquals(freeTypeVars(outer), Set(b))
  }

  // ---------- freeVars ----------

  test("freeVars(x): 単独の Var はそれ自身が自由") {
    val expr = varrType(x, intTypeT)
    assertEquals(freeVars(expr), Set(x))
  }

  test("freeVars(1) / freeVars(true) / freeVars(()): リテラルは自由変数を持たない") {
    assertEquals(freeVars(numT("1", "i32", intTypeT)), Set.empty[Variable])
    assertEquals(freeVars(boolT(true, boolTypeT)), Set.empty[Variable])
    assertEquals(freeVars(unitLitT(unitTypeT)), Set.empty[Variable])
  }

  test("freeVars(λx: i32. x): Abs は仮引数を束縛する") {
    val expr = absT(x, intTypeT, intTypeT, varrType(x, intTypeT))
    assertEquals(freeVars(expr), Set.empty[Variable])
  }

  test("freeVars(λx: i32. y): Abs の本体で参照される自由変数は残る") {
    val expr = absT(x, intTypeT, intTypeT, varrType(y, intTypeT))
    assertEquals(freeVars(expr), Set(y))
  }

  test("freeVars(x y): App は関数側と引数側の自由変数を合併する") {
    val expr = appT(intTypeT, varrType(x, intTypeT), varrType(y, intTypeT))
    assertEquals(freeVars(expr), Set(x, y))
  }

  test("freeVars(let x: i32 = x in x): Let は本体側だけ束縛する (value 側では自由)") {
    val expr = letT(x, intTypeT, intTypeT, varrType(x, intTypeT), varrType(x, intTypeT))
    assertEquals(freeVars(expr), Set(x))
  }

  test("freeVars(letrec x: i32 = x in x): LetRec は value と body の両方で束縛する") {
    val expr = letRecT(x, intTypeT, intTypeT, varrType(x, intTypeT), varrType(x, intTypeT))
    assertEquals(freeVars(expr), Set.empty[Variable])
  }

  test("freeVars(letrec x: i32 = y in x): LetRec の value で他の自由変数は残る") {
    val expr = letRecT(x, intTypeT, intTypeT, varrType(y, intTypeT), varrType(x, intTypeT))
    assertEquals(freeVars(expr), Set(y))
  }

  test("freeVars(Λa: *. x): TyAbs は項変数を束縛しない") {
    val expr = tyAbsT(a, intTypeT, k, varrType(x, intTypeT))
    assertEquals(freeVars(expr), Set(x))
  }

  test("freeVars(if x then y else z): If は cond / then / else を合併する") {
    val expr = ifT(
      intTypeT,
      varrType(x, boolTypeT),
      varrType(y, intTypeT),
      varrType(z, intTypeT)
    )
    assertEquals(freeVars(expr), Set(x, y, z))
  }

  test("freeVars(x + y): BinOp は左右を合併する") {
    val expr = binopT(BinOps.Add, intTypeT, varrType(x, intTypeT), varrType(y, intTypeT))
    assertEquals(freeVars(expr), Set(x, y))
  }

  test("freeVars(-x): UnaryOp は本体を伝播する") {
    val expr = unopT(UnaryOps.Neg, intTypeT, varrType(x, intTypeT))
    assertEquals(freeVars(expr), Set(x))
  }

  test("freeVars({ x; y; z }): Block は discarded と result を合併する") {
    val expr = blockT(
      intTypeT,
      Seq(varrType(x, intTypeT), varrType(y, intTypeT)),
      Some(varrType(z, intTypeT))
    )
    assertEquals(freeVars(expr), Set(x, y, z))
  }

  test("freeVars({ x; }): Block で result が None なら discarded のみ") {
    val expr = blockT(unitTypeT, Seq(varrType(x, intTypeT)), None)
    assertEquals(freeVars(expr), Set(x))
  }

  test("freeVars(foreign printf: i32): Foreign は自由変数を持たない") {
    val expr = foreignT(Variable("printf"), intTypeT, intTypeT)
    assertEquals(freeVars(expr), Set.empty[Variable])
  }

  test("freeVars(match x with | Cons y z -> y): Match は scrutinee と各 case を合併し、binders を case 内で束縛する") {
    val matchCase = MatchCase[TypeRec](
      constructor = Variable("Cons"),
      binders = Seq(y, z),
      body = varrType(y, intTypeT)
    )
    val expr = matchExprT(intTypeT, varrType(x, intTypeT), Seq(matchCase))
    assertEquals(freeVars(expr), Set(x))
  }

  test("freeVars(match x with | Cons y -> z): Match の case 本体で binders 外の変数は自由のまま") {
    val matchCase = MatchCase[TypeRec](
      constructor = Variable("Cons"),
      binders = Seq(y),
      body = varrType(z, intTypeT)
    )
    val expr = matchExprT(intTypeT, varrType(x, intTypeT), Seq(matchCase))
    assertEquals(freeVars(expr), Set(x, z))
  }

  test("freeVars('a') / freeVars(\"abc\"): Char / StringLit は自由変数を持たない") {
    assertEquals(freeVars(charT('a', charTypeT)), Set.empty[Variable])
    assertEquals(freeVars(stringLitT("abc", stringTypeT)), Set.empty[Variable])
  }

  test("freeVars(x [i32]): TyApp は関数側のみ伝播する (型引数は項自由変数に寄与しない)") {
    val expr = tyAppT(intTypeT, varrType(x, intTypeT), intTypeT)
    assertEquals(freeVars(expr), Set(x))
  }

  test("freeVars(intrinsic add(x, y)): Intrinsic は引数列を合併する") {
    val op = IntrinsicOps.BinOp(BinOps.Add, "i32")
    val expr = intrinsicT(op, intTypeT, Seq(varrType(x, intTypeT), varrType(y, intTypeT)))
    assertEquals(freeVars(expr), Set(x, y))
  }

  test("freeVars(type T = i32 in x): TypeLet は本体のみ走査する") {
    val expr = typeLetT(TypeVariable("T"), Seq.empty, intTypeT, intTypeT, varrType(x, intTypeT))
    assertEquals(freeVars(expr), Set(x))
  }

  test("freeVars(data D = ... in x): DataLet は本体のみ走査する") {
    val expr = dataLetT(
      TypeVariable("D"),
      Seq.empty,
      intTypeT,
      Seq(DataConstructor[TypeRec](Variable("MkD"), Seq.empty)),
      varrType(x, intTypeT)
    )
    assertEquals(freeVars(expr), Set(x))
  }

  test("freeVars(fold x: i32 with | Cons y z -> y): Fold も Match と同様に binders を束縛する") {
    val foldCase = MatchCase[TypeRec](
      constructor = Variable("Cons"),
      binders = Seq(y, z),
      body = varrType(y, intTypeT)
    )
    val expr = foldExprT(intTypeT, varrType(x, intTypeT), intTypeT, Seq(foldCase))
    assertEquals(freeVars(expr), Set(x))
  }

  test("freeVars(fold x: i32 with | Cons y -> z): Fold の case 本体で binders 外の変数は自由のまま") {
    val foldCase = MatchCase[TypeRec](
      constructor = Variable("Cons"),
      binders = Seq(y),
      body = varrType(z, intTypeT)
    )
    val expr = foldExprT(intTypeT, varrType(x, intTypeT), intTypeT, Seq(foldCase))
    assertEquals(freeVars(expr), Set(x, z))
  }

  test("freeVars(λx: i32. λy: i32. x + y + z): ネストした Abs はそれぞれ束縛する") {
    val body = binopT(
      BinOps.Add,
      intTypeT,
      binopT(BinOps.Add, intTypeT, varrType(x, intTypeT), varrType(y, intTypeT)),
      varrType(z, intTypeT)
    )
    val expr = absT(x, intTypeT, intTypeT, absT(y, intTypeT, intTypeT, body))
    assertEquals(freeVars(expr), Set(z))
  }
}