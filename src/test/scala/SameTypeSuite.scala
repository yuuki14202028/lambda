package com.yuuki14202028

class SameTypeSuite extends munit.FunSuite {

  test("sameType: プリミティブ型に対して反射的である") {
    assert(Equivalence.alpha(intTypeT, intTypeT))
    assert(Equivalence.alpha(boolTypeT, boolTypeT))
  }

  test("sameType: 異なるプリミティブ型は区別される") {
    assert(!Equivalence.alpha(intTypeT, boolTypeT))
  }

  test("sameType: ForAll に対して α 同値である") {
    val a = TypeVariable("a")
    val b = TypeVariable("b")
    val k = Kind.Star
    val idA = forallTypeT(a, k, arrowT(typeVarT(a), typeVarT(a)))
    val idB = forallTypeT(b, k, arrowT(typeVarT(b), typeVarT(b)))
    assert(Equivalence.alpha(idA, idB))
  }

  // !bound.contains(rv) は Map の key(=左側の束縛変数)しか見ていないため、
  // 「左で自由な変数」と「右で束縛された変数」が同名のとき誤って同値と判定する。
  test("sameType: 左で自由・右で束縛された同名変数を区別する") {
    val a = TypeVariable("a")
    val b = TypeVariable("b")
    val k = Kind.Star
    // ∀b. a  … body は自由変数 a(b は未使用)
    val left = forallTypeT(b, k, typeVarT(a))
    // ∀a. a  … body は束縛変数 a
    val right = forallTypeT(a, k, typeVarT(a))
    // 左は a が自由な開いた型、右は閉じた多相型なので α 同値ではない。
    assert(!Equivalence.alpha(left, right))
  }

  // Some(b) => b == rv も、右側で同名変数がシャドウイングされると誤判定する。
  // bound は単方向 Map なので、複数の左変数が同じ右変数を value に持ち得る。
  test("sameType: 右側のシャドウイングを区別する") {
    val x = TypeVariable("x")
    val y = TypeVariable("y")
    val a = TypeVariable("a")
    val k = Kind.Star
    // ∀x. ∀y. x  … body は外側の x
    val left = forallTypeT(x, k, forallTypeT(y, k, typeVarT(x)))
    // ∀a. ∀a. a  … body は内側の a(外側をシャドウ)
    val right = forallTypeT(a, k, forallTypeT(a, k, typeVarT(a)))
    // 左 body は外側、右 body は内側の束縛子を指すので α 同値ではない。
    assert(!Equivalence.alpha(left, right))
  }

  // 上記の正常系: 内側・外側が正しく対応していれば α 同値。
  test("sameType: ネストした ForAll で外側の束縛変数が一致する") {
    val x = TypeVariable("x")
    val y = TypeVariable("y")
    val a = TypeVariable("a")
    val b = TypeVariable("b")
    val k = Kind.Star
    val left = forallTypeT(x, k, forallTypeT(y, k, typeVarT(x)))
    val right = forallTypeT(a, k, forallTypeT(b, k, typeVarT(a)))
    assert(Equivalence.alpha(left, right))
  }
}