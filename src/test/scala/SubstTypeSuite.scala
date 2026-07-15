package com.yuuki14202028

class SubstTypeSuite extends munit.FunSuite {

  private val a = TypeVariable("a")
  private val al = TypeVariable("a'")
  private val b = TypeVariable("b")
  private val bl = TypeVariable("b'")
  private val c = TypeVariable("c")
  private val cl = TypeVariable("c'")
  private val x = TypeVariable("x")
  private val k = Kind.Star

  test("freshTypeVariable(a, Set.empty): aをa'に写す") {
    assertEquals(freshTypeVariable(a, Set.empty), TypeVariable("a'"))
  }

  test("freshTypeVariable(a, Set(a)): aをa'に写す") {
    assertEquals(freshTypeVariable(a, Set(a)), TypeVariable("a'"))
  }

  test("freshTypeVariable(a, Set(b)): aをa'に写す") {
    assertEquals(freshTypeVariable(a, Set(b)), TypeVariable("a'"))
  }

  test("freshTypeVariable(a, Set(a, a')): a'は使用済みなので、aをa'1に写す") {
    assertEquals(freshTypeVariable(a, Set(a, al)), TypeVariable("a'1"))
  }

  test("substType(a, b, a): = b") {
    assertEquals(substType(a, typeVarT(b), typeVarT(a)), typeVarT(b))
  }

  test("substType(a, b, ∀c. a): = ∀c. b") {
    assertEquals(substType(a, typeVarT(b), forallTypeT(c, k, typeVarT(a))), forallTypeT(c, k, typeVarT(b)))
  }

  test("substType(a, b, ∀a. a): aは束縛されてシャドウされるので = ∀a. a") {
    assertEquals(substType(a, typeVarT(b), forallTypeT(a, k, typeVarT(a))), forallTypeT(a, k, typeVarT(a)))
  }

  test("substType(a, b, ∀b. a): replaceのbが束縛子bに捕まらないようα変換 = ∀b'. b") {
    assertEquals(substType(a, typeVarT(b), forallTypeT(b, k, typeVarT(a))), forallTypeT(bl, k, typeVarT(b)))
  }

  test("substType(x, a, ∀a. ∀b. (a → x)) = ") {
    val t = forallTypeT(a, k, forallTypeT(b, k, arrowT(typeVarT(a), typeVarT(x))))
    assertEquals(substType(x, typeVarT(a), t), forallTypeT(al, k, forallTypeT(b, k, arrowT(typeVarT(al), typeVarT(a)))))
  }

  test("substMany は同時代入: [a := b, b := c] を a → b に適用すると b → c") {
    // 逐次代入だと a := b の結果がさらに b := c で書き換えられ c → c になってしまう
    val result = substMany(Seq(a, b), Seq(typeVarT(b), typeVarT(c)), arrowT(typeVarT(a), typeVarT(b)))
    assert(Equivalence.alpha(result, arrowT(typeVarT(b), typeVarT(c))), clue(result.show))
  }

  test("substMany は同時代入: 引数が後続パラメータ名の関数型を含む場合") {
    // Either[a][b] のフィールドに [a := b, b := a → c] を適用するケース（match の束縛型計算に相当）
    val left = substMany(Seq(a, b), Seq(typeVarT(b), arrowT(typeVarT(a), typeVarT(c))), typeVarT(a))
    val right = substMany(Seq(a, b), Seq(typeVarT(b), arrowT(typeVarT(a), typeVarT(c))), typeVarT(b))
    assert(Equivalence.alpha(left, typeVarT(b)), clue(left.show))
    assert(Equivalence.alpha(right, arrowT(typeVarT(a), typeVarT(c))), clue(right.show))
  }
}
