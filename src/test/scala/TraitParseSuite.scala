package com.yuuki14202028

class TraitParseSuite extends munit.FunSuite {

  private def parseDecls(src: String): Seq[Rec[Decl]] =
    ParserAST.programParser.parseAll(src) match {
      case Right(p) => eraseIndex(p).unfix match { case AST.Program(decls) => decls }
      case Left(e)  => fail(s"parse failed: $e")
    }

  private def parseDecl(src: String): AST[Rec, Decl] = {
    val decls = parseDecls(src)
    assertEquals(decls.size, 1, clue(decls.map(_.show)))
    decls.head.unfix
  }

  test("trait: 単一メソッド（注釈なしパラメータ）") {
    parseDecl("trait Show[a] {\n  def show: a → foreign.C.String\n}\n") match {
      case AST.TopTrait(v, params, supers, methods) =>
        assertEquals(v, TypeVariable("Show"))
        assertEquals(params, Seq((TypeVariable("a"), Kind.Star)))
        assert(supers.isEmpty)
        assertEquals(methods.map(_.name), Seq(Variable("show")))
        assert(methods.forall(_.body.isEmpty))
      case other => fail(s"not TopTrait: $other")
    }
  }

  test("trait: 高階カインドパラメータと型パラメータ付きメソッド") {
    parseDecl("trait Functor[f: * → *] {\n  def fmap[a][b](ab: a → b): f[a] → f[b]\n}\n") match {
      case AST.TopTrait(v, params, supers, methods) =>
        assertEquals(v, TypeVariable("Functor"))
        assertEquals(params, Seq((TypeVariable("f"), Kind.Arrow(Kind.Star, Kind.Star))))
        assert(supers.isEmpty)
        assertEquals(methods.map(_.name), Seq(Variable("fmap")))
      case other => fail(s"not TopTrait: $other")
    }
  }

  test("trait: 複数メソッド") {
    parseDecl("trait Monad[m: * → *] {\n  def ret: ∀a. a → m[a]\n  def bind: ∀a. ∀b. m[a] → (a → m[b]) → m[b]\n}\n") match {
      case AST.TopTrait(_, _, _, methods) =>
        assertEquals(methods.map(_.name.name), Seq("ret", "bind"))
      case other => fail(s"not TopTrait: $other")
    }
  }

  test("trait: スーパークラス制約 with") {
    parseDecl("trait Ord[a] with Eq[a] {\n  def cmp(x: a)(y: a): i32\n}\n") match {
      case AST.TopTrait(v, _, supers, _) =>
        assertEquals(v, TypeVariable("Ord"))
        assertEquals(supers.map(_.name), Seq(TypeVariable("Eq")))
        assertEquals(supers.head.arg.map(_.show), Seq("a"))
      case other => fail(s"not TopTrait: $other")
    }
  }

  test("impl: 注釈なしメソッド") {
    parseDecl("impl Show[i32] {\n  def show = λx: i32. x\n}\n") match {
      case AST.TopImpl(v, params, targets, context, methods) =>
        assertEquals(v, TypeVariable("Show"))
        assert(params.isEmpty)
        assertEquals(targets.map(_.show), Seq("i32"))
        assert(context.isEmpty)
        assertEquals(methods.map(_.name), Seq(Variable("show")))
        assert(methods.head.sig.isEmpty)
      case other => fail(s"not TopImpl: $other")
    }
  }

  test("impl: 型/値パラメータと戻り値注釈付きメソッド") {
    parseDecl("impl Functor[Maybe] {\n  def fmap[a][b](ab: a → b): Maybe[a] → Maybe[b] = λm: Maybe[a]. m\n}\n") match {
      case AST.TopImpl(v, _, targets, _, methods) =>
        assertEquals(v, TypeVariable("Functor"))
        assertEquals(targets.map(_.show), Seq("Maybe"))
        val m = methods.head
        assertEquals(m.name, Variable("fmap"))
        assert(m.sig.isDefined)
        // body は型抽象 → ラムダへ脱糖される
        m.body.unfix match {
          case AST.TyAbs(_, _, _) => ()
          case other => fail(s"body not desugared to TyAbs: $other")
        }
      case other => fail(s"not TopImpl: $other")
    }
  }

  test("impl: 複数メソッド") {
    parseDecl("impl Monad[Maybe] {\n  def ret = λx: i32. x\n  def bind = λx: i32. x\n}\n") match {
      case AST.TopImpl(_, _, _, _, methods) =>
        assertEquals(methods.map(_.name.name), Seq("ret", "bind"))
      case other => fail(s"not TopImpl: $other")
    }
  }

  test("impl: 型パラメータと with 文脈") {
    parseDecl("impl[a] Show[Maybe[a]] with Show[a] {\n  def show = λx: Maybe[a]. x\n}\n") match {
      case AST.TopImpl(v, params, targets, context, methods) =>
        assertEquals(v, TypeVariable("Show"))
        assertEquals(params, Seq((TypeVariable("a"), Kind.Star)))
        assertEquals(targets.map(_.show), Seq("Maybe[a]"))
        assertEquals(context.map(_.name), Seq(TypeVariable("Show")))
        assertEquals(context.head.arg.map(_.show), Seq("a"))
        assertEquals(methods.map(_.name), Seq(Variable("show")))
      case other => fail(s"not TopImpl: $other")
    }
  }

  test("let: with 制約付き関数") {
    parseDecl("let f[a](x: a): i32 with Show[a] = 0\n") match {
      case AST.TopLetWith(v, params, constraints, _, _, recursive) =>
        assertEquals(v, Variable("f"))
        assertEquals(params, Seq((TypeVariable("a"), Kind.Star)))
        assertEquals(constraints.map(_.name), Seq(TypeVariable("Show")))
        assertEquals(recursive, false)
      case other => fail(s"not TopLetWith: $other")
    }
  }

  test("let: with なしは従来どおり TopLet") {
    parseDecl("let f[a](x: a): i32 = 0\n") match {
      case AST.TopLet(v, _, _) => assertEquals(v, Variable("f"))
      case other => fail(s"not TopLet: $other")
    }
  }

  test("trait と impl を同一プログラムで混在") {
    val decls = parseDecls(
      "trait Show[a] {\n  def show: a → foreign.C.String\n}\n\nimpl Show[i32] {\n  def show = λx: i32. x\n}\n"
    )
    assertEquals(decls.size, 2)
    assert(decls(0).unfix.isInstanceOf[AST.TopTrait[?, ?]])
    assert(decls(1).unfix.isInstanceOf[AST.TopImpl[?, ?]])
  }
}