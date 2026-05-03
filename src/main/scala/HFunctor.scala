package com.yuuki14202028

import cats.arrow.FunctionK
import cats.~>

trait HFunctor[H[_[_], _]]:
  def map[R[_], S[_], I](fa: H[R, I])(f: R ~> S): H[S, I]

given HFunctor[AST] with {
  def map[R[_], S[_], I](fa: AST[R, I])(f: R ~> S): AST[S, I] = fa match {
    case AST.Program(seq) => AST.Program(seq.map(f.apply))
    case AST.Abs(v, types, body) => AST.Abs(v, f(types), f(body))
    case AST.TyAbs(v, body) => AST.TyAbs(v, f(body))
    case AST.Let(varr, types, vall, body) => AST.Let(varr, f(types), f(vall), f(body))
    case AST.LetRec(varr, types, vall, body) => AST.LetRec(varr, f(types), f(vall), f(body))
    case AST.TypeLet(varr, params, alias, body) => AST.TypeLet(varr, params, f(alias), f(body))
    case AST.App(func, arg) => AST.App(f(func), f(arg))
    case AST.TyApp(func, arg) => AST.TyApp(f(func), f(arg))
    case AST.Foreign(v) => AST.Foreign(v)
    case AST.Var(v) => AST.Var(v)
    case AST.Num(v) => AST.Num(v)
    case AST.Char(v) => AST.Char(v)
    case AST.Bool(v) => AST.Bool(v)
    case AST.UnitLit() => AST.UnitLit()
    case AST.Block(discarded, result) => AST.Block(discarded.map(f.apply), result.map(f.apply))
    case AST.BinOp(op, l, r) => AST.BinOp(op, f(l), f(r))
    case AST.UnaryOp(op, t) => AST.UnaryOp(op, f(t))
    case AST.If(c, t, e) => AST.If(f(c), f(t), f(e))
    case AST.Primitive(n) => AST.Primitive(n)
    case AST.TypeVar(v) => AST.TypeVar(v)
    case AST.Arrow(from, to) => AST.Arrow(f(from), f(to))
    case AST.ForAll(v, body) => AST.ForAll(v, f(body))
    case AST.TypeApp(func, arg) => AST.TypeApp(f(func), f(arg))
  }
}

type Algebra[H[_[_], _], A[_]] = [x] => H[A, x] => A[x]

extension [H[_[_], _], I](self: HFix[H, I]) {
  def cata[A[_]](alg: Algebra[H, A])(using hf: HFunctor[H]): A[I] = {
    val mapped = hf.map(self.unfix)(new FunctionK[[x] =>> HFix[H, x], A] {
      def apply[X](child: HFix[H, X]): A[X] = child.cata(alg)
    })
    alg(mapped)
  }
}
