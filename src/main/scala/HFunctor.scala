package com.yuuki14202028

import cats.arrow.FunctionK
import cats.~>

trait HFunctor[H[_[_], _]]:
  def map[R[_], S[_], I](fa: H[R, I])(f: R ~> S): H[S, I]

given HFunctor[AST] with {
  def map[R[_], S[_], I](fa: AST[R, I])(f: R ~> S): AST[S, I] = fa match {
    case AST.Program(seq) => AST.Program(seq.map(f.apply))
    case AST.Abs(v, body) => AST.Abs(v, f(body))
    case AST.Let(varr, vall, body) => AST.Let(varr, f(vall), f(body))
    case AST.App(func, arg) => AST.App(f(func), f(arg))
    case AST.Var(v) => AST.Var(v)
    case AST.Num(v) => AST.Num(v)
    case AST.BinOp(op, l, r) => AST.BinOp(op, f(l), f(r))
    case AST.UnaryOp(op, t) => AST.UnaryOp(op, f(t))
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