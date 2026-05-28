package com.yuuki14202028

import cats.arrow.FunctionK
import cats.{~>, Applicative, Id, Monad}
import cats.syntax.all._

trait HFunctor[H[_[_], _]]:
  def map[R[_], S[_], I](fa: H[R, I])(f: R ~> S): H[S, I]

trait HTraverse[H[_[_], _]] extends HFunctor[H]:
  def traverse[G[_]: Applicative, R[_], S[_], I](fa: H[R, I])(f: [x] => R[x] => G[S[x]]): G[H[S, I]]

  override def map[R[_], S[_], I](fa: H[R, I])(f: R ~> S): H[S, I] =
    traverse[Id, R, S, I](fa)([x] => (r: R[x]) => f(r): Id[S[x]])

given HTraverse[AST] with {
  def traverse[G[_]: Applicative, R[_], S[_], I](fa: AST[R, I])(f: [x] => R[x] => G[S[x]]): G[AST[S, I]] = fa match {
    case AST.Program(decls) =>
      decls.toList.traverse(d => f(d)).map(AST.Program(_))
    case AST.TopLet(v, types, value) =>
      (f(types), f(value)).mapN(AST.TopLet(v, _, _))
    case AST.TopLetRec(v, types, value) =>
      (f(types), f(value)).mapN(AST.TopLetRec(v, _, _))
    case AST.TopImport(path) =>
      Applicative[G].pure(AST.TopImport(path))
    case AST.TopType(v, params, alias) =>
      f(alias).map(AST.TopType(v, params, _))
    case AST.TopData(v, params, ctors, recursive) =>
      ctors.toList.traverse { c =>
        c.fields.toList.traverse(field => f(field)).map(fs => DataConstructor[S](c.name, fs))
      }.map(AST.TopData(v, params, _, recursive))
    case AST.Abs(v, types, body) =>
      (f(types), f(body)).mapN(AST.Abs(v, _, _))
    case AST.TyAbs(v, k, body) =>
      f(body).map(AST.TyAbs(v, k, _))
    case AST.Let(v, types, vall, body) =>
      (f(types), f(vall), f(body)).mapN(AST.Let(v, _, _, _))
    case AST.LetRec(v, types, vall, body) =>
      (f(types), f(vall), f(body)).mapN(AST.LetRec(v, _, _, _))
    case AST.TypeLet(v, params, alias, body) =>
      (f(alias), f(body)).mapN(AST.TypeLet(v, params, _, _))
    case AST.DataLet(v, params, ctors, body, recursive) =>
      val ctorsG = ctors.toList.traverse { c =>
        c.fields.toList.traverse(field => f(field)).map(fs => DataConstructor[S](c.name, fs))
      }
      (ctorsG, f(body)).mapN(AST.DataLet(v, params, _, _, recursive))
    case AST.Match(scrut, cases) =>
      val casesG = cases.toList.traverse { c =>
        f(c.body).map(b => MatchCase[S](c.constructor, c.binders, b))
      }
      (f(scrut), casesG).mapN(AST.Match(_, _))
    case AST.Fold(scrut, resultType, cases) =>
      val casesG = cases.toList.traverse { c =>
        f(c.body).map(b => MatchCase[S](c.constructor, c.binders, b))
      }
      (f(scrut), f(resultType), casesG).mapN(AST.Fold(_, _, _))
    case AST.App(func, arg) =>
      (f(func), f(arg)).mapN(AST.App(_, _))
    case AST.TyApp(func, arg) =>
      (f(func), f(arg)).mapN(AST.TyApp(_, _))
    case AST.Foreign(v, types) =>
      f(types).map(AST.Foreign(v, _))
    case AST.Var(v) => Applicative[G].pure(AST.Var(v))
    case AST.Num(v, t) => Applicative[G].pure(AST.Num(v, t))
    case AST.Char(v) => Applicative[G].pure(AST.Char(v))
    case AST.StringLit(v) => Applicative[G].pure(AST.StringLit(v))
    case AST.Bool(v) => Applicative[G].pure(AST.Bool(v))
    case AST.UnitLit() => Applicative[G].pure(AST.UnitLit())
    case AST.Block(discarded, result) =>
      (discarded.toList.traverse(e => f(e)), result.traverse(e => f(e))).mapN(AST.Block(_, _))
    case AST.BinOp(op, l, r) =>
      (f(l), f(r)).mapN(AST.BinOp(op, _, _))
    case AST.Intrinsic(op, args) =>
      args.toList.traverse(arg => f(arg)).map(AST.Intrinsic(op, _))
    case AST.UnaryOp(op, t) =>
      f(t).map(AST.UnaryOp(op, _))
    case AST.If(c, t, e) =>
      (f(c), f(t), f(e)).mapN(AST.If(_, _, _))
    case AST.Primitive(n) => Applicative[G].pure(AST.Primitive(n))
    case AST.TypeVar(v) => Applicative[G].pure(AST.TypeVar(v))
    case AST.Arrow(from, to) =>
      (f(from), f(to)).mapN(AST.Arrow(_, _))
    case AST.ForAll(v, k, body) =>
      f(body).map(AST.ForAll(v, k, _))
    case AST.TypeApp(func, arg) =>
      (f(func), f(arg)).mapN(AST.TypeApp(_, _))
    case AST.TypeAbs(v, k, body) =>
      f(body).map(AST.TypeAbs(v, k, _))
  }
}

type Algebra[H[_[_], _], A[_]] = [x] => H[A, x] => A[x]
type RAlgebra[H[_[_], _], A[_], B[_]] =
  [x] => H[[y] =>> (A[y], B[y]), x] => B[x]
type ApoCoalgebra[H[_[_], _], B[_]] =
  [x] => B[x] => Either[HFix[H, x], H[[y] =>> Either[HFix[H, y], B[y]], x]]

def apo[H[_[_], _], B[_], I]
       (seed: B[I])(coalg: ApoCoalgebra[H, B])
       (using hf: HFunctor[H]): HFix[H, I] = {
  coalg(seed) match {
    case Left(done) => done
    case Right(layer) =>
      val tail = hf.map(layer)(new FunctionK[
        [y] =>> Either[HFix[H, y], B[y]],
        [y] =>> HFix[H, y]
      ] {
        def apply[Y](e: Either[HFix[H, Y], B[Y]]): HFix[H, Y] = e match {
          case Left(tree) => tree
          case Right(s) => apo(s)(coalg)
        }
      })
      HFix(tail)
  }
}

extension [H[_[_], _], I](self: HFix[H, I]) {
  def cata[A[_]](alg: Algebra[H, A])(using hf: HFunctor[H]): A[I] = {
    val mapped = hf.map(self.unfix)(new FunctionK[[x] =>> HFix[H, x], A] {
      def apply[X](child: HFix[H, X]): A[X] = child.cata(alg)
    })
    alg(mapped)
  }

  def para[B[_]](alg: RAlgebra[H, [y] =>> HFix[H, y], B])(using hf: HFunctor[H]): B[I] = {
    val mapped = hf.map(self.unfix)(new FunctionK[
      [x] =>> HFix[H, x],
      [x] =>> (HFix[H, x], B[x])
    ] {
      def apply[X](child: HFix[H, X]): (HFix[H, X], B[X]) =
        (child, child.para(alg))
    })
    alg(mapped)
  }
}
