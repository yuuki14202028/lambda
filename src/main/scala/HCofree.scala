package com.yuuki14202028

import cats.arrow.FunctionK
import cats.Monad
import cats.syntax.all._

case class HCofree[H[_[_], _], A[_], I](
  head: A[I],
  tail: H[[x] =>> HCofree[H, A, x], I]
)

type HCofreeAlgebra[H[_[_], _], A[_], B[_]] = [x] => (A[x], H[B, x]) => B[x]

type HCofreeAlgebraM[H[_[_], _], G[_], A[_], B[_]] = [x] => (A[x], H[B, x]) => G[B[x]]

type HCofreeParaAlgebra[H[_[_], _], A[_], B[_]] =
  [x] => (A[x], H[[y] =>> (HCofree[H, A, y], B[y]), x]) => B[x]

type HCofreeParaAlgebraM[H[_[_], _], G[_], A[_], B[_]] =
  [x] => (A[x], H[[y] =>> (HCofree[H, A, y], B[y]), x]) => G[B[x]]

def paraOriginals[H[_[_], _], A[_], B[_], I](
  node: H[[y] =>> (HCofree[H, A, y], B[y]), I]
)(using hf: HFunctor[H]): H[[y] =>> HCofree[H, A, y], I] =
  hf.map(node)(new FunctionK[[y] =>> (HCofree[H, A, y], B[y]), [y] =>> HCofree[H, A, y]] {
    def apply[Y](p: (HCofree[H, A, Y], B[Y])): HCofree[H, A, Y] = p._1
  })

def paraRecursed[H[_[_], _], A[_], B[_], I](
  node: H[[y] =>> (HCofree[H, A, y], B[y]), I]
)(using hf: HFunctor[H]): H[B, I] =
  hf.map(node)(new FunctionK[[y] =>> (HCofree[H, A, y], B[y]), B] {
    def apply[Y](p: (HCofree[H, A, Y], B[Y])): B[Y] = p._2
  })

extension [H[_[_], _], A[_], I](self: HCofree[H, A, I]) {
  def cataAnn[B[_]](alg: HCofreeAlgebra[H, A, B])(using hf: HFunctor[H]): B[I] = {
    val mapped = hf.map(self.tail)(new FunctionK[[x] =>> HCofree[H, A, x], B] {
      def apply[X](child: HCofree[H, A, X]): B[X] = child.cataAnn(alg)
    })
    alg(self.head, mapped)
  }

  def cataAnnM[G[_], B[_]](alg: HCofreeAlgebraM[H, G, A, B])(using ht: HTraverse[H], G: Monad[G]): G[B[I]] = {
    val recursed: G[H[B, I]] = ht.traverse(self.tail)(
      [x] => (child: HCofree[H, A, x]) => child.cataAnnM(alg)
    )
    recursed.flatMap(t => alg(self.head, t))
  }

  def paraAnn[B[_]](alg: HCofreeParaAlgebra[H, A, B])(using hf: HFunctor[H]): B[I] = {
    val mapped = hf.map(self.tail)(new FunctionK[
      [x] =>> HCofree[H, A, x],
      [x] =>> (HCofree[H, A, x], B[x])
    ] {
      def apply[X](child: HCofree[H, A, X]): (HCofree[H, A, X], B[X]) =
        (child, child.paraAnn(alg))
    })
    alg(self.head, mapped)
  }

  def paraAnnM[G[_], B[_]](alg: HCofreeParaAlgebraM[H, G, A, B])(using ht: HTraverse[H], G: Monad[G]): G[B[I]] = {
    type Pair[x] = (HCofree[H, A, x], B[x])
    val recursed: G[H[Pair, I]] = ht.traverse[G, [x] =>> HCofree[H, A, x], Pair, I](self.tail)(
      [x] => (child: HCofree[H, A, x]) => child.paraAnnM(alg).map(b => (child, b))
    )
    recursed.flatMap(t => alg(self.head, t))
  }
}