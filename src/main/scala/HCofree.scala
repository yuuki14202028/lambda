package com.yuuki14202028

import cats.arrow.FunctionK

case class HCofree[H[_[_], _], A[_], I](
  head: A[I],
  tail: H[[x] =>> HCofree[H, A, x], I]
)

type HCofreeAlgebra[H[_[_], _], A[_], B[_]] = [x] => (A[x], H[B, x]) => B[x]

type HCofreeParaAlgebra[H[_[_], _], A[_], B[_]] =
  [x] => (A[x], H[[y] =>> (HCofree[H, A, y], B[y]), x]) => B[x]

extension [H[_[_], _], A[_], I](self: HCofree[H, A, I]) {
  def cataAnn[B[_]](alg: HCofreeAlgebra[H, A, B])(using hf: HFunctor[H]): B[I] = {
    val mapped = hf.map(self.tail)(new FunctionK[[x] =>> HCofree[H, A, x], B] {
      def apply[X](child: HCofree[H, A, X]): B[X] = child.cataAnn(alg)
    })
    alg(self.head, mapped)
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
}
