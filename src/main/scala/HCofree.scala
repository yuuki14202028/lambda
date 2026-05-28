package com.yuuki14202028

import cats.~>
import cats.arrow.FunctionK

case class HCofreeT[A[_], H[_[_], _], R[_], I](ask: A[I], lower: H[R, I])

given hCofreeTHFunctor[A[_], H[_[_], _]](using hf: HFunctor[H])
    : HFunctor[[R[_], x] =>> HCofreeT[A, H, R, x]] with
  def map[R[_], S[_], I](fa: HCofreeT[A, H, R, I])(f: R ~> S): HCofreeT[A, H, S, I] =
    HCofreeT(fa.ask, hf.map(fa.lower)(f))

type HCofree[H[_[_], _], A[_], I] = HFix[[R[_], x] =>> HCofreeT[A, H, R, x], I]

object HCofree:
  def apply[H[_[_], _], A[_], I](
    extract: A[I],
    project: H[[x] =>> HCofree[H, A, x], I]
  ): HCofree[H, A, I] =
    HFix[[R[_], x] =>> HCofreeT[A, H, R, x], I](HCofreeT(extract, project))

extension [H[_[_], _], A[_], I](self: HCofree[H, A, I]) {
  def extract: A[I] = self.unfix.ask
  def project: H[[x] =>> HCofree[H, A, x], I] = self.unfix.lower
}

def paraOriginals[H[_[_], _], A[_], B[_], I]
                 (node: H[[y] =>> (HCofree[H, A, y], B[y]), I])
                 (using hf: HFunctor[H]): H[[y] =>> HCofree[H, A, y], I] = {
  hf.map(node)(new FunctionK[[y] =>> (HCofree[H, A, y], B[y]), [y] =>> HCofree[H, A, y]] {
    def apply[Y](p: (HCofree[H, A, Y], B[Y])): HCofree[H, A, Y] = p._1
  })
}