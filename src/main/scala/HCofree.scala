package com.yuuki14202028

case class HCofree[H[_[_], _], A[_], I](
  head: A[I],
  tail: H[[x] =>> HCofree[H, A, x], I]
)
