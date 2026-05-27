package com.yuuki14202028

case class HFix[H[_[_], _], I](unfix: H[[x] =>> HFix[H, x], I])