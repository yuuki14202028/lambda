package com.yuuki14202028

type ShowResult[I] = String

val showAlg: Algebra[AST, ShowResult] = [x] => node => node match {
  case AST.Program(seq)            => seq.mkString("\n")
  case AST.Abs(v, body)            => s"λ${v.name}. $body"
  case AST.Let(v, value, body)     => s"let ${v.name} = $value in $body"
  case AST.App(func, arg)          => s"$func($arg)"
  case AST.Var(v)                  => s"${v.name}"
  case AST.Num(v)                  => s"$v"
  case AST.BinOp(op, l, r)         => s"($l $op $r)"
  case AST.UnaryOp(op, t)          => s"$op $t"
}

extension [I](t: Rec[I]) {
  def show: String = t.cata(showAlg)
}