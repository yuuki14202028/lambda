package com.yuuki14202028

type ShowResult[I] = String

val showAlg: Algebra[AST, ShowResult] = [x] => node => node match {
  case AST.Program(seq)            => seq.mkString("\n")
  case AST.Abs(v, types, body)     => s"λ${v.name}: $types. $body"
  case AST.Let(v, types, value, body) => s"let ${v.name}: $types = $value in $body"
  case AST.App(func, arg)          => s"$func($arg)"
  case AST.Foreign(v)              => s"foreign ${v.name}"
  case AST.Var(v)                  => s"${v.name}"
  case AST.Num(v)                  => s"$v"
  case AST.Char(v)                 => s"$v"
  case AST.BinOp(op, l, r)         => s"($l $op $r)"
  case AST.UnaryOp(op, t)          => s"$op $t"
  case AST.If(c, t, e)             => s"if $c then $t else $e"
  case AST.Primitive(name)         => name
  case AST.Arrow(from, to)         => s"$from → $to"
}

extension [I](t: Rec[I]) {
  def show: String = t.cata(showAlg)
}
