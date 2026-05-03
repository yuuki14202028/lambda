package com.yuuki14202028

type ShowResult[I] = String

val showAlg: Algebra[AST, ShowResult] = [x] => node => node match {
  case AST.Program(seq)            => seq.mkString("\n")
  case AST.Abs(v, types, body)     => s"λ${v.name}: $types. $body"
  case AST.TyAbs(v, body)          => s"Λ${v.name}. $body"
  case AST.Let(v, types, value, body) => s"let ${v.name}: $types = $value in $body"
  case AST.LetRec(v, types, value, body) => s"let rec ${v.name}: $types = $value in $body"
  case AST.TypeLet(v, params, alias, body) =>
    val suffix = params.map(param => s"[${param.name}]").mkString
    val head = s"${v.name}$suffix"
    s"type $head = $alias in $body"
  case AST.App(func, arg)          => s"$func($arg)"
  case AST.TyApp(func, arg)        => s"$func[$arg]"
  case AST.Foreign(v)              => s"foreign ${v.name}"
  case AST.Var(v)                  => s"${v.name}"
  case AST.Num(v)                  => s"$v"
  case AST.Char(v)                 => s"$v"
  case AST.Bool(v)                 => s"$v"
  case AST.BinOp(op, l, r)         => s"($l $op $r)"
  case AST.UnaryOp(op, t)          => s"$op $t"
  case AST.If(c, t, e)             => s"if $c then $t else $e"
  case AST.Primitive(name)         => name
  case AST.TypeVar(v)              => v.name
  case AST.Arrow(from, to)         => s"$from → $to"
  case AST.ForAll(v, body)         => s"∀${v.name}. $body"
  case AST.TypeApp(func, arg)      => s"$func[$arg]"
}

extension [I](t: Rec[I]) {
  def show: String = t.cata(showAlg)
}

val typedShowAlg: HCofreeAlgebra[AST, TypeAnn, ShowResult] = [x] => (ann, node) => {
  val shown = showAlg[x](node)
  ann match {
    case ProgramAnn        => shown
    case ExprAnn(exprType) => s"($shown)[${exprType.show}]"
    case TypeAnn           => shown
  }
}

extension [I](t: TypeRec[I]) {
  def show: String = t.cataAnn(typedShowAlg)
}
