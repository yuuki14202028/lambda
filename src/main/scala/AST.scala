package com.yuuki14202028

sealed trait NodeType
case object ExprNode extends NodeType
case object TypeNode extends NodeType

type Expr = ExprNode.type
type Type = TypeNode.type

enum AST[R[_], I] {
  case Program(body: Seq[R[Expr]]) extends AST[R, Program.type]
  case Abs(variable: Variable, types: R[Type], body: R[Expr]) extends AST[R, Expr]
  case Let(variable: Variable, types: R[Type], value: R[Expr], body: R[Expr]) extends AST[R, Expr]
  case App(function: R[Expr], argument: R[Expr]) extends AST[R, Expr]
  case Foreign(value: Variable) extends AST[R, Expr]
  case Var(value: Variable) extends AST[R, Expr]
  case Num(value: Int) extends AST[R, Expr]
  case Char(value: scala.Char) extends AST[R, Expr]
  case BinOp(op: BinOps, left: R[Expr], right: R[Expr]) extends AST[R, Expr]
  case UnaryOp(op: UnaryOps, body: R[Expr]) extends AST[R, Expr]
  case If(cond: R[Expr], thenBranch: R[Expr], elseBranch: R[Expr]) extends AST[R, Expr]
  case Primitive(name: String) extends AST[R, Type]
  case Arrow(from: R[Type], to: R[Type]) extends AST[R, Type]
}

enum BinOps {
  case Add, Sub, Mul, Div, Eq, Neq, Lt, Leq, Gt, Geq

  override def toString: String = this match {
    case BinOps.Add => "+"
    case BinOps.Sub => "-"
    case BinOps.Mul => "*"
    case BinOps.Div => "/"
    case BinOps.Eq  => "=="
    case BinOps.Neq => "!="
    case BinOps.Lt  => "<"
    case BinOps.Leq => "<="
    case BinOps.Gt  => ">"
    case BinOps.Geq => ">="
  }
}

enum UnaryOps {
  case Neg
  override def toString: String = this match {
    case UnaryOps.Neg => "-"
  }
}

case class Variable(name: String) {
}

case class HFix[H[_[_], _], I](unfix: H[[x] =>> HFix[H, x], I])
type Rec[I] = HFix[AST, I]

def program(defines: Seq[Rec[Expr]]): Rec[AST.Program.type] = HFix(AST.Program(defines))
def abs(variable: Variable, types: Rec[Type], body: Rec[Expr]): Rec[Expr] = HFix(AST.Abs(variable, types, body))
def let(variable: Variable, types: Rec[Type], value: Rec[Expr], body: Rec[Expr]): Rec[Expr] = HFix(AST.Let(variable, types, value, body))
def app(function: Rec[Expr], argument: Rec[Expr]): Rec[Expr] = HFix(AST.App(function, argument))
def foreign(variable: Variable): Rec[Expr] = HFix(AST.Foreign(variable))
def varr(variable: Variable): Rec[Expr] = HFix(AST.Var(variable))
def num(value: Int): Rec[Expr] = HFix(AST.Num(value))
def char(value: scala.Char): Rec[Expr] = HFix(AST.Char(value))
def binop(op: BinOps, left: Rec[Expr], right: Rec[Expr]): Rec[Expr] = HFix(AST.BinOp(op, left, right))
def unop(op: UnaryOps, body: Rec[Expr]): Rec[Expr] = HFix(AST.UnaryOp(op, body))
def iff(cond: Rec[Expr], thenBranch: Rec[Expr], elseBranch: Rec[Expr]): Rec[Expr] = HFix(AST.If(cond, thenBranch, elseBranch))
def primitive(name: String): Rec[Type] = HFix(AST.Primitive(name))
def arrow(from: Rec[Type], to: Rec[Type]): Rec[Type] = HFix(AST.Arrow(from, to))
