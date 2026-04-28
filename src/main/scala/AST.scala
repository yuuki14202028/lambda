package com.yuuki14202028

sealed trait NodeType
case object ExprNode extends NodeType
case object TypeNode extends NodeType

type Expr = ExprNode.type
type Type = TypeNode.type

enum AST[R[_], I] {
  case Program(body: Seq[R[Expr]]) extends AST[R, Program.type]
  case Abs(variable: Variable, body: R[Expr]) extends AST[R, Expr]
  case App(function: R[Expr], argument: R[Expr]) extends AST[R, Expr]
  case Var(value: Variable) extends AST[R, Expr]
  case Num(value: Int) extends AST[R, Expr]
  case BinOp(op: BinOps, left: R[Expr], right: R[Expr]) extends AST[R, Expr]
  case UnaryOp(op: UnaryOps, body: R[Expr]) extends AST[R, Expr]
}

enum BinOps {
  case Add, Sub, Mul, Div

  override def toString: String = this match {
    case BinOps.Add => "+"
    case BinOps.Sub => "-"
    case BinOps.Mul => "*"
    case BinOps.Div => "/"
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
def abs(variable: Variable, body: Rec[Expr]): Rec[Expr] = HFix(AST.Abs(variable, body))
def app(function: Rec[Expr], argument: Rec[Expr]): Rec[Expr] = HFix(AST.App(function, argument))
def varr(variable: Variable): Rec[Expr] = HFix(AST.Var(variable))
def num(value: Int): Rec[Expr] = HFix(AST.Num(value))
def binop(op: BinOps, left: Rec[Expr], right: Rec[Expr]): Rec[Expr] = HFix(AST.BinOp(op, left, right))
def unop(op: UnaryOps, body: Rec[Expr]): Rec[Expr] = HFix(AST.UnaryOp(op, body))