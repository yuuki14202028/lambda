package com.yuuki14202028

import cats.~>

sealed trait NodeType
case object ExprNode extends NodeType
case object TypeNode extends NodeType

type Expr = ExprNode.type
type Type = TypeNode.type

enum AST[R[_], I] {
  case Program(body: Seq[R[Expr]]) extends AST[R, Program.type]
  case Abs(variable: Variable, types: R[Type], body: R[Expr]) extends AST[R, Expr]
  case TyAbs(variable: TypeVariable, body: R[Expr]) extends AST[R, Expr]
  case Let(variable: Variable, types: R[Type], value: R[Expr], body: R[Expr]) extends AST[R, Expr]
  case LetRec(variable: Variable, types: R[Type], value: R[Expr], body: R[Expr]) extends AST[R, Expr]
  case TypeLet(variable: TypeVariable, params: Seq[TypeVariable], alias: R[Type], body: R[Expr]) extends AST[R, Expr]
  case App(function: R[Expr], argument: R[Expr]) extends AST[R, Expr]
  case TyApp(function: R[Expr], argument: R[Type]) extends AST[R, Expr]
  case Foreign(value: Variable) extends AST[R, Expr]
  case Var(value: Variable) extends AST[R, Expr]
  case Num(value: Int) extends AST[R, Expr]
  case Char(value: scala.Char) extends AST[R, Expr]
  case Bool(value: Boolean) extends AST[R, Expr]
  case BinOp(op: BinOps, left: R[Expr], right: R[Expr]) extends AST[R, Expr]
  case UnaryOp(op: UnaryOps, body: R[Expr]) extends AST[R, Expr]
  case If(cond: R[Expr], thenBranch: R[Expr], elseBranch: R[Expr]) extends AST[R, Expr]
  case Primitive(name: String) extends AST[R, Type]
  case TypeVar(value: TypeVariable) extends AST[R, Type]
  case Arrow(from: R[Type], to: R[Type]) extends AST[R, Type]
  case ForAll(variable: TypeVariable, body: R[Type]) extends AST[R, Type]
  case TypeApp(function: R[Type], argument: R[Type]) extends AST[R, Type]
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
  case Neg, Not
  override def toString: String = this match {
    case UnaryOps.Neg => "-"
    case UnaryOps.Not => "!"
  }
}

case class Variable(name: String) {
}

case class TypeVariable(name: String) {
}

case class HFix[H[_[_], _], I](unfix: H[[x] =>> HFix[H, x], I])
type Rec[I] = HFix[AST, I]

def program(defines: Seq[Rec[Expr]]): Rec[AST.Program.type] = HFix(AST.Program(defines))
def abs(variable: Variable, types: Rec[Type], body: Rec[Expr]): Rec[Expr] = HFix(AST.Abs(variable, types, body))
def tyAbs(variable: TypeVariable, body: Rec[Expr]): Rec[Expr] = HFix(AST.TyAbs(variable, body))
def let(variable: Variable, types: Rec[Type], value: Rec[Expr], body: Rec[Expr]): Rec[Expr] = HFix(AST.Let(variable, types, value, body))
def letRec(variable: Variable, types: Rec[Type], value: Rec[Expr], body: Rec[Expr]): Rec[Expr] = HFix(AST.LetRec(variable, types, value, body))
def typeLet(variable: TypeVariable, params: Seq[TypeVariable], alias: Rec[Type], body: Rec[Expr]): Rec[Expr] =
  HFix(AST.TypeLet(variable, params, alias, body))
def app(function: Rec[Expr], argument: Rec[Expr]): Rec[Expr] = HFix(AST.App(function, argument))
def tyApp(function: Rec[Expr], argument: Rec[Type]): Rec[Expr] = HFix(AST.TyApp(function, argument))
def foreign(variable: Variable): Rec[Expr] = HFix(AST.Foreign(variable))
def varr(variable: Variable): Rec[Expr] = HFix(AST.Var(variable))
def num(value: Int): Rec[Expr] = HFix(AST.Num(value))
def char(value: scala.Char): Rec[Expr] = HFix(AST.Char(value))
def bool(value: Boolean): Rec[Expr] = HFix(AST.Bool(value))
def binop(op: BinOps, left: Rec[Expr], right: Rec[Expr]): Rec[Expr] = HFix(AST.BinOp(op, left, right))
def unop(op: UnaryOps, body: Rec[Expr]): Rec[Expr] = HFix(AST.UnaryOp(op, body))
def iff(cond: Rec[Expr], thenBranch: Rec[Expr], elseBranch: Rec[Expr]): Rec[Expr] = HFix(AST.If(cond, thenBranch, elseBranch))
def primitive(name: String): Rec[Type] = HFix(AST.Primitive(name))
def typeVar(variable: TypeVariable): Rec[Type] = HFix(AST.TypeVar(variable))
def arrow(from: Rec[Type], to: Rec[Type]): Rec[Type] = HFix(AST.Arrow(from, to))
def forallType(variable: TypeVariable, body: Rec[Type]): Rec[Type] = HFix(AST.ForAll(variable, body))
def typeApp(function: Rec[Type], argument: Rec[Type]): Rec[Type] = HFix(AST.TypeApp(function, argument))

def intType: Rec[Type] = primitive("Int")
def charType: Rec[Type] = primitive("Char")
def boolType: Rec[Type] = primitive("Bool")
def foreignType: Rec[Type] = arrow(intType, intType)

type TypeRec[I] = HCofree[AST, TypeAnn, I]

private val eraseAnn: TypeRec ~> Rec = new (TypeRec ~> Rec) {
  def apply[I](t: TypeRec[I]): Rec[I] = {
    HFix(summon[HFunctor[AST]].map(t.tail)(this))
  }
}

def programT(defines: Seq[TypeRec[Expr]]): TypeRec[AST.Program.type] = HCofree(ProgramAnn, AST.Program(defines))
def absT(variable: Variable, t: Rec[Type], types: TypeRec[Type], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.Abs(variable, types, body))
def tyAbsT(variable: TypeVariable, t: Rec[Type], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.TyAbs(variable, body))
def letT(variable: Variable, t: Rec[Type], types: TypeRec[Type], value: TypeRec[Expr], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.Let(variable, types, value, body))
def letRecT(variable: Variable, t: Rec[Type], types: TypeRec[Type], value: TypeRec[Expr], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.LetRec(variable, types, value, body))
def typeLetT(variable: TypeVariable, params: Seq[TypeVariable], t: Rec[Type], alias: TypeRec[Type], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.TypeLet(variable, params, alias, body))
def appT(t: Rec[Type], function: TypeRec[Expr], argument: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.App(function, argument))
def tyAppT(t: Rec[Type], function: TypeRec[Expr], argument: TypeRec[Type]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.TyApp(function, argument))
def foreignT(variable: Variable, t: Rec[Type]): TypeRec[Expr] = HCofree(ExprAnn(t), AST.Foreign(variable))
def varrType(variable: Variable, t: Rec[Type]): TypeRec[Expr] = HCofree(ExprAnn(t), AST.Var(variable))
def numT(value: Int, t: Rec[Type]): TypeRec[Expr] = HCofree(ExprAnn(t), AST.Num(value))
def charT(value: scala.Char, t: Rec[Type]): TypeRec[Expr] = HCofree(ExprAnn(t), AST.Char(value))
def boolT(value: Boolean, t: Rec[Type]): TypeRec[Expr] = HCofree(ExprAnn(t), AST.Bool(value))
def binopT(op: BinOps, t: Rec[Type], left: TypeRec[Expr], right: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.BinOp(op, left, right))
def unopT(op: UnaryOps, t: Rec[Type], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.UnaryOp(op, body))
def ifT(t: Rec[Type], cond: TypeRec[Expr], thenBranch: TypeRec[Expr], elseBranch: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.If(cond, thenBranch, elseBranch))
def primitiveT(name: String): TypeRec[Type] = HCofree(TypeAnn, AST.Primitive(name))
def typeVarT(variable: TypeVariable): TypeRec[Type] = HCofree(TypeAnn, AST.TypeVar(variable))
def arrowT(from: TypeRec[Type], to: TypeRec[Type]): TypeRec[Type] = HCofree(TypeAnn, AST.Arrow(from, to))
def forallTypeT(variable: TypeVariable, body: TypeRec[Type]): TypeRec[Type] = HCofree(TypeAnn, AST.ForAll(variable, body))
def typeAppT(function: TypeRec[Type], argument: TypeRec[Type]): TypeRec[Type] = HCofree(TypeAnn, AST.TypeApp(function, argument))

def typeOf(expr: TypeRec[Expr]): Rec[Type] = expr.head match {
  case ExprAnn(t) => t
}

def destructArrow(t: Rec[Type]): Option[(Rec[Type], Rec[Type])] = t.unfix match {
  case AST.Arrow(from, to) => Some((from, to))
  case _ => None
}

def destructForAll(t: Rec[Type]): Option[(TypeVariable, Rec[Type])] = t.unfix match {
  case AST.ForAll(variable, body) => Some((variable, body))
  case _ => None
}

def freeTypeVars(t: Rec[Type]): Set[TypeVariable] = t.unfix match {
  case AST.Primitive(_) => Set.empty
  case AST.TypeVar(variable) => Set(variable)
  case AST.Arrow(from, to) => freeTypeVars(from) ++ freeTypeVars(to)
  case AST.ForAll(variable, body) => freeTypeVars(body) - variable
  case AST.TypeApp(function, argument) => freeTypeVars(function) ++ freeTypeVars(argument)
}

private def freshTypeVariable(base: TypeVariable, used: Set[TypeVariable]): TypeVariable = {
  Iterator.from(0)
    .map {
      case 0 => TypeVariable(s"${base.name}'")
      case n => TypeVariable(s"${base.name}'$n")
    }
    .find(v => !used.contains(v))
    .get
}

def renameTypeVar(from: TypeVariable, to: TypeVariable, in: Rec[Type]): Rec[Type] = in.unfix match {
  case AST.Primitive(name) => primitive(name)
  case AST.TypeVar(variable) if variable == from => typeVar(to)
  case AST.TypeVar(variable) => typeVar(variable)
  case AST.Arrow(left, right) => arrow(renameTypeVar(from, to, left), renameTypeVar(from, to, right))
  case AST.ForAll(variable, body) if variable == from => forallType(variable, body)
  case AST.ForAll(variable, body) => forallType(variable, renameTypeVar(from, to, body))
  case AST.TypeApp(function, argument) => typeApp(renameTypeVar(from, to, function), renameTypeVar(from, to, argument))
}

def substType(target: TypeVariable, replacement: Rec[Type], in: Rec[Type]): Rec[Type] = in.unfix match {
  case AST.Primitive(name) => primitive(name)
  case AST.TypeVar(variable) if variable == target => replacement
  case AST.TypeVar(variable) => typeVar(variable)
  case AST.Arrow(left, right) => arrow(substType(target, replacement, left), substType(target, replacement, right))
  case AST.ForAll(variable, body) if variable == target => forallType(variable, body)
  case AST.ForAll(variable, body) =>
    val replacementFreeVars = freeTypeVars(replacement)
    if (replacementFreeVars.contains(variable)) {
      val used = replacementFreeVars ++ freeTypeVars(body) + target + variable
      val fresh = freshTypeVariable(variable, used)
      val renamedBody = renameTypeVar(variable, fresh, body)
      forallType(fresh, substType(target, replacement, renamedBody))
    } else {
      forallType(variable, substType(target, replacement, body))
    }
  case AST.TypeApp(function, argument) => typeApp(substType(target, replacement, function), substType(target, replacement, argument))
}

def sameType(left: Rec[Type], right: Rec[Type]): Boolean = {
  def loop(l: Rec[Type], r: Rec[Type], bound: Map[TypeVariable, TypeVariable]): Boolean = (l.unfix, r.unfix) match {
    case (AST.Primitive(ln), AST.Primitive(rn)) => ln == rn
    case (AST.TypeVar(lv), AST.TypeVar(rv)) =>
      bound.get(lv) match {
        case Some(boundRight) => boundRight == rv
        case None => !bound.values.toSet.contains(rv) && lv == rv
      }
    case (AST.Arrow(lf, lt), AST.Arrow(rf, rt)) => loop(lf, rf, bound) && loop(lt, rt, bound)
    case (AST.ForAll(lv, lb), AST.ForAll(rv, rb)) => loop(lb, rb, bound + (lv -> rv))
    case (AST.TypeApp(lf, la), AST.TypeApp(rf, ra)) => loop(lf, rf, bound) && loop(la, ra, bound)
    case _ => false
  }

  loop(left, right, Map.empty)
}

def isNumericType(t: Rec[Type]): Boolean = {
  sameType(t, intType) || sameType(t, charType)
}

def isEquatableType(t: Rec[Type]): Boolean = {
  isNumericType(t) || sameType(t, boolType)
}
