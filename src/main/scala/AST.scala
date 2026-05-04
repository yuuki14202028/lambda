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
  case DataLet(variable: TypeVariable, params: Seq[TypeVariable], constructors: Seq[DataConstructor[R]], body: R[Expr]) extends AST[R, Expr]
  case Match(scrutinee: R[Expr], cases: Seq[MatchCase[R]]) extends AST[R, Expr]
  case App(function: R[Expr], argument: R[Expr]) extends AST[R, Expr]
  case TyApp(function: R[Expr], argument: R[Type]) extends AST[R, Expr]
  case Foreign(value: Variable, types: R[Type]) extends AST[R, Expr]
  case Var(value: Variable) extends AST[R, Expr]
  case Num(value: Int) extends AST[R, Expr]
  case Char(value: scala.Char) extends AST[R, Expr]
  case StringLit(value: String) extends AST[R, Expr]
  case Bool(value: Boolean) extends AST[R, Expr]
  case UnitLit() extends AST[R, Expr]
  case Block(discarded: Seq[R[Expr]], result: Option[R[Expr]]) extends AST[R, Expr]
  case BinOp(op: BinOps, left: R[Expr], right: R[Expr]) extends AST[R, Expr]
  case UnaryOp(op: UnaryOps, body: R[Expr]) extends AST[R, Expr]
  case If(cond: R[Expr], thenBranch: R[Expr], elseBranch: R[Expr]) extends AST[R, Expr]
  case Primitive(name: String) extends AST[R, Type]
  case TypeVar(value: TypeVariable) extends AST[R, Type]
  case Arrow(from: R[Type], to: R[Type]) extends AST[R, Type]
  case ForAll(variable: TypeVariable, body: R[Type]) extends AST[R, Type]
  case TypeApp(function: R[Type], argument: R[Type]) extends AST[R, Type]
}

case class DataConstructor[R[_]](name: Variable, fields: Seq[R[Type]])
case class MatchCase[R[_]](constructor: Variable, binders: Seq[Variable], body: R[Expr])

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
def dataLet(variable: TypeVariable, params: Seq[TypeVariable], constructors: Seq[DataConstructor[[x] =>> Rec[x]]], body: Rec[Expr]): Rec[Expr] =
  HFix(AST.DataLet(variable, params, constructors, body))
def matchExpr(scrutinee: Rec[Expr], cases: Seq[MatchCase[[x] =>> Rec[x]]]): Rec[Expr] =
  HFix(AST.Match(scrutinee, cases))
def app(function: Rec[Expr], argument: Rec[Expr]): Rec[Expr] = HFix(AST.App(function, argument))
def tyApp(function: Rec[Expr], argument: Rec[Type]): Rec[Expr] = HFix(AST.TyApp(function, argument))
def foreign(variable: Variable, types: Rec[Type]): Rec[Expr] = HFix(AST.Foreign(variable, types))
def varr(variable: Variable): Rec[Expr] = HFix(AST.Var(variable))
def num(value: Int): Rec[Expr] = HFix(AST.Num(value))
def char(value: scala.Char): Rec[Expr] = HFix(AST.Char(value))
def stringLit(value: String): Rec[Expr] = HFix(AST.StringLit(value))
def bool(value: Boolean): Rec[Expr] = HFix(AST.Bool(value))
def unitLit: Rec[Expr] = HFix(AST.UnitLit())
def block(discarded: Seq[Rec[Expr]], result: Option[Rec[Expr]]): Rec[Expr] = HFix(AST.Block(discarded, result))
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
def stringType: Rec[Type] = primitive("String")
def boolType: Rec[Type] = primitive("Bool")
def unitType: Rec[Type] = primitive("Unit")

type TypeRec[I] = HCofree[AST, TypeAnn, I]

private val eraseAnn: TypeRec ~> Rec = new (TypeRec ~> Rec) {
  def apply[I](t: TypeRec[I]): Rec[I] = {
    HFix(summon[HFunctor[AST]].map(t.tail)(this))
  }
}

def programT(defines: Seq[TypeRec[Expr]]): TypeRec[AST.Program.type] = HCofree(ProgramAnn, AST.Program(defines))
def absT(variable: Variable, t: TypeRec[Type], types: TypeRec[Type], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.Abs(variable, types, body))
def tyAbsT(variable: TypeVariable, t: TypeRec[Type], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.TyAbs(variable, body))
def letT(variable: Variable, t: TypeRec[Type], types: TypeRec[Type], value: TypeRec[Expr], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.Let(variable, types, value, body))
def letRecT(variable: Variable, t: TypeRec[Type], types: TypeRec[Type], value: TypeRec[Expr], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.LetRec(variable, types, value, body))
def typeLetT(variable: TypeVariable, params: Seq[TypeVariable], t: TypeRec[Type], alias: TypeRec[Type], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.TypeLet(variable, params, alias, body))
def dataLetT(variable: TypeVariable, params: Seq[TypeVariable], t: TypeRec[Type], constructors: Seq[DataConstructor[TypeRec]], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.DataLet(variable, params, constructors, body))
def matchExprT(t: TypeRec[Type], scrutinee: TypeRec[Expr], cases: Seq[MatchCase[TypeRec]]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.Match(scrutinee, cases))
def appT(t: TypeRec[Type], function: TypeRec[Expr], argument: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.App(function, argument))
def tyAppT(t: TypeRec[Type], function: TypeRec[Expr], argument: TypeRec[Type]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.TyApp(function, argument))
def foreignT(variable: Variable, t: TypeRec[Type], types: TypeRec[Type]): TypeRec[Expr] = HCofree(ExprAnn(t), AST.Foreign(variable, types))
def varrType(variable: Variable, t: TypeRec[Type]): TypeRec[Expr] = HCofree(ExprAnn(t), AST.Var(variable))
def numT(value: Int, t: TypeRec[Type]): TypeRec[Expr] = HCofree(ExprAnn(t), AST.Num(value))
def charT(value: scala.Char, t: TypeRec[Type]): TypeRec[Expr] = HCofree(ExprAnn(t), AST.Char(value))
def stringLitT(value: String, t: TypeRec[Type]): TypeRec[Expr] = HCofree(ExprAnn(t), AST.StringLit(value))
def boolT(value: Boolean, t: TypeRec[Type]): TypeRec[Expr] = HCofree(ExprAnn(t), AST.Bool(value))
def unitLitT(t: TypeRec[Type]): TypeRec[Expr] = HCofree(ExprAnn(t), AST.UnitLit())
def blockT(t: TypeRec[Type], discarded: Seq[TypeRec[Expr]], result: Option[TypeRec[Expr]]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.Block(discarded, result))
def binopT(op: BinOps, t: TypeRec[Type], left: TypeRec[Expr], right: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.BinOp(op, left, right))
def unopT(op: UnaryOps, t: TypeRec[Type], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.UnaryOp(op, body))
def ifT(t: TypeRec[Type], cond: TypeRec[Expr], thenBranch: TypeRec[Expr], elseBranch: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.If(cond, thenBranch, elseBranch))
def primitiveT(name: String): TypeRec[Type] = HCofree(TypeAnn, AST.Primitive(name))
def typeVarT(variable: TypeVariable): TypeRec[Type] = HCofree(TypeAnn, AST.TypeVar(variable))
def arrowT(from: TypeRec[Type], to: TypeRec[Type]): TypeRec[Type] = HCofree(TypeAnn, AST.Arrow(from, to))
def forallTypeT(variable: TypeVariable, body: TypeRec[Type]): TypeRec[Type] = HCofree(TypeAnn, AST.ForAll(variable, body))
def typeAppT(function: TypeRec[Type], argument: TypeRec[Type]): TypeRec[Type] = HCofree(TypeAnn, AST.TypeApp(function, argument))

def intTypeT: TypeRec[Type] = primitiveT("Int")
def charTypeT: TypeRec[Type] = primitiveT("Char")
def stringTypeT: TypeRec[Type] = primitiveT("String")
def boolTypeT: TypeRec[Type] = primitiveT("Bool")
def unitTypeT: TypeRec[Type] = primitiveT("Unit")

def typeOf(expr: TypeRec[Expr]): TypeRec[Type] = expr.head match {
  case ExprAnn(t) => t
}

extension (r: TypeRec[Type]) {
  def projectT: AST[TypeRec, Type] = r.tail
}

def destructArrow(t: TypeRec[Type]): Option[(TypeRec[Type], TypeRec[Type])] = t.projectT match {
  case AST.Arrow(from, to) => Some((from, to))
  case _ => None
}

def destructForAll(t: TypeRec[Type]): Option[(TypeVariable, TypeRec[Type])] = t.projectT match {
  case AST.ForAll(variable, body) => Some((variable, body))
  case _ => None
}

def freeTypeVars(t: TypeRec[Type]): Set[TypeVariable] = {
  type FV[I] = Set[TypeVariable]
  val alg: HCofreeAlgebra[AST, TypeAnn, FV] = [x] => (_, node) => node match {
    case AST.TypeVar(v) => Set(v)
    case AST.ForAll(v, body) => body - v
    case AST.Arrow(from, to) => from ++ to
    case AST.TypeApp(f, a) => f ++ a
    case _ => Set.empty
  }
  t.cataAnn(alg)
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

def renameTypeVar(from: TypeVariable, to: TypeVariable, in: TypeRec[Type]): TypeRec[Type] = {
  val algebra: HCofreeAlgebra[AST, TypeAnn, TypeRec] = [x] => (ann, node) => node match {
    case AST.TypeVar(variable) if variable == from => typeVarT(to)
    case AST.ForAll(variable, body) if variable == from => forallTypeT(to, body)
    case ast => HCofree(ann, ast)
  }
  in.cataAnn(algebra)
}

def substType(target: TypeVariable, replace: TypeRec[Type], in: TypeRec[Type]): TypeRec[Type] = {
  val algebra: HCofreeAlgebra[AST, TypeAnn, TypeRec] = [x] => (ann, node) => node match {
    case AST.TypeVar(variable) if variable == target => replace
    case AST.ForAll(variable, body) if variable != target =>
      val replaceFreeVars = freeTypeVars(replace)
      if (replaceFreeVars.contains(variable)) {
        val used = replaceFreeVars ++ freeTypeVars(body) + target + variable
        val fresh = freshTypeVariable(variable, used)
        val renamedBody = renameTypeVar(variable, fresh, body)
        forallTypeT(fresh, renamedBody)
      } else forallTypeT(variable, body)
    case ast => HCofree(ann, ast)
  }
  in.cataAnn(algebra)
}

private def substMany(params: Seq[TypeVariable], args: Seq[TypeRec[Type]], in: TypeRec[Type]): TypeRec[Type] = {
  params.zip(args).foldLeft(in) { case (acc, (param, arg)) => substType(param, arg, acc) }
}

def sameType(left: TypeRec[Type], right: TypeRec[Type]): Boolean = {
  type Eq[I] = Map[TypeVariable, TypeVariable] => TypeRec[I] => Boolean
  val alg: HCofreeAlgebra[AST, TypeAnn, Eq] = [x] => (_, node) => bound => other =>
    (node, other.tail) match {
      case (AST.Primitive(ln), AST.Primitive(rn)) => ln == rn
      case (AST.TypeVar(lv), AST.TypeVar(rv)) =>
        bound.get(lv) match {
          case Some(b) => b == rv
          case None => !bound.values.toSet.contains(rv) && lv == rv
        }
      case (AST.Arrow(lf, lt), AST.Arrow(rf, rt)) => lf(bound)(rf) && lt(bound)(rt)
      case (AST.ForAll(lv, lb), AST.ForAll(rv, rb)) => lb(bound + (lv -> rv))(rb)
      case (AST.TypeApp(lf, la), AST.TypeApp(rf, ra)) => lf(bound)(rf) && la(bound)(ra)
      case _ => false
    }
  left.cataAnn(alg)(Map.empty)(right)
}

def isNumericType(t: TypeRec[Type]): Boolean = {
  sameType(t, intTypeT) || sameType(t, charTypeT)
}

def isEquatableType(t: TypeRec[Type]): Boolean = {
  isNumericType(t) || sameType(t, boolTypeT)
}

def collectTypeApps(t: TypeRec[Type]): (TypeRec[Type], Seq[TypeRec[Type]]) = {
  type Collected[I] = (TypeRec[I], Seq[TypeRec[Type]])
  val alg: HCofreeParaAlgebra[AST, TypeAnn, Collected] = [x] => (ann, node) => node match {
    case AST.TypeApp(function, argument) =>
      val (_, (head, args)) = function
      val (origArg, _) = argument
      (head, args :+ origArg)
    case _ =>
      val self = HCofree[AST, TypeAnn, x](ann, paraOriginals[AST, TypeAnn, Collected, x](node))
      (self, Seq.empty)
  }
  t.paraAnn(alg)
}

def dataTypeApplication[D](
    t: TypeRec[Type],
    dataTypes: Map[TypeVariable, D]
)(paramsOf: D => Seq[TypeVariable]): Either[String, (TypeVariable, D, Seq[TypeRec[Type]])] = {
  val (head, args) = collectTypeApps(t)
  head.projectT match {
    case AST.TypeVar(variable) => dataTypes.get(variable) match {
      case Some(dataDef) if paramsOf(dataDef).length == args.length => Right((variable, dataDef, args))
      case Some(dataDef) => Left(s"Data type ${variable.name} expects ${paramsOf(dataDef).length} arguments, got ${args.length}")
      case None => Left(s"Not a data type: ${t.show}")
    }
    case _ => Left(s"Not a data type: ${t.show}")
  }
}

def applyTypeConstructor(head: TypeVariable, args: Seq[TypeRec[Type]]): TypeRec[Type] =
  args.foldLeft(typeVarT(head)) { (acc, arg) => typeAppT(acc, arg) }
