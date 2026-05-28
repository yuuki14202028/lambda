package com.yuuki14202028

object Equivalence {
  
  def alpha(left: TypeRec[Type], right: TypeRec[Type]): Boolean = {
    type Eq[I] = Map[TypeVariable, TypeVariable] => TypeRec[I] => Boolean
    val alg: Algebra[TypedAST, Eq] = [x] => (he: TypedAST[Eq, x]) => bound => other =>
      (he.ast, other.project) match {
        case (AST.Primitive(ln), AST.Primitive(rn)) => ln == rn
        case (AST.TypeVar(lv), AST.TypeVar(rv)) =>
          bound.get(lv) match {
            case Some(b) => b == rv
            case None => !bound.contains(rv) && lv == rv
          }
        case (AST.Arrow(lf, lt), AST.Arrow(rf, rt)) => lf(bound)(rf) && lt(bound)(rt)
        case (AST.ForAll(lv, lk, lb), AST.ForAll(rv, rk, rb)) => lk == rk && lb(bound + (lv -> rv))(rb)
        case (AST.TypeAbs(lv, lk, lb), AST.TypeAbs(rv, rk, rb)) => lk == rk && lb(bound + (lv -> rv))(rb)
        case (AST.TypeApp(lf, la), AST.TypeApp(rf, ra)) => lf(bound)(rf) && la(bound)(ra)
        case _ => false
      }
    left.cata(alg)(Map.empty)(right)
  }

  def normalize(t: TypeRec[Type]): TypeRec[Type] = {
    val alg: Algebra[TypedAST, TypeRec] = [x] => (he: TypedAST[TypeRec, x]) => he match {
      case HCofreeT(ann, AST.TypeApp(function, argument)) =>
        function.project match {
          case AST.TypeAbs(variable, _, body) =>
            normalize(substType(variable, argument, body))
          case _ =>
            HCofree(ann, AST.TypeApp(function, argument))
        }
      case HCofreeT(ann, ast) => HCofree(ann, ast)
    }
    t.cata(alg)
  }

  def beta(left: TypeRec[Type], right: TypeRec[Type]): Boolean = {
    alpha(normalize(left), normalize(right))
  }



}