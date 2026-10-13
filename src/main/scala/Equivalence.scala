package com.yuuki14202028

object Equivalence {
  
  def alpha(left: TypeRec[Type], right: TypeRec[Type]): Boolean = {
    type Bound = (Map[TypeVariable, TypeVariable], Map[TypeVariable, TypeVariable])
    type Eq[I] = Bound => TypeRec[I] => Boolean
    val alg: Algebra[TypedAST, Eq] = [x] => he => bound => other => {
      val (l2r, r2l) = bound
      (he.ast, other.project) match {
        case (AST.Primitive(ln), AST.Primitive(rn)) => ln == rn
        case (AST.TypeVar(lv), AST.TypeVar(rv)) =>
          (l2r.get(lv), r2l.get(rv)) match {
            case (Some(rb), Some(lb)) => rb == rv && lb == lv
            case (None, None) => lv == rv
            case _ => false
          }
        case (AST.Arrow(lf, lt), AST.Arrow(rf, rt)) => lf(bound)(rf) && lt(bound)(rt)
        case (AST.ForAll(lv, lk, lb), AST.ForAll(rv, rk, rb)) =>
          lk == rk && lb((l2r + (lv -> rv), r2l + (rv -> lv)))(rb)
        case (AST.TypeAbs(lv, lk, lb), AST.TypeAbs(rv, rk, rb)) =>
          lk == rk && lb((l2r + (lv -> rv), r2l + (rv -> lv)))(rb)
        case (AST.TypeApp(lf, la), AST.TypeApp(rf, ra)) => lf(bound)(rf) && la(bound)(ra)
        case (AST.InstArrow(lc, lt), AST.InstArrow(rc, rt)) =>
          // 量化された制約のパラメータは、制約の中だけを束縛する
          def same(l: Constraint[Eq], r: Constraint[TypeRec], outer: Bound): Boolean =
            l.name == r.name && l.arg.length == r.arg.length &&
              l.params.map(_._2) == r.params.map(_._2) && l.premises.length == r.premises.length && {
                val inner = l.params.zip(r.params).foldLeft(outer) { case ((toRight, toLeft), ((lv, _), (rv, _))) =>
                  (toRight + (lv -> rv), toLeft + (rv -> lv))
                }
                l.arg.zip(r.arg).forall { case (la, ra) => la(inner)(ra) } &&
                  l.premises.zip(r.premises).forall { case (lp, rp) => same(lp, rp, inner) }
              }
          same(lc, rc, bound) && lt(bound)(rt)
        case _ => false
      }
    }
    left.cata(alg)((Map.empty, Map.empty))(right)
  }

  def normalize(t: TypeRec[Type]): TypeRec[Type] = {
    val alg: Algebra[TypedAST, TypeRec] = [x] => he => he match {
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

}