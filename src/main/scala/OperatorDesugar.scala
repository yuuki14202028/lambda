package com.yuuki14202028

/** 演算子の脱糖。型検査が記録した「演算子に宣言された関数を、左の値の型で具体化したもの」を使って、
 * 関数の呼び出しへ書き換える。&& と || は bool 専用の組み込みで、if にする。
 *
 *   a ⊕ b  ↦  f[T](a)(b)        ⊖a  ↦  g[T](a)
 *   a && b ↦  if a then b else false        a || b ↦  if a then true else b
 */
object OperatorDesugar {

  private def invariant(msg: String): Nothing = sys.error(s"Compiler invariant violation: $msg")

  def desugar(program: TypeRec[AST.Program.type]): TypeRec[AST.Program.type] = program.cata(desugarAlg)

  private val desugarAlg: Algebra[TypedAST, TypeRec] = [x] => he => (he.ann, he.ast) match {
    case (ExprAnn(t), AST.BinOp(Operators.shortAnd, left, right, _)) => ifT(t, left, right, boolT(false, boolTypeT))
    case (ExprAnn(t), AST.BinOp(Operators.shortOr, left, right, _)) => ifT(t, left, boolT(true, boolTypeT), right)
    case (ExprAnn(_), AST.BinOp(op, left, right, resolved)) =>
      mkApp(mkApp(resolved.getOrElse(invariant(s"operator $op reached OperatorDesugar without a resolved function")), left), right)
    case (ExprAnn(_), AST.UnaryOp(op, body, resolved)) =>
      mkApp(resolved.getOrElse(invariant(s"operator $op reached OperatorDesugar without a resolved function")), body)
    case (ann, node) => HCofree(ann, node)
  }
}
