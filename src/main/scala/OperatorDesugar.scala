package com.yuuki14202028

/** 構文に宣言された関数の脱糖。型検査が記録した「宣言された関数を、値の型で具体化したもの」を使って、
 * 関数の呼び出しへ書き換える。&& と || は bool 専用の組み込みで、if にする。
 *
 *   a ⊕ b  ↦  f[T](a)(b)        ⊖a  ↦  g[T](a)
 *   a && b ↦  if a then b else false        a || b ↦  if a then true else b
 *   `s{x}`  ↦  concat("s")(interpolate[T](x))（文字列どうしは組み込みの concat でつなぐ）
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
    case (ExprAnn(_), AST.StrInterp(parts)) =>
      val concatType = arrowT(stringTypeT, arrowT(stringTypeT, stringTypeT))
      val strings = parts.map {
        case InterpPart.Text(value) => stringLitT(value, stringTypeT)
        case InterpPart.Embed(value, resolved) => mkApp(resolved.getOrElse(invariant("an embedded value reached OperatorDesugar without a resolved function")), value)
      }
      strings.reduceLeftOption((acc, part) => mkApp(mkApp(foreignT(Variable("concat"), concatType, concatType), acc), part))
        .getOrElse(stringLitT("", stringTypeT))
    case (ann, node) => HCofree(ann, node)
  }
}
