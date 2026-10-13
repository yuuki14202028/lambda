package com.yuuki14202028

/** ドット記法の脱糖。型検査が記録した「受け手の型の module にある関数を、型引数と制約まで適用したもの」に受け手を渡す。
 *
 *   x.f[T]  ↦  M.f[受け手の型から読んだ型引数][T](x)
 */
object SelectDesugar {

  private def invariant(msg: String): Nothing = sys.error(s"Compiler invariant violation: $msg")

  def desugar(program: TypeRec[AST.Program.type]): TypeRec[AST.Program.type] = program.cata(desugarAlg)

  private val desugarAlg: Algebra[TypedAST, TypeRec] = [x] => he => (he.ann, he.ast) match {
    case (ExprAnn(_), AST.Select(receiver, name, _, resolved)) =>
      mkApp(resolved.getOrElse(invariant(s".$name reached SelectDesugar without a resolved function")), receiver)
    case (ExprAnn(_), AST.Path(name, _)) => invariant(s"the path ${name.name} must be resolved by the type checker")
    case (ann, node) => HCofree(ann, node)
  }
}
