package com.yuuki14202028

object Normalize {

  def normalize(t: TypeRec[Type]): TypeRec[Type] = {
    val alg: HCofreeAlgebra[AST, TypeAnn, TypeRec] = [x] => (ann, node) => node match {
      case AST.TypeApp(function, argument) =>
        function.projectT match {
          case AST.TypeAbs(variable, _, body) =>
            normalize(substType(variable, argument, body))
          case _ =>
            HCofree(ann, AST.TypeApp(function, argument))
        }
      case ast => HCofree(ann, ast)
    }
    t.cataAnn(alg)
  }

  def equalType(left: TypeRec[Type], right: TypeRec[Type]): Boolean = {
    sameType(normalize(left), normalize(right))
  }

}