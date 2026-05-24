package com.yuuki14202028

case class DataDef(params: Seq[(TypeVariable, Kind)], constructors: Seq[ConstructorDef], recursive: Boolean) {
  def paramVars: Seq[TypeVariable] = params.map(_._1)
}
case class ConstructorDef(name: Variable, owner: TypeVariable, fields: Seq[TypeRec[Type]], tag: Int)
