package com.yuuki14202028

case class DataDef(params: Seq[TypeVariable], constructors: Seq[ConstructorDef])
case class ConstructorDef(name: Variable, owner: TypeVariable, fields: Seq[TypeRec[Type]], tag: Int)
