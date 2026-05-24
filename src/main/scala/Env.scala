package com.yuuki14202028

case class TypeAlias(params: Seq[(TypeVariable, Kind)], body: TypeRec[Type]) {
  def paramVars: Seq[TypeVariable] = params.map(_._1)
}

case class Env(
    values: Map[Variable, TypeRec[Type]],
    typeVars: Map[TypeVariable, Kind],
    typeAliases: Map[TypeVariable, TypeAlias],
    dataTypes: Map[TypeVariable, DataDef],
    constructors: Map[Variable, ConstructorDef]
)

object Env {
  val empty: Env = Env(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)
}

case class DataEnv(dataTypes: Map[TypeVariable, DataDef])

object DataEnv {
  def from(env: Env): DataEnv = DataEnv(env.dataTypes)
}
