package com.yuuki14202028

case class TypeAlias(params: Seq[TypeVariable], body: TypeRec[Type])

case class Env(
    values: Map[Variable, TypeRec[Type]],
    typeVars: Set[TypeVariable],
    typeAliases: Map[TypeVariable, TypeAlias],
    dataTypes: Map[TypeVariable, DataDef],
    constructors: Map[Variable, ConstructorDef]
)

object Env {
  val empty: Env = Env(Map.empty, Set.empty, Map.empty, Map.empty, Map.empty)
}

case class DataEnv(dataTypes: Map[TypeVariable, DataDef])

object DataEnv {
  def from(env: Env): DataEnv = DataEnv(env.dataTypes)
}
