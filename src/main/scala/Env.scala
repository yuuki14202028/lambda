package com.yuuki14202028

case class TypeAlias(params: Seq[(TypeVariable, Kind)], body: TypeRec[Type]) {
  def paramVars: Seq[TypeVariable] = params.map(_._1)
}

case class DataDef(params: Seq[(TypeVariable, Kind)], constructors: Seq[ConstructorDef], recursive: Boolean) {
  def paramVars: Seq[TypeVariable] = params.map(_._1)
}

case class ConstructorDef(name: Variable, owner: TypeVariable, fields: Seq[TypeRec[Type]], tag: Int)

case class TypeConstraint(name: TypeVariable, arg: Seq[TypeRec[Type]])
case class TraitDef(param: Seq[(TypeVariable, Kind)], methods: Seq[(Variable, TypeRec[Type])], supers: Seq[TypeConstraint])
case class InstanceDef(name: TypeVariable, target: TypeRec[Type], context: Seq[TypeConstraint], dictName: Variable)

def dictionaryConstructor(traitName: TypeVariable): Variable = Variable(s"Mk${traitName.name}")

def instanceDictionaryName(traitName: TypeVariable, head: String): Variable =
  Variable(s"$$inst_${traitName.name}_$head")

def instanceKey(traitName: TypeVariable, head: String): (Seq[TypeVariable], Variable) =
  (Seq(traitName), Variable(head))

case class Env(
    values: Map[Variable, TypeRec[Type]],
    typeVars: Map[TypeVariable, Kind],
    typeAliases: Map[TypeVariable, TypeAlias],
    dataTypes: Map[TypeVariable, DataDef],
    constructors: Map[Variable, ConstructorDef],
    traits: Map[TypeVariable, TraitDef],
    instances: Map[(Seq[TypeVariable], Variable), InstanceDef],
    constrains: Map[Variable, Seq[TypeConstraint]],
    dictsInScope: Map[TypeConstraint, Variable]
)

object Env {
  val empty: Env = Env(
    Map.empty, Map.empty, Map.empty, Map.empty, Map.empty,
    Map.empty, Map.empty, Map.empty, Map.empty
  )
}

case class DataEnv(dataTypes: Map[TypeVariable, DataDef])

object DataEnv {
  def from(env: Env): DataEnv = DataEnv(env.dataTypes)
}
