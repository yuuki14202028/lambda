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
case class InstanceDef(name: TypeVariable, targets: Seq[TypeRec[Type]], context: Seq[TypeConstraint], dictName: Variable, params: Seq[(TypeVariable, Kind)])

def dictionaryConstructor(traitName: TypeVariable): Variable = Variable(s"Mk${traitName.name}")

def instanceDictionaryName(traitName: TypeVariable, heads: Seq[String]): Variable =
  Variable(s"$$inst_${traitName.name}_${heads.mkString("_")}")

def superDictionaryName(traitName: TypeVariable, index: Int): Variable =
  Variable(s"$$super_${traitName.name}_$index")

def instanceKey(traitName: TypeVariable, heads: Seq[String]): (TypeVariable, Seq[Variable]) =
  (traitName, heads.map(Variable.apply))

// 文脈付きインスタンスの内部型: ∀ā. D₁ → … → Dₖ → C[T̄]
def instanceType(inst: InstanceDef): TypeRec[Type] = {
  val dictType = applyTypeConstructor(inst.name, inst.targets)
  val withContext = inst.context.map(tc => applyTypeConstructor(tc.name, tc.arg)).foldRight(dictType)(arrowT)
  inst.params.foldRight(withContext) { case ((p, k), acc) => forallTypeT(p, k, acc) }
}

case class Env(
    values: Map[Variable, TypeRec[Type]],
    typeVars: Map[TypeVariable, Kind],
    typeAliases: Map[TypeVariable, TypeAlias],
    dataTypes: Map[TypeVariable, DataDef],
    constructors: Map[Variable, ConstructorDef],
    traits: Map[TypeVariable, TraitDef],
    instances: Map[(TypeVariable, Seq[Variable]), InstanceDef],
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
