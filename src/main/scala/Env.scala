package com.yuuki14202028

case class TypeAlias(params: Seq[(TypeVariable, Kind)], body: TypeRec[Type]) {
  def paramVars: Seq[TypeVariable] = params.map(_._1)
}

case class DataDef(params: Seq[(TypeVariable, Kind)], constructors: Seq[ConstructorDef], recursive: Boolean) {
  def paramVars: Seq[TypeVariable] = params.map(_._1)
}

case class ConstructorDef(name: Variable, owner: TypeVariable, fields: Seq[TypeRec[Type]], tag: Int)

// 引数が展開済み（別名解決・β 正規化済み）の制約
type TypeConstraint = Constraint[TypeRec]

object TypeConstraint {
  def apply(name: TypeVariable, arg: Seq[TypeRec[Type]]): TypeConstraint = Constraint[TypeRec](name, arg)
}

// methods のシグネチャは trait パラメータを自由変数とする型（制約 C[p̄] を含まない）
case class TraitDef(param: Seq[(TypeVariable, Kind)], methods: Seq[(Variable, TypeRec[Type])], supers: Seq[TypeConstraint]) {
  def paramVars: Seq[TypeVariable] = param.map(_._1)
}
case class InstanceDef(name: TypeVariable, targets: Seq[TypeRec[Type]], context: Seq[TypeConstraint], dictName: Variable, params: Seq[(TypeVariable, Kind)])

// メソッドを値として参照したときの型: ∀p̄. [C[p̄]] → sig
def methodType(traitName: TypeVariable, params: Seq[(TypeVariable, Kind)], sig: TypeRec[Type]): TypeRec[Type] = {
  val self = TypeConstraint(traitName, params.map { case (p, _) => typeVarT(p) })
  params.foldRight(instArrowT(self, sig)) { case ((p, k), acc) => forallTypeT(p, k, acc) }
}

// 制約 C[τ̄] の辞書の型。trait C は同名のデータ型（辞書）でもある
def dictionaryType(constraint: TypeConstraint): TypeRec[Type] = applyTypeConstructor(constraint.name, constraint.arg)

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
  val withContext = inst.context.map(dictionaryType).foldRight(dictType)(arrowT)
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
    givens: Seq[TypeConstraint] // スコープ中の制約（with 節・impl の文脈）
) {
  def withValue(variable: Variable, t: TypeRec[Type]): Env = copy(values = values + (variable -> t))
  def withValues(bindings: Iterable[(Variable, TypeRec[Type])]): Env = copy(values = values ++ bindings)
  def withTypeVar(variable: TypeVariable, kind: Kind): Env = copy(typeVars = typeVars + (variable -> kind))
  def withTypeVars(params: Iterable[(TypeVariable, Kind)]): Env = copy(typeVars = typeVars ++ params)
  def withGivens(constraints: Seq[TypeConstraint]): Env = copy(givens = givens ++ constraints)

  def definesTypeName(name: TypeVariable): Boolean =
    typeVars.contains(name) || typeAliases.contains(name) || dataTypes.contains(name)
}

object Env {
  val empty: Env = Env(
    Map.empty, Map.empty, Map.empty, Map.empty, Map.empty,
    Map.empty, Map.empty, Seq.empty
  )
}

case class DataEnv(dataTypes: Map[TypeVariable, DataDef])

object DataEnv {
  def from(env: Env): DataEnv = DataEnv(env.dataTypes)
}
