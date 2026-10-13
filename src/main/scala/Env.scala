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

// module の中の trait M.C の辞書コンストラクタは M.MkC（修飾は最後の名前に付ける）
def dictionaryConstructor(traitName: TypeVariable): Variable = {
  val (path, short) = traitName.name.splitAt(traitName.name.lastIndexOf('.') + 1)
  Variable(s"${path}Mk$short")
}

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

/** 短い名前から完全な名前（module M の中の x は M.x）への対応。
 * module の公開メンバーの一覧にも、修飾なしで書ける名前の一覧（Env.scope）にも使う。
 */
case class Names(values: Map[String, Variable], types: Map[String, TypeVariable], modules: Map[String, String]) {
  def withValue(short: String, full: Variable): Names = copy(values = values + (short -> full))
  def withType(short: String, full: TypeVariable): Names = copy(types = types + (short -> full))
  def withModule(short: String, full: String): Names = copy(modules = modules + (short -> full))
  def ++(other: Names): Names = Names(values ++ other.values, types ++ other.types, modules ++ other.modules)
}

object Names {
  val empty: Names = Names(Map.empty, Map.empty, Map.empty)
}

/** 定義（values / typeAliases / dataTypes / …）は完全な名前で引く。ソースに書かれた名前は
 * resolveValue / resolveType で完全な名前に直してから使う（型検査後の AST は完全な名前だけを持つ）。
 */
case class Env(
    values: Map[Variable, TypeRec[Type]],
    typeVars: Map[TypeVariable, Kind],
    typeAliases: Map[TypeVariable, TypeAlias],
    dataTypes: Map[TypeVariable, DataDef],
    constructors: Map[Variable, ConstructorDef],
    traits: Map[TypeVariable, TraitDef],
    instances: Map[(TypeVariable, Seq[Variable]), InstanceDef],
    givens: Seq[TypeConstraint], // スコープ中の制約（with 節・impl の文脈）
    path: Seq[String] = Seq.empty,                          // 検査中の module（外側から順）
    scope: Names = Names.empty,                             // 修飾なしで書ける module メンバーと、見えている module
    members: Names = Names.empty,                           // 検査中の module が公開する名前
    modules: Map[String, Names] = Map.empty,                // 宣言済みの module（完全な名前）と、その公開メンバー
    abstractTypes: Map[TypeVariable, TypeAlias] = Map.empty // 封印された opaque type と、その表現（ModuleDesugar が読み替える）
) {
  // 局所的な束縛は、同名の module メンバーを隠す
  def withValue(variable: Variable, t: TypeRec[Type]): Env =
    copy(values = values + (variable -> t), scope = scope.copy(values = scope.values - variable.name))
  def withValues(bindings: Iterable[(Variable, TypeRec[Type])]): Env =
    copy(values = values ++ bindings, scope = scope.copy(values = scope.values -- bindings.map(_._1.name)))
  def withTypeVar(variable: TypeVariable, kind: Kind): Env = withTypeVars(Seq(variable -> kind))
  def withTypeVars(params: Iterable[(TypeVariable, Kind)]): Env =
    copy(typeVars = typeVars ++ params, scope = scope.copy(types = scope.types -- params.map(_._1.name)))
  def withGivens(constraints: Seq[TypeConstraint]): Env = copy(givens = givens ++ constraints)
  def withAlias(variable: TypeVariable, alias: TypeAlias): Env =
    copy(typeAliases = typeAliases + (variable -> alias), scope = scope.copy(types = scope.types - variable.name))
  def withData(variable: TypeVariable, dataDef: DataDef): Env =
    copy(dataTypes = dataTypes + (variable -> dataDef), scope = scope.copy(types = scope.types - variable.name))

  def definesTypeName(name: TypeVariable): Boolean =
    typeVars.contains(name) || typeAliases.contains(name) || dataTypes.contains(name) || abstractTypes.contains(name)

  // ---- module ----

  def qualify(name: String): String = (path :+ name).mkString(".")

  // 検査中の module のメンバーとして宣言する名前。以降は短い名前で書け、module の外へ公開される
  def exportValue(short: Variable): Env = {
    val full = Variable(qualify(short.name))
    copy(scope = scope.withValue(short.name, full), members = members.withValue(short.name, full))
  }
  def exportType(short: TypeVariable): Env = {
    val full = TypeVariable(qualify(short.name))
    copy(scope = scope.withType(short.name, full), members = members.withType(short.name, full))
  }

  // 修飾された名前 A.B.x の A.B が指す module の公開メンバー
  private def moduleAt(qualifier: Seq[String]): Option[Names] = qualifier match {
    case Seq() => Some(scope)
    case head +: rest => rest.foldLeft(scope.modules.get(head)) { (found, name) =>
      found.flatMap(modules.get).flatMap(_.modules.get(name))
    }.flatMap(modules.get)
  }

  def resolveModule(modulePath: Seq[String]): Option[Names] = if (modulePath.isEmpty) None else moduleAt(modulePath)

  // 修飾なしの名前は、module メンバーの短い名前でなければそのまま（局所的な束縛か top-level の名前）
  def resolveValue(name: Variable): Option[Variable] = {
    val segments = name.name.split('.').toSeq
    if (segments.length == 1) Some(scope.values.getOrElse(name.name, name))
    else moduleAt(segments.init).flatMap(_.values.get(segments.last))
  }

  def resolveType(name: TypeVariable): Option[TypeVariable] = {
    val segments = name.name.split('.').toSeq
    if (segments.length == 1) Some(scope.types.getOrElse(name.name, name))
    else moduleAt(segments.init).flatMap(_.types.get(segments.last))
  }

  def lookupValue(name: Variable): Option[(Variable, TypeRec[Type])] =
    resolveValue(name).flatMap(full => values.get(full).map(full -> _))
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
