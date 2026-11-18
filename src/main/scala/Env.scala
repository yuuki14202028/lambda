package com.yuuki14202028

/** tpe は構成子の型 ∀ā. フィールド… → D[ā]。index はデータ型の中での順番。
 *  fields はフィールドごとの名前と、それを読む関数（名前付きフィールドだけが持つ） */
final case class ConstructorDef(symbol: Symbol, data: Symbol, index: Int, tpe: Type, fields: List[FieldDef]) {
  def arity: Int = fields.length

  /** 型引数を渡したときの、フィールドの型 */
  def fieldTypes(typeArguments: List[Type]): List[Type] = {
    val instantiated = typeArguments.foldLeft(tpe) {
      case (Type.Forall(_, _, body), argument) => Type.open(body, argument)
      case (other, _) => sys.error(s"Compiler invariant violation: constructor $symbol has type ${Type.show(other)}")
    }
    Type.arrows(instantiated)._1.take(arity)
  }

  /** index 番目のフィールドを読む関数の型 ∀ā. D[ā] → フィールドの型 */
  def accessorType(index: Int): Type = {
    val (parameters, body) = Type.openForalls(tpe)
    val (fieldTypes, result) = Type.arrows(body)
    parameters.foldRight(Type.Arrow(result, fieldTypes(index)): Type) { case ((parameter, kind), inner) => Type.forall(parameter, kind, inner) }
  }
}

final case class FieldDef(name: Option[String], accessor: Option[Symbol])

final case class DataDef(symbol: Symbol, parameters: List[Kind], constructors: List[ConstructorDef]) {
  def kind: Kind = parameters.foldRight(Kind.Star: Kind)(Kind.Arrow(_, _))
}

/** signature は trait のパラメータ（parameters の Symbol）で書いたメソッドの型。default はデフォルトの本体を持つ関数 */
final case class MethodDef(name: String, symbol: Symbol, signature: Type, default: Option[Symbol])

/** trait C[p̄] with S̄ { メソッド… }。辞書のデータ型でもある: data C[p̄] = { MkC(S̄ の辞書…)(メソッド…) }。
 *  supers は、スーパークラスの制約と、辞書からその辞書を取り出す関数 */
final case class TraitDef(
    symbol: Symbol,
    parameters: List[(Symbol, Kind)],
    constructor: Symbol,
    supers: List[(Type, Symbol)],
    methods: List[MethodDef]
) {
  def instantiate(tpe: Type, arguments: List[Type]): Type = Type.substitute(tpe, parameters.map(_._1).zip(arguments).toMap)
  def required: List[MethodDef] = methods.filter(_.default.isEmpty)

  /** C[p̄]: 辞書の型 */
  def dictionaryType: Type = parameters.foldLeft(Type.Data(symbol): Type) { case (applied, (parameter, _)) => Type.Apply(applied, Type.Var(parameter)) }

  private def quantified(tpe: Type): Type = parameters.foldRight(tpe) { case ((parameter, kind), inner) => Type.forall(parameter, kind, inner) }

  /** メソッドの型 ∀p̄. [C[p̄]] → シグネチャ */
  def methodType(method: MethodDef): Type = quantified(Type.Given(dictionaryType, method.signature))

  /** スーパークラスの辞書を取り出す関数の型 ∀p̄. C[p̄] → S */
  def projectionType(superConstraint: Type): Type = quantified(Type.Arrow(dictionaryType, superConstraint))

  /** デフォルトの本体を持つ関数の型 ∀p̄. [スーパークラス…] → 必須メソッド… → シグネチャ */
  def defaultType(method: MethodDef): Type =
    quantified(supers.map(_._1).foldRight(required.map(_.signature).foldRight(method.signature)(Type.Arrow(_, _)))(Type.Given(_, _)))

  /** 辞書の構成子の型 ∀p̄. スーパークラスの辞書… → メソッド… → C[p̄] */
  def constructorType: Type = quantified((supers.map(_._1) ++ methods.map(_.signature)).foldRight(dictionaryType)(Type.Arrow(_, _)))

  /** 辞書のデータ型 */
  def dictionary: DataDef = {
    val fields = (supers ++ methods).map(_ => FieldDef(None, None))
    DataDef(symbol, parameters.map(_._2), List(ConstructorDef(constructor, symbol, 0, constructorType, fields)))
  }
}

/** 解決に使える辞書: 制約（辞書の型）と、その辞書を表す式 */
final case class Evidence(constraint: Type, term: Typed)

/** 型の先頭の形。インスタンスは「trait と、引数それぞれの先頭の形」ごとに 1 つまで */
enum TypeHead {
  case Named(symbol: Symbol)
  case Primitive(name: String)
  case Function
}

object TypeHead {
  def of(tpe: Type): Option[TypeHead] = tpe match {
    case Type.Data(symbol) => Some(Named(symbol))
    case Type.Var(symbol) => Some(Named(symbol))
    case Type.Primitive(name) => Some(Primitive(name))
    case Type.Arrow(_, _) => Some(Function)
    case Type.Apply(function, _) => of(function)
    case _ => None
  }
}

final case class InstanceKey(traitSymbol: Symbol, heads: List[TypeHead])

/** 型の検査に使う環境（型検査と Core の検査で共有する） */
final case class Env(
    values: Map[Symbol, Type],
    typeVariables: Map[Symbol, Kind],
    aliases: Map[Symbol, (Type, Kind)],
    dataTypes: Map[Symbol, DataDef],
    constructors: Map[Symbol, ConstructorDef],
    traits: Map[Symbol, TraitDef],
    // 宣言されたインスタンス（辞書を指す変数）
    instances: Map[InstanceKey, Evidence],
    // スコープにある制約（with 節・using）。内側が先頭
    givens: List[Evidence],
    // 宣言されているが、ここからは見えないインスタンス（エラーの説明にだけ使う）。true なら、同じ rec の組の後ろにある
    hidden: List[(Type, Boolean)] = Nil,
    // 宣言に失敗したインスタンス（これを使う所のエラーは報告しない）
    failedInstances: Set[InstanceKey] = Set.empty
) {
  def withValue(symbol: Symbol, tpe: Type): Env = copy(values = values + (symbol -> tpe))
  def withValues(bindings: Iterable[(Symbol, Type)]): Env = copy(values = values ++ bindings)
  def withTypeVariable(symbol: Symbol, kind: Kind): Env = copy(typeVariables = typeVariables + (symbol -> kind))
  def withTypeVariables(parameters: Iterable[(Symbol, Kind)]): Env = copy(typeVariables = typeVariables ++ parameters)
  def withAlias(symbol: Symbol, tpe: Type, kind: Kind): Env = copy(aliases = aliases + (symbol -> (tpe, kind)))
  def withGiven(evidence: Evidence): Env = copy(givens = evidence :: givens)
  def withInstances(added: Iterable[(InstanceKey, Evidence)]): Env = copy(instances = instances ++ added)

  /** データ型と、その構成子（構成子は値でもある） */
  def withDataTypes(definitions: List[DataDef]): Env = {
    val all = definitions.flatMap(_.constructors)
    copy(
      dataTypes = dataTypes ++ definitions.map(d => d.symbol -> d),
      constructors = constructors ++ all.map(c => c.symbol -> c),
      values = values ++ all.map(c => c.symbol -> c.tpe)
    )
  }

  /** 型のカインド。型は検査済みのものに限る（合わなければコンパイラのバグ） */
  def kindOf(tpe: Type): Kind = tpe match {
    case Type.Var(symbol) => typeVariables.getOrElse(symbol, invariant(s"type variable $symbol is not in scope"))
    case Type.Data(symbol) => dataTypes.getOrElse(symbol, invariant(s"data type $symbol is not defined")).kind
    case Type.Primitive(name) => (1 to Primitives.arity(name)).foldLeft(Kind.Star: Kind)((kind, _) => Kind.Arrow(Kind.Star, kind))
    case Type.Arrow(_, _) | Type.Given(_, _) | Type.Forall(_, _, _) => Kind.Star
    case Type.Lambda(name, kind, body) =>
      val parameter = new Symbol(name.name)
      Kind.Arrow(kind, withTypeVariable(parameter, kind).kindOf(Type.open(body, Type.Var(parameter))))
    case Type.Apply(function, _) => kindOf(function) match {
      case Kind.Arrow(_, result) => result
      case Kind.Star => invariant(s"${Type.show(function)} is applied but has kind *")
    }
    case Type.Bound(_) => invariant("a bound type variable escaped its binder")
  }

  private def invariant(message: String): Nothing = sys.error(s"Compiler invariant violation: $message")
}

object Env {
  val empty: Env = Env(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Nil)
}
