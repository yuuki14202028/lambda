package com.yuuki14202028

/** 名前解決の結果。名前はすべて Symbol になり、演算子は木に組まれ、パターンの名前は構成子か変数かが決まっている。
 * 糖衣（リテラル・組・文字列埋め込み・context）は形を保ち、宣言された関数の Symbol を持つ。
 */
object Resolved {

  enum TypeExpr {
    def span: Span

    case Named(symbol: Symbol, span: Span)
    case Primitive(name: String, span: Span)
    case Arrow(from: TypeExpr, to: TypeExpr, span: Span)
    case Forall(parameter: Symbol, kind: Kind, body: TypeExpr, span: Span)
    case Lambda(parameter: Symbol, kind: Kind, body: TypeExpr, span: Span)
    case Apply(function: TypeExpr, argument: TypeExpr, span: Span)
    // (A, B): 組に宣言された関数 function の結果の型
    case Tuple(elements: List[TypeExpr], function: Symbol, span: Span)
    case Given(constraint: TypeExpr, to: TypeExpr, span: Span)
    // 検査済みの型（derive が作るコードの注釈に使う）
    case Known(tpe: Type, span: Span)
  }

  enum Expr {
    def span: Span

    case Variable(symbol: Symbol, span: Span)
    case Constant(value: Literal, span: Span)
    case Lambda(parameter: Symbol, parameterType: TypeExpr, body: Expr, span: Span)
    case TypeLambda(parameter: Symbol, kind: Kind, body: Expr, span: Span)
    case GivenLambda(constraint: TypeExpr, body: Expr, span: Span)
    case Apply(head: Expr, arguments: List[Argument], span: Span)
    // e.name: どの関数かは e の型で決まるので、候補を持っておく
    case Select(receiver: Expr, name: String, candidates: Candidates, span: Span)
    // a ⊕ b: ⊕ に宣言された関数 function の適用 function(a)(b)
    case Binary(operator: String, function: Symbol, left: Expr, right: Expr, span: Span)
    case Unary(operator: String, function: Symbol, operand: Expr, span: Span)
    // && と ||
    case Logic(isAnd: Boolean, left: Expr, right: Expr, span: Span)
    case If(condition: Expr, thenBranch: Expr, elseBranch: Expr, span: Span)
    case Let(binding: Binding, body: Expr, span: Span)
    case LetRec(bindings: List[Binding], body: Expr, span: Span)
    case LetPattern(pattern: Pattern, tpe: TypeExpr, value: Expr, body: Expr, span: Span)
    case LocalData(group: List[Data], body: Expr, span: Span)
    case LocalAlias(alias: Alias, body: Expr, span: Span)
    case Match(scrutinee: Expr, cases: List[Case], span: Span)
    case Fold(scrutinee: Expr, resultType: TypeExpr, cases: List[Case], span: Span)
    case Block(discarded: List[Expr], result: Option[Expr], span: Span)
    // (x, y): function(x)(y)
    case Tuple(elements: List[Expr], function: Symbol, span: Span)
    // [x, y] / setOf[x, y]: cons(x)(cons(y)(nil))
    case ListLiteral(elements: List[Expr], nil: Symbol, cons: Symbol, span: Span)
    // `text {e}`: 埋め込んだ式は show(e)
    case Interpolation(parts: List[Either[String, Expr]], show: Symbol, span: Span)
    case Context(monad: TypeExpr, steps: List[ContextStep], result: Expr, flatMap: Symbol, pure: Symbol, span: Span)
    case Summon(constraint: TypeExpr, span: Span)
    case Using(dictionary: Expr, body: Expr, span: Span)
    case Instance(instance: Impl, span: Span)
    case Primitive(intrinsic: Intrinsic, arguments: List[Expr], span: Span)
    case Foreign(tpe: TypeExpr, name: String, span: Span)
  }

  /** 型と同じ名前の module と、その場所から見えるメンバー */
  final case class TypeModule(name: String, members: Map[String, Symbol])

  /** x.name の name を探す所: 型（または組み込みの型）ごとの module と、スコープにある trait のメソッド name */
  final case class Candidates(byType: Map[Symbol, TypeModule], byPrimitive: Map[String, TypeModule], method: Option[Symbol])

  enum Argument {
    case OfType(tpe: TypeExpr)
    case OfValue(value: Expr)
    case OfDictionary(value: Expr)
    case Named(fields: List[(String, Span, Expr)], span: Span)
  }

  final case class Binding(symbol: Symbol, tpe: TypeExpr, value: Expr, span: Span)

  enum ContextStep {
    case Bind(pattern: Pattern, tpe: TypeExpr, value: Expr, span: Span)
    case Let(binding: Binding)
    case LetPattern(pattern: Pattern, tpe: TypeExpr, value: Expr, span: Span)
    case Rec(bindings: List[Binding])
  }

  enum Pattern {
    def span: Span

    case Wildcard(span: Span)
    case Bind(symbol: Symbol, span: Span)
    case Constructor(symbol: Symbol, arguments: List[Pattern], span: Span)
    // equals は == に宣言された関数（宣言がなければ None。bool と unit は比べずに済むので、なくてもよい）
    case Constant(value: Literal, equals: Option[Symbol], span: Span)
    case Or(alternatives: List[Pattern], span: Span)
  }

  final case class Case(pattern: Pattern, guard: Option[Expr], body: Expr)

  /** accessor は、名前付きフィールドを読む関数（型の module のメンバー） */
  final case class Field(name: Option[String], accessor: Option[Symbol], tpe: TypeExpr)

  final case class Constructor(symbol: Symbol, fields: List[Field], span: Span)

  final case class Data(symbol: Symbol, parameters: List[(Symbol, Kind)], constructors: List[Constructor], span: Span)

  final case class Alias(symbol: Symbol, parameters: List[(Symbol, Kind)], body: TypeExpr, opaque: Boolean, span: Span)

  /** local は、デフォルトの本体の中でこのメソッドを指す名前（必須メソッドだけが見える） */
  final case class Method(name: String, symbol: Symbol, local: Symbol, tpe: TypeExpr, default: Option[Expr], span: Span)

  final case class Trait(
      symbol: Symbol,
      constructor: Symbol,
      parameters: List[(Symbol, Kind)],
      supers: List[TypeExpr],
      methods: List[Method],
      span: Span
  )

  final case class MethodImpl(name: String, tpe: Option[TypeExpr], value: Expr, span: Span)

  /** traitParameters は、メソッドの注釈の中で trait のパラメータの名前を指す Symbol（その名前の型がスコープにあれば None） */
  final case class Impl(
      dictionary: Symbol,
      parameters: List[(Symbol, Kind)],
      traitSymbol: Symbol,
      traitParameters: List[Option[Symbol]],
      arguments: List[TypeExpr],
      context: List[TypeExpr],
      methods: List[MethodImpl],
      span: Span
  )

  enum Instance {
    case Written(impl: Impl)
    case Derived(dictionary: Symbol, traitSymbol: Symbol, target: Symbol, span: Span)
  }

  enum Decl {
    case Let(binding: Binding)
    case Rec(bindings: List[Binding])
    // 一緒に宣言されたデータ型の組（rec でなければ 1 つ）
    case DataTypes(group: List[Data])
    case TypeAlias(alias: Alias)
    case TraitDecl(definition: Trait)
    // recursive なら、組のインスタンスがメソッドの本体から見える
    case Instances(group: List[Instance], recursive: Boolean)
    // module のブロック。名前は解決済みなので、opaque type を閉じる範囲としてだけ残る
    case Module(declarations: List[Decl])
  }

  final case class Program(declarations: List[Decl], main: Option[Symbol])
}
