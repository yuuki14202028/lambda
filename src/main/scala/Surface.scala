package com.yuuki14202028

/** 構文解析の結果。名前は書かれた綴りのまま、演算子は並びのまま持つ */
object Surface {

  /** A.B.x のように . でつないだ名前 */
  final case class Path(segments: List[String], span: Span) {
    def show: String = segments.mkString(".")
  }

  enum TypeExpr {
    def span: Span

    case Name(path: Path, span: Span)
    case Arrow(from: TypeExpr, to: TypeExpr, span: Span)
    case Forall(parameter: String, kind: Kind, body: TypeExpr, span: Span)
    case Lambda(parameter: String, kind: Kind, body: TypeExpr, span: Span)
    case Apply(function: TypeExpr, argument: TypeExpr, span: Span)
    case Tuple(elements: List[TypeExpr], span: Span)
    // with C[a] が付いた型 [C[a]] → T。量化された制約の P ⇒ C も同じ形（制約は辞書の型）
    case Given(constraint: TypeExpr, to: TypeExpr, span: Span)
  }

  enum Expr {
    def span: Span

    case Variable(name: String, span: Span)
    case Constant(value: Literal, span: Span)
    case Lambda(parameter: String, parameterType: TypeExpr, body: Expr, span: Span)
    case TypeLambda(parameter: String, kind: Kind, body: Expr, span: Span)
    // 制約をスコープに入れる（with C[a] が付いた束縛の値）
    case GivenLambda(constraint: TypeExpr, body: Expr, span: Span)
    // f[T](x)(y): 頭と、左から並んだ引数
    case Apply(head: Expr, arguments: List[Argument], span: Span)
    // e.name
    case Select(receiver: Expr, name: String, nameSpan: Span, span: Span)
    // a ⊕ b ⊗ c: 優先順位は宣言で決まるので、並びのまま持つ
    case Operators(first: Expr, rest: List[OperatorStep], span: Span)
    case Prefix(operator: String, operand: Expr, span: Span)
    case If(condition: Expr, thenBranch: Expr, elseBranch: Expr, span: Span)
    case Let(binding: Binding, body: Expr, span: Span)
    case LetRec(bindings: List[Binding], body: Expr, span: Span)
    case LetPattern(pattern: Pattern, tpe: TypeExpr, value: Expr, body: Expr, span: Span)
    // 式の中の data / type / rec data
    case Local(declaration: Decl, body: Expr, span: Span)
    case Match(scrutinee: Expr, cases: List[Case], span: Span)
    case Fold(scrutinee: Expr, resultType: TypeExpr, cases: List[Case], span: Span)
    case Block(discarded: List[Expr], result: Option[Expr], span: Span)
    case Tuple(elements: List[Expr], span: Span)
    case ListLiteral(elements: List[Expr], span: Span)
    case Interpolation(parts: List[Either[String, Expr]], span: Span)
    case Context(monad: TypeExpr, steps: List[ContextStep], result: Expr, span: Span)
    case Summon(constraint: TypeExpr, span: Span)
    case Using(dictionary: Expr, body: Expr, span: Span)
    case Instance(traitName: Path, arguments: List[TypeExpr], methods: List[MethodImpl], span: Span)
    case Intrinsic(name: String, arguments: List[Expr], span: Span)
    case Foreign(tpe: TypeExpr, name: String, span: Span)
  }

  enum Argument {
    case OfType(tpe: TypeExpr)
    case OfValue(value: Expr)
    // (with d)
    case OfDictionary(value: Expr)
    // (x = e, …)
    case Named(fields: List[(String, Span, Expr)], span: Span)
    // 名前の直後の [ … ]: 型引数（f[T]）とも、名前付きのリテラルの要素（setOf[x, y]）とも読める
    case Bracket(tpe: Option[TypeExpr], elements: Option[List[Expr]], span: Span)
  }

  final case class OperatorStep(operator: String, span: Span, operand: Expr)

  final case class Binding(name: String, tpe: TypeExpr, value: Expr, span: Span)

  enum ContextStep {
    // 名前: 型 = 計算;
    case Bind(pattern: Pattern, tpe: TypeExpr, value: Expr, span: Span)
    case Let(binding: Binding)
    case LetPattern(pattern: Pattern, tpe: TypeExpr, value: Expr, span: Span)
    case Rec(bindings: List[Binding])
  }

  enum Pattern {
    def span: Span

    case Wildcard(span: Span)
    // 引数のない名前が構成子か変数かは、名前解決が決める
    case Name(path: Path, arguments: List[Pattern], span: Span)
    case Constant(value: Literal, span: Span)
    case Or(alternatives: List[Pattern], span: Span)
    case Tuple(elements: List[Pattern], span: Span)
    case ListLiteral(elements: List[Pattern], span: Span)
  }

  final case class Case(pattern: Pattern, guard: Option[Expr], body: Expr)

  final case class Field(name: Option[String], tpe: TypeExpr)

  final case class Constructor(name: String, fields: List[Field], span: Span)

  final case class Data(name: String, parameters: List[(String, Kind)], constructors: List[Constructor], span: Span)

  /** trait のメソッド。tpe はシグネチャ、default は本体（あれば） */
  final case class MethodSignature(name: String, tpe: TypeExpr, default: Option[Expr], span: Span)

  /** impl のメソッド。def m(x: T): U = e なら tpe がある。def m = e ならない */
  final case class MethodImpl(name: String, tpe: Option[TypeExpr], value: Expr, span: Span)

  enum LiteralKind {
    case Brackets
    case Named(name: String)
    case Tuple(size: Int)
  }

  enum Decl {
    def span: Span

    case Let(binding: Binding, span: Span)
    case Rec(members: List[Decl], span: Span)
    case DataType(data: Data, span: Span)
    case Alias(name: String, parameters: List[(String, Kind)], body: TypeExpr, span: Span)
    // fixity が None なら前置
    case Operator(symbol: String, fixity: Option[Fixity], function: Path, span: Span)
    case Interpolate(function: Path, span: Span)
    case LiteralSyntax(kind: LiteralKind, functions: List[Path], span: Span)
    case Trait(name: String, parameters: List[(String, Kind)], supers: List[TypeExpr], methods: List[MethodSignature], span: Span)
    case Impl(parameters: List[(String, Kind)], traitName: Path, arguments: List[TypeExpr], context: List[TypeExpr], methods: List[MethodImpl], span: Span)
    case Derive(traitName: Path, target: Path, span: Span)
    case Module(name: String, declarations: List[Decl], span: Span)
    case Open(path: Path, span: Span)
    case Private(declaration: Decl, span: Span)
    case Opaque(declaration: Decl, span: Span)
    case Import(path: String, span: Span)
  }
}
