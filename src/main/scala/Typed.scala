package com.yuuki14202028

/** 型検査の結果。どの式も型を持ち、検査が決めたこと（読んだ型引数、解決した辞書、match の決定木）が入っている */
enum Typed {
  def tpe: Type

  case Variable(symbol: Symbol, tpe: Type)
  case Constant(value: Literal, tpe: Type)
  case Lambda(parameter: Symbol, parameterType: Type, body: Typed, tpe: Type)
  case TypeLambda(parameter: Symbol, kind: Kind, body: Typed, tpe: Type)
  // λ[constraint]. body: 辞書を受け取る関数（型は [constraint] → …）
  case GivenLambda(parameter: Symbol, constraint: Type, body: Typed, tpe: Type)
  // 型引数も辞書も、書かれたものも検査が決めたものも、渡す位置に入っている
  case Apply(head: Typed, arguments: List[Typed.Argument], tpe: Type)
  // 糖衣（演算子・ドット記法・リテラルなど）。meaning は検査が決めたその意味で、Lower はそれを使う
  case Sugar(form: SugarForm, meaning: Typed, tpe: Type)
  case Logic(isAnd: Boolean, left: Typed, right: Typed, tpe: Type)
  case If(condition: Typed, thenBranch: Typed, elseBranch: Typed, tpe: Type)
  case Let(binding: Typed.Binding, body: Typed, tpe: Type)
  case LetRec(bindings: List[Typed.Binding], body: Typed, tpe: Type)
  case LocalData(group: List[DataDef], body: Typed, tpe: Type)
  // root は scrutinee の値を束縛する変数。decision の葉が cases の番号を指す
  case Match(scrutinee: Typed, root: Occurrence, decision: Decision, cases: List[Typed.Case], tpe: Type)
  case Block(discarded: List[Typed], result: Option[Typed], tpe: Type)
  // `text {e}`: parts はどれも文字列の式
  case Interpolation(parts: List[Typed], tpe: Type)
  case Primitive(intrinsic: Intrinsic, arguments: List[Typed], tpe: Type)
  case Foreign(name: String, tpe: Type)

  /** 式の中に symbols のどれかが現れるか */
  def mentions(symbols: Set[Symbol]): Boolean = this match {
    case Variable(symbol, _) => symbols(symbol)
    case Constant(_, _) | Foreign(_, _) => false
    case Lambda(_, _, body, _) => body.mentions(symbols)
    case TypeLambda(_, _, body, _) => body.mentions(symbols)
    case GivenLambda(_, _, body, _) => body.mentions(symbols)
    case Apply(head, arguments, _) => head.mentions(symbols) || arguments.exists {
      case Typed.Argument.OfValue(value) => value.mentions(symbols)
      case Typed.Argument.OfType(_) => false
    }
    case Sugar(_, meaning, _) => meaning.mentions(symbols)
    case Logic(_, left, right, _) => left.mentions(symbols) || right.mentions(symbols)
    case If(condition, thenBranch, elseBranch, _) => List(condition, thenBranch, elseBranch).exists(_.mentions(symbols))
    case Let(binding, body, _) => binding.value.mentions(symbols) || body.mentions(symbols)
    case LetRec(bindings, body, _) => bindings.exists(_.value.mentions(symbols)) || body.mentions(symbols)
    case LocalData(_, body, _) => body.mentions(symbols)
    case Match(scrutinee, _, decision, cases, _) =>
      scrutinee.mentions(symbols) || decision.mentions(symbols) || cases.exists(c => c.body.mentions(symbols) || c.guard.exists(_.mentions(symbols)))
    case Block(discarded, result, _) => (discarded ++ result).exists(_.mentions(symbols))
    case Interpolation(parts, _) => parts.exists(_.mentions(symbols))
    case Primitive(_, arguments, _) => arguments.exists(_.mentions(symbols))
  }
}

object Typed {
  enum Argument {
    case OfType(tpe: Type)
    // 値。関数の型が [C] → … なら、C の辞書
    case OfValue(value: Typed)
  }

  final case class Binding(symbol: Symbol, tpe: Type, value: Typed)

  /** bindings は、パターンが束縛する変数とその型 */
  final case class Case(bindings: List[(Symbol, Type)], guard: Option[Typed], body: Typed)

  enum Decl {
    case Let(binding: Binding)
    case Rec(bindings: List[Binding])
    case DataTypes(group: List[DataDef])
    // dictionary は辞書のデータ型、defaults はメソッドのデフォルトの本体を持つ関数
    case Trait(definition: TraitDef, dictionary: DataDef, defaults: List[Binding])
  }

  /** opaque は、opaque type と、その中身の型（Lower が読み替える） */
  final case class Program(declarations: List[Decl], main: Symbol, opaque: Map[Symbol, Type])
}

/** 糖衣の種類（書かれた形の記録） */
enum SugarForm {
  case Operator(symbol: String)
  case Select(name: String)
  case Tuple
  case ListLiteral
  case NamedArguments
  case Context
  case Fold
  case LetPattern
  case Using
}

/** match で調べる値（それを束縛した変数と、その型） */
final case class Occurrence(symbol: Symbol, tpe: Type)

/** match の決定木 */
enum Decision {
  case Fail
  // 分岐の本体に着く。条件付きの分岐なら、条件が偽のときに otherwise へ進む
  case Leaf(caseIndex: Int, bindings: List[(Symbol, Occurrence)], otherwise: Option[Decision])
  // 値の構成子で分かれる
  case Switch(on: Occurrence, branches: List[Decision.Branch])
  case IfBool(on: Occurrence, whenTrue: Decision, whenFalse: Decision)
  // リテラルと等しいかで分かれる。test は「値を受け取って、そのリテラルと等しいかを返す関数」
  case Equal(on: Occurrence, literal: Literal, test: Typed, whenEqual: Decision, otherwise: Decision)

  def mentions(symbols: Set[Symbol]): Boolean = this match {
    case Fail => false
    case Leaf(_, _, otherwise) => otherwise.exists(_.mentions(symbols))
    case Switch(_, branches) => branches.exists(_.next.mentions(symbols))
    case IfBool(_, whenTrue, whenFalse) => whenTrue.mentions(symbols) || whenFalse.mentions(symbols)
    case Equal(_, _, test, whenEqual, otherwise) => test.mentions(symbols) || whenEqual.mentions(symbols) || otherwise.mentions(symbols)
  }
}

object Decision {
  final case class Branch(constructor: ConstructorDef, fields: List[Occurrence], next: Decision)
}
