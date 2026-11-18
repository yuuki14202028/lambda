package com.yuuki14202028

/** 糖衣を外した後の言語: Fω とデータ型。型つきで、CoreCheck が検査できる */
enum Core {
  case Variable(symbol: Symbol)
  case Constant(value: Literal)
  case Lambda(parameter: Symbol, parameterType: Type, body: Core)
  case TypeLambda(parameter: Symbol, kind: Kind, body: Core)
  case Apply(function: Core, argument: Core)
  case TypeApply(function: Core, argument: Type)
  case Let(binding: Core.Binding, body: Core)
  case LetRec(bindings: List[Core.Binding], body: Core)
  // 式の中で宣言されたデータ型
  case LetData(group: List[DataDef], body: Core)
  case If(condition: Core, thenBranch: Core, elseBranch: Core)
  // 構成子 1 段の分岐。どの構成子にも分岐が 1 つずつある。tpe は結果の型
  case Match(scrutinee: Core, cases: List[Core.Case], tpe: Type)
  case Primitive(intrinsic: Intrinsic, arguments: List[Core])
  case Foreign(name: String, tpe: Type)
}

object Core {
  final case class Binding(symbol: Symbol, tpe: Type, value: Core)

  final case class Case(constructor: Symbol, binders: List[Symbol], body: Core)

  enum Decl {
    case Let(binding: Binding)
    case Rec(bindings: List[Binding])
    case DataTypes(group: List[DataDef])
  }

  final case class Program(declarations: List[Decl], main: Symbol)
}
