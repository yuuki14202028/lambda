package com.yuuki14202028

/** 最後の言語: 型のないラムダ式と、組み込みの値・演算。データ型も型もここにはない */
enum Lambda {
  case Variable(symbol: Symbol)
  case Constant(value: Literal)
  case Abstraction(parameter: Symbol, body: Lambda)
  case Apply(function: Lambda, argument: Lambda)
  case Let(symbol: Symbol, value: Lambda, body: Lambda)
  case LetRec(bindings: List[(Symbol, Lambda)], body: Lambda)
  case If(condition: Lambda, thenBranch: Lambda, elseBranch: Lambda)
  case Primitive(intrinsic: Intrinsic, arguments: List[Lambda])
  // 引数を 1 つずつ受け取り、揃ったら呼ばれる C の関数。
  // parameters と result は C に渡すときの型（組み込みの型の名前。それ以外の値は "closure"）
  case Foreign(name: String, parameters: List[String], result: String)
}

object Lambda {
  enum Decl {
    case Let(symbol: Symbol, value: Lambda)
    case Rec(bindings: List[(Symbol, Lambda)])
  }

  final case class Program(declarations: List[Decl], main: Symbol)
}
