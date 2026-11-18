package com.yuuki14202028

/** Core をラムダ式に落とす。型はここで消える（Λ と型の適用はなくなる）。
 *
 * データ型は「構成子ごとの関数を受け取り、自分の構成子に当たる関数にフィールドを渡す関数」として表す:
 *
 *   data D = { A(x)(y), B }
 *     A = λx. λy. λonA. λonB. onA(x)(y)
 *     B = λonA. λonB. onB(())
 *   match e { A(x)(y) => p   B => q }
 *     e(λx. λy. p)(λ_. q)
 *
 * フィールドのない構成子の関数は unit を受け取る（選ばれなかった分岐を評価しないため）。
 */
object Encode {

  def program(core: Core.Program): Lambda.Program =
    Lambda.Program(core.declarations.flatMap {
      case Core.Decl.Let(bound) => List(Lambda.Decl.Let(bound.symbol, term(bound.value)))
      case Core.Decl.Rec(group) => List(Lambda.Decl.Rec(group.map(bound => bound.symbol -> term(bound.value))))
      case Core.Decl.DataTypes(group) => constructors(group).map { case (symbol, value) => Lambda.Decl.Let(symbol, value) }
    }, core.main)

  private def constructors(group: List[DataDef]): List[(Symbol, Lambda)] =
    group.flatMap(data => data.constructors.map(c => c.symbol -> constructor(c, data.constructors.length)))

  private def constructor(definition: ConstructorDef, siblings: Int): Lambda = {
    val fields = List.fill(definition.arity)(new Symbol("field"))
    val handlers = List.tabulate(siblings)(index => new Symbol(s"on$index"))
    val selected = Lambda.Variable(handlers(definition.index))
    val result =
      if (fields.isEmpty) Lambda.Apply(selected, Lambda.Constant(Literal.Unit))
      else fields.foldLeft(selected: Lambda)((function, field) => Lambda.Apply(function, Lambda.Variable(field)))
    (fields ++ handlers).foldRight(result)(Lambda.Abstraction(_, _))
  }

  private def term(core: Core): Lambda = core match {
    case Core.Variable(symbol) => Lambda.Variable(symbol)
    case Core.Constant(value) => Lambda.Constant(value)
    case Core.Lambda(parameter, _, body) => Lambda.Abstraction(parameter, term(body))
    case Core.TypeLambda(_, _, body) => term(body)
    case Core.Apply(function, argument) => Lambda.Apply(term(function), term(argument))
    case Core.TypeApply(function, _) => term(function)
    case Core.Let(bound, body) => Lambda.Let(bound.symbol, term(bound.value), term(body))
    case Core.LetRec(group, body) => Lambda.LetRec(group.map(bound => bound.symbol -> term(bound.value)), term(body))
    case Core.LetData(group, body) =>
      constructors(group).foldRight(term(body)) { case ((symbol, value), rest) => Lambda.Let(symbol, value, rest) }
    case Core.If(condition, thenBranch, elseBranch) => Lambda.If(term(condition), term(thenBranch), term(elseBranch))

    case Core.Match(scrutinee, cases, _) =>
      cases.foldLeft(term(scrutinee)) { (function, branch) =>
        val handler =
          if (branch.binders.isEmpty) Lambda.Abstraction(new Symbol("_"), term(branch.body))
          else branch.binders.foldRight(term(branch.body))(Lambda.Abstraction(_, _))
        Lambda.Apply(function, handler)
      }

    case Core.Primitive(intrinsic, arguments) => Lambda.Primitive(intrinsic, arguments.map(term))
    case Core.Foreign(name, tpe) =>
      val (parameters, result) = Type.arrows(tpe)
      Lambda.Foreign(name, parameters.map(foreignType), foreignType(result))
  }

  private def foreignType(tpe: Type): String = tpe match {
    case Type.Primitive(name) => name
    case _ => "closure"
  }
}
