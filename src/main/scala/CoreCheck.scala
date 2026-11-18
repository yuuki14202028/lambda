package com.yuuki14202028

/** Core の型検査。Core は型検査を通ったプログラムから機械的に作るので、ここで合わなければコンパイラのバグ。
 * ユーザーに見せるエラーではなく、Lower（とそれより前）の誤りを早く見つけるためにある。
 */
object CoreCheck {

  private def invariant(message: String): Nothing = sys.error(s"Compiler invariant violation: Core does not type check: $message")

  private def expect(expected: Type, actual: Type, where: String): Unit =
    if (expected != actual) invariant(s"$where: expected ${Type.show(expected)}, found ${Type.show(actual)}")

  def program(core: Core.Program): Unit = {
    val env = core.declarations.foldLeft(Env.empty) {
      case (env, Core.Decl.Let(bound)) =>
        binding(env, bound)
        env.withValue(bound.symbol, bound.tpe)
      case (env, Core.Decl.Rec(group)) =>
        val inner = env.withValues(group.map(bound => bound.symbol -> bound.tpe))
        group.foreach(binding(inner, _))
        inner
      case (env, Core.Decl.DataTypes(group)) => env.withDataTypes(group)
    }
    expect(Type.Arrow(Type.unit, Type.i32), typeOf(env, Core.Variable(core.main)), "main")
  }

  private def binding(env: Env, bound: Core.Binding): Unit =
    expect(bound.tpe, typeOf(env, bound.value), s"the value of ${bound.symbol}")

  private def typeOf(env: Env, term: Core): Type = term match {
    case Core.Variable(symbol) => env.values.getOrElse(symbol, invariant(s"$symbol is not in scope"))

    case Core.Constant(value) => value match {
      case Literal.Int(_, typeName) => Type.Primitive(typeName)
      case Literal.Float(_, typeName) => Type.Primitive(typeName)
      case Literal.Bool(_) => Type.bool
      case Literal.Char(_) => Type.Primitive("char")
      case Literal.Str(_) => Type.string
      case Literal.Unit => Type.unit
    }

    case Core.Lambda(parameter, parameterType, body) =>
      Type.Arrow(parameterType, typeOf(env.withValue(parameter, parameterType), body))

    case Core.TypeLambda(parameter, kind, body) => Type.forall(parameter, kind, typeOf(env.withTypeVariable(parameter, kind), body))

    case Core.Apply(function, argument) => typeOf(env, function) match {
      case Type.Arrow(from, to) =>
        expect(from, typeOf(env, argument), "an argument")
        to
      case other => invariant(s"${Type.show(other)} is applied to a value")
    }

    case Core.TypeApply(function, argument) => typeOf(env, function) match {
      case Type.Forall(_, kind, body) =>
        if (env.kindOf(argument) != kind) invariant(s"the type argument ${Type.show(argument)} does not have kind ${kind.show}")
        Type.open(body, argument)
      case other => invariant(s"${Type.show(other)} is applied to a type")
    }

    case Core.Let(bound, body) =>
      binding(env, bound)
      typeOf(env.withValue(bound.symbol, bound.tpe), body)

    case Core.LetRec(group, body) =>
      val inner = env.withValues(group.map(bound => bound.symbol -> bound.tpe))
      group.foreach(binding(inner, _))
      typeOf(inner, body)

    case Core.LetData(group, body) => typeOf(env.withDataTypes(group), body)

    case Core.If(condition, thenBranch, elseBranch) =>
      expect(Type.bool, typeOf(env, condition), "the condition of an if")
      val result = typeOf(env, thenBranch)
      expect(result, typeOf(env, elseBranch), "the else branch")
      result

    case Core.Match(scrutinee, cases, tpe) =>
      val scrutineeType = typeOf(env, scrutinee)
      val (data, typeArguments) = Type.dataApplication(scrutineeType).getOrElse(invariant(s"${Type.show(scrutineeType)} is matched"))
      val constructors = env.dataTypes.getOrElse(data, invariant(s"data type $data is not defined")).constructors
      if (cases.map(_.constructor) != constructors.map(_.symbol)) invariant(s"a match on $data does not have one case per constructor, in order")
      cases.zip(constructors).foreach { case (branch, constructor) =>
        val fields = constructor.fieldTypes(typeArguments)
        if (fields.length != branch.binders.length) invariant(s"the case ${branch.constructor} binds the wrong number of fields")
        expect(tpe, typeOf(env.withValues(branch.binders.zip(fields)), branch.body), s"the case ${branch.constructor}")
      }
      tpe

    case Core.Primitive(intrinsic, arguments) =>
      val operand = Type.Primitive(intrinsic.typeName)
      if (arguments.length != intrinsic.op.arity) invariant(s"intrinsic ${intrinsic.name} takes ${intrinsic.op.arity} argument(s)")
      arguments.foreach(argument => expect(operand, typeOf(env, argument), s"an argument of ${intrinsic.name}"))
      intrinsic.op match {
        case PrimOp.Eq | PrimOp.Neq | PrimOp.Lt | PrimOp.Leq | PrimOp.Gt | PrimOp.Geq => Type.bool
        case _ => operand
      }

    case Core.Foreign(_, tpe) => tpe
  }
}
