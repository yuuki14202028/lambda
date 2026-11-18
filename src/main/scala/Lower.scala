package com.yuuki14202028

/** 型検査の結果から糖衣を外して Core にする。Typed のどの形も、ここで Core の形に決まる。
 *
 *  - 制約は普通の引数になる: 型 [C] → T は C → T に、λ[C]. e は辞書を受け取る関数に
 *  - trait は辞書のデータ型と、辞書からメソッドを取り出す関数になる
 *  - opaque type は中身の型に読み替える
 *  - match の決定木は、構成子 1 段の match と if の入れ子になる
 */
object Lower {
  def program(typed: Typed.Program): Core.Program = new Lower(typed.opaque).program(typed)
}

private final class Lower(opaque: Map[Symbol, Type]) {

  def program(typed: Typed.Program): Core.Program =
    Core.Program(typed.declarations.flatMap {
      case Typed.Decl.Let(checked) => List(Core.Decl.Let(binding(checked)))
      case Typed.Decl.Rec(group) => List(Core.Decl.Rec(group.map(binding)))
      case Typed.Decl.DataTypes(group) =>
        val lowered = group.map(dataType)
        Core.Decl.DataTypes(lowered) :: lowered.flatMap(accessors).map(Core.Decl.Let(_))
      case Typed.Decl.Trait(definition, dictionary, defaults) =>
        Core.Decl.DataTypes(List(dataType(dictionary))) :: (projections(definition) ++ defaults.map(binding)).map(Core.Decl.Let(_))
    }, typed.main)

  // ---- 型 ----

  private def tpe(written: Type): Type = withoutGiven(Type.substitute(written, opaque))

  private def withoutGiven(written: Type): Type = written match {
    case Type.Given(constraint, to) => Type.Arrow(withoutGiven(constraint), withoutGiven(to))
    case Type.Arrow(from, to) => Type.Arrow(withoutGiven(from), withoutGiven(to))
    case Type.Forall(name, kind, body) => Type.Forall(name, kind, withoutGiven(body))
    case Type.Lambda(name, kind, body) => Type.Lambda(name, kind, withoutGiven(body))
    case Type.Apply(function, argument) => Type.Apply(withoutGiven(function), withoutGiven(argument))
    case Type.Var(_) | Type.Bound(_) | Type.Data(_) | Type.Primitive(_) => written
  }

  private def dataType(definition: DataDef): DataDef =
    definition.copy(constructors = definition.constructors.map(constructor => constructor.copy(tpe = tpe(constructor.tpe))))

  // ---- データ型と trait から作る関数 ----

  /** Λā. λvalue: D[ā]. match value { C(x̄) => xᵢ }: 構成子が 1 つのデータ型の、index 番目のフィールドを取り出す関数 */
  private def projection(symbol: Symbol, constructor: ConstructorDef, index: Int): Core.Binding = {
    val (parameters, body) = Type.openForalls(constructor.tpe)
    val (fieldTypes, result) = Type.arrows(body)
    val value = new Symbol("value")
    val fields = fieldTypes.map(_ => new Symbol("field"))
    val selected = Core.Match(Core.Variable(value), List(Core.Case(constructor.symbol, fields, Core.Variable(fields(index)))), fieldTypes(index))
    val function = parameters.foldRight(Core.Lambda(value, result, selected): Core) { case ((parameter, kind), inner) => Core.TypeLambda(parameter, kind, inner) }
    Core.Binding(symbol, constructor.accessorType(index), function)
  }

  /** 名前付きフィールドを読む関数 */
  private def accessors(definition: DataDef): List[Core.Binding] = for {
    constructor <- definition.constructors
    (field, index) <- constructor.fields.zipWithIndex
    accessor <- field.accessor
  } yield projection(accessor, constructor, index)

  /** 辞書からスーパークラスの辞書とメソッドを取り出す関数（辞書のフィールドは、スーパークラス、メソッドの順） */
  private def projections(definition: TraitDef): List[Core.Binding] = {
    val constructor = dataType(definition.dictionary).constructors.head
    (definition.supers.map(_._2) ++ definition.methods.map(_.symbol)).zipWithIndex.map { case (symbol, index) => projection(symbol, constructor, index) }
  }

  // ---- 式 ----

  private def binding(checked: Typed.Binding): Core.Binding = Core.Binding(checked.symbol, tpe(checked.tpe), expr(checked.value))

  private def expr(typed: Typed): Core = typed match {
    case Typed.Variable(symbol, _) => Core.Variable(symbol)
    case Typed.Constant(value, _) => Core.Constant(value)
    case Typed.Lambda(parameter, parameterType, body, _) => Core.Lambda(parameter, tpe(parameterType), expr(body))
    case Typed.GivenLambda(parameter, constraint, body, _) => Core.Lambda(parameter, tpe(constraint), expr(body))
    case Typed.TypeLambda(parameter, kind, body, _) => Core.TypeLambda(parameter, kind, expr(body))

    case Typed.Apply(head, arguments, _) =>
      arguments.foldLeft(expr(head)) {
        case (function, Typed.Argument.OfType(argument)) => Core.TypeApply(function, tpe(argument))
        case (function, Typed.Argument.OfValue(value)) => Core.Apply(function, expr(value))
      }

    case Typed.Sugar(_, meaning, _) => expr(meaning)

    // a && b は if a then b else false、a || b は if a then true else b（右は必要なときだけ評価する）
    case Typed.Logic(isAnd, left, right, _) =>
      if (isAnd) Core.If(expr(left), expr(right), Core.Constant(Literal.Bool(false)))
      else Core.If(expr(left), Core.Constant(Literal.Bool(true)), expr(right))

    case Typed.If(condition, thenBranch, elseBranch, _) => Core.If(expr(condition), expr(thenBranch), expr(elseBranch))
    case Typed.Let(checked, body, _) => Core.Let(binding(checked), expr(body))
    case Typed.LetRec(group, body, _) => Core.LetRec(group.map(binding), expr(body))
    case Typed.LocalData(group, body, _) => Core.LetData(group.map(dataType), expr(body))
    case Typed.Match(scrutinee, root, decision, cases, resultType) => matchExpression(scrutinee, root, decision, cases, tpe(resultType))

    // 途中の式は、使わない変数に束縛して順に評価する
    case Typed.Block(discarded, result, _) =>
      discarded.foldRight(result.fold(Core.Constant(Literal.Unit): Core)(expr)) { (item, rest) =>
        Core.Let(Core.Binding(new Symbol("_"), tpe(item.tpe), expr(item)), rest)
      }

    case Typed.Interpolation(parts, _) =>
      val concat = Core.Foreign("concat", Type.Arrow(Type.string, Type.Arrow(Type.string, Type.string)))
      parts.map(expr).reduceLeft((left, right) => Core.Apply(Core.Apply(concat, left), right))

    case Typed.Primitive(intrinsic, arguments, _) => Core.Primitive(intrinsic, arguments.map(expr))
    case Typed.Foreign(name, foreignType) => Core.Foreign(name, tpe(foreignType))
  }

  // ---- match ----

  /** 決定木を、構成子 1 段の match と if の入れ子にする。
   *  複数の葉から着く分岐の本体は、パターンの変数を受け取る関数として先に置き、葉からはそれを呼ぶ */
  private def matchExpression(scrutinee: Typed, root: Occurrence, decision: Decision, cases: List[Typed.Case], resultType: Type): Core = {
    val shared = Patterns.reached(decision).collect { case (index, count) if count > 1 => index -> new Symbol(s"case$index") }

    /** 分岐の本体を関数にするときの引数（変数がなければ unit を受け取る） */
    def parameters(branch: Typed.Case): List[(Symbol, Type)] =
      if (branch.bindings.isEmpty) List(new Symbol("_") -> Type.unit) else branch.bindings.map { case (symbol, bound) => symbol -> tpe(bound) }

    def body(index: Int): Core = shared.get(index) match {
      case None => expr(cases(index).body)
      case Some(function) =>
        val arguments = if (cases(index).bindings.isEmpty) List(Core.Constant(Literal.Unit)) else cases(index).bindings.map(bound => Core.Variable(bound._1))
        arguments.foldLeft(Core.Variable(function): Core)(Core.Apply(_, _))
    }

    def tree(node: Decision): Core = node match {
      case Decision.Fail => sys.error("Compiler invariant violation: a non-exhaustive match passed the type checker")
      case Decision.Leaf(index, bindings, otherwise) =>
        val reached = (cases(index).guard, otherwise) match {
          case (Some(guard), Some(next)) => Core.If(expr(guard), body(index), tree(next))
          case _ => body(index)
        }
        bindings.foldRight(reached) { case ((symbol, on), inner) => Core.Let(Core.Binding(symbol, tpe(on.tpe), Core.Variable(on.symbol)), inner) }
      case Decision.Switch(on, branches) =>
        Core.Match(Core.Variable(on.symbol), branches.map { branch =>
          Core.Case(branch.constructor.symbol, branch.fields.map(_.symbol), tree(branch.next))
        }, resultType)
      case Decision.IfBool(on, whenTrue, whenFalse) => Core.If(Core.Variable(on.symbol), tree(whenTrue), tree(whenFalse))
      case Decision.Equal(on, _, test, whenEqual, otherwise) =>
        Core.If(Core.Apply(expr(test), Core.Variable(on.symbol)), tree(whenEqual), tree(otherwise))
    }

    val functions = shared.toList.sortBy(_._1).map { case (index, function) =>
      val accepted = parameters(cases(index))
      Core.Binding(
        function,
        accepted.map(_._2).foldRight(resultType)(Type.Arrow(_, _)),
        accepted.foldRight(expr(cases(index).body)) { case ((parameter, parameterType), inner) => Core.Lambda(parameter, parameterType, inner) }
      )
    }
    Core.Let(Core.Binding(root.symbol, tpe(root.tpe), expr(scrutinee)), functions.foldRight(tree(decision))(Core.Let(_, _)))
  }
}
