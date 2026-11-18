package com.yuuki14202028

/** derive C[D]: データ型の形から、手で書くのと同じ impl を作る。作った impl は、普通の impl と同じように検査される
 * （フィールドの型のインスタンスがなければ、そこで「インスタンスがない」エラーになる）。
 *
 * 導出できるのは、形から一意に決まる trait だけ（名前で選び、メソッドの型が決まった形であることを確かめる）:
 *  - Eq / Ord / Show: impl[ā] C[D[ā]] with C[a₁], …。構造の比較、構成子の宣言順 → フィールドの辞書順、C(name = 値, 値) の表示
 *  - Functor / Foldable / Traversable: 最後の型パラメータについての impl[p̄] C[D[p̄]]
 *
 * 入れ子のフィールド（List[D[a]] など）や自分自身は、その型のインスタンスのメソッドを呼ぶだけでよい
 * （導出したインスタンスは、自分のメソッドから見える）。
 */
object Derive {
  import Resolved as R

  private def shortName(symbol: Symbol): String = symbol.name.substring(symbol.name.lastIndexOf('.') + 1)

  def impl(env: Env, derived: R.Instance.Derived): R.Impl = new Derive(env, derived).impl()
}

private final class Derive(env: Env, derived: Resolved.Instance.Derived) {
  import Resolved as R
  import Derive.shortName

  private val span = derived.span

  private def fail(error: CompileError): Nothing = throw CompileFailure(error)

  private val definition: TraitDef = env.traits.getOrElse(derived.traitSymbol, fail(
    if (env.dataTypes.contains(derived.traitSymbol)) CompileError.NotATrait(derived.traitSymbol.name, span) else CompileError.DependsOnFailedDeclaration(span)
  ))
  private val data: DataDef = env.dataTypes.get(derived.target).filterNot(_ => env.traits.contains(derived.target)).getOrElse(fail(
    if (env.aliases.contains(derived.target) || env.typeVariables.contains(derived.target) || env.traits.contains(derived.target))
      CompileError.DeriveUnsupported(shortName(derived.traitSymbol), derived.target.name, "it is not a data type", span)
    else CompileError.DependsOnFailedDeclaration(span)
  ))

  private def unsupported(reason: String): Nothing =
    fail(CompileError.DeriveUnsupported(shortName(definition.symbol), data.symbol.name, reason, span))

  // ---- 作るコードの部品 ----

  private def known(tpe: Type): R.TypeExpr = R.TypeExpr.Known(tpe, span)
  private def variable(symbol: Symbol): R.Expr = R.Expr.Variable(symbol, span)
  private def call(function: R.Expr, arguments: R.Expr*): R.Expr = R.Expr.Apply(function, arguments.toList.map(R.Argument.OfValue(_)), span)
  private def lambda(parameter: Symbol, tpe: Type, body: R.Expr): R.Expr = R.Expr.Lambda(parameter, known(tpe), body, span)
  private def typeLambda(parameter: Symbol, kind: Kind, body: R.Expr): R.Expr = R.Expr.TypeLambda(parameter, kind, body, span)
  private def bool(value: Boolean): R.Expr = R.Expr.Constant(Literal.Bool(value), span)
  private def text(value: String): R.Expr = R.Expr.Constant(Literal.Str(value), span)

  /** match scrutinee { 構成子ごとの分岐 }。body は、構成子とフィールドの変数から分岐の本体を作る */
  private def matchOn(scrutinee: R.Expr, body: (ConstructorDef, List[Symbol]) => R.Expr): R.Expr =
    R.Expr.Match(scrutinee, data.constructors.map { constructor =>
      val fields = constructor.fields.map(_ => new Symbol("field"))
      R.Case(R.Pattern.Constructor(constructor.symbol, fields.map(R.Pattern.Bind(_, span)), span), None, body(constructor, fields))
    }, span)

  private def method(name: String): MethodDef =
    definition.methods.find(_.name == name).getOrElse(unsupported(s"the trait has no method `$name`"))

  /** メソッドの型が、導出の規則が前提にする形であること */
  private def expectSignature(name: String, canonical: Type): MethodDef = {
    val found = method(name)
    if (found.signature != canonical) unsupported(s"the method `$name` does not have the type `${Type.show(canonical)}`")
    found
  }

  private def forall(parameters: List[(Symbol, Kind)], body: Type): Type =
    parameters.foldRight(body) { case ((parameter, kind), inner) => Type.forall(parameter, kind, inner) }

  def impl(): R.Impl = shortName(definition.symbol) match {
    case "Eq" | "Ord" | "Show" => firstOrder()
    case "Functor" | "Foldable" | "Traversable" => higherKinded()
    case other => fail(CompileError.DeriveUnsupported(other, data.symbol.name, "only Eq, Ord, Show, Functor, Foldable and Traversable can be derived", span))
  }

  private def make(parameters: List[(Symbol, Kind)], target: Type, context: List[Type], methods: List[(String, R.Expr)]): R.Impl = R.Impl(
    derived.dictionary, parameters, definition.symbol, definition.parameters.map(_ => None), List(known(target)), context.map(known),
    methods.map { case (name, value) => R.MethodImpl(name, None, value, span) }, span
  )

  // ---- Eq / Ord / Show ----

  private def firstOrder(): R.Impl = {
    if (definition.parameters.map(_._2) != List(Kind.Star)) unsupported("the trait must take one type of kind `*`")
    if (data.parameters.exists(_ != Kind.Star)) unsupported("every type parameter of the data type must have kind `*`")
    val traitParameter = Type.Var(definition.parameters.head._1)
    val parameters = data.parameters.map(kind => new Symbol("a") -> kind)
    val arguments = parameters.map { case (parameter, _) => Type.Var(parameter) }
    val self = arguments.foldLeft(Type.Data(data.symbol): Type)(Type.Apply(_, _))
    val context = arguments.map(Type.Apply(Type.Data(definition.symbol), _))
    val comparison = Type.Arrow(traitParameter, Type.Arrow(traitParameter, Type.bool))

    /** λx. λy. match x { Kᵢ(x̄) => match y { Kⱼ(ȳ) => body(i, x̄, j, ȳ) } } */
    def compare(body: (ConstructorDef, List[Symbol], ConstructorDef, List[Symbol]) => R.Expr): R.Expr = {
      val (left, right) = (new Symbol("left"), new Symbol("right"))
      lambda(left, self, lambda(right, self, matchOn(variable(left), (first, xs) => matchOn(variable(right), (second, ys) => body(first, xs, second, ys)))))
    }
    def all(conditions: List[R.Expr]): R.Expr =
      if (conditions.isEmpty) bool(true) else conditions.reduceRight((condition, rest) => R.Expr.Logic(isAnd = true, condition, rest, span))

    val (name, value) = shortName(definition.symbol) match {
      case "Eq" =>
        val eq = expectSignature("eq", comparison)
        "eq" -> compare { (first, xs, second, ys) =>
          if (first.index == second.index) all(xs.zip(ys).map { case (x, y) => call(variable(eq.symbol), variable(x), variable(y)) }) else bool(false)
        }

      case "Ord" =>
        val lt = expectSignature("lt", comparison)
        // フィールドが等しいかは、スーパークラスの Eq で調べる
        val eq = definition.supers.headOption.flatMap { case (constraint, _) => Types.constraintHead(env, constraint) }
          .flatMap(_._1.methods.find(_.name == "eq")).getOrElse(unsupported("the trait must have a superclass with the method `eq`"))
        def lexicographic(pairs: List[(Symbol, Symbol)]): R.Expr = pairs match {
          case Nil => bool(false)
          case List((x, y)) => call(variable(lt.symbol), variable(x), variable(y))
          case (x, y) :: rest =>
            val equalThenRest = R.Expr.Logic(isAnd = true, call(variable(eq.symbol), variable(x), variable(y)), lexicographic(rest), span)
            R.Expr.Logic(isAnd = false, call(variable(lt.symbol), variable(x), variable(y)), equalThenRest, span)
        }
        "lt" -> compare { (first, xs, second, ys) =>
          if (first.index == second.index) lexicographic(xs.zip(ys)) else bool(first.index < second.index)
        }

      case _ =>
        val show = expectSignature("show", Type.Arrow(traitParameter, Type.string))
        val concat = R.Expr.Foreign(known(Type.Arrow(Type.string, Type.Arrow(Type.string, Type.string))), "concat", span)
        val shown = new Symbol("value")
        "show" -> lambda(shown, self, matchOn(variable(shown), { (constructor, fields) =>
          val values = constructor.fields.zip(fields).map { case (field, x) =>
            val value = call(variable(show.symbol), variable(x))
            field.name.fold(value)(fieldName => call(concat, text(s"$fieldName = "), value))
          }
          val separated = values.zipWithIndex.flatMap { case (value, index) => if (index == 0) List(value) else List(text(", "), value) }
          val parts = text(shortName(constructor.symbol)) :: (if (values.isEmpty) Nil else text("(") :: separated ::: List(text(")")))
          parts.reduceLeft((left, right) => call(concat, left, right))
        }))
    }
    make(parameters, self, context, List(name -> value))
  }

  // ---- Functor / Foldable / Traversable ----

  /** フィールドの型の分類: 要素そのもの / 要素を含まない / 要素を含む型に型構成子を適用したもの（inner は中の型） */
  private enum Shape {
    case Element
    case Constant
    case Nested(inner: Type)
  }

  private def higherKinded(): R.Impl = {
    if (definition.parameters.map(_._2) != List(Kind.Arrow(Kind.Star, Kind.Star))) unsupported("the trait must take one type constructor of kind `* → *`")
    if (data.parameters.lastOption != Some(Kind.Star)) unsupported("the last type parameter of the data type must have kind `*`")
    val container = Type.Var(definition.parameters.head._1)
    val parameters = data.parameters.init.map(kind => new Symbol("p") -> kind)
    val target = parameters.foldLeft(Type.Data(data.symbol): Type) { case (applied, (parameter, _)) => Type.Apply(applied, Type.Var(parameter)) }
    val prefix = parameters.map { case (parameter, _) => Type.Var(parameter) }

    // メソッドの型の束縛子と同じ並びの型変数（a: 要素の型、b: 結果の型、g: traverse の計算の型）
    val (a, b, g) = (new Symbol("a"), new Symbol("b"), new Symbol("g"))
    val (elementType, resultType) = (Type.Var(a), Type.Var(b))
    def of(function: Type, argument: Type): Type = Type.Apply(function, argument)
    val (values, function, start) = (new Symbol("values"), new Symbol("function"), new Symbol("start"))

    /** 構成子のフィールドの型（最後の型パラメータが a のとき） */
    def fieldTypes(constructor: ConstructorDef): List[Type] = constructor.fieldTypes(prefix :+ elementType)

    def shape(constructor: ConstructorDef, tpe: Type): Shape = tpe match {
      case _ if tpe == elementType => Shape.Element
      case _ if !Type.variables(tpe)(a) => Shape.Constant
      case Type.Apply(outer, inner) if !Type.variables(outer)(a) => Shape.Nested(inner)
      case _ => unsupported(s"the field of type `${Type.show(tpe)}` in `${shortName(constructor.symbol)}` uses the last type parameter in an unsupported position")
    }

    /** K[p̄][b]: 結果の型の構成子 */
    def rebuilt(constructor: ConstructorDef): R.Expr =
      R.Expr.Apply(variable(constructor.symbol), (prefix :+ resultType).map(tpe => R.Argument.OfType(known(tpe))), span)

    def header(parameters: List[(Symbol, Kind)], body: R.Expr): R.Expr = parameters.foldRight(body) { case ((parameter, kind), inner) => typeLambda(parameter, kind, inner) }

    val methods = shortName(definition.symbol) match {
      case "Functor" =>
        val mapType = Type.Arrow(elementType, resultType)
        val map = expectSignature("map", forall(List(a -> Kind.Star, b -> Kind.Star), Type.Arrow(of(container, elementType), Type.Arrow(mapType, of(container, resultType)))))
        // φ(x): 要素なら function(x)、入れ子なら map(x)(λy. φ(y))
        def transform(constructor: ConstructorDef, tpe: Type, x: R.Expr): R.Expr = tpe match {
          // 結果にだけ要素が現れる関数は、結果を変換する関数にする
          case Type.Arrow(from, to) if !Type.variables(from)(a) && Type.variables(to)(a) =>
            val argument = new Symbol("argument")
            lambda(argument, from, transform(constructor, to, call(x, variable(argument))))
          case _ => transformShape(constructor, tpe, x)
        }
        def transformShape(constructor: ConstructorDef, tpe: Type, x: R.Expr): R.Expr = shape(constructor, tpe) match {
          case Shape.Element => call(variable(function), x)
          case Shape.Constant => x
          case Shape.Nested(inner) if inner == elementType => call(variable(map.symbol), x, variable(function))
          case Shape.Nested(inner) =>
            val y = new Symbol("inner")
            call(variable(map.symbol), x, lambda(y, inner, transform(constructor, inner, variable(y))))
        }
        val body = matchOn(variable(values), { (constructor, fields) =>
          val arguments = fieldTypes(constructor).zip(fields).map { case (tpe, x) => transform(constructor, tpe, variable(x)) }
          if (arguments.isEmpty) rebuilt(constructor) else call(rebuilt(constructor), arguments*)
        })
        List("map" -> header(List(a -> Kind.Star, b -> Kind.Star), lambda(values, of(target, elementType), lambda(function, mapType, body))))

      case "Foldable" =>
        def foldMethod(name: String, rightward: Boolean): (String, R.Expr) = {
          val step = if (rightward) Type.Arrow(elementType, Type.Arrow(resultType, resultType)) else Type.Arrow(resultType, Type.Arrow(elementType, resultType))
          val canonical = forall(List(a -> Kind.Star, b -> Kind.Star), Type.Arrow(of(container, elementType), Type.Arrow(resultType, Type.Arrow(step, resultType))))
          val fold = expectSignature(name, canonical)
          // acc に、フィールド x の要素を畳み込む
          def combine(constructor: ConstructorDef, tpe: Type, x: R.Expr, acc: R.Expr): R.Expr = shape(constructor, tpe) match {
            case Shape.Element => if (rightward) call(variable(function), x, acc) else call(variable(function), acc, x)
            case Shape.Constant => acc
            case Shape.Nested(inner) if inner == elementType => call(variable(fold.symbol), x, acc, variable(function))
            case Shape.Nested(inner) =>
              val (y, innerAcc) = (new Symbol("inner"), new Symbol("acc"))
              val combined = combine(constructor, inner, variable(y), variable(innerAcc))
              val each = if (rightward) lambda(y, inner, lambda(innerAcc, resultType, combined)) else lambda(innerAcc, resultType, lambda(y, inner, combined))
              call(variable(fold.symbol), x, acc, each)
          }
          val body = matchOn(variable(values), { (constructor, fields) =>
            val typed = fieldTypes(constructor).zip(fields)
            if (rightward) typed.foldRight(variable(start)) { case ((tpe, x), acc) => combine(constructor, tpe, variable(x), acc) }
            else typed.foldLeft(variable(start)) { case (acc, (tpe, x)) => combine(constructor, tpe, variable(x), acc) }
          })
          name -> header(List(a -> Kind.Star, b -> Kind.Star), lambda(values, of(target, elementType), lambda(start, resultType, lambda(function, step, body))))
        }
        List(foldMethod("foldRight", rightward = true), foldMethod("foldLeft", rightward = false))

      case _ =>
        val computation = Type.Var(g)
        val binders = List(a -> Kind.Star, g -> Kind.Arrow(Kind.Star, Kind.Star), b -> Kind.Star)
        // traverse のシグネチャの制約から、Applicative とそのメソッドを見つける
        val traverse = method("traverse")
        val applicative = Type.givens(Type.openForalls(traverse.signature)._2)._1.headOption.flatMap(Types.constraintHead(env, _)).map(_._1)
          .getOrElse(unsupported("the method `traverse` must have a constraint (an applicative functor)"))
        val effect = Type.Arrow(elementType, of(computation, resultType))
        val canonical = forall(binders, Type.Given(of(Type.Data(applicative.symbol), computation),
          Type.Arrow(of(container, elementType), Type.Arrow(effect, of(computation, of(container, resultType))))))
        if (traverse.signature != canonical) unsupported(s"the method `traverse` does not have the type `${Type.show(canonical)}`")
        val (pure, apply) = (applicative.methods.find(_.name == "pure"), applicative.methods.find(_.name == "apply")) match {
          case (Some(pure), Some(apply)) => (pure, apply)
          case _ => unsupported(s"`${applicative.symbol.name}` must have the methods `pure` and `apply`")
        }
        def pureOf(value: R.Expr): R.Expr =
          R.Expr.Apply(variable(pure.symbol), List(R.Argument.OfType(known(computation)), R.Argument.OfValue(value)), span)
        def toResult(tpe: Type): Type = Type.substitute(tpe, Map(a -> resultType))
        // τ(x): x を計算の中へ持ち上げる。要素なら function(x)、入れ子なら traverse(x)(λy. τ(y))、それ以外は pure(x)
        def lift(constructor: ConstructorDef, tpe: Type, x: R.Expr): R.Expr = shape(constructor, tpe) match {
          case Shape.Element => call(variable(function), x)
          case Shape.Constant => pureOf(x)
          case Shape.Nested(inner) if inner == elementType => call(variable(traverse.symbol), x, variable(function))
          case Shape.Nested(inner) =>
            val y = new Symbol("inner")
            call(variable(traverse.symbol), x, lambda(y, inner, lift(constructor, inner, variable(y))))
        }
        // pure(λȳ. K[p̄][b](ȳ)) に、持ち上げたフィールドを apply で順に渡す
        val body = matchOn(variable(values), { (constructor, fields) =>
          val types = fieldTypes(constructor)
          val results = types.map(_ => new Symbol("result"))
          val build = if (results.isEmpty) rebuilt(constructor) else call(rebuilt(constructor), results.map(variable)*)
          val constructorFunction = results.zip(types).foldRight(build) { case ((result, tpe), inner) => lambda(result, toResult(tpe), inner) }
          types.zip(fields).foldLeft(pureOf(constructorFunction)) { case (acc, (tpe, x)) => call(variable(apply.symbol), acc, lift(constructor, tpe, variable(x))) }
        })
        val constrained = R.Expr.GivenLambda(known(of(Type.Data(applicative.symbol), computation)), lambda(values, of(target, elementType), lambda(function, effect, body)), span)
        List("traverse" -> header(binders, constrained))
    }
    make(parameters, target, Nil, methods)
  }
}
