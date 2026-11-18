package com.yuuki14202028

import scala.collection.mutable.ListBuffer

/** 宣言の型検査。
 *
 * 宣言は「シグネチャ（注釈から決まる、環境に足すもの）」と「本体」に分けて検査するので、
 * 本体が失敗しても、後続の宣言はその名前を使って検査できる。エラーは宣言ごとに集める。
 */
object Declarations {
  import Resolved as R
  import Types.{convert, convertStar}

  private def fail(error: CompileError): Nothing = throw CompileFailure(error)

  def program(resolved: R.Program): Either[List[CompileError], Typed.Program] = new Declarations().program(resolved)

  // ---- data と type ----

  def dataTypes(env: Env, group: List[R.Data]): List[DataDef] = {
    // フィールドの型から組の全員を参照できるよう、構成子のない定義を先に入れる
    val provisional = env.withDataTypes(group.map(data => DataDef(data.symbol, data.parameters.map(_._2), Nil)))
    group.map { data =>
      val inner = provisional.withTypeVariables(data.parameters)
      val result = data.parameters.foldLeft(Type.Data(data.symbol): Type) { case (applied, (parameter, _)) => Type.Apply(applied, Type.Var(parameter)) }
      val constructors = data.constructors.zipWithIndex.map { case (constructor, index) =>
        val function = constructor.fields.map(field => convertStar(inner, field.tpe)).foldRight(result)(Type.Arrow(_, _))
        val tpe = data.parameters.foldRight(function) { case ((parameter, parameterKind), body) => Type.forall(parameter, parameterKind, body) }
        ConstructorDef(constructor.symbol, data.symbol, index, tpe, constructor.fields.map(field => FieldDef(field.name, field.accessor)))
      }
      DataDef(data.symbol, data.parameters.map(_._2), constructors)
    }
  }

  /** データ型と構成子に加えて、名前付きフィールドを読む関数も値として足す */
  def withDataTypes(env: Env, definitions: List[DataDef]): Env =
    env.withDataTypes(definitions).withValues(for {
      definition <- definitions
      constructor <- definition.constructors
      (field, index) <- constructor.fields.zipWithIndex
      accessor <- field.accessor
    } yield accessor -> constructor.accessorType(index))

  def withAlias(env: Env, alias: R.Alias): Env = {
    val (tpe, kind) = convert(env.withTypeVariables(alias.parameters), alias.body)
    env.withAlias(
      alias.symbol,
      alias.parameters.foldRight(tpe) { case ((parameter, parameterKind), inner) => Type.lambda(parameter, parameterKind, inner) },
      alias.parameters.foldRight(kind) { case ((_, parameterKind), inner) => Kind.Arrow(parameterKind, inner) }
    )
  }

  // ---- trait ----

  /** trait の定義（シグネチャだけで決まる部分）。previous があれば、その関数の Symbol を使い続ける */
  private def traitDefinition(env: Env, parsed: R.Trait, previous: Option[TraitDef]): TraitDef = {
    val inner = env.withTypeVariables(parsed.parameters)
    val supers = parsed.supers.zipWithIndex.map { case (written, index) =>
      Types.constraint(inner, written) -> previous.fold(new Symbol(s"${parsed.symbol.name} super $index"))(_.supers(index)._2)
    }
    val methods = parsed.methods.zipWithIndex.map { case (method, index) =>
      val default = method.default.map(_ => previous.flatMap(_.methods(index).default).getOrElse(new Symbol(s"${method.symbol.name} default")))
      MethodDef(method.name, method.symbol, convertStar(inner, method.tpe), default)
    }
    TraitDef(parsed.symbol, parsed.parameters, parsed.constructor, supers, methods)
  }

  private def withTrait(env: Env, definition: TraitDef): Env =
    env.withDataTypes(List(definition.dictionary)).copy(traits = env.traits + (definition.symbol -> definition)).withValues(
      definition.methods.map(method => method.symbol -> definition.methodType(method)) ++
        definition.supers.map { case (constraint, projection) => projection -> definition.projectionType(constraint) } ++
        definition.methods.flatMap(method => method.default.map(_ -> definition.defaultType(method)))
    )

  /** デフォルトの本体。スーパークラスの制約と、デフォルトを持たないメソッド（普通の値として）がスコープにある */
  private def defaultBinding(env: Env, definition: TraitDef, parsed: R.Trait, method: R.Method, default: R.Expr, function: Symbol): Typed.Binding = {
    val dictionaries = definition.supers.map { case (constraint, _) => new Symbol("super") -> constraint }
    val locals = parsed.methods.zip(definition.methods).collect { case (written, checked) if checked.default.isEmpty => written.local -> checked.signature }
    val scoped = dictionaries.foldLeft(env.withTypeVariables(definition.parameters)) { case (acc, (dictionary, constraint)) =>
      acc.withGiven(Evidence(constraint, Typed.Variable(dictionary, constraint)))
    }
    val signature = definition.methods.find(_.symbol eq method.symbol).get.signature
    val body = Check.infer(scoped.withValues(locals), default)
    if (body.tpe != signature) {
      val (expected, actual, at, _) = Check.blame(signature, body, default, None)
      fail(CompileError.TypeMismatch(expected, actual, at))
    }
    val withMethods = locals.foldRight(body) { case ((local, tpe), inner) => Typed.Lambda(local, tpe, inner, Type.Arrow(tpe, inner.tpe)) }
    val withSupers = dictionaries.foldRight(withMethods) { case ((dictionary, constraint), inner) =>
      Typed.GivenLambda(dictionary, constraint, inner, Type.Given(constraint, inner.tpe))
    }
    val value = definition.parameters.foldRight(withSupers) { case ((parameter, kind), inner) =>
      Typed.TypeLambda(parameter, kind, inner, Type.forall(parameter, kind, inner.tpe))
    }
    Typed.Binding(function, value.tpe, value)
  }

  // ---- インスタンス ----

  /** impl[ā] C[T̄] with 文脈。tpe は辞書の型 ∀ā. [文脈…] → C[T̄] */
  private final case class Head(impl: R.Impl, definition: TraitDef, arguments: List[Type], context: List[Type], key: InstanceKey, tpe: Type) {
    def evidence: (InstanceKey, Evidence) = key -> Evidence(tpe, Typed.Variable(impl.dictionary, tpe))
  }

  private def traitOf(env: Env, impl: R.Impl): TraitDef = env.traits.getOrElse(impl.traitSymbol, {
    val isOtherType = env.dataTypes.contains(impl.traitSymbol) || env.aliases.contains(impl.traitSymbol) || env.typeVariables.contains(impl.traitSymbol)
    fail(if (isOtherType) CompileError.NotATrait(impl.traitSymbol.name, impl.span) else CompileError.DependsOnFailedDeclaration(impl.span))
  })

  /** trait の引数として書かれた型（カインドが trait のパラメータと合うこと） */
  private def traitArguments(env: Env, definition: TraitDef, impl: R.Impl): List[Type] = {
    if (impl.arguments.length != definition.parameters.length)
      fail(CompileError.ImplTargetArity(definition.symbol.name, definition.parameters.length, impl.arguments.length, impl.span))
    impl.arguments.zip(definition.parameters).map { case (written, (_, expected)) =>
      val (tpe, kind) = convert(env, written)
      if (kind != expected) fail(CompileError.KindMismatch(expected, kind, written.span))
      tpe
    }
  }

  private def head(env: Env, impl: R.Impl): Head = {
    val definition = traitOf(env, impl)
    val inner = env.withTypeVariables(impl.parameters)
    val arguments = traitArguments(inner, definition, impl)
    val parameters = impl.parameters.map(_._1)
    val heads = arguments.zip(impl.arguments).map { case (tpe, written) =>
      TypeHead.of(tpe).filter {
        case TypeHead.Named(symbol) => !parameters.exists(_ eq symbol)
        case _ => true
      }.getOrElse(fail(CompileError.InstanceHeadInvalid(tpe, written.span)))
    }
    parameters.find(parameter => !arguments.exists(Type.variables(_)(parameter))).foreach { unused =>
      fail(CompileError.ImplParameterUnused(unused.name, impl.span))
    }
    val context = impl.context.map(Types.constraint(inner, _))
    val applied = definition.instantiate(definition.dictionaryType, arguments)
    val tpe = impl.parameters.foldRight(context.foldRight(applied)(Type.Given(_, _))) { case ((parameter, kind), body) => Type.forall(parameter, kind, body) }
    Head(impl, definition, arguments, context, InstanceKey(definition.symbol, heads), tpe)
  }

  /** インスタンスの辞書の束縛。env はメソッドの本体から、superEnv はスーパークラスの解決で見える環境。
   *  eager は、この辞書と一緒に作られる辞書（rec の組）。文脈のない辞書はすぐに作られるので、本体でそれらを読めない */
  private def dictionaryBinding(env: Env, superEnv: Env, head: Head, eager: Set[Symbol]): Typed.Binding = {
    val dictionaries = head.context.map(constraint => new Symbol("given") -> constraint)
    def scoped(outer: Env): Env = dictionaries.foldLeft(outer.withTypeVariables(head.impl.parameters)) { case (acc, (dictionary, constraint)) =>
      acc.withGiven(Evidence(constraint, Typed.Variable(dictionary, constraint)))
    }
    val built = dictionaryValue(scoped(env), scoped(superEnv), head.definition, head.arguments, head.impl, if (head.context.isEmpty) eager else Set.empty)
    val withContext = dictionaries.foldRight(built) { case ((dictionary, constraint), inner) =>
      Typed.GivenLambda(dictionary, constraint, inner, Type.Given(constraint, inner.tpe))
    }
    val value = head.impl.parameters.foldRight(withContext) { case ((parameter, kind), inner) =>
      Typed.TypeLambda(parameter, kind, inner, Type.forall(parameter, kind, inner.tpe))
    }
    Typed.Binding(head.impl.dictionary, head.tpe, value)
  }

  /** 辞書の値 MkC[T̄](スーパークラスの辞書…)(メソッド…)。省略されたメソッドは、デフォルトの関数で埋める */
  private def dictionaryValue(env: Env, superEnv: Env, definition: TraitDef, arguments: List[Type], impl: R.Impl, eager: Set[Symbol]): Typed = {
    val traitName = definition.symbol.name
    val result = definition.instantiate(definition.dictionaryType, arguments)
    val shown = s"impl ${Type.show(result)}"
    impl.methods.find(written => !definition.methods.exists(_.name == written.name)).foreach { extra =>
      fail(CompileError.ExtraMethod(traitName, extra.name, extra.span))
    }
    val missing = definition.required.map(_.name).filterNot(name => impl.methods.exists(_.name == name))
    if (missing.nonEmpty) fail(CompileError.MissingMethods(shown, missing, impl.span))

    // メソッドの注釈では、trait のパラメータの名前がその引数の型を指す
    val methodEnv = impl.traitParameters.zip(arguments).foldLeft(env) {
      case (acc, (Some(parameter), argument)) => acc.withAlias(parameter, argument, acc.kindOf(argument))
      case (acc, (None, _)) => acc
    }
    val written = CompileFailure.collect(impl.methods) { method =>
      val expected = definition.instantiate(definition.methods.find(_.name == method.name).get.signature, arguments)
      val value = Check.infer(methodEnv, method.value)
      if (expected != value.tpe) {
        val (wanted, actual, at, _) = Check.blame(expected, value, method.value, None)
        fail(CompileError.MethodTypeMismatch(shown, method.name, wanted, actual, at))
      }
      method.tpe.foreach(annotation => Check.expectType(expected, convertStar(methodEnv, annotation), annotation.span))
      if (!Check.isFunction(value) && value.mentions(eager)) fail(CompileError.RecursiveInstanceMethodNotFunction(method.name, method.span))
      method.name -> value
    }.toMap
    // スーパークラスの辞書がなければ、どの trait のために必要だったかを添える
    val through = s"trait $traitName${definition.supers.map(s => Type.show(s._1)).mkString(" with ", ", ", "")}"
    val supers = definition.supers.map { case (constraint, _) =>
      try Instances.resolve(superEnv, definition.instantiate(constraint, arguments), impl.span)
      catch {
        case failure: CompileFailure => throw failure.map {
          case CompileError.NoInstance(goal, requiredBy, known, span) => CompileError.NoInstance(goal, requiredBy :+ (Type.show(result), through), known, span)
          case CompileError.Detailed(CompileError.NoInstance(goal, requiredBy, known, span), related, hints, at) =>
            CompileError.Detailed(CompileError.NoInstance(goal, requiredBy :+ (Type.show(result), through), known, span), related, hints, at)
          case other => other
        }
      }
    }

    val typeArguments = arguments.map(Typed.Argument.OfType(_))
    val superArguments = supers.map(Typed.Argument.OfValue(_))
    // デフォルトの関数に渡すので、書かれたメソッドを変数に束縛しておく
    val usesDefault = definition.methods.exists(method => !written.contains(method.name))
    val bound = if (usesDefault) written.map { case (name, value) => name -> Typed.Binding(new Symbol(name), value.tpe, value) } else Map.empty[String, Typed.Binding]
    def methodValue(name: String): Typed = bound.get(name).fold(written(name))(binding => Typed.Variable(binding.symbol, binding.tpe))
    val fields = definition.methods.map { method =>
      if (written.contains(method.name)) methodValue(method.name)
      else {
        val function = method.default.get
        val required = definition.required.map(needed => Typed.Argument.OfValue(methodValue(needed.name)))
        Typed.Apply(Typed.Variable(function, env.values(function)), typeArguments ++ superArguments ++ required, definition.instantiate(method.signature, arguments))
      }
    }
    val constructed = Typed.Apply(
      Typed.Variable(definition.constructor, definition.constructorType),
      typeArguments ++ superArguments ++ fields.map(Typed.Argument.OfValue(_)),
      result
    )
    impl.methods.flatMap(method => bound.get(method.name)).foldRight(constructed: Typed)((binding, rest) => Typed.Let(binding, rest, rest.tpe))
  }

  /** 式としての impl C[T̄] { … }: 辞書の値を作る（インスタンスとしては登録しない） */
  def instanceExpression(env: Env, impl: R.Impl): Typed = {
    val definition = traitOf(env, impl)
    dictionaryValue(env, env, definition, traitArguments(env, definition, impl), impl, Set.empty)
  }
}

final class Declarations {
  import Declarations.*
  import Resolved as R

  private var env = Env.empty
  private val errors = ListBuffer.empty[CompileError]
  private val declarations = ListBuffer.empty[Typed.Decl]
  private val spans = scala.collection.mutable.Map.empty[Symbol, Span]
  // opaque type と、その中身の型（中身は、先に閉じた opaque type を読み替えた形で持つ）
  private var opaque = Map.empty[Symbol, Type]
  // 検査中の module のブロックで宣言された、まだ閉じていない opaque type
  private var unsealed = List.empty[R.Alias]

  private def attempt[A](body: => A): Option[A] =
    try Some(body)
    catch {
      case failure: CompileFailure =>
        errors ++= failure.errors.filterNot(_.core.isInstanceOf[CompileError.DependsOnFailedDeclaration]).map(explainOpaque)
        None
    }

  /** 型の不一致に opaque type が関わっていれば、そのことを添える */
  private def explainOpaque(error: CompileError): CompileError = error.core match {
    case CompileError.TypeMismatch(expected, actual, _) =>
      error.withHints((Type.variables(expected) ++ Type.variables(actual)).toList.filter(opaque.contains).map(symbol => Hint.OpaqueType(symbol.name)))
    case _ => error
  }

  def program(resolved: R.Program): Either[List[CompileError], Typed.Program] = {
    resolved.declarations.foreach(declaration)
    seal(resolved.declarations)
    val main = resolved.main match {
      case None =>
        errors += CompileError.MainMissing(Span.none)
        None
      case Some(symbol) =>
        env.values.get(symbol).filter(_ != Type.Arrow(Type.unit, Type.i32)).foreach { actual =>
          errors += CompileError.MainTypeInvalid(actual, spans.getOrElse(symbol, Span.none))
        }
        Some(symbol)
    }
    main.filter(_ => errors.isEmpty).map(Typed.Program(declarations.toList, _, opaque)).toRight(errors.toList)
  }

  private def declaration(decl: R.Decl): Unit = decl match {
    case R.Decl.Let(parsed) =>
      attempt(Check.signature(env, parsed)).foreach { declared =>
        val before = env
        env = env.withValue(parsed.symbol, declared)
        spans(parsed.symbol) = parsed.span
        attempt(Check.binding(before, parsed, declared)).foreach(checked => declarations += Typed.Decl.Let(checked))
      }

    case R.Decl.Rec(group) =>
      attempt(group.map(parsed => parsed.symbol -> Check.signature(env, parsed))).foreach { signed =>
        val before = env
        env = env.withValues(signed)
        group.foreach(parsed => spans(parsed.symbol) = parsed.span)
        attempt(Check.recursiveBindings(before, group)._2).foreach(checked => declarations += Typed.Decl.Rec(checked))
      }

    case R.Decl.DataTypes(group) =>
      attempt(dataTypes(env, group)).foreach { definitions =>
        env = withDataTypes(env, definitions)
        declarations += Typed.Decl.DataTypes(definitions)
      }

    case R.Decl.TypeAlias(alias) =>
      attempt(withAlias(env, alias)).foreach { extended =>
        env = extended
        if (alias.opaque) unsealed = alias :: unsealed
      }

    case R.Decl.TraitDecl(parsed) =>
      attempt(traitDefinition(env, parsed, None)).foreach { definition =>
        env = withTrait(env, definition)
        val defaults = parsed.methods.zip(definition.methods).flatMap { case (written, checked) =>
          for (body <- written.default; function <- checked.default; binding <- attempt(defaultBinding(env, definition, parsed, written, body, function))) yield binding
        }
        declarations += Typed.Decl.Trait(definition, definition.dictionary, defaults)
      }

    case R.Decl.Instances(group, recursive) =>
      if (attempt(instances(group, recursive)).isEmpty) {
        // 失敗した derive のインスタンスを使う所は、エラーにしない
        val failed = group.collect { case R.Instance.Derived(_, traitSymbol, target, _) => InstanceKey(traitSymbol, List(TypeHead.Named(target))) }
        env = env.copy(failedInstances = env.failedInstances ++ failed.filterNot(env.instances.contains))
      }

    case R.Decl.Module(members) =>
      val outer = unsealed
      unsealed = Nil
      members.foreach(declaration)
      seal(members)
      unsealed = outer
  }

  /** インスタンスの組。先に全員の頭（どの型のインスタンスか）を決めてから、本体を検査する */
  private def instances(group: List[R.Instance], recursive: Boolean): Unit = {
    // derive が作るインスタンスは、自分自身のメソッドから見える（自身を含む型のフィールドのため）
    val written = group.map {
      case R.Instance.Written(impl) => (impl, recursive, None)
      case derived: R.Instance.Derived => (Derive.impl(env, derived), true, Some(derived))
    }
    val heads = written.foldLeft(List.empty[Head]) { case (earlier, (impl, _, _)) =>
      val found = head(env, impl)
      if ((env.instances ++ earlier.map(_.evidence)).contains(found.key))
        throw CompileFailure(CompileError.OverlappingInstance(Type.show(found.definition.instantiate(found.definition.dictionaryType, found.arguments)), impl.span))
      earlier :+ found
    }
    val before = env
    env = env.withInstances(heads.map(_.evidence))
    val bindings = CompileFailure.collect(heads.zip(written).zipWithIndex) { case ((found, (_, selfVisible, derived)), index) =>
      val earlier = before.withInstances(heads.take(index).map(_.evidence))
      val visible = if (recursive) env else if (selfVisible) before.withInstances(List(found.evidence)) else before.copy(hidden = List(found.tpe -> false))
      // スーパークラスの辞書は、この辞書を作るときに読む。文脈のない辞書はすぐに作られるので、組の中では先に宣言されたものしか読めない
      val superEnv = if (recursive && found.context.nonEmpty) env else earlier.copy(hidden = heads.drop(index + 1).map(_.tpe -> true))
      val eager = if (recursive) heads.map(_.impl.dictionary).toSet else Set(found.impl.dictionary)
      val binding =
        try dictionaryBinding(visible, superEnv, found, eager)
        catch {
          // derive が作った impl で足りないインスタンスは、derive のエラーとして伝える
          case failure: CompileFailure if derived.nonEmpty => throw failure.map(error => error.core match {
            case CompileError.NoInstance(goal, _, _, _) =>
              CompileError.DeriveNeeds(shortName(found.definition.symbol), shortName(derived.get.target), goal, derived.get.span)
            case _ => error
          })
        }
      (binding, recursive || selfVisible)
    }
    if (recursive) declarations += Typed.Decl.Rec(bindings.map(_._1))
    else bindings.foreach { case (binding, selfVisible) => declarations += (if (selfVisible) Typed.Decl.Rec(List(binding)) else Typed.Decl.Let(binding)) }
  }

  private def shortName(symbol: Symbol): String = symbol.name.substring(symbol.name.lastIndexOf('.') + 1)

  // ---- opaque type ----

  /** module のブロックを閉じる。そのブロックの opaque type を中身のわからない型にして、
   *  ブロックの宣言が環境に足した型（値の型・データ型・別名・trait）を、注釈から読み直す */
  private def seal(members: List[R.Decl]): Unit = if (unsealed.nonEmpty) {
    unsealed.foreach { alias =>
      val (representation, kind) = env.aliases(alias.symbol)
      opaque += alias.symbol -> Type.substitute(representation, opaque)
      env = env.copy(aliases = env.aliases - alias.symbol).withTypeVariable(alias.symbol, kind)
    }
    members.foreach(reread)
  }

  private def reread(decl: R.Decl): Unit = {
    def quietly(update: => Env): Unit = try env = update catch { case _: CompileFailure => () }
    decl match {
      case R.Decl.Let(parsed) => quietly(env.withValue(parsed.symbol, Check.signature(env, parsed)))
      case R.Decl.Rec(group) => quietly(env.withValues(group.map(parsed => parsed.symbol -> Check.signature(env, parsed))))
      case R.Decl.DataTypes(group) => quietly(withDataTypes(env, dataTypes(env, group)))
      case R.Decl.TypeAlias(alias) => if (!alias.opaque) quietly(withAlias(env, alias))
      case R.Decl.TraitDecl(parsed) => quietly(withTrait(env, traitDefinition(env, parsed, env.traits.get(parsed.symbol))))
      case R.Decl.Instances(_, _) => ()
      case R.Decl.Module(members) => members.foreach(reread)
    }
  }
}
