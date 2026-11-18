package com.yuuki14202028

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** 演算子に宣言された関数（fixity が None なら前置） */
final case class OperatorDef(function: Symbol, fixity: Option[Fixity])

/** リテラル [x, y] / name[x, y] に宣言された関数 */
final case class LiteralDef(nil: Symbol, cons: Symbol)

/** 名前の表。スコープ（その場所から修飾なしで書ける名前）にも、module のメンバーの一覧にも使う */
final case class Names(
    values: Map[String, Symbol],
    types: Map[String, Symbol],
    modules: Map[String, Module],
    operators: Map[String, OperatorDef],
    interpolate: Option[Symbol],
    // キーは "[]" か、名前付きのリテラルの名前（値と同じ名前の空間にあり、互いに隠す）
    literals: Map[String, LiteralDef],
    tuples: Map[Int, Symbol]
) {
  def withValue(name: String, symbol: Symbol): Names =
    if (name == "_") this else copy(values = values + (name -> symbol), literals = literals - name)

  def withType(name: String, symbol: Symbol): Names = copy(types = types + (name -> symbol))

  /** added の名前を足す（同じ名前は added が隠す） */
  def ++(added: Names): Names = Names(
    (values -- added.literals.keys) ++ added.values,
    types ++ added.types,
    modules ++ added.modules,
    operators ++ added.operators,
    added.interpolate.orElse(interpolate),
    (literals -- added.values.keys) ++ added.literals,
    tuples ++ added.tuples
  )
}

object Names {
  val empty: Names = Names(Map.empty, Map.empty, Map.empty, Map.empty, None, Map.empty, Map.empty)
}

/** module（top-level も、名前のない module として扱う）。同じ名前でもう一度開くとメンバーが増えるので、可変 */
final class Module(val path: String) {
  var exported: Names = Names.empty
  var hidden: Names = Names.empty

  def all: Names = exported ++ hidden
  def qualify(name: String): String = if (path.isEmpty) name else s"$path.$name"
}

/** enclosing は、その場所を囲む module（内側が先頭、最後が top-level）。
 *  later（まだ宣言されていない、後ろの宣言の名前）と defining（定義している途中の名前）は、エラーの説明にだけ使う */
final case class Scope(names: Names, enclosing: List[Module], later: Set[String] = Set.empty, defining: Set[String] = Set.empty) {
  def current: Module = enclosing.head
  def withValue(name: String, symbol: Symbol): Scope = copy(names = names.withValue(name, symbol))
  def withType(name: String, symbol: Symbol): Scope = copy(names = names.withType(name, symbol))

  /** この場所から見える module のメンバー（private なものは、その module の中からだけ見える） */
  def visible(module: Module): Names = if (enclosing.exists(_ eq module)) module.all else module.exported
}

/** 名前解決。型を見ずにスコープだけで決まることを、ここで全部決める:
 *  名前が指す束縛（module の修飾・private・open を含む）、演算子の結び付き方、パターンの名前が構成子か変数か、
 *  リテラルや文字列埋め込みに宣言された関数。
 *
 *  宣言は書かれた順に処理するので、その時点までに宣言された名前だけが見える。
 */
object Resolve {
  /** 解決できなかった宣言は結果から外し、エラーと一緒に返す（残りの宣言は、型検査まで進められる） */
  def program(declarations: List[Surface.Decl]): (List[CompileError], Resolved.Program) = new Resolve().program(declarations)
}

final class Resolve {
  import Resolved as R
  import Surface as S

  private val builtinOperators: Map[String, Fixity] = Map("&&" -> Fixity(Assoc.Right, 3), "||" -> Fixity(Assoc.Right, 2))

  private def prefixKey(operator: String): String = s"prefix $operator"

  private def fail(error: CompileError): Nothing = throw CompileFailure(error)

  private val errors = ListBuffer.empty[CompileError]
  private val root = new Module("")

  // Symbol についてわかっていること（スコープには依らない）
  private val constructors = mutable.Set.empty[Symbol]
  private val methods = mutable.Set.empty[Symbol]
  private val traitParameters = mutable.Map.empty[Symbol, List[String]]
  private val fieldAccessors = mutable.Map.empty[Symbol, Map[String, Symbol]]
  // 型と、同じ場所にある同じ名前の module（x.f の f を探す所）
  private val typeModules = mutable.Map.empty[Symbol, Module]

  def program(declarations: List[S.Decl]): (List[CompileError], R.Program) = {
    val resolved = block(Scope(Names.empty, List(root)), declarations)
    (errors.toList, R.Program(resolved, root.exported.values.get("main")))
  }

  // ---- 宣言 ----

  private def block(start: Scope, declarations: List[S.Decl]): List[R.Decl] = {
    val resolved = ListBuffer.empty[R.Decl]
    declarations.foldLeft(start.copy(later = start.later ++ declarations.flatMap(declaredNames))) { (scope, decl) =>
      declaration(scope, decl, isPrivate = false, resolved)
    }
    resolved.toList
  }

  /** 宣言が定義する名前（値と型） */
  private def declaredNames(decl: S.Decl): List[String] = decl match {
    case S.Decl.Let(binding, _) => List(binding.name)
    case S.Decl.Rec(members, _) => members.flatMap(declaredNames)
    case S.Decl.DataType(data, _) => data.name :: data.constructors.map(_.name)
    case S.Decl.Alias(name, _, _, _) => List(name)
    case S.Decl.Trait(name, _, _, methods, _) => name :: methods.map(_.name)
    case S.Decl.Private(inner, _) => declaredNames(inner)
    case S.Decl.Opaque(inner, _) => declaredNames(inner)
    case _ => Nil
  }

  /** 宣言を 1 つ処理して、その後のスコープを返す。解決に失敗しても、宣言した名前は後続から見える */
  private def declaration(scope: Scope, decl: S.Decl, isPrivate: Boolean, resolved: ListBuffer[R.Decl]): Scope = decl match {
    case S.Decl.Private(_, span) if scope.enclosing.length == 1 => errors += CompileError.OutsideModule("private", span); scope
    case S.Decl.Opaque(_, span) if scope.enclosing.length == 1 => errors += CompileError.OutsideModule("opaque", span); scope
    case S.Decl.Private(inner, _) => declaration(scope, inner, isPrivate = true, resolved)

    case S.Decl.Module(name, members, _) =>
      val module = scope.current.all.modules.getOrElse(name, new Module(scope.current.qualify(name)))
      val added = Names.empty.copy(modules = Map(name -> module))
      publish(scope.current, added, isPrivate)
      scope.current.all.types.get(name).foreach(tpe => typeModules(tpe) = module)
      resolved += R.Decl.Module(block(Scope(scope.names ++ added ++ module.all, module :: scope.enclosing), members))
      scope.copy(names = scope.names ++ added)

    case S.Decl.Open(path, _) =>
      try scope.copy(names = scope.names ++ scope.visible(moduleAt(scope, path.segments, path.span)))
      catch { case failure: CompileFailure => errors ++= failure.errors; scope }

    case _ =>
      val (exported, hidden) = declare(scope, decl)
      val after = scope.copy(names = scope.names ++ exported ++ hidden)
      try resolved ++= resolve(scope.copy(defining = declaredNames(decl).toSet), after, decl)
      catch { case failure: CompileFailure => errors ++= failure.errors }
      publish(scope.current, exported, isPrivate)
      publish(scope.current, hidden, isPrivate = true)
      after
  }

  private def publish(module: Module, added: Names, isPrivate: Boolean): Unit =
    if (isPrivate) module.hidden = module.hidden ++ added else module.exported = module.exported ++ added

  /** 宣言が足す名前（公開するものと、module の外に見せないもの）。失敗はしない */
  private def declare(scope: Scope, decl: S.Decl): (Names, Names) = {
    def symbol(name: String): Symbol = new Symbol(scope.current.qualify(name))
    def only(exported: Names): (Names, Names) = (exported, Names.empty)

    decl match {
      case S.Decl.Let(binding, _) => only(Names.empty.withValue(binding.name, symbol(binding.name)))

      case S.Decl.Rec(members, _) =>
        members.map(declare(scope, _)).foldLeft((Names.empty, Names.empty)) { case ((exported, hidden), (nextExported, nextHidden)) =>
          (exported ++ nextExported, hidden ++ nextHidden)
        }

      case S.Decl.DataType(data, _) =>
        val tpe = symbol(data.name)
        val created = data.constructors.map(c => c.name -> symbol(c.name))
        constructors ++= created.map(_._2)
        scope.current.all.modules.get(data.name).foreach(module => typeModules(tpe) = module)
        val names = created.foldLeft(Names.empty.withType(data.name, tpe)) { case (acc, (name, constructor)) => acc.withValue(name, constructor) }
        // 名前付きフィールドを読む関数は、型と同じ名前の module のメンバーになる
        val fieldNames = data.constructors.flatMap(_.fields.flatMap(_.name))
        if (fieldNames.isEmpty) only(names)
        else {
          val module = scope.current.all.modules.getOrElse(data.name, new Module(scope.current.qualify(data.name)))
          val accessors = fieldNames.map(name => name -> new Symbol(module.qualify(name))).toMap
          fieldAccessors(tpe) = accessors
          typeModules(tpe) = module
          module.exported = module.exported ++ Names.empty.copy(values = accessors)
          only(names.copy(modules = Map(data.name -> module)))
        }

      case S.Decl.Alias(name, _, _, _) => only(Names.empty.withType(name, symbol(name)))

      case S.Decl.Operator(operator, fixity, function, _) =>
        val key = if (fixity.isEmpty) prefixKey(operator) else operator
        only(lookupValue(scope, function.segments).fold(Names.empty)(target => Names.empty.copy(operators = Map(key -> OperatorDef(target, fixity)))))

      case S.Decl.Interpolate(function, _) => only(Names.empty.copy(interpolate = lookupValue(scope, function.segments)))

      case S.Decl.LiteralSyntax(kind, functions, _) =>
        only((kind, functions.map(f => lookupValue(scope, f.segments))) match {
          case (S.LiteralKind.Brackets, List(Some(nil), Some(cons))) => Names.empty.copy(literals = Map("[]" -> LiteralDef(nil, cons)))
          case (S.LiteralKind.Named(name), List(Some(nil), Some(cons))) => Names.empty.copy(literals = Map(name -> LiteralDef(nil, cons)))
          case (S.LiteralKind.Tuple(size), List(Some(function))) => Names.empty.copy(tuples = Map(size -> function))
          case _ => Names.empty
        })

      case S.Decl.Trait(name, parameters, _, signatures, _) =>
        val tpe = symbol(name)
        val constructor = symbol(s"Mk$name")
        val created = signatures.map(m => m.name -> symbol(m.name))
        constructors += constructor
        methods ++= created.map(_._2)
        traitParameters(tpe) = parameters.map(_._1)
        only(created.foldLeft(Names.empty.withType(name, tpe).withValue(s"Mk$name", constructor)) { case (acc, (method, target)) => acc.withValue(method, target) })

      // opaque data は型の名前だけを公開し、構成子を隠す
      case S.Decl.Opaque(inner, _) =>
        val (exported, _) = declare(scope, inner)
        val hiddenValues = exported.values.filter { case (_, target) => constructors(target) }
        (exported.copy(values = exported.values -- hiddenValues.keys), Names.empty.copy(values = hiddenValues))

      case S.Decl.Impl(_, _, _, _, _, _) | S.Decl.Derive(_, _, _) => only(Names.empty)
      case S.Decl.Module(_, _, _) | S.Decl.Open(_, _) | S.Decl.Private(_, _) | S.Decl.Import(_, _) => only(Names.empty)
    }
  }

  /** before は宣言の前、after は宣言が足した名前を含むスコープ。rec でない宣言の中身は before で解決する */
  private def resolve(before: Scope, after: Scope, decl: S.Decl): List[R.Decl] = decl match {
    case S.Decl.Let(parsed, _) => List(R.Decl.Let(binding(before, declared(after, parsed.name), parsed)))

    case S.Decl.Rec(members, span) =>
      val lets = members.collect { case S.Decl.Let(parsed, _) => parsed }
      val datas = members.collect { case S.Decl.DataType(parsed, _) => parsed }
      val instances = members.collect { case instance @ (S.Decl.Impl(_, _, _, _, _, _) | S.Decl.Derive(_, _, _)) => instance }
      if (lets.length == members.length) {
        duplicate(lets.map(b => b.name -> b.span))
        List(R.Decl.Rec(lets.map(parsed => binding(after, declared(after, parsed.name), parsed))))
      } else if (datas.length == members.length) {
        duplicate(datas.map(d => d.name -> d.span))
        duplicate(datas.flatMap(d => d.constructors.map(c => c.name -> c.span)))
        datas.foreach(parsed => freshType(before, parsed.name, parsed.span))
        List(R.Decl.DataTypes(datas.map(parsed => data(after, after, parsed, None))))
      } else if (instances.length == members.length) {
        List(R.Decl.Instances(instances.map(instance(before, _)), recursive = true))
      } else fail(CompileError.MixedRecursiveGroup(span))

    case S.Decl.DataType(parsed, _) =>
      freshType(before, parsed.name, parsed.span)
      List(R.Decl.DataTypes(List(data(before, after, parsed, None))))

    case S.Decl.Alias(name, parameters, body, span) =>
      freshType(before, name, span)
      List(R.Decl.TypeAlias(alias(before, after, name, parameters, body, span, opaque = false)))

    case S.Decl.Operator(operator, fixity, function, span) =>
      if (builtinOperators.contains(operator) && fixity.nonEmpty) fail(CompileError.OperatorBuiltin(operator, span))
      value(before, function.segments, function.span)
      Nil

    case S.Decl.Interpolate(function, _) =>
      value(before, function.segments, function.span)
      Nil

    case S.Decl.LiteralSyntax(_, functions, _) =>
      functions.foreach(function => value(before, function.segments, function.span))
      Nil

    case S.Decl.Trait(name, parameters, supers, signatures, span) =>
      freshType(before, name, span)
      duplicate(signatures.map(m => m.name -> m.span))
      val (inner, symbols) = typeParameters(before, parameters, span)
      val locals = signatures.map(m => m.name -> new Symbol(m.name))
      // デフォルトの本体からは、デフォルトを持たないメソッドだけが、普通の値として見える
      val defaultScope = signatures.zip(locals).foldLeft(inner) { case (acc, (m, (methodName, local))) =>
        if (m.default.isEmpty) acc.withValue(methodName, local) else acc
      }
      val resolvedMethods = signatures.zip(locals).map { case (m, (_, local)) =>
        R.Method(m.name, declared(after, m.name), local, typeExpr(inner, m.tpe), m.default.map(expr(defaultScope, _)), m.span)
      }
      List(R.Decl.TraitDecl(R.Trait(after.names.types(name), declared(after, s"Mk$name"), symbols, supers.map(typeExpr(inner, _)), resolvedMethods, span)))

    case S.Decl.Impl(_, _, _, _, _, _) | S.Decl.Derive(_, _, _) => List(R.Decl.Instances(List(instance(before, decl)), recursive = false))

    case S.Decl.Opaque(inner, span) => inner match {
      case S.Decl.Alias(name, parameters, body, aliasSpan) =>
        freshType(before, name, aliasSpan)
        List(R.Decl.TypeAlias(alias(before, after, name, parameters, body, aliasSpan, opaque = true)))
      case S.Decl.DataType(parsed, _) =>
        freshType(before, parsed.name, parsed.span)
        List(R.Decl.DataTypes(List(data(before, after, parsed, Some(FieldNamesReason.OpaqueData)))))
      case S.Decl.Rec(members, _) if members.forall(_.isInstanceOf[S.Decl.DataType]) =>
        val datas = members.collect { case S.Decl.DataType(parsed, _) => parsed }
        List(R.Decl.DataTypes(datas.map(parsed => data(after, after, parsed, Some(FieldNamesReason.OpaqueData)))))
      case _ => fail(CompileError.Syntax("`data` or `type` after `opaque`", "another declaration", span))
    }

    case S.Decl.Module(_, _, span) => fail(CompileError.Syntax("a declaration", "a module", span))
    case S.Decl.Open(_, span) => fail(CompileError.Syntax("a declaration", "`open`", span))
    case S.Decl.Private(_, span) => fail(CompileError.Syntax("a declaration", "`private`", span))
    case S.Decl.Import(_, span) => fail(CompileError.Syntax("a declaration", "`import`", span))
  }

  private def declared(after: Scope, name: String): Symbol = after.names.values.getOrElse(name, new Symbol(name))

  /** 同じ module に、同じ名前の型がまだないこと */
  private def freshType(before: Scope, name: String, span: Span): Unit =
    if (before.current.all.types.contains(name)) fail(CompileError.AlreadyDefined(name, span))

  private def duplicate(names: List[(String, Span)]): Unit =
    names.groupBy(_._1).values.find(_.length > 1).foreach(found => fail(CompileError.AlreadyDefined(found.head._1, found.last._2)))

  private def typeParameters(scope: Scope, parameters: List[(String, Kind)], span: Span): (Scope, List[(Symbol, Kind)]) = {
    parameters.map(_._1).groupBy(identity).collectFirst { case (name, found) if found.length > 1 => fail(CompileError.AlreadyDefined(name, span)) }
    val symbols = parameters.map { case (name, kind) => (name, new Symbol(name), kind) }
    (symbols.foldLeft(scope)((acc, p) => acc.withType(p._1, p._2)), symbols.map(p => (p._2, p._3)))
  }

  /** fieldScope はフィールドの型を解決するスコープ（rec なら自身と組の全員が見える）。
   *  noFieldNames は、フィールドに名前を付けられない理由（付けられるなら None） */
  private def data(fieldScope: Scope, after: Scope, parsed: S.Data, noFieldNames: Option[FieldNamesReason]): R.Data = {
    duplicate(parsed.constructors.map(c => c.name -> c.span))
    val symbol = after.names.types(parsed.name)
    val named = parsed.constructors.flatMap(c => c.fields.flatMap(_.name.map(_ -> c.span)))
    if (named.nonEmpty) {
      val reason = noFieldNames.orElse(Option.when(parsed.constructors.length != 1)(FieldNamesReason.SeveralConstructors))
      reason.foreach(found => fail(CompileError.FieldNamesNotAllowed(symbol.name, found, parsed.span)))
    }
    duplicate(named)
    val accessors = fieldAccessors.getOrElse(symbol, Map.empty)
    val (inner, parameters) = typeParameters(fieldScope, parsed.parameters, parsed.span)
    val resolved = parsed.constructors.map { c =>
      R.Constructor(declared(after, c.name), c.fields.map(field => R.Field(field.name, field.name.flatMap(accessors.get), typeExpr(inner, field.tpe))), c.span)
    }
    R.Data(symbol, parameters, resolved, parsed.span)
  }

  private def alias(before: Scope, after: Scope, name: String, parameters: List[(String, Kind)], body: S.TypeExpr, span: Span, opaque: Boolean): R.Alias = {
    val (inner, symbols) = typeParameters(before, parameters, span)
    R.Alias(after.names.types(name), symbols, typeExpr(inner, body), opaque, span)
  }

  private def binding(scope: Scope, symbol: Symbol, parsed: S.Binding): R.Binding =
    R.Binding(symbol, typeExpr(scope, parsed.tpe), expr(scope, parsed.value), parsed.span)

  private def instance(scope: Scope, decl: S.Decl): R.Instance = decl match {
    case S.Decl.Impl(parameters, traitName, arguments, context, written, span) =>
      val (inner, symbols) = typeParameters(scope, parameters, span)
      R.Instance.Written(impl(inner, symbols, traitName, arguments, context, written, span))
    case S.Decl.Derive(traitName, target, span) =>
      R.Instance.Derived(new Symbol(s"derive ${traitName.show}[${target.show}]"), typeSymbol(scope, traitName), typeSymbol(scope, target), span)
    case other => fail(CompileError.MixedRecursiveGroup(other.span))
  }

  /** scope には impl の型引数が入っている */
  private def impl(scope: Scope, parameters: List[(Symbol, Kind)], traitName: S.Path, arguments: List[S.TypeExpr], context: List[S.TypeExpr], written: List[S.MethodImpl], span: Span): R.Impl = {
    val traitSymbol = typeSymbol(scope, traitName)
    duplicate(written.map(m => m.name -> m.span))
    // メソッドの注釈では、trait のパラメータの名前も使える（その名前の型がすでにスコープにあれば、そちらを指す）
    val traitNames = traitParameters.getOrElse(traitSymbol, Nil).map(name => Option.unless(scope.names.types.contains(name))(name -> new Symbol(name)))
    val methodScope = traitNames.flatten.foldLeft(scope) { case (acc, (name, symbol)) => acc.withType(name, symbol) }
    val resolved = written.map(m => R.MethodImpl(m.name, m.tpe.map(typeExpr(methodScope, _)), expr(methodScope, m.value), m.span))
    val dictionary = new Symbol(s"impl ${traitName.show}")
    R.Impl(dictionary, parameters, traitSymbol, traitNames.map(_.map(_._2)), arguments.map(typeExpr(scope, _)), context.map(typeExpr(scope, _)), resolved, span)
  }

  // ---- 名前を引く ----

  /** A.B のような module の名前 */
  private def moduleAt(scope: Scope, segments: List[String], span: Span): Module = {
    val first = scope.names.modules.getOrElse(segments.head, fail(CompileError.UndefinedModule(segments.head, span)))
    segments.tail.foldLeft(first)((module, name) => scope.visible(module).modules.getOrElse(name, fail(CompileError.UndefinedModule(name, span))))
  }

  /** segments の先頭から module をたどって着く、最後の名前を引く表 */
  private def namesAt(scope: Scope, qualifier: List[String]): Option[Names] = qualifier match {
    case Nil => Some(scope.names)
    case first :: rest =>
      rest.foldLeft(scope.names.modules.get(first))((module, name) => module.flatMap(scope.visible(_).modules.get(name))).map(scope.visible)
  }

  private def lookupValue(scope: Scope, segments: List[String]): Option[Symbol] =
    namesAt(scope, segments.init).flatMap(_.values.get(segments.last))

  private def value(scope: Scope, segments: List[String], span: Span): Symbol =
    lookupValue(scope, segments).getOrElse(fail(undefined(scope, segments, span, isType = false)))

  private def typeSymbol(scope: Scope, path: S.Path): Symbol =
    namesAt(scope, path.segments.init).flatMap(_.types.get(path.segments.last))
      .getOrElse(fail(undefined(scope, path.segments, path.span, isType = true)))

  /** 見つからなかった名前のエラー。その場所からわかる手がかり（private・後ろで宣言・rec が要る・似た名前）を付ける */
  private def undefined(scope: Scope, segments: List[String], span: Span, isType: Boolean): CompileError = {
    val name = segments.mkString(".")
    def table(names: Names): Map[String, Symbol] = if (isType) names.types else names.values
    val owner = segments.init match {
      case Nil => None
      case first :: rest => rest.foldLeft(scope.names.modules.get(first))((module, next) => module.flatMap(_.all.modules.get(next)))
    }
    if (owner.exists(module => table(module.hidden).contains(segments.last))) CompileError.PrivateName(name, isType, span)
    else {
      val error = if (isType) CompileError.UndefinedType(name, span) else CompileError.UndefinedVariable(name, span)
      val hint =
        if (segments.length > 1) owner.map(module => Hint.Similar(similar(segments.last, table(scope.visible(module)).keys).map(found => (segments.init :+ found).mkString("."))))
        else if (scope.defining(name)) Some(Hint.NeedsRec(name, if (isType) "rec data" else "rec let"))
        else if (scope.later(name)) Some(Hint.DeclaredLater(name))
        else {
          // 見えている module に同じ名前のメンバーがあれば、修飾した名前を勧める
          val qualified = scope.names.modules.toList.collect { case (module, found) if table(found.exported).contains(name) => s"$module.$name" }
          Some(Hint.Similar(if (qualified.nonEmpty) qualified.sorted else similar(name, table(scope.names).keys)))
        }
      error.withHints(hint.filter(_ != Hint.Similar(Nil)).toList)
    }
  }

  /** candidates のうち、綴りが target に近いもの（近い順に 3 つまで） */
  private def similar(target: String, candidates: Iterable[String]): List[String] = {
    def distance(a: String, b: String): Int = {
      val row = Array.tabulate(b.length + 1)(identity)
      for (i <- 1 to a.length) {
        var previous = row(0)
        row(0) = i
        for (j <- 1 to b.length) {
          val current = row(j)
          row(j) = (row(j) + 1).min(row(j - 1) + 1).min(previous + (if (a(i - 1) == b(j - 1)) 0 else 1))
          previous = current
        }
      }
      row(b.length)
    }
    val limit = (target.length / 3).max(1)
    candidates.toList.filter(_ != target).map(candidate => candidate -> distance(target, candidate)).filter(_._2 <= limit).sortBy(found => (found._2, found._1)).take(3).map(_._1)
  }

  // ---- 型 ----

  private def typeExpr(scope: Scope, tpe: S.TypeExpr): R.TypeExpr = tpe match {
    case S.TypeExpr.Name(path, span) =>
      if (Primitives.arity.contains(path.show)) R.TypeExpr.Primitive(path.show, span)
      else R.TypeExpr.Named(typeSymbol(scope, path), span)
    case S.TypeExpr.Arrow(from, to, span) => R.TypeExpr.Arrow(typeExpr(scope, from), typeExpr(scope, to), span)
    case S.TypeExpr.Forall(parameter, kind, body, span) =>
      val symbol = new Symbol(parameter)
      R.TypeExpr.Forall(symbol, kind, typeExpr(scope.withType(parameter, symbol), body), span)
    case S.TypeExpr.Lambda(parameter, kind, body, span) =>
      val symbol = new Symbol(parameter)
      R.TypeExpr.Lambda(symbol, kind, typeExpr(scope.withType(parameter, symbol), body), span)
    case S.TypeExpr.Apply(function, argument, span) => R.TypeExpr.Apply(typeExpr(scope, function), typeExpr(scope, argument), span)
    case S.TypeExpr.Tuple(elements, span) => R.TypeExpr.Tuple(elements.map(typeExpr(scope, _)), tupleFunction(scope, elements.length, span), span)
    case S.TypeExpr.Given(constraint, to, span) => R.TypeExpr.Given(typeExpr(scope, constraint), typeExpr(scope, to), span)
  }

  private def tupleFunction(scope: Scope, size: Int, span: Span): Symbol =
    scope.names.tuples.getOrElse(size, fail(CompileError.SyntaxNotDeclared(s"tuples of $size elements", s"literal (${"," * (size - 1)})", span)))

  private def listFunctions(scope: Scope, span: Span): LiteralDef =
    scope.names.literals.getOrElse("[]", fail(CompileError.SyntaxNotDeclared("list literals", "literal []", span)))

  // ---- 式 ----

  /** a.b.c のように名前だけを . でつないだ式なら、その名前の並び */
  private def namePath(e: S.Expr): Option[List[(String, Span)]] = e match {
    case S.Expr.Variable(name, span) => Some(List(name -> span))
    case S.Expr.Select(receiver, name, nameSpan, _) => namePath(receiver).map(_ :+ (name -> nameSpan))
    case _ => None
  }

  /** a.b.c を読む。全体が修飾された名前ならその値。そうでなければ、値を指すいちばん長い前半を値として、残りをドット記法として読む */
  private def path(scope: Scope, segments: List[(String, Span)]): R.Expr = {
    val names = segments.map(_._1)
    val whole = segments.head._2.to(segments.last._2)
    (segments.length to 1 by -1).iterator.flatMap(length => lookupValue(scope, names.take(length)).map(length -> _)).nextOption() match {
      case Some((length, symbol)) =>
        val start: R.Expr = R.Expr.Variable(symbol, segments.head._2.to(segments(length - 1)._2))
        segments.drop(length).foldLeft(start) { case (receiver, (name, span)) => R.Expr.Select(receiver, name, candidates(scope, name), receiver.span.to(span)) }
      case None =>
        // 先頭が module なら、たどれる所まで進んで、その次の名前がないと報告する
        if (names.length > 1 && scope.names.modules.contains(names.head)) {
          val reachable = (names.length - 1 to 1 by -1).find(length => namesAt(scope, names.take(length)).nonEmpty).getOrElse(1)
          fail(undefined(scope, names.take(reachable + 1), whole, isType = false))
        } else fail(undefined(scope, List(names.head), segments.head._2, isType = false))
    }
  }

  private def candidates(scope: Scope, name: String): R.Candidates = {
    def moduleName(primitive: String): String = if (primitive == Primitives.string) "String" else primitive
    def view(module: Module): R.TypeModule = R.TypeModule(module.path, scope.visible(module).values)
    R.Candidates(
      typeModules.iterator.map { case (tpe, module) => tpe -> view(module) }.toMap,
      Primitives.arity.keys.flatMap(primitive => scope.names.modules.get(moduleName(primitive)).map(module => primitive -> view(module))).toMap,
      scope.names.values.get(name).filter(methods)
    )
  }

  /** 名前付きのリテラル name[…] の name なら、宣言された関数 */
  private def namedLiteral(scope: Scope, head: S.Expr): Option[LiteralDef] =
    namePath(head).map(_.map(_._1)).flatMap(names => namesAt(scope, names.init).flatMap(_.literals.get(names.last)))

  private def expr(scope: Scope, e: S.Expr): R.Expr = e match {
    case S.Expr.Variable(_, _) | S.Expr.Select(_, _, _, _) if namePath(e).nonEmpty => path(scope, namePath(e).get)
    case S.Expr.Variable(name, span) => fail(CompileError.UndefinedVariable(name, span))
    case S.Expr.Select(receiver, name, _, span) => R.Expr.Select(expr(scope, receiver), name, candidates(scope, name), span)
    case S.Expr.Constant(value, span) => R.Expr.Constant(value, span)
    case S.Expr.Lambda(parameter, parameterType, body, span) =>
      val symbol = new Symbol(parameter)
      R.Expr.Lambda(symbol, typeExpr(scope, parameterType), expr(scope.withValue(parameter, symbol), body), span)
    case S.Expr.TypeLambda(parameter, kind, body, span) =>
      val symbol = new Symbol(parameter)
      R.Expr.TypeLambda(symbol, kind, expr(scope.withType(parameter, symbol), body), span)
    case S.Expr.GivenLambda(constraint, body, span) => R.Expr.GivenLambda(typeExpr(scope, constraint), expr(scope, body), span)

    case S.Expr.Apply(head, arguments, span) =>
      (namedLiteral(scope, head), arguments.head) match {
        case (Some(literal), S.Argument.Bracket(_, Some(elements), bracketSpan)) =>
          if (elements.isEmpty) fail(CompileError.EmptyListLiteral(bracketSpan))
          val list = R.Expr.ListLiteral(elements.map(expr(scope, _)), literal.nil, literal.cons, head.span.to(bracketSpan))
          if (arguments.tail.isEmpty) list else R.Expr.Apply(list, arguments.tail.map(argument(scope, _)), span)
        case _ => R.Expr.Apply(expr(scope, head), arguments.map(argument(scope, _)), span)
      }

    case S.Expr.Operators(first, rest, _) => operators(scope, first, rest)
    case S.Expr.Prefix(operator, operand, span) =>
      val definition = scope.names.operators.getOrElse(prefixKey(operator), fail(CompileError.UndefinedOperator(operator, isPrefix = true, span)))
      R.Expr.Unary(operator, definition.function, expr(scope, operand), span)
    case S.Expr.If(condition, thenBranch, elseBranch, span) =>
      R.Expr.If(expr(scope, condition), expr(scope, thenBranch), expr(scope, elseBranch), span)
    case S.Expr.Let(parsed, body, span) =>
      val symbol = new Symbol(parsed.name)
      R.Expr.Let(binding(scope.copy(defining = scope.defining + parsed.name), symbol, parsed), expr(scope.withValue(parsed.name, symbol), body), span)
    case S.Expr.LetRec(bindings, body, span) =>
      val (inner, resolved) = recursiveBindings(scope, bindings)
      R.Expr.LetRec(resolved, expr(inner, body), span)
    case S.Expr.LetPattern(bound, tpe, value, body, span) =>
      val (resolved, inner) = pattern(scope, bound)
      R.Expr.LetPattern(resolved, typeExpr(scope, tpe), expr(scope, value), expr(inner, body), span)
    case S.Expr.Local(local, body, span) => localDeclaration(scope, local, body, span)
    case S.Expr.Match(scrutinee, cases, span) => R.Expr.Match(expr(scope, scrutinee), cases.map(matchCase(scope, _)), span)
    case S.Expr.Fold(scrutinee, resultType, cases, span) =>
      R.Expr.Fold(expr(scope, scrutinee), typeExpr(scope, resultType), cases.map(matchCase(scope, _)), span)
    case S.Expr.Block(discarded, result, span) => R.Expr.Block(discarded.map(expr(scope, _)), result.map(expr(scope, _)), span)
    case S.Expr.Tuple(elements, span) => R.Expr.Tuple(elements.map(expr(scope, _)), tupleFunction(scope, elements.length, span), span)
    case S.Expr.ListLiteral(elements, span) =>
      if (elements.isEmpty) fail(CompileError.EmptyListLiteral(span))
      val literal = listFunctions(scope, span)
      R.Expr.ListLiteral(elements.map(expr(scope, _)), literal.nil, literal.cons, span)

    case S.Expr.Interpolation(parts, span) =>
      if (parts.forall(_.isLeft)) R.Expr.Constant(Literal.Str(parts.collect { case Left(text) => text }.mkString), span)
      else {
        val show = scope.names.interpolate.getOrElse(fail(CompileError.SyntaxNotDeclared("string interpolation", "interpolate", span)))
        R.Expr.Interpolation(parts.map(_.map(expr(scope, _))), show, span)
      }

    case S.Expr.Context(monad, steps, result, span) =>
      val flatMap = value(scope, List("flatMap"), span)
      val pure = value(scope, List("pure"), span)
      val (inner, resolved) = steps.foldLeft((scope, List.empty[R.ContextStep])) { case ((acc, done), step) =>
        val (next, extended) = contextStep(acc, step)
        (extended, done :+ next)
      }
      R.Expr.Context(typeExpr(scope, monad), resolved, expr(inner, result), flatMap, pure, span)

    case S.Expr.Summon(constraint, span) => R.Expr.Summon(typeExpr(scope, constraint), span)
    case S.Expr.Using(dictionary, body, span) => R.Expr.Using(expr(scope, dictionary), expr(scope, body), span)
    case S.Expr.Instance(traitName, arguments, written, span) =>
      R.Expr.Instance(impl(scope, Nil, traitName, arguments, Nil, written, span), span)
    case S.Expr.Intrinsic(name, arguments, span) =>
      val intrinsic = Intrinsic.parse(name).getOrElse(fail(CompileError.UnknownIntrinsic(name, span)))
      R.Expr.Primitive(intrinsic, arguments.map(expr(scope, _)), span)
    case S.Expr.Foreign(tpe, name, span) => R.Expr.Foreign(typeExpr(scope, tpe), name, span)
  }

  private def argument(scope: Scope, written: S.Argument): R.Argument = written match {
    case S.Argument.OfType(tpe) => R.Argument.OfType(typeExpr(scope, tpe))
    case S.Argument.OfValue(value) => R.Argument.OfValue(expr(scope, value))
    case S.Argument.OfDictionary(value) => R.Argument.OfDictionary(expr(scope, value))
    case S.Argument.Named(fields, span) => R.Argument.Named(fields.map { case (name, nameSpan, value) => (name, nameSpan, expr(scope, value)) }, span)
    case S.Argument.Bracket(Some(tpe), _, _) => R.Argument.OfType(typeExpr(scope, tpe))
    case S.Argument.Bracket(None, _, span) => fail(CompileError.Syntax("a type argument", "a list of values (the name before `[` is not a named literal)", span))
  }

  private def recursiveBindings(scope: Scope, bindings: List[S.Binding]): (Scope, List[R.Binding]) = {
    duplicate(bindings.map(b => b.name -> b.span))
    val symbols = bindings.map(b => b -> new Symbol(b.name))
    val inner = symbols.foldLeft(scope) { case (acc, (b, symbol)) => acc.withValue(b.name, symbol) }
    (inner, symbols.map { case (b, symbol) => binding(inner, symbol, b) })
  }

  /** 式の中の data / type / rec data。module のメンバーにはならない */
  private def localDeclaration(scope: Scope, local: S.Decl, body: S.Expr, span: Span): R.Expr = {
    def declareData(acc: Scope, parsed: S.Data): Scope = {
      val created = parsed.constructors.map(c => c.name -> new Symbol(c.name))
      constructors ++= created.map(_._2)
      created.foldLeft(acc.withType(parsed.name, new Symbol(parsed.name))) { case (inner, (name, symbol)) => inner.withValue(name, symbol) }
    }
    local match {
      case S.Decl.DataType(parsed, _) =>
        val after = declareData(scope, parsed)
        R.Expr.LocalData(List(data(scope, after, parsed, Some(FieldNamesReason.LocalData))), expr(after, body), span)
      case S.Decl.Rec(members, recSpan) =>
        val datas = members.collect { case S.Decl.DataType(parsed, _) => parsed }
        if (datas.length != members.length) fail(CompileError.MixedRecursiveGroup(recSpan))
        duplicate(datas.map(d => d.name -> d.span))
        val after = datas.foldLeft(scope)(declareData)
        R.Expr.LocalData(datas.map(parsed => data(after, after, parsed, Some(FieldNamesReason.LocalData))), expr(after, body), span)
      case S.Decl.Alias(name, parameters, aliased, aliasSpan) =>
        val after = scope.withType(name, new Symbol(name))
        R.Expr.LocalAlias(alias(scope, after, name, parameters, aliased, aliasSpan, opaque = false), expr(after, body), span)
      case other => fail(CompileError.Syntax("`data` or `type`", "another declaration", other.span))
    }
  }

  private def contextStep(scope: Scope, step: S.ContextStep): (R.ContextStep, Scope) = step match {
    case S.ContextStep.Bind(bound, tpe, value, span) =>
      val (resolved, inner) = pattern(scope, bound)
      (R.ContextStep.Bind(resolved, typeExpr(scope, tpe), expr(scope, value), span), inner)
    case S.ContextStep.Let(parsed) =>
      val symbol = new Symbol(parsed.name)
      (R.ContextStep.Let(binding(scope, symbol, parsed)), scope.withValue(parsed.name, symbol))
    case S.ContextStep.LetPattern(bound, tpe, value, span) =>
      val (resolved, inner) = pattern(scope, bound)
      (R.ContextStep.LetPattern(resolved, typeExpr(scope, tpe), expr(scope, value), span), inner)
    case S.ContextStep.Rec(bindings) =>
      val (inner, resolved) = recursiveBindings(scope, bindings)
      (R.ContextStep.Rec(resolved), inner)
  }

  // ---- パターン ----

  private def matchCase(scope: Scope, c: S.Case): R.Case = {
    val (resolved, inner) = pattern(scope, c.pattern)
    R.Case(resolved, c.guard.map(expr(inner, _)), expr(inner, c.body))
  }

  /** パターンと、その変数が見えるスコープ */
  private def pattern(scope: Scope, p: S.Pattern): (R.Pattern, Scope) = {
    val bound = mutable.LinkedHashMap.empty[String, Symbol]
    val resolved = pattern(scope, p, bound, reuse = None)
    (resolved, bound.foldLeft(scope) { case (acc, (name, symbol)) => acc.withValue(name, symbol) })
  }

  /** bound にパターンの変数を集める。reuse があれば（p | q の 2 つ目以降）、変数はそこにある Symbol を使う */
  private def pattern(scope: Scope, p: S.Pattern, bound: mutable.LinkedHashMap[String, Symbol], reuse: Option[Map[String, Symbol]]): R.Pattern = {
    def constructor(symbol: Symbol, arguments: List[S.Pattern], span: Span): R.Pattern =
      R.Pattern.Constructor(symbol, arguments.map(pattern(scope, _, bound, reuse)), span)

    p match {
      case S.Pattern.Wildcard(span) => R.Pattern.Wildcard(span)

      case S.Pattern.Name(written, arguments, span) =>
        lookupValue(scope, written.segments).filter(constructors) match {
          case Some(symbol) => constructor(symbol, arguments, span)
          case None if arguments.isEmpty && written.segments.length == 1 =>
            val name = written.segments.head
            if (bound.contains(name)) fail(CompileError.PatternBindsTwice(name, span))
            val symbol = reuse match {
              case Some(first) => first.getOrElse(name, fail(CompileError.AlternativesBindDifferently(name, span)))
              case None => new Symbol(name)
            }
            bound(name) = symbol
            R.Pattern.Bind(symbol, span)
          case None => fail(CompileError.NotAConstructor(written.show, span))
        }

      case S.Pattern.Constant(value, span) =>
        R.Pattern.Constant(value, scope.names.operators.get("==").map(_.function), span)

      case S.Pattern.Or(alternatives, span) =>
        val first = mutable.LinkedHashMap.empty[String, Symbol]
        val head = pattern(scope, alternatives.head, first, reuse)
        val rest = alternatives.tail.map { alternative =>
          val again = mutable.LinkedHashMap.empty[String, Symbol]
          val resolved = pattern(scope, alternative, again, Some(first.toMap))
          first.keys.find(!again.contains(_)).foreach(name => fail(CompileError.AlternativesBindDifferently(name, alternative.span)))
          resolved
        }
        first.foreach { case (name, symbol) =>
          if (bound.contains(name)) fail(CompileError.PatternBindsTwice(name, span))
          bound(name) = symbol
        }
        R.Pattern.Or(head :: rest, span)

      case S.Pattern.Tuple(elements, span) =>
        val function = tupleFunction(scope, elements.length, span)
        if (!constructors(function)) fail(CompileError.SyntaxPatternNotConstructor("tuple", span))
        constructor(function, elements, span)

      case S.Pattern.ListLiteral(elements, span) =>
        val literal = listFunctions(scope, span)
        if (!constructors(literal.nil) || !constructors(literal.cons)) fail(CompileError.SyntaxPatternNotConstructor("list", span))
        elements.foldRight(R.Pattern.Constructor(literal.nil, Nil, span): R.Pattern) { (element, rest) =>
          R.Pattern.Constructor(literal.cons, List(pattern(scope, element, bound, reuse), rest), span)
        }
    }
  }

  // ---- 演算子の並びを、宣言された優先順位と結合性で木に組む ----

  private final case class Step(operator: String, fixity: Fixity, span: Span, operand: R.Expr)

  private def operators(scope: Scope, first: S.Expr, rest: List[S.OperatorStep]): R.Expr = {
    val steps = rest.map { step =>
      val fixity = builtinOperators.get(step.operator).orElse(scope.names.operators.get(step.operator).flatMap(_.fixity))
        .getOrElse(fail(CompileError.UndefinedOperator(step.operator, isPrefix = false, step.span)))
      Step(step.operator, fixity, step.span, expr(scope, step.operand))
    }
    climb(scope, expr(scope, first), steps, minLevel = 0)._1
  }

  /** left に、優先順位が minLevel 以上の演算子を左から結び付けていく。結び付けた式と、残りの並びを返す */
  private def climb(scope: Scope, left: R.Expr, steps: List[Step], minLevel: Int): (R.Expr, List[Step]) = steps match {
    case step :: after if step.fixity.level >= minLevel =>
      var right = step.operand
      var remaining = after
      // 右側に、より強く結び付く演算子（同じ優先順位なら、どちらも右結合のもの）が続くかぎり、先に右をまとめる
      while (remaining.headOption.exists(next => bindsTighter(next.fixity, step.fixity))) {
        val (grouped, rest) = climb(scope, right, remaining, remaining.head.fixity.level)
        right = grouped
        remaining = rest
      }
      remaining.headOption.foreach { next =>
        val bothLeft = step.fixity.assoc == Assoc.Left && next.fixity.assoc == Assoc.Left
        if (next.fixity.level == step.fixity.level && !bothLeft) fail(CompileError.OperatorsNotAssociative(step.operator, step.fixity, next.operator, next.fixity, next.span))
      }
      climb(scope, combine(scope, step, left, right), remaining, minLevel)
    case _ => (left, steps)
  }

  private def bindsTighter(next: Fixity, current: Fixity): Boolean =
    next.level > current.level || (next.level == current.level && current.assoc == Assoc.Right && next.assoc == Assoc.Right)

  private def combine(scope: Scope, step: Step, left: R.Expr, right: R.Expr): R.Expr = {
    val span = left.span.to(right.span)
    step.operator match {
      case "&&" => R.Expr.Logic(isAnd = true, left, right, span)
      case "||" => R.Expr.Logic(isAnd = false, left, right, span)
      case operator => R.Expr.Binary(operator, scope.names.operators(operator).function, left, right, span)
    }
  }
}
