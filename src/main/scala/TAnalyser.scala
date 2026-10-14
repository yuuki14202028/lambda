package com.yuuki14202028

import cats.syntax.all._
import cats.data.{ReaderT, StateT}
import Check.{ask, fail, guard, lift}

object TAnalyser {

  private type TC[I] = Check[TypeRec[I]]

  private def invariant(msg: String): Nothing = sys.error(s"Compiler invariant violation: $msg")

  private def okT[I](t: TypeRec[I]): TC[I] = Check.pure(t)

  private def expect(expected: TypeRec[Type], actual: TypeRec[Type]): Check[Unit] =
    guard(Equivalence.alpha(expected, actual), CompileError.TypeMismatch(expected, actual))

  // ---- エラーの位置 ----
  // 規則は親のノードで走るが、原因になった子の式（node）の位置で報告する。related は一緒に示す位置

  private def failAt[A](node: TypeRec[?], error: CompileError, related: (TypeRec[?], Related)*): Check[A] = ask.flatMap { env =>
    val places = related.flatMap { case (place, reason) => env.spans.get(place).map(_ -> reason) }
    val withPlaces = if (places.isEmpty) error else CompileError.RelatedTo(error, places)
    fail(env.spans.get(node).fold(withPlaces)(withPlaces.at))
  }

  private def guardAt(node: TypeRec[?], cond: Boolean, error: => CompileError, related: (TypeRec[?], Related)*): Check[Unit] =
    if (cond) Check.pure(()) else failAt(node, error, related*)

  private def expectAt(expected: TypeRec[Type], actual: TypeRec[Expr], related: (TypeRec[?], Related)*): Check[Unit] =
    guardAt(actual, Equivalence.alpha(expected, typeOf(actual)), CompileError.TypeMismatch(expected, typeOf(actual)), related*)

  /** 型が合わない値 value のうち、実際に原因になっている部分を探す。
   * 引数の型が合っている λ・Λ・λ[C] は中へ進み、本体と結果の型を比べる（`let f(x: T): U = e` なら e と U）。
   * annotation は期待される型を書いた注釈のノードで、同じように中へ進む（別名などで形が違えばそこで止まる）。
   */
  @annotation.tailrec
  private def blame(
      expected: TypeRec[Type],
      value: TypeRec[Expr],
      annotation: Option[TypeRec[Type]]
  ): (TypeRec[Type], TypeRec[Expr], Option[TypeRec[Type]]) = (expected.project, value.project) match {
    case (AST.Arrow(from, to), AST.Abs(_, _, body)) if destructArrow(typeOf(value)).exists { case (param, _) => Equivalence.alpha(from, param) } =>
      blame(to, body, annotation.map(a => a.project match {
        case AST.Arrow(_, result) => result
        case _ => a
      }))
    case (AST.ForAll(variable, kind, inner), AST.TyAbs(bound, boundKind, body)) if kind == boundKind =>
      blame(Equivalence.normalize(substType(variable, typeVarT(bound), inner)), body, annotation.map(a => a.project match {
        case AST.ForAll(_, _, result) => result
        case _ => a
      }))
    case (AST.InstArrow(_, to), AST.InstAbs(_, body)) =>
      blame(to, body, annotation.map(a => a.project match {
        case AST.InstArrow(_, result) => result
        case _ => a
      }))
    case _ => (expected, value, annotation)
  }

  // 注釈の型 expected を持つはずの値 value を検査し、合わなければ原因の部分式と、対応する注釈の部分を示す
  private def expectAnnotated(expected: TypeRec[Type], value: TypeRec[Expr], annotation: TypeRec[Type]): Check[Unit] =
    if (Equivalence.alpha(expected, typeOf(value))) Check.pure(())
    else {
      val (wanted, culprit, place) = blame(expected, value, Some(annotation))
      failAt(culprit, CompileError.TypeMismatch(wanted, typeOf(culprit)), place.map(_ -> Related.ExpectedByAnnotation(wanted)).toSeq*)
    }

  // check の中で起きた（まだ位置のない）エラーを、node の位置で報告する
  private def locatedAt[A](node: TypeRec[?])(check: Check[A]): Check[A] =
    ReaderT((env: Env) => check.run(env).left.map(error => env.spans.get(node).fold(error)(error.at)))

  // ---- 手がかり ----
  // エラーが起きた位置の Env から、直し方の手がかり（似た名前・宣言済みのインスタンスなど）を付ける

  private def editDistance(a: String, b: String): Int = {
    val row = Array.tabulate(b.length + 1)(identity)
    for (i <- 1 to a.length) {
      var diagonal = row(0)
      row(0) = i
      for (j <- 1 to b.length) {
        val above = row(j)
        row(j) = (row(j) + 1).min(row(j - 1) + 1).min(diagonal + (if (a(i - 1) == b(j - 1)) 0 else 1))
        diagonal = above
      }
    }
    row(b.length)
  }

  // 修飾された名前（M.x）は、最後の名前が一致または近ければ候補にする
  private def similarNames(target: String, candidates: Iterable[String]): Seq[String] = {
    val limit = (target.length / 3).max(1)
    candidates.toSeq.distinct
      .filter(c => c != target && !c.startsWith("$") && !c.startsWith("__"))
      .map(c => c -> editDistance(target, c).min(editDistance(target, c.substring(c.lastIndexOf('.') + 1))))
      .filter(_._2 <= limit)
      .sortBy { case (c, distance) => (distance, c) }
      .map(_._1).take(3)
  }

  private def hintsFor(env: Env, error: CompileError): Seq[Hint] = {
    def similar(target: String, candidates: Iterable[String]): Seq[Hint] =
      Option(similarNames(target, candidates)).filter(_.nonEmpty).map(Hint.SimilarNames(_)).toSeq
    def defining(name: String, keyword: String): Option[Hint] =
      Option.when(env.defining.contains(name))(Hint.DeclareRec(keyword, name))
    def later(name: String): Option[Hint] = Option.when(env.declared(name))(Hint.DefinedLater(name))

    error match {
      case CompileError.UndefinedVariable(name) if name.name.contains('.') && env.values.contains(name) =>
        Seq(Hint.PrivateMember(name.name))
      case CompileError.UndefinedTypeVariable(name) if name.name.contains('.') && env.definesTypeName(name) =>
        Seq(Hint.PrivateMember(name.name))
      case CompileError.TypeMismatch(expected, actual) =>
        (freeTypeVars(expected) ++ freeTypeVars(actual)).filter(env.abstractTypes.contains).toSeq.sortBy(_.name).map(t => Hint.OpaqueType(t.name))
      case CompileError.UndefinedVariable(name) =>
        val visible = (env.values.keys.map(_.name) ++ env.scope.values.keys)
          .filter(c => env.resolveValue(Variable(c)).exists(env.values.contains))
        defining(name.name, "rec let").orElse(later(name.name)).toSeq ++ similar(name.name, visible)
      case CompileError.UndefinedTypeVariable(name) =>
        val visible = (env.typeVars.keys ++ env.typeAliases.keys ++ env.dataTypes.keys ++ env.abstractTypes.keys).map(_.name) ++ env.scope.types.keys
        later(name.name).toSeq ++
          similar(name.name, visible.filter(c => env.resolveType(TypeVariable(c)).exists(env.definesTypeName)) ++ BuiltinTypes.primitiveArities.keys)
      case CompileError.UndefinedTrait(name, _) =>
        later(name.name).toSeq ++ similar(name.name, env.traits.keys.map(_.name).filter(c => env.resolveType(TypeVariable(c)).nonEmpty))
      case CompileError.OperatorNotDeclared(_) if env.scope.operators.isEmpty => Seq(Hint.ImportOperators)
      case CompileError.OperatorNotDeclared(op) =>
        def binary(symbol: String): Boolean = Operators.builtin.contains(symbol) || env.scope.operators.get(symbol).exists(_.fixity.nonEmpty)
        (1 until op.length).map(op.splitAt).collectFirst {
          case (first, second) if binary(first) && env.scope.operators.contains(Operators.prefixKey(second)) => Hint.SplitOperator(first, second)
        }.toSeq
      case CompileError.SelectUndefined(member, module, _) =>
        val members = env.modules.get(module).map(_.values.keys.toSeq.sorted).getOrElse(Seq.empty)
        val close = similar(member, members)
        if (close.nonEmpty) close else Seq(Hint.ModuleMembers(module, members))
      case CompileError.UndefinedModule(path) => similar(path.mkString("."), env.modules.keys ++ env.scope.modules.keys)
      case CompileError.NoInstance(name, args)
          if args.flatMap(a => typeConstructorHead(a).map(_._1)).length == args.length &&
            env.laterInstances(instanceDictionaryName(name, args.flatMap(a => typeConstructorHead(a).map(_._1))).name) =>
        Seq(Hint.SuperclassDeclaredLater(showTypeConstraint(TypeConstraint(name, args))))
      case CompileError.NoInstance(name, args) =>
        val wanted = showTypeConstraint(TypeConstraint(name, args))
        val heads = args.flatMap(a => typeConstructorHead(a).map(_._1))
        val self = Option.when(heads.length == args.length && env.defining.contains(instanceDictionaryName(name, heads).name))(Hint.DeclareRec("rec impl", wanted))
        val parameter = Option.when(args.exists(a => collectTypeApps(a)._1.project match {
          case AST.TypeVar(v) => env.typeVars.contains(v)
          case _ => false
        }))(Hint.AddConstraint(wanted))
        val declared = env.instances.values.filter(_.name == name).toSeq.sortBy(_.dictName.name).map(Resolver.showInstance)
        // 自身のインスタンスが要るなら、rec の案内だけで足りる
        self.fold(Hint.Instances(wanted, declared, env.givens.filter(_.name == name).map(showTypeConstraint)) +: parameter.toSeq)(Seq(_))
      case CompileError.InContext(inner) => hintsFor(env, inner)
      case _ => Seq.empty
    }
  }

  private def enrich(env: Env, error: CompileError): CompileError =
    if (error.hinted) error else CompileError.Hinted(error, hintsFor(env, error.core))

  /** 互いに独立な部分（impl の各メソッド、rec の組の各メンバーなど）を、失敗しても残りを検査して、エラーをまとめる。
   * 1 つの宣言の中でも、独立な部分のエラーは一度に報告される */
  private def checkEach[A, B](items: Seq[A])(check: A => Check[B]): Check[Seq[B]] = ReaderT { (env: Env) =>
    val results = items.map(item => check(item).run(env).left.map(enrich(env, _)))
    results.collect { case Left(error) => error }.flatMap(_.all) match {
      case Seq() => Right(results.collect { case Right(value) => value })
      case Seq(only) => Left(only)
      case many => Left(CompileError.Multiple(many))
    }
  }

  // check の中のエラーに、その場の Env から手がかりを付ける（宣言の規則が Env を局所的に変えているとき用）
  private def withHints[A](check: Check[A]): Check[A] =
    ReaderT((env: Env) => check.run(env).left.map(enrich(env, _)))

  private def expectNumeric(actual: TypeRec[Type]): Check[Unit] =
    guard(isNumericType(actual), CompileError.ExpectedNumeric(actual))

  private def expectEquatable(actual: TypeRec[Type]): Check[Unit] =
    guard(isEquatableType(actual), CompileError.ExpectedEquatable(actual))

  // ---- 演算子 ----
  // a ⊕ b は、⊕ に宣言された多相関数 f を a の型で具体化して呼ぶ: f[T](a)(b)。
  // 型検査は f[T]（制約を解決したもの）を記録し、関数呼び出しへの書き換えは OperatorDesugar が行う

  // 演算子（key: 二項は記号、前置は Operators.prefixKey）に宣言された関数を、型 operandType で具体化する
  private def operatorFunction(key: String, operandType: TypeRec[Type]): Check[TypeRec[Expr]] = for {
    env <- ask
    definition <- lift(env.scope.operators.get(key).toRight(CompileError.OperatorNotDeclared(key)))
    resolved <- instantiateDeclared(definition.function, operandType)
  } yield resolved

  // 構文（演算子・埋め込み・リテラル）に宣言された多相関数 ∀a. … を、型 t で具体化して制約を解決する
  private def instantiateDeclared(function: Variable, t: TypeRec[Type]): Check[TypeRec[Expr]] = for {
    env <- ask
    fnType = env.values.getOrElse(function, invariant(s"syntax is declared for an undefined function ${function.name}"))
    applied <- lift(ContextDesugar.applyTypeArg(varrType(function, fnType), t, Kind.Star))
    resolved <- discharge(applied)
  } yield resolved

  // 構文 hook に宣言された関数（スコープに従う）
  private def syntaxFunctions(hook: SyntaxHook): Check[Seq[Variable]] =
    ask.flatMap(env => lift(env.scope.syntax.get(hook).toRight(CompileError.SyntaxNotDeclared(hook))))

  // 関数の型 ∀ā. [制約…] → … の型パラメータ（量化と制約が交互に並んでもよい）と、その後の型
  private def quantifiers(t: TypeRec[Type]): (Seq[(TypeVariable, Kind)], TypeRec[Type]) = {
    @annotation.tailrec
    def go(t: TypeRec[Type], acc: Seq[(TypeVariable, Kind)]): (Seq[(TypeVariable, Kind)], TypeRec[Type]) = destructForAllK(t) match {
      case Some((variable, kind, body)) => go(body, acc :+ (variable -> kind))
      case None => destructInstArrow(t) match {
        case Some((_, to)) => go(to, acc)
        case None => (acc, t)
      }
    }
    go(t, Seq.empty)
  }

  // 関数の型の引数の型を、前から count 個
  private def parameterTypes(t: TypeRec[Type], count: Int): Seq[TypeRec[Type]] =
    (1 to count).foldLeft((Seq.empty[TypeRec[Type]], t)) { case ((acc, rest), _) =>
      destructArrow(rest).fold(invariant(s"${t.show} takes fewer than $count arguments")) { case (from, to) => (acc :+ from, to) }
    }._1

  /** 構文に宣言された関数 f: ∀ā. [制約…] → A₁ → … を、値の型から型引数を読んで具体化する。
   *  Aᵢ を値 vᵢ の型と見比べて ā を決め（ドット記法と同じ照合）、順に渡しながら制約を解決する。決めた型引数も返す */
  private def instantiateFromValues(function: Variable, values: Seq[TypeRec[Expr]]): Check[(TypeRec[Expr], Seq[(TypeRec[Type], Kind)])] = for {
    env <- ask
    fnType = env.values.getOrElse(function, invariant(s"syntax is declared for an undefined function ${function.name}"))
    (params, body) = quantifiers(fnType)
    variables = params.map(_._1).toSet
    matched <- parameterTypes(body, values.length).zip(values).foldLeft(Check.pure(Map.empty[TypeVariable, TypeRec[Type]])) {
      case (acc, (param, value)) => acc.flatMap { found =>
        matchType(param, typeOf(value), variables, found) match {
          case Some(more) => Check.pure(more)
          case None => failAt[Map[TypeVariable, TypeRec[Type]]](value, CompileError.TypeMismatch(param, typeOf(value)))
        }
      }
    }
    // 宣言のときに、どの型パラメータも引数の型に現れることを確かめている
    arguments = params.map { case (variable, kind) => (matched.getOrElse(variable, invariant(s"${function.name}: ${variable.name} is not determined")), kind) }
    resolved <- instantiateWith(function, arguments)
  } yield (resolved, arguments)

  // 宣言された関数に型引数を順に渡し、間の制約を解決する
  private def instantiateWith(function: Variable, arguments: Seq[(TypeRec[Type], Kind)]): Check[TypeRec[Expr]] = ask.flatMap { env =>
    val fnType = env.values.getOrElse(function, invariant(s"syntax is declared for an undefined function ${function.name}"))
    arguments.foldLeft(Check.pure(varrType(function, fnType))) { case (acc, (argument, kind)) =>
      acc.flatMap(satisfyConstraints(_, Nil)).flatMap { case (f, _) => lift(ContextDesugar.applyTypeArg(f, argument, kind)) }
    }.flatMap(satisfyConstraints(_, Nil)).map(_._1).local((e: Env) => e.copy(keepConstraints = false))
  }

  // ---- リストリテラル: [x, y]（literal []）と setOf[x, y]（literal setOf）は、宣言された nil / cons で右から組み立てる ----
  // 型引数は、cons の最初の引数の型を最初の要素の型と見比べて読む

  private def checkListLiteral(functions: Seq[Variable], elements: Seq[TypeRec[Expr]]): Check[TypeRec[Expr]] = elements match {
    case Seq() => fail(CompileError.EmptyListLiteral)
    case first +: _ => for {
      (nilFunction, consFunction) = functions match {
        case Seq(n, c) => (n, c)
        case _ => invariant("a list literal must declare two functions")
      }
      instantiated <- instantiateFromValues(consFunction, Seq(first))
      (cons, arguments) = instantiated
      nil <- instantiateWith(nilFunction, arguments)
      // 宣言のときに形（cons: E → R → R、nil: R）を確かめている
      param = parameterTypes(typeOf(cons), 1).head
      _ <- elements.drop(1).traverse_(element => expectAt(param, element, first -> Related.FirstElement(typeOf(first))))
    } yield listLitT(typeOf(nil), elements, nil, cons)
  }

  // 名前付きのリテラルに宣言された関数（修飾された名前なら、その module の宣言）
  private def namedLiteral(env: Env, name: Variable): Option[Seq[Variable]] = {
    val segments = name.name.split('.').toSeq
    if (segments.length == 1) env.scope.syntax.get(SyntaxHook.NamedLiteral(name.name))
    else env.resolveModule(segments.init).flatMap(_.syntax.get(SyntaxHook.NamedLiteral(segments.last)))
  }

  // ---- 組: (x, y) は、literal (,) に宣言された関数を要素の型で具体化して要素に適用したもの ----

  private def checkTuple(elements: Seq[TypeRec[Expr]]): Check[TypeRec[Expr]] = for {
    functions <- syntaxFunctions(SyntaxHook.Tuple(elements.length))
    instantiated <- instantiateFromValues(functions.head, elements)
    (resolved, _) = instantiated
    params = parameterTypes(typeOf(resolved), elements.length)
    _ <- params.zip(elements).traverse_ { case (param, element) => expectAt(param, element) }
    resultType = elements.foldLeft(typeOf(resolved))((t, _) => destructArrow(t).fold(t)(_._2))
  } yield tupleLitT(resultType, elements, resolved)

  // 組の型 (A, B) は、literal (,) に宣言された関数の結果の型（引数の型を A, B と見比べて型引数を読む）
  private def tupleType(elements: Seq[TypeRec[Type]]): Check[TypeRec[Type]] = for {
    env <- ask
    functions <- syntaxFunctions(SyntaxHook.Tuple(elements.length))
    function = functions.head
    fnType = env.values.getOrElse(function, invariant(s"syntax is declared for an undefined function ${function.name}"))
    (params, body) = quantifiers(fnType)
    normalized = elements.map(Equivalence.normalize)
    matched <- lift(parameterTypes(body, elements.length).zip(normalized).foldLeft(Option(Map.empty[TypeVariable, TypeRec[Type]])) {
      case (acc, (param, element)) => acc.flatMap(found => matchType(param, element, params.map(_._1).toSet, found))
    }.toRight(CompileError.TupleTypeMismatch(function, fnType, normalized)))
    result = elements.foldLeft(body)((t, _) => destructArrow(t).fold(t)(_._2))
  } yield Equivalence.normalize(substMany(params.map(_._1), params.map { case (v, _) => matched.getOrElse(v, typeVarT(v)) }, result))

  // ---- 文字列埋め込み: `{x}` は、interpolate に宣言された関数 f を x の型で具体化して x に適用したもの ----

  private def checkInterpolation(parts: Seq[InterpPart[TypeRec]]): Check[TypeRec[Expr]] =
    parts.traverse {
      case InterpPart.Text(value) => Check.pure(InterpPart.Text[TypeRec](value))
      case InterpPart.Embed(value, _) => for {
        functions <- syntaxFunctions(SyntaxHook.Interpolate)
        instantiated <- instantiateFromValues(functions.head, Seq(value))
        (resolved, _) = instantiated
        _ <- expectAt(parameterTypes(typeOf(resolved), 1).head, value)
      } yield InterpPart.Embed[TypeRec](value, Some(resolved))
    }.map(strInterpT(stringTypeT, _))

  private def checkBinaryOperator(op: String, left: TypeRec[Expr], right: TypeRec[Expr]): Check[TypeRec[Expr]] =
    // && と || は bool 専用の組み込み（右を評価しないことがあるので、関数ではない）
    if (Operators.builtin.contains(op)) for {
      _ <- expectAt(boolTypeT, left)
      _ <- expectAt(boolTypeT, right, left -> Related.LeftOperand(boolTypeT))
    } yield binopT(op, boolTypeT, left, right, None)
    else for {
      resolved <- operatorFunction(op, typeOf(left))
      // 宣言のときに形を確かめているので、f[T] は T → β → γ
      (_, afterLeft) = destructArrow(typeOf(resolved)).getOrElse(invariant(s"operator $op: ${typeOf(resolved).show} is not a function"))
      (rightParam, resultType) = destructArrow(afterLeft).getOrElse(invariant(s"operator $op: ${typeOf(resolved).show} does not take two arguments"))
      _ <- expectAt(rightParam, right, left -> Related.LeftOperand(typeOf(left)))
    } yield binopT(op, resultType, left, right, Some(resolved))

  private def checkUnaryOperator(op: String, body: TypeRec[Expr]): Check[TypeRec[Expr]] = for {
    resolved <- operatorFunction(Operators.prefixKey(op), typeOf(body))
    (_, resultType) = destructArrow(typeOf(resolved)).getOrElse(invariant(s"operator $op: ${typeOf(resolved).show} is not a function"))
  } yield unopT(op, resultType, body, Some(resolved))

  // ---- ドット記法: x.f は、x の型の module にある関数 f の呼び出し ----

  // 型の module の名前。data・opaque type・trait は型と同じ完全な名前、組み込みの型はその名前（foreign.C.String は String）
  private def moduleOfType(env: Env, t: TypeRec[Type]): Option[String] = typeConstructorHead(t).collect {
    case (name, _) if name.startsWith("foreign.C.") => name.stripPrefix("foreign.C.")
    case (name, _) if !env.typeVars.contains(TypeVariable(name)) => name
  }

  // pattern の中の型変数 variables を、target と一致するように決める（型の形を上から順に見比べるだけで、推論はしない）
  private def matchType(
      pattern: TypeRec[Type],
      target: TypeRec[Type],
      variables: Set[TypeVariable],
      initial: Map[TypeVariable, TypeRec[Type]] = Map.empty
  ): Option[Map[TypeVariable, TypeRec[Type]]] = {
    def go(p: TypeRec[Type], t: TypeRec[Type], found: Map[TypeVariable, TypeRec[Type]]): Option[Map[TypeVariable, TypeRec[Type]]] =
      (p.project, t.project) match {
        case (AST.TypeVar(v), _) if variables(v) => found.get(v) match {
          case Some(previous) => Option.when(Equivalence.alpha(previous, t))(found)
          case None => Some(found + (v -> t))
        }
        case (AST.TypeApp(pf, pa), AST.TypeApp(tf, ta)) => go(pf, tf, found).flatMap(go(pa, ta, _))
        case (AST.Arrow(pf, pt), AST.Arrow(tf, tt)) => go(pf, tf, found).flatMap(go(pt, tt, _))
        case _ => Option.when(Equivalence.alpha(p, t))(found)
      }
    go(pattern, target, initial)
  }

  // スコープにある trait のメソッド（短い名前で書けて、局所的な束縛に隠されていないもの）。
  // 局所的な束縛は同じ名前の値の型を置き換えるので、名前の型がメソッドの型のままかで確かめる
  private def traitMethodInScope(env: Env, member: String): Option[Variable] = for {
    full <- env.resolveValue(Variable(member))
    current <- env.values.get(full)
    if env.traits.exists { case (traitName, traitDef) =>
      traitDef.methods.exists { case (method, sig) => method == full && Equivalence.alpha(current, methodType(traitName, traitDef.param, sig)) }
    }
  } yield full

  // x.f の f の候補は、x の型の module の関数と、スコープにある trait のメソッド。両方にあれば曖昧
  private def selectTarget(receiver: TypeRec[Expr], member: String): Check[Variable] = ask.flatMap { env =>
    val receiverType = typeOf(receiver)
    val module = moduleOfType(env, receiverType)
    val function = module.flatMap(env.moduleMember(_, member))
    (function, traitMethodInScope(env, member)) match {
      case (Some(found), Some(method)) => failAt[Variable](receiver, CompileError.SelectAmbiguous(member, found, method, receiverType))
      case (Some(found), None) => Check.pure(found)
      case (None, Some(method)) => Check.pure(method)
      case (None, None) => module match {
        case Some(name) => lift[Variable](Left(CompileError.SelectUndefined(member, name, receiverType)))
        case None => failAt[Variable](receiver, CompileError.SelectWithoutModule(member, receiverType))
      }
    }
  }

  /** 式 e の型の先頭に並ぶ制約を満たす。dictionaries が残っていれば先頭から順にそれで満たし（f(with d) と同じ）、なくなれば解決する。
   *  使わなかった辞書を返す */
  private def satisfyConstraints(e: TypeRec[Expr], dictionaries: List[TypeRec[Expr]]): Check[(TypeRec[Expr], List[TypeRec[Expr]])] =
    destructInstArrow(typeOf(e)) match {
      case Some((constraint, to)) => dictionaries match {
        case dictionary :: rest => for {
          _ <- expectAt(dictionaryType(constraint), dictionary)
          satisfied <- satisfyConstraints(instAppT(to, e, Evidence.Explicit(dictionary)), rest)
        } yield satisfied
        case Nil => Resolver.resolve(constraint).flatMap(evidence => satisfyConstraints(instAppT(to, e, evidence), Nil))
      }
      case None => Check.pure((e, dictionaries))
    }

  private def checkSelect(
      receiver: TypeRec[Expr],
      member: String,
      typeArgs: Seq[TypeRec[Type]],
      dictionaries: Seq[TypeRec[Expr]] = Seq.empty
  ): Check[TypeRec[Expr]] = for {
    env <- ask
    receiverType = typeOf(receiver)
    function <- selectTarget(receiver, member)
    fnType = env.values.getOrElse(function, invariant(s"dot notation target ${function.name} has no type"))
    // f: ∀ā. [制約…] → P → …（trait のメソッドは ∀p̄. [C[p̄]] → ∀ā. [制約…] → P → … のように量化と制約が交互に並ぶ）。
    // P に現れる型引数は受け手の型から読み、残りは名前の後に書く
    (params, afterParams) = {
      @annotation.tailrec
      def binders(t: TypeRec[Type], acc: Seq[(TypeVariable, Kind)]): (Seq[(TypeVariable, Kind)], TypeRec[Type]) = destructForAllK(t) match {
        case Some((variable, kind, body)) => binders(body, acc :+ (variable -> kind))
        case None => destructInstArrow(t) match {
          case Some((_, to)) => binders(to, acc)
          case None => (acc, t)
        }
      }
      binders(fnType, Seq.empty)
    }
    notApplicable = CompileError.SelectNotApplicable(function, fnType, receiverType)
    firstParam <- lift(destructArrow(afterParams).map(_._1).toRight(notApplicable))
    matched <- lift(matchType(firstParam, receiverType, params.map(_._1).toSet).toRight(notApplicable))
    fromReceiver = params.takeWhile { case (variable, _) => matched.contains(variable) }
    _ <- guard(fromReceiver.length == matched.size, notApplicable)
    explicit = params.drop(fromReceiver.length)
    _ <- guard(explicit.length == typeArgs.length, CompileError.SelectTypeArguments(function, fnType, explicit.length, typeArgs.length))
    written <- typeArgs.traverse(expandWellKinded)
    arguments = fromReceiver.map { case (variable, kind) => (matched(variable), kind) } ++ written
    // 制約はここで満たす。x.f(with d) の d は先頭の制約から順に使い、残りは解決する。量化の間にある制約は、その前の型引数を渡した時点で満たす
    satisfied <- arguments.foldLeft(Check.pure((varrType(function, fnType), dictionaries.toList))) { case (acc, (argument, kind)) =>
      acc.flatMap { case (f, rest) => satisfyConstraints(f, rest) }.flatMap { case (f, rest) =>
        lift(ContextDesugar.applyTypeArg(f, argument, kind)).map(_ -> rest)
      }
    }.flatMap { case (f, rest) => satisfyConstraints(f, rest) }.local((e: Env) => e.copy(keepConstraints = false))
    (resolved, unused) = satisfied
    _ <- unused.headOption.fold(Check.pure(()))(extra => failAt[Unit](extra, CompileError.NoConstraintToSatisfy(fnType)))
    (param, resultType) <- lift(destructArrow(typeOf(resolved)).toRight(notApplicable))
    _ <- guard(Equivalence.alpha(param, receiverType), notApplicable)
  } yield selectT(resultType, receiver, member, typeArgs, dictionaries, resolved)

  // ---- 名前付きの構築: C[T̄](x = e, …) は、すべてのフィールドに名前がある構成子に、フィールドを名前で渡す ----

  private def checkNamedApp(constructor: TypeRec[Expr], arguments: Seq[NamedArg[TypeRec]]): Check[TypeRec[Expr]] = {
    // 構成子に型引数を渡したもの C[T̄] の、構成子の名前
    @annotation.tailrec
    def head(e: TypeRec[Expr]): Option[Variable] = e.project match {
      case AST.TyApp(function, _) => head(function)
      case AST.Var(name) => Some(name)
      case _ => None
    }
    def invalid[A](reason: NamedArgumentsReason, name: Option[Variable]): Check[A] =
      failAt(constructor, CompileError.NamedArgumentsInvalid(name, reason))
    for {
      env <- ask
      definition <- head(constructor).flatMap(env.constructors.get) match {
        case Some(found) => Check.pure(found)
        case None => invalid[ConstructorDef](NamedArgumentsReason.NotConstructor, None)
      }
      name = Some(definition.name)
      fieldNames <- definition.names.sequence match {
        case Some(all) if all.nonEmpty => Check.pure(all)
        case _ => invalid[Seq[String]](NamedArgumentsReason.UnnamedFields, name)
      }
      // 型引数を渡し終えた構成子の型は、フィールドの型を順に取る関数
      _ <- destructForAllK(typeOf(constructor)) match {
        case Some(_) =>
          val remaining = Iterator.iterate(Option(typeOf(constructor)))(_.flatMap(t => destructForAllK(t).map(_._3))).drop(1).takeWhile(_.nonEmpty).length
          invalid[Unit](NamedArgumentsReason.TypeArgumentsMissing(remaining), name)
        case None => Check.pure(())
      }
      positions <- arguments.zipWithIndex.traverse { case (argument, index) =>
        val position = fieldNames.indexOf(argument.name)
        if (position < 0) lift[Int](Left(CompileError.NamedArgumentsInvalid(name, NamedArgumentsReason.UnknownField(argument.name, fieldNames)).at(argument.span)))
        else if (arguments.take(index).exists(_.name == argument.name))
          lift[Int](Left(CompileError.NamedArgumentsInvalid(name, NamedArgumentsReason.DuplicateField(argument.name)).at(argument.span)))
        else Check.pure(position)
      }
      missing = fieldNames.indices.filterNot(positions.contains).map(fieldNames)
      _ <- if (missing.isEmpty) Check.pure(()) else invalid[Unit](NamedArgumentsReason.MissingFields(missing), name)
      (fieldTypes, resultType) = fieldNames.foldLeft((Seq.empty[TypeRec[Type]], typeOf(constructor))) { case ((acc, t), _) =>
        val (from, to) = destructArrow(t).getOrElse(invariant(s"constructor ${definition.name.name} has type ${typeOf(constructor).show}"))
        (acc :+ from, to)
      }
      _ <- arguments.zip(positions).traverse_ { case (argument, position) => expectAt(fieldTypes(position), argument.value) }
    } yield namedAppT(resultType, constructor, arguments, positions)
  }

  // . を含む名前 a.b.c: 値を指すいちばん長い前半（a.b.c か a.b か a）を値として読み、残りをドット記法として読む。
  // 全体が修飾された名前（M.x）として読めるなら、それが優先される。前半のどれも値でなければ、全体を修飾された名前として報告する
  private def checkPath(name: Variable, typeArgs: Seq[TypeRec[Type]], dictionaries: Seq[TypeRec[Expr]], nameSpan: Option[Span] = None): Check[TypeRec[Expr]] = ask.flatMap { env =>
    // 名前の部分と、型引数を 1 つずつ渡した途中の式の位置を記録する（エラーが型引数を受け取った値を指せるように）
    def located(node: TypeRec[Expr], end: Option[TypeRec[Type]]): TypeRec[Expr] = {
      nameSpan.foreach(start => env.spans.record(node, end.flatMap(env.spans.get).fold(start)(start.to)))
      node
    }
    val segments = name.name.split('.').toSeq
    val prefix = (segments.length to 1 by -1).find(n => env.lookupValue(Variable(segments.take(n).mkString("."))).nonEmpty)
    def variable(written: Variable): Check[TypeRec[Expr]] = for {
      found <- lift(env.lookupValue(written).toRight(CompileError.UndefinedVariable(written)))
      resolved <- discharge(varrType(found._1, found._2))
    } yield resolved
    prefix match {
      case Some(n) if n < segments.length =>
        val selectors = segments.drop(n)
        for {
          base <- variable(Variable(segments.take(n).mkString("."))).local((e: Env) => e.copy(keepConstraints = false))
          inner <- selectors.init.foldLeft(Check.pure(base))((receiver, member) => receiver.flatMap(checkSelect(_, member, Seq.empty)))
          selected <- checkSelect(inner, selectors.last, typeArgs, dictionaries)
        } yield selected
      case _ =>
        // 修飾された名前の後の (with d) は f(with d) と同じ: 型引数を渡し終えた所で制約を残し、先頭から d で満たす
        val keep = (e: Env) => if (dictionaries.nonEmpty) e.copy(keepConstraints = true) else e
        for {
          applied <- (for {
            base <- (if (typeArgs.isEmpty) variable(name) else variable(name).local((e: Env) => e.copy(keepConstraints = false))).map(located(_, None))
            applied <- typeArgs.zipWithIndex.foldLeft(Check.pure(base)) { case (function, (argument, index)) =>
              val last = index == typeArgs.length - 1
              function.flatMap(f => if (last) checkTyApp(f, argument) else checkTyApp(f, argument).local((e: Env) => e.copy(keepConstraints = false)).map(located(_, Some(argument))))
            }
          } yield applied).local(keep)
          satisfied <- if (dictionaries.isEmpty) Check.pure((applied, Nil))
            else satisfyConstraints(applied, dictionaries.toList).local((e: Env) => e.copy(keepConstraints = false))
          (filled, unused) = satisfied
          _ <- unused.headOption.fold(Check.pure(()))(extra => failAt[Unit](extra, CompileError.NoConstraintToSatisfy(typeOf(applied))))
        } yield filled
    }
  }

  private def checkTyApp(typedFunction: TypeRec[Expr], typedArgument: TypeRec[Type]): Check[TypeRec[Expr]] = for {
    expanded <- expandWellKinded(typedArgument)
    (argumentType, argKind) = expanded
    resultType <- destructForAllK(typeOf(typedFunction)) match {
      case Some((variable, expectedKind, bodyType)) =>
        if (expectedKind == argKind) okT(Equivalence.normalize(substType(variable, argumentType, bodyType)))
        else failAt(typedArgument, CompileError.KindMismatchInTypeApp(expectedKind, argKind, Some(argumentType)),
          typedFunction -> Related.FunctionType(typeOf(typedFunction)))
      case None => failAt(typedFunction, CompileError.NotPolymorphic(typeOf(typedFunction)))
    }
    resolved <- discharge(tyAppT(resultType, typedFunction, typedArgument))
  } yield resolved

  // ---- 演算子の並びを、スコープ中の宣言の優先順位と結合性で木に組む ----

  private enum OpTree {
    case Leaf(operand: TypeRec[Expr])
    case Node(op: String, span: Span, left: OpTree, right: OpTree)
  }

  private def operatorFixity(env: Env, step: OpStep[TypeRec]): EitherS[Fixity] =
    Operators.builtin.get(step.op).orElse(env.scope.operators.get(step.op).flatMap(_.fixity))
      .toRight(CompileError.OperatorNotDeclared(step.op).at(step.span))

  // 優先順位の高い演算子が先に結び付く。同じ優先順位は、両方 infixl なら左から、両方 infixr なら右から。それ以外の並びは決まらない
  private def groupOperators(first: TypeRec[Expr], rest: Seq[(OpStep[TypeRec], Fixity)]): EitherS[OpTree] = {
    var remaining = rest.toList
    def conflict(left: (OpStep[TypeRec], Fixity), right: (OpStep[TypeRec], Fixity)): CompileError =
      CompileError.OperatorsNotAssociative(left._1.op, left._2, right._1.op, right._2).at(right._1.span)
    def climb(start: OpTree, minLevel: Int): EitherS[OpTree] = {
      var left = start
      while (remaining.headOption.exists(_._2.level >= minLevel)) {
        val current = remaining.head
        val (step, fixity) = current
        remaining = remaining.tail
        var right: OpTree = OpTree.Leaf(step.operand)
        def bindsTighter(next: (OpStep[TypeRec], Fixity)): Boolean =
          next._2.level > fixity.level || (next._2.level == fixity.level && fixity.assoc == Assoc.Right && next._2.assoc == Assoc.Right)
        while (remaining.headOption.exists(bindsTighter)) {
          climb(right, remaining.head._2.level) match {
            case Right(grouped) => right = grouped
            case Left(error) => return Left(error)
          }
        }
        remaining.headOption match {
          case Some(next) if next._2.level == fixity.level && !(fixity.assoc == Assoc.Left && next._2.assoc == Assoc.Left) =>
            return Left(conflict(current, next))
          case _ => ()
        }
        left = OpTree.Node(step.op, step.span, left, right)
      }
      Right(left)
    }
    climb(OpTree.Leaf(first), 0)
  }

  private def checkOperatorTree(tree: OpTree): Check[TypeRec[Expr]] = tree match {
    case OpTree.Leaf(operand) => Check.pure(operand)
    case OpTree.Node(op, span, left, right) => for {
      env <- ask
      typedLeft <- checkOperatorTree(left)
      typedRight <- checkOperatorTree(right)
      whole = for { from <- env.spans.get(typedLeft); to <- env.spans.get(typedRight) } yield from.to(to)
      checked <- locatedAt(whole)(checkBinaryOperator(op, typedLeft, typedRight))
      _ = whole.foreach(env.spans.record(checked, _))
    } yield checked
  }

  private def checkOperatorSequence(first: TypeRec[Expr], rest: Seq[OpStep[TypeRec]]): Check[TypeRec[Expr]] = for {
    env <- ask
    fixities <- lift(rest.traverse(step => operatorFixity(env, step).map(step -> _)))
    tree <- lift(groupOperators(first, fixities))
    checked <- checkOperatorTree(tree)
  } yield checked

  // 演算子の宣言に書ける関数の型: ∀(a: *). [制約…] → a → …（二項なら、さらに引数を 1 つ取る）
  private def operatorShape(binary: Boolean, fnType: TypeRec[Type]): Boolean = destructForAllK(fnType) match {
    case Some((variable, Kind.Star, body)) =>
      @annotation.tailrec
      def afterConstraints(t: TypeRec[Type]): TypeRec[Type] = destructInstArrow(t) match {
        case Some((_, to)) => afterConstraints(to)
        case None => t
      }
      destructArrow(afterConstraints(body)) match {
        case Some((first, rest)) if Equivalence.alpha(first, typeVarT(variable)) => !binary || destructArrow(rest).nonEmpty
        case _ => false
      }
    case _ => false
  }

  private def checkIntrinsic(op: IntrinsicOps, args: Seq[TypeRec[Expr]]): Check[TypeRec[Expr]] = op match {
    case IntrinsicOps.BinOp(binOp, operandTypeName) =>
      args match {
        case Seq(left, right) =>
          val operandType = primitiveT(operandTypeName)
          for {
            _ <- expectAt(operandType, left)
            _ <- expectAt(operandType, right)
            _ <- binOp match {
              case BinOps.Add | BinOps.Sub | BinOps.Mul | BinOps.Div =>
                guard(BuiltinTypes.numericTypes.contains(operandTypeName), CompileError.IntrinsicOperandInvalid(binOp, OperandRequirement.Numeric))
              case BinOps.Mod =>
                guard(BuiltinTypes.integerTypes.contains(operandTypeName), CompileError.IntrinsicOperandInvalid(binOp, OperandRequirement.Integer))
              case BinOps.Eq | BinOps.Neq =>
                guard(BuiltinTypes.equatableTypes.contains(operandTypeName), CompileError.IntrinsicOperandInvalid(binOp, OperandRequirement.Equatable))
              case BinOps.Lt | BinOps.Leq | BinOps.Gt | BinOps.Geq =>
                guard(BuiltinTypes.numericTypes.contains(operandTypeName), CompileError.IntrinsicOperandInvalid(binOp, OperandRequirement.Numeric))
              case BinOps.And | BinOps.Or | BinOps.Xor =>
                guard(
                  BuiltinTypes.numericTypes.contains(operandTypeName) || operandTypeName == "bool",
                  CompileError.IntrinsicOperandInvalid(binOp, OperandRequirement.NumericOrBool)
                )
              case BinOps.ShortAnd | BinOps.ShortOr =>
                fail(CompileError.IntrinsicUnsupported(binOp))
            }
            resultType = binOp match {
              case BinOps.Add | BinOps.Sub | BinOps.Mul | BinOps.Div | BinOps.Mod |
                  BinOps.And | BinOps.Or | BinOps.Xor => operandType
              case BinOps.Eq | BinOps.Neq | BinOps.Lt | BinOps.Leq | BinOps.Gt | BinOps.Geq => boolTypeT
              case BinOps.ShortAnd | BinOps.ShortOr => boolTypeT
            }
          } yield intrinsicT(op, resultType, args)
        case _ => fail(CompileError.IntrinsicArityMismatch(binOp, args.length))
      }
    case IntrinsicOps.UnaryOp(unaryOp, operandTypeName) =>
      args match {
        case Seq(body) =>
          val operandType = primitiveT(operandTypeName)
          for {
            _ <- expectAt(operandType, body)
            _ <- unaryOp match {
              case UnaryOps.Neg =>
                guard(BuiltinTypes.numericTypes.contains(operandTypeName), CompileError.IntrinsicOperandInvalid(unaryOp, OperandRequirement.Numeric))
              case UnaryOps.Not =>
                guard(operandTypeName == "bool", CompileError.IntrinsicOperandInvalid(unaryOp, OperandRequirement.Bool))
            }
            resultType = unaryOp match {
              case UnaryOps.Neg => operandType
              case UnaryOps.Not => boolTypeT
            }
          } yield intrinsicT(op, resultType, args)
        case _ => fail(CompileError.IntrinsicArityMismatch(unaryOp, args.length))
      }
  }

  private def foreignArity(t: TypeRec[Type]): Int = {
    @annotation.tailrec
    def loop(current: TypeRec[Type], count: Int): Int = destructArrow(current) match {
      case Some((_, to)) => loop(to, count + 1)
      case None => count
    }
    loop(t, 0)
  }

  private def expectForeignType(t: TypeRec[Type]): Check[Unit] =
    guard(foreignArity(t) > 0, CompileError.ForeignNotFunction(t))


  // ---- 制約 ----

  // 式の型が [C[τ̄]] → τ になった時点で制約を解決し、根拠を InstApp ノードに記録する
  // ただし辞書を明示的に渡す適用 f(with d) の f の位置では、制約を残す（Env.keepConstraints）
  private def discharge(e: TypeRec[Expr]): Check[TypeRec[Expr]] = ask.flatMap { env =>
    destructInstArrow(typeOf(e)) match {
      case Some((constraint, to)) if !env.keepConstraints =>
        Resolver.resolve(constraint).flatMap(evidence => discharge(instAppT(to, e, evidence)))
      case _ => okT(e)
    }
  }

  private def resolveTrait(name: TypeVariable): Check[TypeVariable] =
    ask.flatMap(env => lift(env.resolveType(name).toRight(CompileError.UndefinedTrait(name, None))))

  // 量化された制約のパラメータは、その制約の引数と前提の中だけで見える型変数
  private def underParams[A](params: Seq[(TypeVariable, Kind)])(body: Check[A]): Check[A] =
    params.foldRight(body) { case ((variable, kind), inner) => underTypeVar(variable, kind)(inner) }

  // ソースに書かれた制約についてのエラーは、その制約の位置で報告する
  private def locatedAt[A](span: Option[Span])(check: Check[A]): Check[A] =
    ReaderT((env: Env) => check.run(env).left.map(error => span.fold(error)(found => enrich(env, error).at(found))))

  private def typedConstraint(c: Constraint[TC]): Check[Constraint[TypeRec]] = locatedAt(c.span) {
    underParams(c.params) {
      (resolveTrait(c.name), c.arg.traverse(identity), c.premises.traverse(typedConstraint)).mapN(Constraint[TypeRec](_, _, c.params, _, c.span))
    }
  }

  // trait の存在・引数の数・カインドを確かめ、引数を展開した制約を返す。site はエラー表示用の宣言
  private def expandConstraint(c: Constraint[TypeRec], site: Option[DeclRef]): Check[TypeConstraint] = locatedAt(c.span)(for {
    resolved <- resolveConstraintNames(c)
    _ <- KAnalyser.checkConstraint(resolved, site)
    _ <- checkQuantifiedParams(resolved)
  } yield resolved.hmap([y] => (t: TypeRec[y]) => t.extract match {
    case TypeAnn => Equivalence.normalize(t)
    case _ => t
  }))

  // 解決はヘッドの照合でパラメータを決めるので、パラメータはヘッドの引数に現れなければならない（前提の中でも同じ）
  private def checkQuantifiedParams(c: TypeConstraint): Check[Unit] = for {
    _ <- c.params.traverse_ { case (param, _) =>
      guard(c.arg.exists(arg => freeTypeVars(arg).contains(param)), CompileError.QuantifiedParamUnused(c.name, param))
    }
    _ <- c.premises.traverse_(checkQuantifiedParams)
  } yield ()

  private def resolveConstraintNames(c: Constraint[TypeRec]): Check[TypeConstraint] = underParams(c.params) {
    (c.arg.traverse(resolveNames), c.premises.traverse(resolveConstraintNames)).mapN(Constraint[TypeRec](c.name, _, c.params, _))
  }

  // ---- 文字列埋め込みの脱糖 ----
  // `a = {e}` は型検査時に concat("a = ")(show[τ](e)) へ展開する。
  // concat はスコープ中の変数ではなくランタイムの C 関数 concat を Foreign として直接参照する。
  // ---- 型の展開 ----

  private def underTypeVar[A](variable: TypeVariable, kind: Kind)(body: Check[A]): Check[A] = for {
    env <- ask
    _ <- guard(!env.typeVars.contains(variable), CompileError.AlreadyDefined(NameKind.TypeVariableName, variable.name))
    result <- body.local((e: Env) => e.withTypeVar(variable, kind))
  } yield result

  private def aliasAsTypeAbs(params: Seq[(TypeVariable, Kind)], body: TypeRec[Type]): TypeRec[Type] =
    params.foldRight(body) { case ((v, k), acc) => typeAbsT(v, k, acc) }

  private val resolveNamesAlg: Algebra[TypedAST, TC] = [x] => he => he.ast match {
    case AST.TypeVar(variable) => ask.flatMap { env =>
      if (env.typeVars.contains(variable)) okT(typeVarT(variable))
      else env.typeAliases.get(variable) match {
        case Some(alias) => okT(aliasAsTypeAbs(alias.params, alias.body))
        case None if env.dataTypes.contains(variable) || env.abstractTypes.contains(variable) => okT(typeVarT(variable))
        case None => fail(CompileError.UndefinedTypeVariable(variable))
      }
    }
    case AST.ForAll(variable, kind, body) => underTypeVar(variable, kind)(body).map(forallTypeT(variable, kind, _))
    case AST.TypeAbs(variable, kind, body) => underTypeVar(variable, kind)(body).map(typeAbsT(variable, kind, _))
    case AST.InstArrow(constraint, to) if constraint.quantified =>
      def resolved(c: Constraint[TC]): Check[TypeConstraint] = underParams(c.params) {
        (c.arg.traverse(identity), c.premises.traverse(resolved)).mapN(Constraint[TypeRec](c.name, _, c.params, _))
      }
      (resolved(constraint), to).mapN(instArrowT)
    case node => node.htraverse([y] => (child: TC[y]) => child).map(HCofree(he.ann, _))
  }

  private def resolveNames(t: TypeRec[Type]): Check[TypeRec[Type]] = t.cata(resolveNamesAlg)

  // 展開済みの型は常に β 正規形になる（比較は Equivalence.alpha で足りる）
  private def expandType(t: TypeRec[Type]): Check[TypeRec[Type]] =
    resolveNames(t).map(Equivalence.normalize)

  // 正規化は well-kinded な型に対してのみ停止が保証されるため、カインドを先に確定する
  private def expandWellKinded(types: TypeRec[Type]): Check[(TypeRec[Type], Kind)] = locatedAt(types) {
    for {
      resolved <- resolveNames(types)
      kind <- KAnalyser.kindOf(resolved)
    } yield (Equivalence.normalize(resolved), kind)
  }

  private def expandChecked(types: TypeRec[Type]): Check[TypeRec[Type]] =
    expandWellKinded(types).flatMap { case (expanded, kind) =>
      guardAt(types, kind == Kind.Star, CompileError.KindNotStar(expanded, kind)).as(expanded)
    }

  // ---- 宣言と式（top-level / in 式）で共有する検査 ----

  private def checkTypeNameFresh(kind: NameKind, name: TypeVariable): Check[Unit] =
    ask.flatMap(env => guard(!env.definesTypeName(name), CompileError.AlreadyDefined(kind, name.name)))

  private def checkParams(owner: DeclRef, params: Seq[(TypeVariable, Kind)]): Check[Unit] = for {
    env <- ask
    _ <- guard(params.map(_._1).distinct.length == params.length, CompileError.DuplicateParams(owner))
    _ <- guard(params.forall { case (p, _) => !env.typeVars.contains(p) }, CompileError.ParamAlreadyDefined(owner))
  } yield ()

  // 評価しても本体が走らない値か（Λ は実行時に残らないので剥がして見る）
  @annotation.tailrec
  private def isFunctionValue(e: TypeRec[Expr]): Boolean = e.project match {
    case AST.TyAbs(_, _, body) => isFunctionValue(body)
    case AST.Abs(_, _, _) | AST.InstAbs(_, _) => true
    case _ => false
  }

  // 式の中で制約の解決に使われたインスタンス（文脈として間接的に使われたものを含む）
  private def instancesUsed(e: TypeRec[Expr]): Set[(TypeVariable, Seq[Variable])] = {
    type Keys[I] = Set[(TypeVariable, Seq[Variable])]
    def inEvidence(evidence: Evidence[Keys]): Set[(TypeVariable, Seq[Variable])] = evidence match {
      case Evidence.Given(_) => Set.empty
      case Evidence.Instance(traitName, heads, _, context) => context.flatMap(inEvidence).toSet + instanceKey(traitName, heads)
      case Evidence.Super(_, _, of) => inEvidence(of)
      case Evidence.Apply(of, _, premises) => inEvidence(of) ++ premises.flatMap(inEvidence)
      case Evidence.Abs(_, body) => inEvidence(body)
      case Evidence.Explicit(dictionary) => dictionary
    }
    val alg: Algebra[TypedAST, Keys] = [x] => he => {
      val inChildren = he.ast.htraverse[[a] =>> cats.data.Const[Set[(TypeVariable, Seq[Variable])], a], Keys](
        [y] => (child: Keys[y]) => cats.data.Const(child)
      ).getConst
      he.ast match {
        case AST.InstApp(_, evidence) => inChildren ++ inEvidence(evidence)
        case AST.Summon(_, evidence) => inChildren ++ evidence.fold(Set.empty)(inEvidence)
        case AST.Instance(_, _, _, superEvidence) => inChildren ++ superEvidence.flatMap(inEvidence)
        case _ => inChildren
      }
    }
    e.cata(alg)
  }

  // let: (型付きの注釈, 型付きの値, 展開済みの宣言型)
  // valueType は注釈の型から値の型を決める（context のモナド束縛では M[注釈の型]）
  private def checkBinding(
      variable: Variable,
      types: TC[Type],
      value: TC[Expr],
      valueType: TypeRec[Type] => TypeRec[Type] = identity
  ): Check[(TypeRec[Type], TypeRec[Expr], TypeRec[Type])] = for {
    typedTypes <- types
    declaredType <- expandChecked(typedTypes)
    typedValue <- value
    _ <- expectAnnotated(valueType(declaredType), typedValue, typedTypes)
  } yield (typedTypes, typedValue, declaredType)

  private case class CheckedBinding(variable: Variable, types: TypeRec[Type], value: TypeRec[Expr], declaredType: TypeRec[Type])

  // rec の組: 値からは組の全員が見える。関数でない値は、評価時に初期化前の値を読まないよう組の誰も参照できない
  private def checkRecBindings(bindings: Seq[(Variable, TC[Type], TC[Expr])]): Check[Seq[CheckedBinding]] = {
    val names = bindings.map(_._1)
    for {
      _ <- guard(names.distinct.length == names.length, CompileError.AlreadyDefined(NameKind.VariableName, names.diff(names.distinct).head.name))
      annotated <- bindings.traverse { case (variable, types, _) =>
        types.flatMap(typedTypes => expandChecked(typedTypes).map((variable, typedTypes, _)))
      }
      scope = (e: Env) => e.withValues(annotated.map { case (variable, _, declaredType) => variable -> declaredType })
      checked <- checkEach(bindings.zip(annotated)) { case ((_, _, value), (variable, typedTypes, declaredType)) =>
        for {
          typedValue <- value.local(scope)
          _ <- expectAnnotated(declaredType, typedValue, typedTypes)
          _ <- guard(
            isFunctionValue(typedValue) || freeVars(typedValue).intersect(names.toSet).isEmpty,
            CompileError.RecursiveValueNotFunction(variable)
          )
        } yield CheckedBinding(variable, typedTypes, typedValue, declaredType)
      }
    } yield checked
  }

  // 以下の宣言の検査は「型付きの宣言」と「その宣言が Env に足す定義」を返す。
  // 後者は型付きの宣言だけから決まり（aliasOf / dataOf / traitOf）、module を封印するときに読み直す

  // 別名の本体は型構成子でもよい（`type Result = Either[Failure]`）
  private def aliasOf(params: Seq[(TypeVariable, Kind)], alias: TypeRec[Type]): Check[TypeAlias] =
    expandWellKinded(alias).local((e: Env) => e.withTypeVars(params)).map { case (expanded, _) => TypeAlias(params, expanded) }

  // type: (型付きの別名本体, 別名を登録する Env の拡張)
  private def checkAlias(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], alias: TC[Type]): Check[(TypeRec[Type], Env => Env)] = for {
    _ <- checkTypeNameFresh(NameKind.TypeAliasName, variable)
    _ <- checkParams(DeclRef.TypeAliasDecl(variable), params)
    typedAlias <- alias.local((e: Env) => e.withTypeVars(params))
    definition <- aliasOf(params, typedAlias)
  } yield (typedAlias, (e: Env) => e.withAlias(variable, definition))

  private def constructorType(owner: TypeVariable, params: Seq[(TypeVariable, Kind)], fields: Seq[TypeRec[Type]]): TypeRec[Type] = {
    val result = applyTypeConstructor(owner, params.map { case (v, _) => typeVarT(v) })
    params.foldRight(fields.foldRight(result)(arrowT)) { case ((v, k), body) => forallTypeT(v, k, body) }
  }

  private type DataHead = (TypeVariable, Seq[(TypeVariable, Kind)])

  // フィールドの検査中は、自身と rec の組の全員（peers）を参照できるよう、コンストラクタなしの定義を仮登録する。
  // rec でないデータ型も自身を仮登録するのは、「rec が必要」というエラーを出すため
  private def fieldScope(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], peers: Seq[DataHead])(e: Env): Env = {
    val group = peers.map(_._1).toSet
    ((variable, params) +: peers).foldLeft(e) { case (acc, (name, nameParams)) => acc.withData(name, DataDef(nameParams, Seq.empty, group)) }
      .withTypeVars(params)
  }

  private def dataOf(
      variable: TypeVariable,
      params: Seq[(TypeVariable, Kind)],
      constructors: Seq[DataConstructor[TypeRec]],
      peers: Seq[DataHead]
  ): Check[DataDef] =
    constructors.zipWithIndex.traverse { case (c, tag) =>
      c.fields.traverse(field => expandChecked(field).local(fieldScope(variable, params, peers))).map(ConstructorDef(c.name, variable, _, tag, c.names))
    }.map(DataDef(params, _, peers.map(_._1).toSet))

  private def defineData(variable: TypeVariable, dataDef: DataDef)(e: Env): Env =
    e.withData(variable, dataDef)
      .withValues(dataDef.constructors.map(c => c.name -> constructorType(variable, dataDef.params, c.fields)))
      .copy(constructors = e.constructors ++ dataDef.constructors.map(c => c.name -> c))

  // data: (型付きのコンストラクタ, データ型とコンストラクタを登録する Env の拡張)
  private def checkData(
      variable: TypeVariable,
      params: Seq[(TypeVariable, Kind)],
      constructors: Seq[DataConstructor[TC]],
      peers: Seq[DataHead],
      local: Boolean // 式の中の data（module を持たないので、名前付きフィールドは書けない）
  ): Check[(Seq[DataConstructor[TypeRec]], Env => Env)] = {
    val owner = DeclRef.DataDecl(variable)
    val names = constructors.map(_.name)
    for {
      env <- ask
      _ <- checkTypeNameFresh(NameKind.DataTypeName, variable)
      _ <- checkParams(owner, params)
      _ <- guard(names.distinct.length == names.length, CompileError.DuplicateMembers(owner, MemberKind.ConstructorMember))
      _ <- guard(
        names.forall(name => !env.values.contains(name) && !env.constructors.contains(name)),
        CompileError.MemberAlreadyDefined(owner, MemberKind.ConstructorMember)
      )
      fieldNames = constructors.flatMap(_.fieldNames.flatten)
      _ <- guard(fieldNames.distinct.length == fieldNames.length, CompileError.DuplicateMembers(owner, MemberKind.FieldMember))
      _ <- guard(fieldNames.isEmpty || !local, CompileError.FieldNamesNotAllowed(variable, FieldNamesReason.LocalData))
      _ <- guard(fieldNames.isEmpty || constructors.length == 1, CompileError.FieldNamesNotAllowed(variable, FieldNamesReason.SeveralConstructors))
      typed <- constructors.traverse(c => c.fields.traverse(_.local(fieldScope(variable, params, peers))).map(DataConstructor[TypeRec](c.name, _, c.fieldNames)))
      dataDef <- dataOf(variable, params, typed, peers)
      _ <- guard(
        peers.nonEmpty || !dataDef.constructors.exists(_.fields.exists(field => containsDataApplicationOf(field, variable))),
        CompileError.RecursiveDataNotDeclared(variable)
      )
    } yield (typed, defineData(variable, dataDef))
  }

  // trait の型付きの宣言から TraitDef を作る（スーパークラスとシグネチャを展開する）
  private def traitOf(
      variable: TypeVariable,
      params: Seq[(TypeVariable, Kind)],
      supers: Seq[Constraint[TypeRec]],
      methods: Seq[MethodSig[TypeRec]]
  ): Check[TraitDef] = {
    val scope = (e: Env) => e.withTypeVars(params)
    for {
      expandedSupers <- supers.traverse(expandConstraint(_, Some(DeclRef.TraitDecl(variable)))).local(scope)
      expandedSigs <- methods.traverse(m => expandChecked(m.sig).local(scope).map(m.name -> _))
    } yield TraitDef(params, expandedSigs, expandedSupers, methods.filter(_.body.nonEmpty).map(_.name).toSet)
  }

  // trait C[p̄] は辞書のデータ型 C[p̄] = MkC(スーパークラスの辞書…)(メソッド…) でもある
  private def defineTrait(variable: TypeVariable, traitDef: TraitDef)(e: Env): Env = {
    val params = traitDef.param
    val ctorName = dictionaryConstructor(variable)
    val fields = traitDef.supers.map(dictionaryType) ++ traitDef.methods.map(_._2)
    val ctorDef = ConstructorDef(ctorName, variable, fields, 0)
    e.withData(variable, DataDef(params, Seq(ctorDef)))
      .withValues(traitDef.methods.map { case (name, sig) => name -> methodType(variable, params, sig) } :+ (ctorName -> constructorType(variable, params, fields)))
      .copy(constructors = e.constructors + (ctorName -> ctorDef), traits = e.traits + (variable -> traitDef))
  }

  // match / fold: 網羅性と束縛子を検査し、各分岐の本体を型付けする。
  // binderTypes は (データ型名, 型引数を適用したフィールド型) から束縛子の型を決める
  private def checkCases(kind: MatchKindRef, scrutineeType: TypeRec[Type], cases: Seq[MatchCase[TC]])(
      binderTypes: (TypeVariable, Seq[TypeRec[Type]]) => Seq[TypeRec[Type]]
  ): Check[Seq[MatchCase[TypeRec]]] = for {
    env <- ask
    dataApp <- lift(dataTypeApplication(scrutineeType, env.dataTypes)(_.paramVars))
    (dataName, dataDef, typeArgs) = dataApp
    // 分岐のコンストラクタ名も、式の中の名前と同じ規則で解決する
    named <- cases.traverse { c =>
      lift(env.resolveValue(c.constructor).toRight(CompileError.UndefinedVariable(c.constructor))).map(MatchCase[TC](_, c.binders, c.body))
    }
    caseNames = named.map(_.constructor)
    expected = dataDef.constructors.map(_.name)
    _ <- guard(caseNames.distinct.length == caseNames.length, CompileError.DuplicateCases(kind))
    missing = expected.filterNot(caseNames.contains)
    extra = caseNames.filterNot(expected.contains)
    _ <- guard(missing.isEmpty && extra.isEmpty, CompileError.NonExhaustive(kind, missing, extra))
    typedCases <- named.traverse { c =>
      val constructor = dataDef.constructors.find(_.name == c.constructor)
        .getOrElse(invariant(s"constructor ${c.constructor.name} passed the exhaustiveness check but is not defined"))
      val fieldTypes = constructor.fields.map(field => Equivalence.normalize(substMany(dataDef.paramVars, typeArgs, field)))
      for {
        _ <- guard(
          c.binders.length == fieldTypes.length,
          CompileError.BinderArityMismatch(c.constructor, fieldTypes.length, c.binders.length)
        )
        _ <- guard(c.binders.distinct.length == c.binders.length, CompileError.DuplicateBinders(kind, c.constructor))
        typedBody <- c.body.local((e: Env) => e.withValues(c.binders.zip(binderTypes(dataName, fieldTypes))))
      } yield MatchCase[TypeRec](c.constructor, c.binders, typedBody)
    }
  } yield typedCases

  // パターンを型 expected の値に対して検査する: (解決済みのパターン, 束縛される変数と型)
  private def checkPattern(pattern: Pattern[TC], expected: TypeRec[Type]): Check[(Pattern[TypeRec], Seq[(Variable, TypeRec[Type])])] = pattern match {
    // 部分パターンについてのエラーは、その部分パターンの位置で報告する（型付きのパターンには位置を残さない）
    case Pattern.Located(span, inner) => locatedAt(Some(span))(checkPattern(inner, expected))

    case Pattern.Wildcard() => Check.pure((Pattern.Wildcard[TypeRec](), Seq.empty))
    case Pattern.Bind(name) => Check.pure((Pattern.Bind[TypeRec](name), Seq(name -> expected)))

    // [p, q] / (p, q): 宣言された関数が構成子なら、その構成子のパターンとして読む（[p, q] は cons(p)(cons(q)(nil))）
    case Pattern.Syntax(hook, elements) => for {
      env <- ask
      functions <- syntaxFunctions(hook)
      fields = (name: Variable) => env.constructors.get(name).map(_.fields.length)
      rewritten <- (hook, functions) match {
        case (SyntaxHook.Tuple(arity), Seq(f)) if fields(f).contains(arity) =>
          Check.pure(Pattern.Constructor[TC](f, elements, Nil))
        case (SyntaxHook.ListLiteral, Seq(nil, cons)) if fields(nil).contains(0) && fields(cons).contains(2) =>
          Check.pure(elements.foldRight(Pattern.Constructor[TC](nil, Nil, Nil))((element, rest) => Pattern.Constructor[TC](cons, Seq(element, rest), Nil)))
        case _ => fail[Pattern[TC]](CompileError.SyntaxPatternNotConstructor(hook, functions))
      }
      checked <- checkPattern(rewritten, expected)
    } yield checked

    // p | q: どの選択肢も、同じ変数を同じ型で束縛する
    case Pattern.Or(alternatives) => for {
      checked <- alternatives.traverse(checkPattern(_, expected))
      first = checked.head._2
      _ <- checked.tail.traverse_ { case (_, bindings) =>
        guard(
          bindings.map(_._1).toSet == first.map(_._1).toSet && bindings.length == first.length &&
            first.forall { case (name, t) => bindings.exists { case (n, u) => n == name && Equivalence.alpha(t, u) } },
          CompileError.OrPatternBindingMismatch(first.map(_._1), bindings.map(_._1))
        )
      }
    } yield (Pattern.Or[TypeRec](checked.map(_._1)), first)

    case Pattern.Literal(value, _) =>
      val (literalType, typeName) = value match {
        case LiteralValue.Num(_, name) => (primitiveT(name), name)
        case LiteralValue.Char(_) => (charTypeT, "char")
        case LiteralValue.Bool(_) => (boolTypeT, "bool")
        case LiteralValue.Str(_) => (stringTypeT, BuiltinTypes.CString)
      }
      for {
        _ <- guard(typeName == BuiltinTypes.CString || BuiltinTypes.equatableTypes.contains(typeName), CompileError.NumericLiteralUnknown(typeName))
        _ <- expect(expected, literalType)
        // リテラルは、== に宣言された関数で比べる。bool は if で分岐するので要らない
        equals <- if (typeName == "bool") Check.pure(Option.empty[TypeRec[Expr]]) else operatorFunction("==", literalType).flatMap { resolved =>
          guard(
            Equivalence.alpha(typeOf(resolved), arrowT(literalType, arrowT(literalType, boolTypeT))),
            CompileError.TypeMismatch(arrowT(literalType, arrowT(literalType, boolTypeT)), typeOf(resolved))
          ).as(Option(resolved))
        }
      } yield (Pattern.Literal[TypeRec](value, equals), Seq.empty)

    // 引数のない名前は、スコープ中の構成子でなければ変数の束縛
    case Pattern.Constructor(written, args, _) => for {
      env <- ask
      name <- lift(env.resolveValue(written).toRight(CompileError.UndefinedVariable(written)))
      result <- env.constructors.get(name) match {
        case None if args.isEmpty && !written.name.contains('.') => Check.pure((Pattern.Bind[TypeRec](written), Seq(written -> expected)))
        case None => fail(CompileError.NotAConstructor(written))
        case Some(constructor) => for {
          dataApp <- lift(dataTypeApplication(expected, env.dataTypes)(_.paramVars))
          (dataName, dataDef, typeArgs) = dataApp
          _ <- guard(constructor.owner == dataName, CompileError.PatternConstructorMismatch(written, expected))
          _ <- guard(args.length == constructor.fields.length, CompileError.BinderArityMismatch(written, constructor.fields.length, args.length))
          shapes = dataDef.constructors.map { c =>
            ConstructorShape[TypeRec](c.name, c.fields.map(field => Equivalence.normalize(substMany(dataDef.paramVars, typeArgs, field))))
          }
          fieldTypes = shapes.find(_.name == name).map(_.fields).getOrElse(invariant(s"constructor ${name.name} is missing from its data type"))
          checked <- args.zip(fieldTypes).traverse { case (arg, fieldType) => checkPattern(arg, fieldType) }
        } yield (Pattern.Constructor[TypeRec](name, checked.map(_._1), shapes), checked.flatMap(_._2))
      }
    } yield result
  }

  // 束縛のパターン: すべての値に当たらなければならない（当たらない値があれば、その例を示す）
  private def checkBindingPattern(pattern: Pattern[TC], valueType: TypeRec[Type]): Check[(Pattern[TypeRec], Seq[(Variable, TypeRec[Type])])] = {
    val span = pattern match {
      case Pattern.Located(found, _) => Some(found)
      case _ => None
    }
    locatedAt(span)(for {
      checked <- checkPattern(pattern, valueType)
      (typed, bindings) = checked
      names = bindings.map(_._1)
      _ <- guard(names.distinct.length == names.length, CompileError.AlreadyDefined(NameKind.VariableName, names.diff(names.distinct).head.name))
      _ <- lift(PatternDesugar.check(valueType, Seq(PatternCase[TypeRec](typed, None, unitLitT(unitTypeT)))).left.map {
        case CompileError.NonExhaustivePatterns(example, _) => CompileError.RefutablePattern(example)
        case other => other
      })
    } yield checked)
  }

  // context[M] が使うメソッド参照 name[M]（M の制約もここで解決する）
  private def contextMethod(name: Variable, monad: TypeRec[Type]): Check[TypeRec[Expr]] = for {
    env <- ask
    found <- lift(env.lookupValue(name).toRight(CompileError.ContextMethodMissing(name)))
    (method, methodType) = found
    applied <- lift(ContextDesugar.applyTypeArg(varrType(method, methodType), monad, ContextDesugar.monadKind))
    resolved <- discharge(applied)
  } yield resolved

  // ---- 式と型 ----

  private def declarationNode(node: String): Nothing = invariant(s"$node is a declaration and must be checked by checkDecl")

  private val tcAlg: Algebra[AST, TC] = [x] => (node: AST[TC, x]) => node match {

    case AST.Program(_) => declarationNode("Program")
    case AST.TopLet(_, _, _) => declarationNode("TopLet")
    case AST.TopRec(_) => declarationNode("TopRec")
    case AST.TopImport(_) => declarationNode("TopImport")
    case AST.TopType(_, _, _) => declarationNode("TopType")
    case AST.TopData(_, _, _) => declarationNode("TopData")
    case AST.TopTrait(_, _, _, _) => declarationNode("TopTrait")
    case AST.TopImpl(_, _, _, _, _, _) => declarationNode("TopImpl")
    case AST.TopDerive(_, _) => declarationNode("TopDerive")
    case AST.TopModule(_, _) => declarationNode("TopModule")
    case AST.TopOperator(_, _, _) => declarationNode("TopOperator")
    case AST.TopSyntax(_, _) => declarationNode("TopSyntax")
    case AST.TopOpen(_) => declarationNode("TopOpen")
    case AST.TopModified(_, _) => declarationNode("TopModified")

    case AST.Abs(variable, types, body) => for {
      typedTypes <- types
      paramType <- expandChecked(typedTypes)
      typedBody <- body.local((e: Env) => e.withValue(variable, paramType))
    } yield absT(variable, arrowT(paramType, typeOf(typedBody)), typedTypes, typedBody)

    case AST.TyAbs(variable, kind, body) =>
      underTypeVar(variable, kind)(body).map(typedBody => tyAbsT(variable, forallTypeT(variable, kind, typeOf(typedBody)), kind, typedBody))

    case AST.InstAbs(constraint, body) => for {
      typed <- typedConstraint(constraint)
      assumed <- expandConstraint(typed, None)
      typedBody <- body.local((e: Env) => e.withGivens(Seq(assumed)))
    } yield instAbsT(instArrowT(assumed, typeOf(typedBody)), typed, typedBody)

    // f(with d): f の型の先頭の制約を解決せず、辞書の値 d で満たす
    case AST.InstApp(function, Evidence.Explicit(dictionary)) => for {
      typedFunction <- function
      typedDictionary <- dictionary
      expected <- destructInstArrow(typeOf(typedFunction)) match {
        case Some(found) => Check.pure(found)
        case None => failAt(typedFunction, CompileError.NoConstraintToSatisfy(typeOf(typedFunction)))
      }
      (constraint, to) = expected
      _ <- expectAt(dictionaryType(constraint), typedDictionary, typedFunction -> Related.FunctionType(typeOf(typedFunction)))
      resolved <- discharge(instAppT(to, typedFunction, Evidence.Explicit(typedDictionary)))
    } yield resolved

    case AST.InstApp(_, _) => invariant("InstApp with resolved evidence is inserted by the type checker and cannot appear in its input")

    // impl C[τ̄] { … }（式）: インスタンスとして登録しない impl。メソッドの検査は宣言の impl と同じ
    case AST.Instance(writtenTrait, targets, methods, _) => for {
      env <- ask
      traitName <- resolveTrait(writtenTrait)
      traitDef <- lift(env.traits.get(traitName).toRight(CompileError.UndefinedTrait(traitName, None)))
      _ <- guard(targets.length == traitDef.param.length, CompileError.ImplTargetArity(traitName, traitDef.param.length, targets.length))
      typedTargets <- targets.traverse(identity)
      expandedKinded <- typedTargets.traverse(expandWellKinded)
      _ <- expandedKinded.zip(traitDef.param).zip(typedTargets).traverse_ { case (((expanded, targetKind), (_, paramKind)), written) =>
        guardAt(written, targetKind == paramKind, CompileError.ImplKindMismatch(traitName, expanded, paramKind, targetKind))
      }
      expandedTargets = expandedKinded.map(_._1)
      shown = expandedTargets.map(_.show)
      // 宣言の impl と同じ検査に乗せる（型パラメータ・文脈・組はなし。辞書の名前は使われない）
      head = ImplHead(
        traitName, traitDef, Seq.empty, expandedTargets, Seq.empty, DeclRef.ImplDecl(traitName, shown), instanceKey(traitName, shown),
        InstanceDef(traitName, expandedTargets, Seq.empty, Variable("$instance"), Seq.empty)
      )
      checked <- ReaderT((e: Env) => implBody(head, methods, group = Map.empty).run(e).left.map { error =>
        // スーパークラスの辞書がないときは、using で入れる方法を案内する
        error.core match {
          case CompileError.NoInstance(name, args) if error.requirements.nonEmpty =>
            CompileError.Hinted(error, Seq(Hint.ProvideWithUsing(showTypeConstraint(TypeConstraint(name, args)))))
          case _ => error
        }
      })
      typed <- checked.project match {
        case AST.TopImpl(_, _, _, _, typedMethods, superEvidence) =>
          okT(instanceT(applyTypeConstructor(traitName, expandedTargets), traitName, expandedTargets, typedMethods, superEvidence))
        case _ => invariant("implBody must produce an impl")
      }
    } yield typed

    // using d in e: d の型（辞書の型）が表す制約を、e の中でスコープに入れる
    case AST.Using(dictionary, body) => for {
      env <- ask
      typedDictionary <- dictionary
      constraint <- constraintOfDictionary(typeOf(typedDictionary), env.traits) match {
        case Some(found) => Check.pure(found)
        case None => failAt(typedDictionary, CompileError.NotADictionary(typeOf(typedDictionary)))
      }
      typedBody <- body.local((e: Env) => e.withGivens(Seq(constraint)))
    } yield usingT(typeOf(typedBody), typedDictionary, typedBody)

    case AST.Summon(constraint, _) => for {
      typed <- typedConstraint(constraint)
      expanded <- expandConstraint(typed, None)
      evidence <- Resolver.resolve(expanded)
    } yield summonT(dictionaryType(expanded), typed, evidence)

    case AST.Let(variable, types, value, body) => for {
      binding <- checkBinding(variable, types, value.local((e: Env) => e.copy(defining = e.defining + variable.name)))
      (typedTypes, typedValue, declaredType) = binding
      typedBody <- body.local((e: Env) => e.withValue(variable, declaredType))
    } yield letT(variable, typeOf(typedBody), typedTypes, typedValue, typedBody)

    case AST.LetPattern(pattern, types, value, body) => for {
      binding <- checkBinding(Variable("_"), types, value)
      (typedTypes, typedValue, declaredType) = binding
      checked <- checkBindingPattern(pattern, declaredType)
      (typedPattern, bindings) = checked
      typedBody <- body.local((e: Env) => e.withValues(bindings))
    } yield letPatternT(typeOf(typedBody), typedPattern, typedTypes, typedValue, typedBody)

    case AST.LetRec(bindings, body) => for {
      checked <- checkRecBindings(bindings.map(b => (b.variable, b.types, b.value)))
      typedBody <- body.local((e: Env) => e.withValues(checked.map(b => b.variable -> b.declaredType)))
    } yield letRecGroupT(checked.map(b => RecBinding[TypeRec](b.variable, b.types, b.value)), typeOf(typedBody), typedBody)

    case AST.TypeLet(variable, params, alias, body) => for {
      checked <- checkAlias(variable, params, alias)
      (typedAlias, extend) = checked
      typedBody <- body.local(extend)
    } yield typeLetT(variable, params, typeOf(typedBody), typedAlias, typedBody)

    case AST.DataLet(variable, params, constructors, body) => for {
      checked <- checkData(variable, params, constructors, peers = Seq.empty, local = true)
      (typedConstructors, extend) = checked
      typedBody <- body.local(extend)
    } yield dataLetT(variable, params, typeOf(typedBody), typedConstructors, typedBody)

    case AST.DataLetRec(datas, body) =>
      val peers = datas.map(d => (d.variable, d.params))
      for {
        // 先に宣言したメンバーの定義を見せながら順に検査する。名前の重複はここで見つかる
        checked <- datas.foldLeft(Check.pure((Seq.empty[DataBinding[TypeRec]], identity[Env]))) { (acc, d) =>
          acc.flatMap { case (done, extend) =>
            checkData(d.variable, d.params, d.constructors, peers, local = true).local(extend).map { case (typedConstructors, define) =>
              (done :+ DataBinding[TypeRec](d.variable, d.params, typedConstructors), extend.andThen(define))
            }
          }
        }
        (typedDatas, extend) = checked
        typedBody <- body.local(extend)
      } yield dataLetRecT(typedDatas, typeOf(typedBody), typedBody)

    case AST.PatternMatch(scrutinee, cases) => for {
      typedScrutinee <- scrutinee
      typedCases <- cases.traverse { c =>
        for {
          // パターンについてのエラーは、その分岐のパターンの位置で報告する
          checked <- locatedAt(c.span) {
            checkPattern(c.pattern, typeOf(typedScrutinee)).flatMap { case found @ (_, bindings) =>
              val names = bindings.map(_._1)
              guard(names.distinct.length == names.length, CompileError.AlreadyDefined(NameKind.VariableName, names.diff(names.distinct).head.name)).as(found)
            }
          }
          (pattern, bindings) = checked
          typedGuard <- c.guard.traverse(_.local((e: Env) => e.withValues(bindings)))
          _ <- typedGuard.traverse_(g => expectAt(boolTypeT, g))
          typedBody <- c.body.local((e: Env) => e.withValues(bindings))
        } yield PatternCase[TypeRec](pattern, typedGuard, typedBody, c.span)
      }
      resultType <- typedCases.headOption match {
        case Some(first) =>
          typedCases.tail.traverse(c => expectAt(typeOf(first.body), c.body, first.body -> Related.FirstCase(typeOf(first.body)))).as(typeOf(first.body))
        case None => fail(CompileError.EmptyMatch)
      }
      // ノードは保持したまま、網羅性と到達不能な分岐は脱糖と同じ決定木で調べる
      // 到達不能な分岐は、その分岐の位置で報告する
      _ <- lift(PatternDesugar.check(typeOf(typedScrutinee), typedCases).left.map {
        case unreachable @ CompileError.UnreachableCase(index) => typedCases.lift(index - 1).flatMap(_.span).fold(unreachable)(unreachable.at)
        case other => other
      })
    } yield patternMatchT(resultType, typedScrutinee, typedCases)

    case AST.Match(scrutinee, cases) => for {
      typedScrutinee <- scrutinee
      typedCases <- checkCases(MatchKindRef.MatchExpr, typeOf(typedScrutinee), cases)((_, fieldTypes) => fieldTypes)
      resultType <- typedCases.headOption match {
        case Some(first) =>
          typedCases.tail.traverse(c => expectAt(typeOf(first.body), c.body, first.body -> Related.FirstCase(typeOf(first.body)))).as(typeOf(first.body))
        case None => fail(CompileError.EmptyMatch)
      }
    } yield matchExprT(resultType, typedScrutinee, typedCases)

    case AST.Fold(scrutinee, resultTypeNode, cases) => for {
      typedScrutinee <- scrutinee
      typedResultType <- resultTypeNode
      resultType <- expandChecked(typedResultType)
      // 再帰位置のフィールドは、畳み込み済みの結果として束縛される
      typedCases <- checkCases(MatchKindRef.FoldExpr, typeOf(typedScrutinee), cases) { (dataName, fieldTypes) =>
        fieldTypes.map(field => if (isDataApplicationOf(field, dataName)) resultType else field)
      }
      _ <- typedCases.traverse_(c => expectAt(resultType, c.body, typedResultType -> Related.ExpectedByAnnotation(resultType)))
    } yield foldExprT(resultType, typedScrutinee, typedResultType, typedCases)

    case AST.App(function, argument) => for {
      typedFunction <- function
      typedArgument <- argument
      resultType <- destructArrow(typeOf(typedFunction)) match {
        case Some((from, to)) if Equivalence.alpha(from, typeOf(typedArgument)) => okT(to)
        case Some((from, _)) =>
          failAt(typedArgument, CompileError.TypeMismatch(from, typeOf(typedArgument)), typedFunction -> Related.FunctionType(typeOf(typedFunction)))
        case None => failAt(typedFunction, CompileError.NotAFunction(typeOf(typedFunction)))
      }
    } yield appT(resultType, typedFunction, typedArgument)

    case AST.TyApp(function, argument) => for {
      typedFunction <- function
      typedArgument <- argument
      resolved <- checkTyApp(typedFunction, typedArgument)
    } yield resolved

    // name[ … ] は、name が名前付きのリテラルなら要素、そうでなければ型引数
    case AST.Path(name, typeArgs, dictionaries, literal, nameSpan) => ask.flatMap { env =>
      (namedLiteral(env, name), literal) match {
        case (Some(functions), Some(elements)) if dictionaries.isEmpty =>
          elements.traverse(identity).flatMap(checkListLiteral(functions, _))
        case (None, Some(_)) if typeArgs.isEmpty => fail(CompileError.NotALiteral(name))
        case _ => for {
          typedArgs <- typeArgs.traverse(identity)
          typedDictionaries <- dictionaries.traverse(identity)
          checked <- checkPath(name, typedArgs, typedDictionaries, nameSpan)
        } yield checked
      }
    }

    case AST.TupleLit(elements, _) => elements.traverse(identity).flatMap(checkTuple)

    case AST.Select(receiver, member, typeArgs, dictionaries, _) => for {
      typedReceiver <- receiver
      typedArgs <- typeArgs.traverse(identity)
      typedDictionaries <- dictionaries.traverse(identity)
      selected <- checkSelect(typedReceiver, member, typedArgs, typedDictionaries)
    } yield selected

    case AST.NamedApp(constructor, arguments, _) => for {
      typedConstructor <- constructor
      typedValues <- arguments.traverse(_.value)
      checked <- checkNamedApp(typedConstructor, arguments.zip(typedValues).map { case (a, v) => NamedArg[TypeRec](a.name, a.span, v) })
    } yield checked

    case AST.Foreign(value, types) => for {
      typedTypes <- types
      declaredType <- expandChecked(typedTypes)
      _ <- expectForeignType(declaredType)
    } yield foreignT(value, declaredType, typedTypes)

    case AST.Var(value) => for {
      env <- ask
      found <- lift(env.lookupValue(value).toRight(CompileError.UndefinedVariable(value)))
      (name, t) = found
      resolved <- discharge(varrType(name, t))
    } yield resolved

    case AST.Num(value, typeName) =>
      if (BuiltinTypes.numericTypes.contains(typeName)) okT(numT(value, typeName, primitiveT(typeName)))
      else fail(CompileError.NumericLiteralUnknown(typeName))
    case AST.Char(value) => okT(charT(value, charTypeT))
    case AST.StringLit(value) => okT(stringLitT(value, stringTypeT))
    case AST.ListLit(elements, _, _) => for {
      functions <- syntaxFunctions(SyntaxHook.ListLiteral)
      typedElements <- elements.traverse(identity)
      checked <- checkListLiteral(functions, typedElements)
    } yield checked

    case AST.StrInterp(parts) => for {
      typedParts <- parts.traverse {
        case InterpPart.Text(value) => Check.pure(InterpPart.Text[TypeRec](value))
        case InterpPart.Embed(value, _) => value.map(InterpPart.Embed[TypeRec](_, None))
      }
      checked <- checkInterpolation(typedParts)
    } yield checked
    case AST.Bool(value) => okT(boolT(value, boolTypeT))
    case AST.UnitLit() => okT(unitLitT(unitTypeT))

    case AST.Block(discarded, result) => for {
      typedDiscarded <- discarded.traverse(identity)
      typedResult <- result.traverse(identity)
      resultType = typedResult.map(typeOf).getOrElse(unitTypeT)
    } yield blockT(resultType, typedDiscarded, typedResult)

    case AST.Context(monad, bindings, result, _) => for {
      typedMonad <- monad
      expandedMonad <- expandWellKinded(typedMonad)
      (monadType, monadKind) = expandedMonad
      _ <- guard(
        monadKind == ContextDesugar.monadKind,
        CompileError.ContextMonadKindInvalid(monadType, monadKind)
      )
      // 型付きの束縛の注釈は、展開済みの型で持つ（脱糖がそのまま使う）
      checkedBindings <- bindings.toList.foldLeftM(
        (Map.empty[Variable, TypeRec[Type]], List.empty[ContextStep[TypeRec]])
      ) { case ((scope, done), step) =>
        val scopeFn = (e: Env) => e.withValues(scope)
        (step match {
          case ContextStep.Bind(name, annotation, value) =>
            checkBinding(name, annotation, value, typeAppT(monadType, _)).map { case (_, typedValue, annType) =>
              (scope + (name -> annType), done :+ ContextStep.Bind[TypeRec](name, annType, typedValue))
            }
          case ContextStep.Let(name, annotation, value) =>
            checkBinding(name, annotation, value).map { case (_, typedValue, annType) =>
              (scope + (name -> annType), done :+ ContextStep.Let[TypeRec](name, annType, typedValue))
            }
          case ContextStep.BindPattern(pattern, annotation, value) => for {
            binding <- checkBinding(Variable("_"), annotation, value, typeAppT(monadType, _))
            (_, typedValue, annType) = binding
            checked <- checkBindingPattern(pattern, annType)
            (typedPattern, bindings) = checked
          } yield (scope ++ bindings, done :+ ContextStep.BindPattern[TypeRec](typedPattern, annType, typedValue))
          case ContextStep.LetPattern(pattern, annotation, value) => for {
            binding <- checkBinding(Variable("_"), annotation, value)
            (_, typedValue, annType) = binding
            checked <- checkBindingPattern(pattern, annType)
            (typedPattern, bindings) = checked
          } yield (scope ++ bindings, done :+ ContextStep.LetPattern[TypeRec](typedPattern, annType, typedValue))
          case ContextStep.Rec(recBindings) =>
            checkRecBindings(recBindings.map(b => (b.variable, b.types, b.value))).map { checked =>
              (scope ++ checked.map(b => b.variable -> b.declaredType),
                done :+ ContextStep.Rec[TypeRec](checked.map(b => RecBinding[TypeRec](b.variable, b.declaredType, b.value))))
            }
        }).local(scopeFn)
      }
      (bindersScope, typedBindings) = checkedBindings
      typedResult <- result.local((e: Env) => e.withValues(bindersScope))
      pureRef <- contextMethod(ContextDesugar.pureMethod, monadType)
      flatMapRef <- contextMethod(ContextDesugar.flatMapMethod, monadType)
      ops = ContextOps[TypeRec](pureRef, flatMapRef)
      // ノードは保持したまま、脱糖後の式の型をこのノードの型とする
      chain <- lift(ContextDesugar.desugarContext(ops, typedBindings, typedResult))
    } yield contextExprT(typeOf(chain), monadType, typedBindings, typedResult, ops)

    case AST.OpSeq(first, rest) => for {
      typedFirst <- first
      typedRest <- rest.traverse(step => step.operand.map(OpStep[TypeRec](step.op, step.span, _)))
      checked <- checkOperatorSequence(typedFirst, typedRest)
    } yield checked

    case AST.BinOp(op, left, right, _) => for {
      typedLeft <- left
      typedRight <- right
      checked <- checkBinaryOperator(op, typedLeft, typedRight)
    } yield checked

    case AST.Intrinsic(op, args) => for {
      typedArgs <- args.traverse(identity)
      checked <- checkIntrinsic(op, typedArgs)
    } yield checked

    case AST.UnaryOp(op, body, _) => for {
      typedBody <- body
      checked <- checkUnaryOperator(op, typedBody)
    } yield checked

    case AST.If(cond, thenBranch, elseBranch) => for {
      typedCond <- cond
      typedThen <- thenBranch
      typedElse <- elseBranch
      thenType = typeOf(typedThen)
      _ <- expectAt(boolTypeT, typedCond)
      _ <- expectAt(thenType, typedElse, typedThen -> Related.OtherBranch(thenType))
    } yield ifT(thenType, typedCond, typedThen, typedElse)

    case AST.Primitive(name) =>
      BuiltinTypes.arity(name) match {
        case Some(_) => okT(primitiveT(name))
        case None => fail(CompileError.UndefinedPrimitive(name))
      }
    case AST.TypeVar(variable) => for {
      env <- ask
      name <- lift(env.resolveType(variable).filter(env.definesTypeName).toRight(CompileError.UndefinedTypeVariable(variable)))
    } yield typeVarT(name)
    case AST.Arrow(from, to) => (from, to).mapN(arrowT)
    case AST.InstArrow(constraint, to) => (typedConstraint(constraint), to).mapN(instArrowT)
    case AST.ForAll(variable, kind, body) => underTypeVar(variable, kind)(body).map(forallTypeT(variable, kind, _))
    case AST.TypeAbs(variable, kind, body) => underTypeVar(variable, kind)(body).map(typeAbsT(variable, kind, _))
    case AST.TypeApp(function, argument) => (function, argument).mapN(typeAppT)
    // 組の型は、ここで literal (,) に宣言された関数の結果の型に読み替える（要素の型は別名を展開してから見比べる）
    case AST.TupleType(elements) => elements.traverse(identity).flatMap(_.traverse(expandType)).flatMap(tupleType)
  }

  // 子の式では、型に残った制約を解決する。残すのは f(with d) の f の位置だけ
  private val indexedAlg: Algebra[IndexedAST, TC] = [x] => he => {
    val resolving = [y] => (child: TC[y]) => child.local((e: Env) => e.copy(keepConstraints = false))
    val node: AST[TC, x] = he.lower match {
      case AST.InstApp(function, evidence) =>
        AST.InstApp(function.local((e: Env) => e.copy(keepConstraints = true)), evidence.hmap(resolving))
      case other => other.hmap(resolving)
    }
    ReaderT { (env: Env) =>
      tcAlg(node).run(env) match {
        case Right(typed) =>
          env.spans.record(typed, he.ask)
          Right(typed)
        // 手がかりは、エラーが起きた位置の Env から付ける
        case Left(error) => Left(enrich(env, error).at(he.ask))
      }
    }
  }

  private def check[I](t: IndexedRec[I]): TC[I] = t.cata(indexedAlg)

  // ---- 宣言 ----
  // 宣言は検査結果と、後続の宣言から見える Env を同時に返す

  private type Checked = Check[(TypeRec[Decl], Env)]

  private def extending(decl: TypeRec[Decl])(extend: Env => Env): Checked = ask.map(env => (decl, extend(env)))

  // module の中の宣言は、短い名前 x を完全な名前 M.x で定義する
  private def memberName(variable: Variable): Check[Variable] = ask.map(env => Variable(env.qualify(variable.name)))
  private def memberType(variable: TypeVariable): Check[TypeVariable] = ask.map(env => TypeVariable(env.qualify(variable.name)))

  private def shortName(name: String): String = name.substring(name.lastIndexOf('.') + 1)

  private def declRule(decl: AST[TC, Decl]): Checked = decl match {

    case AST.TopLet(variable, types, value) => for {
      name <- memberName(variable)
      binding <- checkBinding(name, types, value.local((e: Env) => e.copy(defining = e.defining + variable.name)))
      (typedTypes, typedValue, declaredType) = binding
      checked <- extending(topLetT(name, typedTypes, typedValue))(_.withValue(name, declaredType).exportValue(variable))
    } yield checked

    case AST.TopImport(path) => fail(CompileError.UnresolvedImport(path))

    case AST.TopType(variable, params, alias) => for {
      name <- memberType(variable)
      checked <- checkAlias(name, params, alias)
      (typedAlias, extend) = checked
      result <- extending(topTypeT(name, params, typedAlias))(e => extend(e).exportType(variable))
    } yield result

    case AST.TopData(variable, params, constructors) => checkTopData(variable, params, constructors, peers = Seq.empty, visible = Seq(variable))

    case AST.TopTrait(variable, params, supers, methods) =>
      val scope = (e: Env) => e.withTypeVars(params)
      for {
        env <- ask
        name <- memberType(variable)
        owner = DeclRef.TraitDecl(name)
        methodNames = methods.map(m => Variable(env.qualify(m.name.name)))
        ctorName = dictionaryConstructor(name)
        _ <- checkTypeNameFresh(NameKind.TraitName, name)
        _ <- guard(params.nonEmpty, CompileError.TraitNeedsParameter(name))
        _ <- checkParams(owner, params)
        _ <- guard(methodNames.distinct.length == methodNames.length, CompileError.DuplicateMembers(owner, MemberKind.MethodMember))
        _ <- guard(
          methodNames.forall(method => !env.values.contains(method) && !env.constructors.contains(method)),
          CompileError.MemberAlreadyDefined(owner, MemberKind.MethodMember)
        )
        _ <- guard(
          !env.values.contains(ctorName) && !env.constructors.contains(ctorName),
          CompileError.DictionaryCtorAlreadyDefined(name, ctorName)
        )
        typedSupers <- supers.traverse(typedConstraint).local(scope)
        typedSigs <- methods.zip(methodNames).traverse { case (m, method) => m.sig.local(scope).map(method -> _) }
        signature <- traitOf(name, params, typedSupers, methods.zip(typedSigs).map { case (m, (method, sig)) =>
          // 本体の有無だけを伝える（本体そのものは下で検査する）
          MethodSig[TypeRec](method, sig, m.body.map(_ => unitLitT(unitTypeT)))
        })
        /* デフォルトの本体からは、必須メソッドが（この trait の型パラメータでの）普通の関数として見え、
         * スーパークラスの制約がスコープに入る。自身のインスタンスは見えないので、辞書は自分自身を参照しない */
        requiredShorts = methods.filter(_.body.isEmpty).map(_.name)
        defaultScope = (e: Env) => requiredShorts.foldLeft(scope(e).withValues(signature.required).withGivens(signature.supers))(_.exportValue(_))
        typedMethods <- checkEach(methods.zip(signature.methods)) { case (m, (method, sig)) =>
          m.body.traverse { body =>
            body.local(defaultScope).flatMap { typedBody =>
              (if (Equivalence.alpha(sig, typeOf(typedBody))) Check.pure(()) else {
                val (wanted, culprit, _) = blame(sig, typedBody, None)
                failAt[Unit](culprit, CompileError.DefaultMethodTypeMismatch(name, method, sig, typeOf(typedBody), Some((wanted, typeOf(culprit)))))
              }).as(typedBody)
            }
          }.map(typedBody => MethodSig[TypeRec](method, typedSigs.find(_._1 == method).get._2, typedBody))
        }
        typed = topTraitT(name, params, typedSupers, typedMethods)
        result <- extending(typed) { e =>
          (methods.map(_.name) :+ Variable(shortName(ctorName.name))).foldLeft(defineTrait(name, signature)(e).exportType(variable))((acc, short) => acc.exportValue(short))
        }
      } yield result

    case AST.TopImpl(writtenTrait, implParams, targets, context, methods, _) => for {
      env <- ask
      head <- implHead(writtenTrait, implParams, targets, context)
      typed <- implBody(head, methods, group = Map.empty)
    } yield (typed, env.copy(instances = env.instances + (head.key -> head.inst)))

    case AST.TopDerive(writtenTrait, writtenTarget) => for {
      env <- ask
      traitName <- resolveTrait(writtenTrait)
      target <- lift(env.resolveType(writtenTarget).toRight(CompileError.UndefinedTypeVariable(writtenTarget)))
      inst <- lift(Deriver.check(traitName, target, env))
    } yield (topDeriveT(traitName, target), env.copy(instances = env.instances + (instanceKey(traitName, Seq(target.name)) -> inst)))
    case AST.TopRec(_) => declarationNode("TopRec")
    case AST.TopModule(_, _) => declarationNode("TopModule")
    case AST.TopOpen(_) => declarationNode("TopOpen")
    case AST.TopModified(_, _) => declarationNode("TopModified")

    // infixl 6 ⊕ = f / prefix ⊖ = f: f は、最初の型引数に左の値の型を受け取る多相関数。他の宣言と同じく、宣言の後で見え、module のメンバーになる
    case AST.TopOperator(op, fixity, written) => for {
      env <- ask
      _ <- guard(fixity.isEmpty || !Operators.builtin.contains(op), CompileError.OperatorBuiltin(op))
      found <- lift(env.lookupValue(written).toRight(CompileError.UndefinedVariable(written)))
      (function, fnType) = found
      key = if (fixity.isEmpty) Operators.prefixKey(op) else op
      _ <- guard(operatorShape(fixity.nonEmpty, fnType), CompileError.OperatorTargetInvalid(key, function, fnType))
      definition = OperatorDef(function, fixity)
    } yield (topOperatorT(op, fixity, function), env.copy(scope = env.scope.withOperator(key, definition), members = env.members.withOperator(key, definition)))

    // interpolate = f / literal [] = nil, cons: 構文が呼ぶ関数。演算子の宣言と同じく、宣言の後で見え、module のメンバーになる
    case AST.TopSyntax(hook, written) => for {
      env <- ask
      found <- written.traverse(name => lift(env.lookupValue(name).toRight(CompileError.UndefinedVariable(name))))
      _ <- guard(syntaxShape(hook, found.map(_._2)), CompileError.SyntaxTargetInvalid(hook, found.map(_._1), found.map(_._2)))
      functions = found.map(_._1)
    } yield (topSyntaxT(hook, functions), env.copy(scope = env.scope.withSyntax(hook, functions), members = env.members.withSyntax(hook, functions)))
  }

  /** 構文に宣言できる関数の型。型パラメータ ā はどれも、値（埋め込む値・要素）を受け取る引数の型に現れなければならない（そこから読む）。
   *  interpolate: ∀ā. [制約…] → A → String
   *  literal [] / literal 名前: nil は ∀ā. [制約…] → R、cons は ∀ā. [制約…] → E → R → R（ā と R は両方で同じ）
   *  literal (,…): ∀ā. [制約…] → A₁ → … → Aₙ → R */
  private def syntaxShape(hook: SyntaxHook, types: Seq[TypeRec[Type]]): Boolean = {
    def arguments(t: TypeRec[Type], count: Int): Option[(Seq[TypeRec[Type]], TypeRec[Type])] =
      (1 to count).foldLeft(Option((Seq.empty[TypeRec[Type]], t))) { case (acc, _) =>
        acc.flatMap { case (done, rest) => destructArrow(rest).map { case (from, to) => (done :+ from, to) } }
      }
    def readable(params: Seq[(TypeVariable, Kind)], from: Seq[TypeRec[Type]]): Boolean =
      params.forall { case (variable, _) => from.exists(t => freeTypeVars(t).contains(variable)) }
    (hook, types.map(quantifiers)) match {
      case (SyntaxHook.Interpolate, Seq((params, body))) =>
        arguments(body, 1).exists { case (from, to) => readable(params, from) && Equivalence.alpha(to, stringTypeT) }
      case (SyntaxHook.ListLiteral | SyntaxHook.NamedLiteral(_), Seq((nilParams, nil), (consParams, cons))) =>
        nilParams.map(_._2) == consParams.map(_._2) && {
          // cons の型パラメータを nil のものに揃える
          val renamed = Equivalence.normalize(substMany(consParams.map(_._1), nilParams.map { case (v, _) => typeVarT(v) }, cons))
          arguments(renamed, 2).exists {
            case (Seq(element, tail), result) => readable(nilParams, Seq(element)) && Equivalence.alpha(tail, nil) && Equivalence.alpha(result, nil)
            case _ => false
          }
        }
      case (SyntaxHook.Tuple(arity), Seq((params, body))) =>
        arguments(body, arity).exists { case (from, _) => readable(params, from) }
      case _ => false
    }
  }

  // top-level の data。フィールドからは visible（自身、rec なら組の全員）を短い名前で参照できる
  private def checkTopData(
      variable: TypeVariable,
      params: Seq[(TypeVariable, Kind)],
      constructors: Seq[DataConstructor[TC]],
      peers: Seq[DataHead],
      visible: Seq[TypeVariable]
  ): Checked = for {
    env <- ask
    name <- memberType(variable)
    scope = (e: Env) => visible.foldLeft(e)(_.exportType(_))
    members = constructors.map(c => DataConstructor[TC](Variable(env.qualify(c.name.name)), c.fields.map(_.local(scope)), c.fieldNames))
    checked <- checkData(name, params, members, peers, local = false)
    (typedConstructors, extend) = checked
    result <- extending(topDataT(name, params, typedConstructors)) { e =>
      val defined = constructors.foldLeft(extend(e).exportType(variable))((acc, c) => acc.exportValue(c.name))
      val accessors = fieldAccessors(name, typedConstructors)
      if (accessors.isEmpty) defined
      else {
        // アクセサは型の module（型と同じ名前の module）の公開メンバー。module は後から開いてメンバーを足せる
        val module = accessors.foldLeft(defined.modules.getOrElse(name.name, Names.empty)) { case (names, (field, accessor)) => names.withValue(field, accessor) }
        defineAccessors(name, typedConstructors)(defined).copy(
          modules = defined.modules + (name.name -> module),
          scope = defined.scope.withModule(variable.name, name.name),
          members = defined.members.withModule(variable.name, name.name)
        )
      }
    }
  } yield result

  // 名前付きフィールドのアクセサ: フィールドの名前 → 型の module の中の完全な名前
  private def fieldAccessors(owner: TypeVariable, constructors: Seq[DataConstructor[TypeRec]]): Seq[(String, Variable)] =
    constructors.flatMap(_.fieldNames.flatten).map(field => field -> fieldAccessorName(owner, field))

  // アクセサの型 ∀ā. T[ā] → フィールドの型 を登録する（フィールドの型は、登録済みのデータ型の定義から読む）
  private def defineAccessors(owner: TypeVariable, constructors: Seq[DataConstructor[TypeRec]])(env: Env): Env =
    env.dataTypes.get(owner).fold(env) { dataDef =>
      val receiver = applyTypeConstructor(owner, dataDef.paramVars.map(typeVarT))
      val typed = for {
        (written, definition) <- constructors.zip(dataDef.constructors)
        case (Some(field), fieldType) <- written.names.zip(definition.fields)
      } yield fieldAccessorName(owner, field) -> dataDef.params.foldRight(arrowT(receiver, fieldType)) { case ((v, k), body) => forallTypeT(v, k, body) }
      env.copy(values = env.values ++ typed)
    }

  // ---- impl ----

  private case class ImplHead(
      traitName: TypeVariable,
      traitDef: TraitDef,
      params: Seq[(TypeVariable, Kind)],
      targets: Seq[TypeRec[Type]],        // 展開済み
      typedContext: Seq[Constraint[TypeRec]],
      ref: DeclRef,
      key: (TypeVariable, Seq[Variable]),
      inst: InstanceDef
  )

  // impl の頭部（trait・ターゲット・文脈）を検査して、登録するインスタンスを決める
  private def implHead(
      writtenTrait: TypeVariable,
      implParams: Seq[(TypeVariable, Kind)],
      targets: Seq[TC[Type]],
      context: Seq[Constraint[TC]]
  ): Check[ImplHead] = {
    val implScope = (e: Env) => e.withTypeVars(implParams)
    for {
      env <- ask
      traitName <- resolveTrait(writtenTrait)
      traitDef <- lift(env.traits.get(traitName).toRight(CompileError.UndefinedTrait(traitName, None)))
      _ <- guard(
        targets.length == traitDef.param.length,
        CompileError.ImplTargetArity(traitName, traitDef.param.length, targets.length)
      )
      _ <- checkParams(DeclRef.ImplDecl(traitName, Seq.empty), implParams)
      typedTargets <- targets.traverse(_.local(implScope))
      expandedKinded <- typedTargets.traverse(t => expandWellKinded(t).local(implScope))
      expandedTargets = expandedKinded.map(_._1)
      _ <- expandedKinded.zip(traitDef.param).traverse_ { case ((expanded, targetKind), (_, paramKind)) =>
        guard(
          targetKind == paramKind,
          CompileError.ImplKindMismatch(traitName, expanded, paramKind, targetKind)
        )
      }
      headNames <- expandedTargets.traverse { expanded =>
        lift(typeConstructorHead(expanded).map(_._1).toRight(
          CompileError.InstanceHeadInvalid(traitName, expanded)
        ))
      }
      implRef = DeclRef.ImplDecl(traitName, headNames)
      key = instanceKey(traitName, headNames)
      _ <- guard(
        headNames.forall(h => !implParams.map(_._1).contains(TypeVariable(h))),
        CompileError.InstanceHeadTypeParameter(implRef)
      )
      _ <- guard(
        implParams.forall { case (p, _) => expandedTargets.exists(t => freeTypeVars(t).contains(p)) },
        CompileError.ImplParamUnused(implRef)
      )
      _ <- guard(!env.instances.contains(key), CompileError.OverlappingInstance(traitName, headNames))
      typedContext <- context.traverse(typedConstraint).local(implScope)
      expandedContext <- typedContext.traverse(expandConstraint(_, Some(implRef))).local(implScope)
      inst = InstanceDef(traitName, expandedTargets, expandedContext, instanceDictionaryName(traitName, headNames), implParams)
    } yield ImplHead(traitName, traitDef, implParams, expandedTargets, typedContext, implRef, key, inst)
  }

  // メソッド本体の検査。group は rec で一緒に宣言されたインスタンス（自身を含む）で、メソッド本体から見える
  // supers はスーパークラスの解決に使える組のインスタンス。スーパークラスの辞書は、この辞書を作るときに読まれるので、
  // 文脈のない（すぐに作られる）インスタンスでは、組の中で先に宣言されたものに限る
  private def implBody(
      head: ImplHead,
      writtenMethods: Seq[MethodImpl[TC]],
      group: Map[(TypeVariable, Seq[Variable]), InstanceDef],
      supers: Map[(TypeVariable, Seq[Variable]), InstanceDef] = Map.empty
  ): Check[TypeRec[Decl]] = {
    val traitDef = head.traitDef
    val implRef = head.ref
    val implScope = (e: Env) => e.withTypeVars(head.params)
    // スーパークラスの解決とメソッド本体からは、impl の文脈がスコープ中の制約として見える
    val contextScope = (e: Env) => implScope(e).withGivens(head.inst.context)
    // rec でない impl の中で自身のインスタンスが要ると、「rec impl と書く」と案内する
    val bodyScope = (e: Env) => contextScope(e).copy(
      instances = e.instances ++ group,
      defining = if (group.contains(head.key)) e.defining else e.defining + head.inst.dictName.name
    )
    val traitMethodNames = traitDef.methods.map(_._1)
    // impl のメソッドは短い名前で書き、trait のメソッド（module の中なら M.m）に対応づける
    val methods = writtenMethods.map { m =>
      MethodImpl[TC](traitMethodNames.find(t => shortName(t.name) == m.name.name).getOrElse(m.name), m.sig, m.body)
    }
    val implMethodNames = methods.map(_.name)
    // メソッドの宣言シグネチャでは trait のパラメータ名も使える（ターゲットに読み替える）。
    // ただし impl の型パラメータと同名のものは impl 側を指す
    val sigParams = traitDef.param.zip(head.targets).filterNot { case ((p, _), _) => head.params.exists(_._1 == p) }
    val sigScope = (e: Env) => implScope(e).withTypeVars(sigParams.map(_._1))
    for {
      _ <- guard(implMethodNames.distinct.length == implMethodNames.length, CompileError.DuplicateMembers(implRef, MemberKind.MethodMember))
      // デフォルトを持つメソッドは省略できる（TraitEncoder がデフォルトで埋める）
      missing = traitDef.required.map(_._1).filterNot(implMethodNames.contains)
      _ <- guard(missing.isEmpty, CompileError.MissingMethods(implRef, missing))
      extra = implMethodNames.filterNot(traitMethodNames.contains)
      _ <- guard(extra.isEmpty, CompileError.ExtraMethods(implRef, extra))
      typedMethods <- checkEach(methods) { m =>
        val expected = Equivalence.normalize(substMany(traitDef.paramVars, head.targets, traitDef.methods.find(_._1 == m.name).get._2))
        for {
          typedBody <- m.body.local(bodyScope)
          _ <- if (Equivalence.alpha(expected, typeOf(typedBody))) Check.pure(()) else {
            val (wanted, culprit, _) = blame(expected, typedBody, None)
            failAt[Unit](culprit, CompileError.MethodBodyTypeMismatch(implRef, m.name, expected, typeOf(typedBody), Some((wanted, typeOf(culprit)))))
          }
          // 文脈のない impl の辞書は関数ではないので、組のインスタンスを使うメソッドは関数でなければ初期化前の辞書を読む
          _ <- guard(
            head.inst.context.nonEmpty || isFunctionValue(typedBody) || !instancesUsed(typedBody).exists(group.contains),
            CompileError.RecursiveInstanceMethodNotFunction(implRef, m.name)
          )
          typedSig <- m.sig.traverse(_.local(sigScope))
          _ <- typedSig.traverse_ { s =>
            expandType(s).local(sigScope).flatMap { expandedSig =>
              val declared = Equivalence.normalize(substMany(sigParams.map(_._1._1), sigParams.map(_._2), expandedSig))
              guardAt(
                s,
                Equivalence.alpha(expected, declared),
                CompileError.MethodSigTypeMismatch(implRef, m.name, declared, expected)
              )
            }
          }
        } yield MethodImpl[TypeRec](m.name, typedSig, typedBody)
      }
      superEvidence <- withHints(Resolver.superConstraints(traitDef, head.targets).traverse { constraint =>
        // どの trait のスーパークラスとして必要になったかを残す
        ReaderT((e: Env) => Resolver.resolve(constraint).run(e).left.map(
          CompileError.RequiredBy(_, showTypeConstraint(head.inst.targets.foldLeft(TypeConstraint(head.traitName, Seq.empty))((c, t) => c.copy(arg = c.arg :+ t))),
            s"trait ${head.traitName.name}${traitDef.supers.map(showTypeConstraint).mkString(" with ", ", ", "")}")))
      }).local((e: Env) => contextScope(e).copy(
        instances = e.instances ++ supers,
        laterInstances = (group.keySet -- supers.keySet - head.key).map(key => group(key).dictName.name)
      ))
      // 後段がインスタンスを引けるよう、ターゲットは展開済みの型で持つ
    } yield topImplT(head.traitName, head.params, head.targets, head.typedContext, typedMethods, superEvidence)
  }

  // ---- rec ----
  // rec は同じ種類の宣言（let / data / impl）を包み、その全員を互いに、また自身から見えるようにする

  private def recLets(lets: Seq[(Variable, TC[Type], TC[Expr])]): Checked = for {
    env <- ask
    shorts = lets.map(_._1)
    // 値からは、組の全員を短い名前で参照できる
    visible = (e: Env) => shorts.foldLeft(e)(_.exportValue(_))
    checked <- checkRecBindings(lets.map { case (variable, types, value) => (Variable(env.qualify(variable.name)), types, value.local(visible)) })
    result <- extending(topRecT(checked.map(b => topLetT(b.variable, b.types, b.value)))) { e =>
      visible(e.withValues(checked.map(b => b.variable -> b.declaredType)))
    }
  } yield result

  private def recData(datas: Seq[(TypeVariable, Seq[(TypeVariable, Kind)], Seq[DataConstructor[TC]])]): Checked = for {
    env <- ask
    shorts = datas.map(_._1)
    peers = datas.map { case (variable, params, _) => (TypeVariable(env.qualify(variable.name)), params) }
    // 先に宣言したメンバーの定義（とコンストラクタ）を見せながら順に検査する。名前の重複はここで見つかる
    checked <- datas.foldLeftM((Seq.empty[TypeRec[Decl]], env)) { case ((done, current), (variable, params, constructors)) =>
      checkTopData(variable, params, constructors, peers, shorts).local((_: Env) => current).map { case (typed, next) => (done :+ typed, next) }
    }
    (typedDecls, extended) = checked
  } yield (topRecT(typedDecls), extended)

  // rec の組のインスタンス宣言（impl と derive）
  private enum RecInstance {
    case Impl(traitName: TypeVariable, params: Seq[(TypeVariable, Kind)], targets: Seq[TC[Type]], context: Seq[Constraint[TC]], methods: Seq[MethodImpl[TC]])
    case Derive(traitName: TypeVariable, target: TypeVariable)
  }

  private type InstanceKey = (TypeVariable, Seq[Variable])

  private def recInstances(members: Seq[RecInstance]): Checked = for {
    env <- ask
    // 先に全員のインスタンスを決める（重複はここで見つかる）。メソッド本体の検査はその後
    heads <- members.foldLeftM(Seq.empty[(InstanceKey, InstanceDef, Option[ImplHead])]) { (done, member) =>
      val visible = (e: Env) => e.copy(instances = e.instances ++ done.map { case (key, inst, _) => key -> inst })
      (member match {
        case RecInstance.Impl(traitName, params, targets, context, _) =>
          implHead(traitName, params, targets, context).map(head => (head.key, head.inst, Option(head)))
        case RecInstance.Derive(writtenTrait, writtenTarget) => for {
          current <- ask
          traitName <- resolveTrait(writtenTrait)
          target <- lift(current.resolveType(writtenTarget).toRight(CompileError.UndefinedTypeVariable(writtenTarget)))
          key = instanceKey(traitName, Seq(target.name))
          _ <- guard(!current.instances.contains(key), CompileError.OverlappingInstance(traitName, Seq(target.name)))
          inst <- lift(Deriver.instanceOf(traitName, target, current))
        } yield (key, inst, Option.empty[ImplHead])
      }).local(visible).map(done :+ _)
    }
    group = heads.map { case (key, inst, _) => key -> inst }.toMap
    typed <- checkEach(heads.zip(members).zipWithIndex) {
      case (((_, _, Some(head)), RecInstance.Impl(_, _, _, _, methods)), index) =>
        val earlier = heads.take(index).map { case (key, inst, _) => key -> inst }.toMap
        implBody(head, methods, group, supers = if (head.inst.context.nonEmpty) group else earlier)
      // derive の検査は、組の他のインスタンスが見える環境で行う
      // 導出したインスタンスは文脈を持たないので、スーパークラスには先に宣言したものだけを使える
      case (((key, inst, _), _), index) =>
        val earlier = heads.take(index).map { case (k, i, _) => k -> i }.toMap
        val later = (group.keySet -- earlier.keySet - key).map(k => group(k).dictName.name)
        withHints(lift(Deriver.check(
          inst.name, TypeVariable(key._2.head.name),
          env.copy(instances = env.instances ++ (group - key)),
          supers = Some(env.copy(instances = env.instances ++ earlier))
        ))).local((e: Env) => e.copy(laterInstances = later)).as(topDeriveT(inst.name, TypeVariable(key._2.head.name)))
    }
  } yield (topRecT(typed), env.copy(instances = env.instances ++ group))

  private def recRule(decls: Seq[AST[TC, Decl]]): Checked = {
    val lets = decls.collect { case AST.TopLet(variable, types, value) => (variable, types, value) }
    val datas = decls.collect { case AST.TopData(variable, params, constructors) => (variable, params, constructors) }
    val instances = decls.collect {
      case AST.TopImpl(traitName, params, targets, context, methods, _) => RecInstance.Impl(traitName, params, targets, context, methods)
      case AST.TopDerive(traitName, target) => RecInstance.Derive(traitName, target)
    }
    if (lets.length == decls.length) recLets(lets)
    else if (datas.length == decls.length) recData(datas)
    else if (instances.length == decls.length) recInstances(instances)
    else fail(CompileError.MixedRecursiveGroup)
  }

  // ---- module ----

  /** module を閉じるとき、中の宣言が Env に足した定義を「外から見た形」に読み直す。
   * opaque type は module の中では通常の別名だが、ここで中身のわからない型（Env.abstractTypes）に変わり、
   * それより後の宣言の型は別名を展開しない形で登録し直される。インスタンスは module の中で登録した形のまま残る
   */
  private def sealDecls(decls: Seq[TypeRec[Decl]]): Check[Env] =
    decls.foldLeft(ask)((sealedSoFar, decl) => sealedSoFar.flatMap(env => sealDecl(decl).local((_: Env) => env)))

  private def sealDecl(decl: TypeRec[Decl]): Check[Env] = decl.project match {
    case AST.TopLet(variable, types, _) => (expandChecked(types), ask).mapN((t, env) => env.withValue(variable, t))
    case AST.TopType(variable, params, alias) => (aliasOf(params, alias), ask).mapN((definition, env) => env.withAlias(variable, definition))
    case AST.TopData(variable, params, constructors) =>
      (dataOf(variable, params, constructors, peers = Seq.empty), ask).mapN((dataDef, env) => defineAccessors(variable, constructors)(defineData(variable, dataDef)(env)))
    case AST.TopRec(decls) =>
      val peers = decls.map(_.project).collect { case AST.TopData(variable, params, _) => (variable, params) }
      decls.foldLeft(ask) { (sealedSoFar, member) =>
        sealedSoFar.flatMap { env =>
          (member.project match {
            case AST.TopData(variable, params, constructors) =>
              (dataOf(variable, params, constructors, peers), ask).mapN((dataDef, e) => defineAccessors(variable, constructors)(defineData(variable, dataDef)(e)))
            case _ => sealDecl(member)
          }).local((_: Env) => env)
        }
      }
    case AST.TopTrait(variable, params, supers, methods) =>
      (traitOf(variable, params, supers, methods), ask).mapN((traitDef, env) => defineTrait(variable, traitDef)(env))
    case AST.TopModule(_, decls) => sealDecls(decls)
    case AST.TopModified(Modifier.Opaque, inner) => inner.project match {
      case AST.TopType(variable, _, _) => ask.map { env =>
        env.typeAliases.get(variable).fold(env) { representation =>
          env.copy(typeAliases = env.typeAliases - variable, abstractTypes = env.abstractTypes + (variable -> representation))
        }
      }
      case _ => sealDecl(inner)
    }
    case AST.TopModified(Modifier.Private, inner) => sealDecl(inner)
    case AST.TopImpl(_, _, _, _, _, _) | AST.TopDerive(_, _) | AST.TopOpen(_) | AST.TopOperator(_, _, _) | AST.TopSyntax(_, _) | AST.TopImport(_) => ask
  }

  private def checkModule(name: String, decls: Seq[IndexedRec[Decl]]): StateT[EitherS, Env, TypeRec[Decl]] = StateT { outer =>
    val full = outer.qualify(name)
    // 同じ module はもう一度開ける（メンバーを足す）。前に宣言したメンバーは、private なものも含めて短い名前で見える
    val declared = outer.modules.getOrElse(full, Names.empty)
    val hidden = outer.modulePrivates.getOrElse(full, Names.empty)
    for {
      checked <- checkAll(decls, outer.copy(path = outer.path :+ name, scope = outer.scope ++ declared ++ hidden, members = declared, privates = hidden))
      (inner, typedDecls) = checked
      sealedEnv <- sealDecls(typedDecls).run(inner)
    } yield (
      sealedEnv.copy(
        path = outer.path,
        scope = outer.scope.withModule(name, full),
        members = outer.members.withModule(name, full),
        privates = outer.privates,
        modules = sealedEnv.modules + (full -> inner.members),
        modulePrivates = sealedEnv.modulePrivates + (full -> inner.privates)
      ),
      topModuleT(name, typedDecls)
    )
  }

  private def checkOpen(path: Seq[String]): StateT[EitherS, Env, TypeRec[Decl]] = StateT { env =>
    env.resolveModule(path).toRight(CompileError.UndefinedModule(path)).map(opened => (env.copy(scope = env.scope ++ opened), topOpenT(path)))
  }

  private def checkModified(modifier: Modifier, inner: IndexedRec[Decl]): StateT[EitherS, Env, TypeRec[Decl]] = for {
    before <- StateT.get[EitherS, Env]
    _ <- StateT.liftF[EitherS, Env, Unit](Either.cond(before.path.nonEmpty, (), CompileError.ModifierOutsideModule(modifier)))
    typed <- checkDecl(inner)
    _ <- StateT.liftF[EitherS, Env, Unit]((modifier, typed.project) match {
      case (Modifier.Opaque, AST.TopData(variable, _, constructors)) if constructors.exists(_.named) =>
        Left(CompileError.FieldNamesNotAllowed(variable, FieldNamesReason.OpaqueData))
      case (Modifier.Opaque, AST.TopRec(decls)) =>
        decls.map(_.project).collectFirst {
          case AST.TopData(variable, _, constructors) if constructors.exists(_.named) => CompileError.FieldNamesNotAllowed(variable, FieldNamesReason.OpaqueData)
        }.toLeft(())
      case _ => Right(())
    })
    _ <- StateT.modify[EitherS, Env] { after =>
      (modifier, typed.project) match {
        case (Modifier.Private, _) => after.copy(members = before.members, privates = after.privates ++ after.members.since(before.members))
        // opaque data は型の名前だけを公開する
        case (Modifier.Opaque, AST.TopData(_, _, _) | AST.TopRec(_)) =>
          val hidden = dataConstructors(typed).toSet
          after.copy(members = after.members.copy(values = after.members.values.filterNot { case (_, full) => hidden(full) }))
        // opaque type は module を閉じるときに封印する（sealDecl）
        case (Modifier.Opaque, _) => after
      }
    }
  } yield topModifiedT(modifier, typed)

  private def dataConstructors(decl: TypeRec[Decl]): Seq[Variable] = decl.project match {
    case AST.TopData(_, _, constructors) => constructors.map(_.name)
    case AST.TopRec(decls) => decls.flatMap(dataConstructors)
    case _ => Seq.empty
  }

  private def checkDecl(decl: IndexedRec[Decl]): StateT[EitherS, Env, TypeRec[Decl]] = {
    def run(rule: Checked): StateT[EitherS, Env, TypeRec[Decl]] =
      StateT(env => rule.run(env).map { case (typedDecl, nextEnv) => (nextEnv, typedDecl) }.left.map(enrich(env, _)))
    def children(node: AST[IndexedRec, Decl]): AST[TC, Decl] = node.hmap([x] => (child: IndexedRec[x]) => check(child))
    val checked = decl.project match {
      case AST.TopRec(decls) => run(recRule(decls.map(member => children(member.project))))
      case AST.TopModule(name, decls) => checkModule(name, decls)
      case AST.TopOpen(path) => checkOpen(path)
      case AST.TopModified(modifier, inner) => checkModified(modifier, inner)
      case node => run(declRule(children(node)))
    }
    StateT(env => checked.run(env).left.map(_.at(decl.extract)))
  }

  // ---- 複数のエラー ----
  // 宣言の検査に失敗しても、後続の宣言の検査を続ける。失敗した宣言の名前を使ったことによるエラーは報告しない

  private def namesOf(decl: IndexedRec[Decl]): (Set[String], Set[String]) = decl.project match {
    case AST.TopModule(name, _) => (Set(name), Set.empty)
    case AST.TopImpl(traitName, _, _, _, _, _) => (Set.empty, Set(traitName.name))
    case AST.TopDerive(traitName, _) => (Set.empty, Set(traitName.name))
    case AST.TopRec(members) =>
      val found = members.map(namesOf)
      (found.flatMap(_._1).toSet, found.flatMap(_._2).toSet)
    case AST.TopModified(_, inner) => namesOf(inner)
    case _ => (declaredNames(Seq(decl)), Set.empty)
  }

  // エラーが、すでに失敗した宣言のせいで起きたものか
  private def causedByFailure(env: Env, error: CompileError): Boolean = {
    // 修飾された名前は、先頭の module が失敗していれば当たる
    def failed(name: String): Boolean = env.failed(name) || env.failed(name.takeWhile(_ != '.'))
    error.core match {
      case CompileError.UndefinedVariable(name) => failed(name.name)
      case CompileError.UndefinedTypeVariable(name) => failed(name.name)
      case CompileError.UndefinedTrait(name, _) => failed(name.name)
      case CompileError.UndefinedModule(path) => path.headOption.exists(failed)
      case CompileError.NotAConstructor(name) => failed(name.name)
      case CompileError.NoInstance(name, _) => env.failedTraits(name.name) || env.failedTraits(shortName(name.name))
      case CompileError.ContextMethodMissing(name) => failed(name.name)
      case CompileError.InContext(inner) => causedByFailure(env, inner)
      case CompileError.Multiple(errors) => errors.forall(causedByFailure(env, _))
      case _ => false
    }
  }

  private def checkAll(decls: Seq[IndexedRec[Decl]], initial: Env): EitherS[(Env, Seq[TypeRec[Decl]])] = {
    val (env, typed, errors) = decls.foldLeft((initial, Vector.empty[TypeRec[Decl]], Vector.empty[CompileError])) {
      case ((current, done, found), decl) =>
        checkDecl(decl).run(current) match {
          case Right((next, typedDecl)) => (next, done :+ typedDecl, found)
          case Left(error) =>
            val (names, traits) = namesOf(decl)
            val poisoned = current.copy(failed = current.failed ++ names, failedTraits = current.failedTraits ++ traits)
            (poisoned, done, if (causedByFailure(current, error)) found else found ++ error.all)
        }
    }
    errors match {
      case Vector() if env.failed == initial.failed && env.failedTraits == initial.failedTraits => Right((env, typed))
      // 報告するエラーが残らなくても、どこかで失敗している（原因は外側ですでに報告されている）
      case Vector() => Left(CompileError.Multiple(Seq.empty))
      case Vector(only) => Left(only)
      case many => Left(CompileError.Multiple(many))
    }
  }

  private def checkMain(env: Env): EitherS[Unit] = env.values.get(Variable("main")) match {
    case Some(t) if Equivalence.alpha(t, arrowT(unitTypeT, intTypeT)) => Right(())
    case Some(t) => Left(CompileError.MainTypeInvalid(t))
    case None => Left(CompileError.MainMissing)
  }

  // プログラムのどこかで宣言される名前（「この後で宣言されている」という案内に使う）
  private def declaredNames(decls: Seq[IndexedRec[Decl]]): Set[String] = decls.flatMap { decl =>
    decl.project match {
      case AST.TopLet(variable, _, _) => Seq(variable.name)
      case AST.TopType(variable, _, _) => Seq(variable.name)
      case AST.TopData(variable, _, constructors) => variable.name +: constructors.map(_.name.name)
      case AST.TopTrait(variable, _, _, methods) => variable.name +: methods.map(_.name.name)
      case AST.TopRec(members) => declaredNames(members).toSeq
      case AST.TopModified(_, inner) => declaredNames(Seq(inner)).toSeq
      // module の中の名前は、外からは修飾して参照する（「後で宣言されている」には当たらない）
      case _ => Seq.empty
    }
  }.toSet

  def validate(prog: IndexedRec[AST.Program.type]): Either[CompileError, TypeRec[AST.Program.type]] = prog.project match {
    case AST.Program(decls) => checkAll(decls, Env.empty.copy(spans = new SpanTable, declared = declaredNames(decls))).flatMap { case (env, typedDecls) =>
      // main についてのエラーは、main の宣言の位置で報告する
      val mainSpan = decls.collectFirst { case decl if (decl.project match {
        case AST.TopLet(Variable("main"), _, _) => true
        case _ => false
      }) => decl.extract }
      checkMain(env).left.map(error => mainSpan.fold(error)(error.at)).as(programT(typedDecls, env))
    }
  }

}
