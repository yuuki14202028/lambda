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
      case CompileError.UndefinedModule(path) => similar(path.mkString("."), env.modules.keys ++ env.scope.modules.keys)
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

  private def expectNumeric(actual: TypeRec[Type]): Check[Unit] =
    guard(isNumericType(actual), CompileError.ExpectedNumeric(actual))

  private def expectEquatable(actual: TypeRec[Type]): Check[Unit] =
    guard(isEquatableType(actual), CompileError.ExpectedEquatable(actual))

  private def isShortCircuit(op: BinOps): Boolean =
    op == BinOps.ShortAnd || op == BinOps.ShortOr

  private def thunkParam(body: TypeRec[Expr]): Variable = {
    val used = freeVars(body).map(_.name)
    LazyList.from(0)
      .map(i => Variable(s"__short_arg_$i"))
      .find(v => !used.contains(v.name))
      .get
  }

  private def resolveBinaryOperator(op: BinOps, left: TypeRec[Expr], right: TypeRec[Expr]): Check[TypeRec[Expr]] = for {
    env <- ask
    typeName <- lift(operatorTypeName(typeOf(left)).toRight(CompileError.OperatorUnresolvable(op, typeOf(left))))
    operator = StandardLibrary.binaryOperatorName(op, typeName)
    found <- lift(env.lookupValue(operator).toRight(CompileError.OperatorUndefined(op, typeOf(left), operator)))
    (fn, fnType) = found
    rightArg = if (isShortCircuit(op)) {
      val thunkType = arrowT(unitTypeT, typeOf(right))
      absT(thunkParam(right), thunkType, unitTypeT, right)
    } else right
    expectedRightType = if (isShortCircuit(op)) arrowT(unitTypeT, typeOf(right)) else typeOf(right)
    result <- destructArrow(fnType) match {
      case Some((leftParam, afterLeft)) if Equivalence.alpha(leftParam, typeOf(left)) =>
        destructArrow(afterLeft) match {
          case Some((rightParam, resultType)) if Equivalence.alpha(rightParam, expectedRightType) =>
            val fnRef = varrType(fn, fnType)
            val appliedLeft = appT(afterLeft, fnRef, left)
            okT(appT(resultType, appliedLeft, rightArg))
          case Some((rightParam, _)) =>
            failAt(right, CompileError.TypeMismatch(rightParam, expectedRightType), left -> Related.LeftOperand(typeOf(left)))
          case None =>
            fail(CompileError.OperatorNotBinary(fn, fnType))
        }
      case Some((leftParam, _)) =>
        failAt(left, CompileError.TypeMismatch(leftParam, typeOf(left)))
      case None =>
        fail(CompileError.OperatorNotAFunction(fn, fnType))
    }
  } yield result

  private def resolveUnaryOperator(op: UnaryOps, body: TypeRec[Expr]): Check[TypeRec[Expr]] = for {
    env <- ask
    typeName <- lift(operatorTypeName(typeOf(body)).toRight(CompileError.OperatorUnresolvable(op, typeOf(body))))
    operator = StandardLibrary.unaryOperatorName(op, typeName)
    found <- lift(env.lookupValue(operator).toRight(CompileError.OperatorUndefined(op, typeOf(body), operator)))
    (fn, fnType) = found
    result <- destructArrow(fnType) match {
      case Some((paramType, resultType)) if Equivalence.alpha(paramType, typeOf(body)) =>
        okT(appT(resultType, varrType(fn, fnType), body))
      case Some((paramType, _)) =>
        fail(CompileError.TypeMismatch(paramType, typeOf(body)))
      case None =>
        fail(CompileError.OperatorNotAFunction(fn, fnType))
    }
  } yield result

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
  private val showMethod = Variable("show")

  private def interpConcatType: TypeRec[Type] = arrowT(stringTypeT, arrowT(stringTypeT, stringTypeT))

  // String 型でない埋め込み式 e: τ を show[τ](e) に包む
  private def stringifyPart(part: TypeRec[Expr]): Check[TypeRec[Expr]] = {
    val partType = typeOf(part)
    if (Equivalence.alpha(stringTypeT, partType)) okT(part)
    else for {
      env <- ask
      found <- lift(env.lookupValue(showMethod).toRight(
        CompileError.StrInterpShowMissing(partType)))
      (show, surface) = found
      destructed <- lift(destructForAllK(surface).toRight(
        CompileError.StrInterpShowNotPolymorphic(surface)))
      (variable, kind, body) = destructed
      _ <- guard(kind == Kind.Star, CompileError.StrInterpShowKindInvalid(kind))
      applied <- discharge(tyAppT(Equivalence.normalize(substType(variable, partType, body)), varrType(show, surface), partType))
      arrowParts <- lift(destructArrow(typeOf(applied)).toRight(
        CompileError.StrInterpShowNotFunction(typeOf(applied))))
      (from, to) = arrowParts
      _ <- guard(Equivalence.alpha(from, partType), CompileError.StrInterpShowParamMismatch(from, partType))
      _ <- guard(Equivalence.alpha(stringTypeT, to), CompileError.StrInterpShowResultMismatch(stringTypeT, to))
    } yield appT(to, applied, part)
  }

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
      checked <- bindings.zip(annotated).traverse { case ((_, _, value), (variable, typedTypes, declaredType)) =>
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

  private def aliasOf(params: Seq[(TypeVariable, Kind)], alias: TypeRec[Type]): Check[TypeAlias] =
    expandChecked(alias).local((e: Env) => e.withTypeVars(params)).map(TypeAlias(params, _))

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
      c.fields.traverse(field => expandChecked(field).local(fieldScope(variable, params, peers))).map(ConstructorDef(c.name, variable, _, tag))
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
      peers: Seq[DataHead]
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
      typed <- constructors.traverse(c => c.fields.traverse(_.local(fieldScope(variable, params, peers))).map(DataConstructor[TypeRec](c.name, _)))
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
    case Pattern.Wildcard() => Check.pure((Pattern.Wildcard[TypeRec](), Seq.empty))
    case Pattern.Bind(name) => Check.pure((Pattern.Bind[TypeRec](name), Seq(name -> expected)))

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
      // 文字列はランタイムの C 関数 string_equal で比べる（文字列埋め込みの concat と同じ扱い）
      val stringEquals = arrowT(stringTypeT, arrowT(stringTypeT, boolTypeT))
      for {
        env <- ask
        _ <- guard(typeName == BuiltinTypes.CString || BuiltinTypes.equatableTypes.contains(typeName), CompileError.NumericLiteralUnknown(typeName))
        _ <- expect(expected, literalType)
        // bool は if で分岐するので == は要らない
        equals <- if (typeName == "bool") Check.pure(Option.empty[TypeRec[Expr]])
        else if (typeName == BuiltinTypes.CString) Check.pure(Option(foreignT(Variable("string_equal"), stringEquals, stringEquals)))
        else {
          val operator = StandardLibrary.binaryOperatorName(BinOps.Eq, typeName)
          for {
            found <- lift(env.lookupValue(operator).toRight(CompileError.OperatorUndefined(BinOps.Eq, literalType, operator)))
            (fn, fnType) = found
            _ <- guard(
              Equivalence.alpha(fnType, arrowT(literalType, arrowT(literalType, boolTypeT))),
              CompileError.OperatorNotBinary(fn, fnType)
            )
          } yield Option(varrType(fn, fnType))
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
      checked <- checkData(variable, params, constructors, peers = Seq.empty)
      (typedConstructors, extend) = checked
      typedBody <- body.local(extend)
    } yield dataLetT(variable, params, typeOf(typedBody), typedConstructors, typedBody)

    case AST.DataLetRec(datas, body) =>
      val peers = datas.map(d => (d.variable, d.params))
      for {
        // 先に宣言したメンバーの定義を見せながら順に検査する。名前の重複はここで見つかる
        checked <- datas.foldLeft(Check.pure((Seq.empty[DataBinding[TypeRec]], identity[Env]))) { (acc, d) =>
          acc.flatMap { case (done, extend) =>
            checkData(d.variable, d.params, d.constructors, peers).local(extend).map { case (typedConstructors, define) =>
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
    case AST.StrInterp(parts) => for {
      typedParts <- parts.toList.traverse(identity)
      stringified <- typedParts.traverse(stringifyPart)
    } yield stringified match {
      case Nil => stringLitT("", stringTypeT)
      case head :: tail =>
        tail.foldLeft(head) { (acc, part) =>
          val concatRef = foreignT(Variable("concat"), interpConcatType, interpConcatType)
          appT(stringTypeT, appT(arrowT(stringTypeT, stringTypeT), concatRef, acc), part)
        }
    }
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

    case AST.BinOp(op, left, right) => for {
      typedLeft <- left
      typedRight <- right
      resolved <- resolveBinaryOperator(op, typedLeft, typedRight)
    } yield resolved

    case AST.Intrinsic(op, args) => for {
      typedArgs <- args.traverse(identity)
      checked <- checkIntrinsic(op, typedArgs)
    } yield checked

    case AST.UnaryOp(op, body) => for {
      typedBody <- body
      resolved <- resolveUnaryOperator(op, typedBody)
    } yield resolved

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
        typedMethods <- methods.zip(signature.methods).traverse { case (m, (method, sig)) =>
          m.body.traverse { body =>
            body.local(defaultScope).flatMap { typedBody =>
              (if (Equivalence.alpha(sig, typeOf(typedBody))) Check.pure(()) else {
                val (wanted, culprit, _) = blame(sig, typedBody, None)
                failAt[Unit](culprit, CompileError.DefaultMethodTypeMismatch(name, method, wanted, typeOf(culprit)))
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
    members = constructors.map(c => DataConstructor[TC](Variable(env.qualify(c.name.name)), c.fields.map(_.local(scope))))
    checked <- checkData(name, params, members, peers)
    (typedConstructors, extend) = checked
    result <- extending(topDataT(name, params, typedConstructors)) { e =>
      constructors.foldLeft(extend(e).exportType(variable))((acc, c) => acc.exportValue(c.name))
    }
  } yield result

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
  private def implBody(head: ImplHead, writtenMethods: Seq[MethodImpl[TC]], group: Map[(TypeVariable, Seq[Variable]), InstanceDef]): Check[TypeRec[Decl]] = {
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
      typedMethods <- methods.traverse { m =>
        val expected = Equivalence.normalize(substMany(traitDef.paramVars, head.targets, traitDef.methods.find(_._1 == m.name).get._2))
        for {
          typedBody <- m.body.local(bodyScope)
          _ <- if (Equivalence.alpha(expected, typeOf(typedBody))) Check.pure(()) else {
            val (wanted, culprit, _) = blame(expected, typedBody, None)
            failAt[Unit](culprit, CompileError.MethodBodyTypeMismatch(implRef, m.name, wanted, typeOf(culprit)))
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
              guard(
                Equivalence.alpha(expected, declared),
                CompileError.MethodSigTypeMismatch(implRef, m.name, declared, expected)
              )
            }
          }
        } yield MethodImpl[TypeRec](m.name, typedSig, typedBody)
      }
      superEvidence <- Resolver.superConstraints(traitDef, head.targets).traverse(Resolver.resolve).local(contextScope)
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
    typed <- heads.zip(members).traverse {
      case ((_, _, Some(head)), RecInstance.Impl(_, _, _, _, methods)) => implBody(head, methods, group)
      // derive の検査は、組の他のインスタンスが見える環境で行う
      case ((key, inst, _), _) =>
        lift(Deriver.check(inst.name, TypeVariable(key._2.head.name), env.copy(instances = env.instances ++ (group - key))))
          .as(topDeriveT(inst.name, TypeVariable(key._2.head.name)))
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
      (dataOf(variable, params, constructors, peers = Seq.empty), ask).mapN((dataDef, env) => defineData(variable, dataDef)(env))
    case AST.TopRec(decls) =>
      val peers = decls.map(_.project).collect { case AST.TopData(variable, params, _) => (variable, params) }
      decls.foldLeft(ask) { (sealedSoFar, member) =>
        sealedSoFar.flatMap { env =>
          (member.project match {
            case AST.TopData(variable, params, constructors) =>
              (dataOf(variable, params, constructors, peers), ask).mapN((dataDef, e) => defineData(variable, dataDef)(e))
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
    case AST.TopImpl(_, _, _, _, _, _) | AST.TopDerive(_, _) | AST.TopOpen(_) | AST.TopImport(_) => ask
  }

  private def checkModule(name: String, decls: Seq[IndexedRec[Decl]]): StateT[EitherS, Env, TypeRec[Decl]] = StateT { outer =>
    val full = outer.qualify(name)
    for {
      _ <- Either.cond(!outer.modules.contains(full), (), CompileError.AlreadyDefined(NameKind.ModuleName, full))
      checked <- decls.traverse(checkDecl).run(outer.copy(path = outer.path :+ name, members = Names.empty))
      (inner, typedDecls) = checked
      sealedEnv <- sealDecls(typedDecls).run(inner)
    } yield (
      sealedEnv.copy(
        path = outer.path,
        scope = outer.scope.withModule(name, full),
        members = outer.members.withModule(name, full),
        modules = sealedEnv.modules + (full -> inner.members)
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
    _ <- StateT.modify[EitherS, Env] { after =>
      (modifier, typed.project) match {
        case (Modifier.Private, _) => after.copy(members = before.members)
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
    case AST.Program(decls) => decls.traverse(checkDecl).run(Env.empty.copy(spans = new SpanTable, declared = declaredNames(decls))).flatMap { case (env, typedDecls) =>
      // main についてのエラーは、main の宣言の位置で報告する
      val mainSpan = decls.collectFirst { case decl if (decl.project match {
        case AST.TopLet(Variable("main"), _, _) => true
        case _ => false
      }) => decl.extract }
      checkMain(env).left.map(error => mainSpan.fold(error)(error.at)).as(programT(typedDecls, env))
    }
  }

}
