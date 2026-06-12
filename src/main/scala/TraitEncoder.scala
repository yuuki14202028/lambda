package com.yuuki14202028

import cats.syntax.all.*

import scala.annotation.tailrec

object TraitEncoder {

  private val maxResolutionDepth = 64

  private def invariant(msg: String): Nothing = sys.error(s"Compiler invariant violation: $msg")

  private enum SpineElem {
    case TyArg(arg: TypeRec[Type], resultType: TypeRec[Type])
    case ValArg(arg: TypeRec[Expr], resultType: TypeRec[Type])
  }

  def encode(program: TypeRec[AST.Program.type]): Either[CompileError, TypeRec[AST.Program.type]] = program.project match {
    case AST.Program(decls) =>
      val env = program.extract match {
        case ProgramAnn(e) => e
      }
      decls.toVector
        .flatTraverse(decl => encodeDecl(decl, env).map(_.toVector))
        .map(encodedDecls => programT(encodedDecls, env))
  }

  private def encodeDecl(decl: TypeRec[Decl], env: Env): EitherS[Seq[TypeRec[Decl]]] = decl.project match {
    case AST.TopTrait(name, params, _, _) => Right(encodeTrait(name, params, env))
    case AST.TopImpl(name, implParams, targets, _, methods) => encodeImpl(name, implParams, targets, methods, env).map(Seq(_))
    case AST.TopLetWhere(variable, params, _, _, value, recursive) =>
      encodeLetWhere(variable, params, value, recursive, env).map(Seq(_))
    case AST.TopLet(variable, types, value) =>
      rewriteExpr(value, env, Set.empty).map(rewritten => Seq(topLetT(variable, types, rewritten)))
    case AST.TopLetRec(variable, types, value) =>
      rewriteExpr(value, env, Set.empty).map(rewritten => Seq(topLetRecT(variable, types, rewritten)))
    case _ => Right(Seq(decl))
  }

  /** trait C[p̄] where [S₁]…[Sₛ] { def mᵢ: τᵢ }
   * data C[p̄] = | MkC(S₁)…(Sₛ)(τ₁)…(τₙ)
   * let $super_C_j : ∀p̄. C[p̄] → Sⱼ = Λp̄. λ$dict: C[p̄]. match $dict with | MkC(x̄) -> xⱼ
   * let mᵢ : ∀p̄. C[p̄] → τᵢ = Λp̄. λ$dict: C[p̄]. match $dict with | MkC(x̄) -> x₍ₛ₊ᵢ₎
   */
  private def encodeTrait(name: TypeVariable, params: Seq[(TypeVariable, Kind)], env: Env): Seq[TypeRec[Decl]] = {
      val traitDef = env.traits.getOrElse(name, invariant(s"trait ${name.name} is missing from ProgramAnn"))
      val ctorName = dictionaryConstructor(name)
      val superFields = traitDef.supers.map(tc => applyTypeConstructor(tc.name, tc.arg))
      val fields = superFields ++ traitDef.methods.map(_._2)
      val dictDecl = topDataT(name, params, Seq(DataConstructor[TypeRec](ctorName, fields)))
      val dictType = applyTypeConstructor(name, params.map { case (p, _) => typeVarT(p) })
      val dictVar = Variable("$dict")
      val binders = fields.indices.map(i => Variable(s"$$field_$i"))
      def projection(letName: Variable, resultType: TypeRec[Type], index: Int): TypeRec[Decl] = {
        val matchE = matchExprT(resultType, varrType(dictVar, dictType), Seq(MatchCase(ctorName, binders, varrType(binders(index), resultType))))
        val lam = absT(dictVar, arrowT(dictType, resultType), dictType, matchE)
        val value = params.foldRight(lam) { case ((p, k), acc) => tyAbsT(p, forallTypeT(p, k, typeOf(acc)), k, acc) }
        topLetT(letName, typeOf(value), value)
      }
      val superProjections = traitDef.supers.zipWithIndex.map { case (tc, index) =>
        projection(superDictionaryName(name, index), applyTypeConstructor(tc.name, tc.arg), index)
      }
      val methodProjections = traitDef.methods.zipWithIndex.map { case ((methodName, sig), index) =>
        projection(methodName, sig, superFields.length + index)
      }
      (dictDecl +: superProjections) ++ methodProjections
  }

  /** impl[ā] C[T̄] where [D₁]…[Dₖ] { def mᵢ = eᵢ }
   * let $inst_C_h̄ : ∀ā. D₁ → … → Dₖ → C[T̄] =
   *   Λā. λ$ctx_1: D₁. … MkC[T̄](resolve(S₁[p̄:=T̄]))…(e₁)…(eₙ)   (eᵢ は trait 宣言順)
   */
  private def encodeImpl(
      traitName: TypeVariable,
      implParams: Seq[(TypeVariable, Kind)],
      targets: Seq[TypeRec[Type]],
      methods: Seq[MethodImpl[TypeRec]],
      env: Env
  ): EitherS[TypeRec[Decl]] = {
    val traitDef = env.traits.getOrElse(traitName, invariant(s"trait ${traitName.name} is missing from ProgramAnn"))
    val scope = env.copy(typeVars = env.typeVars ++ implParams)
    for {
    expandedTargets <- targets.traverse(t => TAnalyser.expandType(t).run(scope))
    headNames = expandedTargets.map { expanded =>
      typeConstructorHead(expanded).map(_._1).getOrElse(invariant(s"impl ${traitName.name}: target is not a type constructor"))
    }
    implShown = s"${traitName.name}${headNames.map(h => s"[$h]").mkString}"
    inst = env.instances.getOrElse(
      instanceKey(traitName, headNames),
      invariant(s"instance $implShown is missing from ProgramAnn")
    )
    orderedBodies = traitDef.methods.map { case (methodName, _) =>
      methods.collectFirst { case m if m.name == methodName => m.body }
        .getOrElse(invariant(s"impl $implShown is missing method ${methodName.name}"))
    }
    ctxVars = inst.context.indices.map(i => Variable(s"$$ctx_$i"))
    scopeEnv = env.copy(dictsInScope = env.dictsInScope ++ inst.context.zip(ctxVars))
    paramVars = traitDef.param.map(_._1)
    superDicts <- traitDef.supers.traverse { sup =>
      val substituted = sup.arg.map(a => Equivalence.normalize(substMany(paramVars, inst.targets, a)))
      resolve(sup.name, substituted, scopeEnv, 0)
    }
    rewrittenBodies <- orderedBodies.traverse(body => rewriteExpr(body, scopeEnv, Set.empty))
  } yield {
    val superFieldTypes = traitDef.supers.map(tc => applyTypeConstructor(tc.name, tc.arg))
    val fields = superFieldTypes ++ traitDef.methods.map(_._2)
    val specialized = fields.map(field => Equivalence.normalize(substMany(paramVars, inst.targets, field)))
    val dictType = applyTypeConstructor(traitName, inst.targets)
    val ctorResult = applyTypeConstructor(traitName, paramVars.map(typeVarT))
    val ctorType = traitDef.param.foldRight(fields.foldRight(ctorResult)(arrowT)) { case ((p, k), acc) => forallTypeT(p, k, acc) }
    val ctorRef = varrType(dictionaryConstructor(traitName), ctorType)
    // 先頭 j 個の型引数を適用した後の注釈型: ∀(残りパラメータ). fields[p̄₁..ⱼ:=T̄₁..ⱼ] → C[T̄₁..ⱼ, p̄ⱼ₊₁..]
    def appliedAnn(j: Int): TypeRec[Type] = {
      val partial = (f: TypeRec[Type]) => Equivalence.normalize(substMany(paramVars.take(j), inst.targets.take(j), f))
      val dictJ = applyTypeConstructor(traitName, inst.targets.take(j) ++ paramVars.drop(j).map(typeVarT))
      traitDef.param.drop(j).foldRight(fields.map(partial).foldRight(dictJ)(arrowT)) { case ((p, k), acc) => forallTypeT(p, k, acc) }
    }
    val tyApplied = targets.zipWithIndex.foldLeft(ctorRef) { case (acc, (t, j)) => tyAppT(appliedAnn(j + 1), acc, t) }
    val applied = (superDicts ++ rewrittenBodies).zipWithIndex.foldLeft(tyApplied) { case (acc, (arg, index)) =>
      appT(specialized.drop(index + 1).foldRight(dictType)(arrowT), acc, arg)
    }
    val ctxTypes = inst.context.map(tc => applyTypeConstructor(tc.name, tc.arg))
    val withContext = ctxVars.zip(ctxTypes).foldRight(applied) { case ((v, t), acc) =>
      absT(v, arrowT(t, typeOf(acc)), t, acc)
    }
    val value = inst.params.foldRight(withContext) { case ((p, k), acc) => tyAbsT(p, forallTypeT(p, k, typeOf(acc)), k, acc) }
    topLetT(inst.dictName, instanceType(inst), value)
  }
  }

  /** let f[ā](x̄) where [C₁]…[Cₖ]: ret = e
   * let f : ∀ā. C₁ → … → Cₖ → x̄ → ret = Λā. λ$where_1: C₁. … e
   * 本体は dictsInScope に制約→辞書変数を入れて書き換える
   */
  private def encodeLetWhere(
      variable: Variable,
      params: Seq[(TypeVariable, Kind)],
      value: TypeRec[Expr],
      recursive: Boolean,
      env: Env
  ): EitherS[TypeRec[Decl]] = {
    val constraints = env.constrains.getOrElse(variable, invariant(s"constraints of ${variable.name} are missing from ProgramAnn"))
    val (binders, inner) = stripTyAbs(value, params.length, variable)
    val dictVars = constraints.indices.map(i => Variable(s"$$where_$i"))
    val scopeEnv = env.copy(dictsInScope = env.dictsInScope ++ constraints.zip(dictVars))
    rewriteExpr(inner, scopeEnv, Set.empty).map { rewrittenInner =>
      val withDicts = constraints.zip(dictVars).foldRight(rewrittenInner) { case ((tc, v), acc) =>
        val dictType = applyTypeConstructor(tc.name, tc.arg)
        absT(v, arrowT(dictType, typeOf(acc)), dictType, acc)
      }
      val newValue = binders.foldRight(withDicts) { case ((p, k), acc) => tyAbsT(p, forallTypeT(p, k, typeOf(acc)), k, acc) }
      if (recursive) topLetRecT(variable, typeOf(newValue), newValue)
      else topLetT(variable, typeOf(newValue), newValue)
    }
  }

  private def stripTyAbs(e: TypeRec[Expr], count: Int, owner: Variable): (Seq[(TypeVariable, Kind)], TypeRec[Expr]) = {
    @tailrec
    def loop(current: TypeRec[Expr], remaining: Int, acc: List[(TypeVariable, Kind)]): (Seq[(TypeVariable, Kind)], TypeRec[Expr]) =
      if (remaining == 0) (acc.reverse, current)
      else current.project match {
        case AST.TyAbs(v, k, body) => loop(body, remaining - 1, (v, k) :: acc)
        case _ => invariant(s"let ${owner.name}: expected $count leading type abstractions")
      }
    loop(e, count, Nil)
  }

  // ---- 辞書解決（§6: 局所辞書 → インスタンス → スーパークラス射影 → NoInstance） ----

  private def sameConstraint(tc: TypeConstraint, name: TypeVariable, args: Seq[TypeRec[Type]]): Boolean =
    tc.name == name && tc.arg.length == args.length && tc.arg.zip(args).forall { case (l, r) => Equivalence.alpha(l, r) }

  // インスタンスヘッドの一方向マッチ: pattern 中の patternVars を actual の部分型に束縛する
  private def matchTypes(
      pattern: TypeRec[Type],
      actual: TypeRec[Type],
      patternVars: Set[TypeVariable],
      acc: Map[TypeVariable, TypeRec[Type]]
  ): Option[Map[TypeVariable, TypeRec[Type]]] =
    (pattern.project, actual.project) match {
      case (AST.TypeVar(v), _) if patternVars.contains(v) =>
        acc.get(v) match {
          case Some(prev) => Option.when(Equivalence.alpha(prev, actual))(acc)
          case None => Some(acc + (v -> actual))
        }
      case (AST.TypeVar(l), AST.TypeVar(r)) => Option.when(l == r)(acc)
      case (AST.Primitive(l), AST.Primitive(r)) => Option.when(l == r)(acc)
      case (AST.TypeApp(lf, la), AST.TypeApp(rf, ra)) =>
        matchTypes(lf, rf, patternVars, acc).flatMap(matchTypes(la, ra, patternVars, _))
      case (AST.Arrow(lf, lt), AST.Arrow(rf, rt)) =>
        matchTypes(lf, rf, patternVars, acc).flatMap(matchTypes(lt, rt, patternVars, _))
      case _ => None
    }

  private def applyTypeArgs(
      expr: TypeRec[Expr],
      exprType: TypeRec[Type],
      args: Seq[(TypeRec[Type], TypeRec[Type])] // (構文上の引数, 注釈計算用の展開済み引数)
  ): (TypeRec[Expr], TypeRec[Type]) =
    args.foldLeft((expr, exprType)) { case ((e, t), (syntax, expanded)) =>
      val (v, _, body) = destructForAllK(t).getOrElse(invariant(s"expected a polymorphic type, got ${t.show}"))
      val next = Equivalence.normalize(substType(v, expanded, body))
      (tyAppT(next, e, syntax), next)
    }

  private def applyDictArgs(expr: TypeRec[Expr], exprType: TypeRec[Type], dicts: Seq[TypeRec[Expr]]): (TypeRec[Expr], TypeRec[Type]) =
    dicts.foldLeft((expr, exprType)) { case ((e, t), dict) =>
      val (_, after) = destructArrow(t).getOrElse(invariant(s"expected a dictionary-taking type, got ${t.show}"))
      (appT(after, e, dict), after)
    }

  private def resolve(traitName: TypeVariable, args: Seq[TypeRec[Type]], env: Env, depth: Int): EitherS[TypeRec[Expr]] = {
    if (depth > maxResolutionDepth) Left(CompileError.ResolutionDepthExceeded(traitName, args))
    else env.dictsInScope.collectFirst {
      case (tc, dictVar) if sameConstraint(tc, traitName, args) =>
        varrType(dictVar, applyTypeConstructor(traitName, args))
    } match {
      case Some(localDict) => Right(localDict)
      case None => resolveViaInstance(traitName, args, env, depth) match {
        case Some(result) => result
        case None => resolveViaSupers(traitName, args, env) match {
          case Some(projected) => Right(projected)
          case None => Left(CompileError.NoInstance(traitName, args))
        }
      }
    }
  }

  private def resolveViaInstance(traitName: TypeVariable, args: Seq[TypeRec[Type]], env: Env, depth: Int): Option[EitherS[TypeRec[Expr]]] = for {
    headNames <- args.traverse(a => typeConstructorHead(a).map(_._1))
    inst <- env.instances.get(instanceKey(traitName, headNames))
    paramVars = inst.params.map(_._1)
    subst <- inst.targets.zip(args).toList.foldLeftM(Map.empty[TypeVariable, TypeRec[Type]]) {
      case (acc, (pattern, actual)) => matchTypes(pattern, actual, paramVars.toSet, acc)
    }
  } yield {
    val bindings = paramVars.map(p => subst.getOrElse(p, invariant(s"instance ${inst.dictName.name}: parameter ${p.name} is unbound")))
    inst.context.traverse { tc =>
      resolve(tc.name, tc.arg.map(a => Equivalence.normalize(substMany(paramVars, bindings, a))), env, depth + 1)
    }.map { contextDicts =>
      val base = varrType(inst.dictName, instanceType(inst))
      val (tyApplied, appliedType) = applyTypeArgs(base, instanceType(inst), bindings.map(b => (b, b)))
      applyDictArgs(tyApplied, appliedType, contextDicts)._1
    }
  }

  // スーパークラス経由: 局所辞書から $super 射影をたどって C[T̄] に到達できれば成功
  private def resolveViaSupers(traitName: TypeVariable, args: Seq[TypeRec[Type]], env: Env): Option[TypeRec[Expr]] = {
    case class Node(constraint: TypeConstraint, expr: TypeRec[Expr])

    def children(node: Node): List[Node] =
      env.traits.get(node.constraint.name).fold(List.empty[Node]) { traitDef =>
        traitDef.supers.zipWithIndex.toList.map { case (sup, index) =>
          val substArgs = sup.arg.map(a => Equivalence.normalize(substMany(traitDef.param.map(_._1), node.constraint.arg, a)))
          val projType = projectionType(node.constraint.name, traitDef, applyTypeConstructor(sup.name, sup.arg))
          val (tyApplied, appliedType) =
            applyTypeArgs(varrType(superDictionaryName(node.constraint.name, index), projType), projType, node.constraint.arg.map(a => (a, a)))
          val (projected, _) = applyDictArgs(tyApplied, appliedType, Seq(node.expr))
          Node(TypeConstraint(sup.name, substArgs), projected)
        }
      }

    @tailrec
    def search(queue: List[Node]): Option[TypeRec[Expr]] = queue match {
      case Nil => None
      case node :: rest =>
        if (sameConstraint(node.constraint, traitName, args)) Some(node.expr)
        else search(rest ++ children(node))
    }

    val locals = env.dictsInScope.toList.map { case (tc, dictVar) =>
      Node(tc, varrType(dictVar, applyTypeConstructor(tc.name, tc.arg)))
    }
    // 深さ 0（局所辞書そのもの）は resolve 手順 1 で処理済みなので、子から探索する
    search(locals.flatMap(children))
  }

  // trait の辞書フィールド射影の内部型: ∀p̄. C[p̄] → fieldType
  private def projectionType(traitName: TypeVariable, traitDef: TraitDef, fieldType: TypeRec[Type]): TypeRec[Type] = {
    val dictType = applyTypeConstructor(traitName, traitDef.param.map { case (p, _) => typeVarT(p) })
    traitDef.param.foldRight(arrowT(dictType, fieldType)) { case ((p, k), acc) => forallTypeT(p, k, acc) }
  }

  // ---- 辞書挿入（§5.3） ----

  private def collectSpine(e: TypeRec[Expr]): (TypeRec[Expr], List[SpineElem]) = {
    @tailrec
    def loop(current: TypeRec[Expr], acc: List[SpineElem]): (TypeRec[Expr], List[SpineElem]) = current.project match {
      case AST.App(function, argument) => loop(function, SpineElem.ValArg(argument, typeOf(current)) :: acc)
      case AST.TyApp(function, argument) => loop(function, SpineElem.TyArg(argument, typeOf(current)) :: acc)
      case _ => (current, acc)
    }
    loop(e, Nil)
  }

  private def replaySpine(head: TypeRec[Expr], elems: List[SpineElem], env: Env, bound: Set[Variable]): EitherS[TypeRec[Expr]] =
    elems.foldLeftM(head) { (acc, elem) =>
      elem match {
        case SpineElem.TyArg(arg, resultType) => Right(tyAppT(resultType, acc, arg))
        case SpineElem.ValArg(arg, resultType) => rewriteExpr(arg, env, bound).map(rewritten => appT(resultType, acc, rewritten))
      }
    }

  // 制約付き名（メソッド/制約付き関数）のスパイン: 先頭の型引数を束縛して各制約を resolve し、辞書を挿入する
  private def rewriteSpine(e: TypeRec[Expr], env: Env, bound: Set[Variable]): EitherS[TypeRec[Expr]] = {
    val (head, elems) = collectSpine(e)
    head.project match {
      case AST.Var(name) if !bound.contains(name) && env.constrains.contains(name) =>
        val constraints = env.constrains(name)
        val surface = env.values.getOrElse(name, invariant(s"surface type of ${name.name} is missing from ProgramAnn"))
        // メソッドは trait パラメータ分、制約付き関数は宣言した型パラメータ全部の型適用直後に挿入する
        val arity = constraints.headOption
          .flatMap(c => env.traits.get(c.name))
          .filter(_.methods.exists(_._1 == name))
          .map(_.param.length)
          .getOrElse(countLeadingForalls(surface))
        val (binders, rest) = stripLeadingForalls(surface, arity)
          .fold(got => invariant(s"${name.name}: expected $arity leading ∀ binders, got $got: ${surface.show}"), identity)
        for {
          tyArgs <- {
            val prefix = elems.take(arity).collect { case SpineElem.TyArg(arg, _) => arg }
            Either.cond(
              prefix.length == arity,
              prefix,
              CompileError.AmbiguousConstraint(name, arity)
            )
          }
          expandedArgs = tyArgs.map(arg => TAnalyser.expandType(arg).run(env).getOrElse(arg))
          binderVars = binders.map(_._1)
          dicts <- constraints.traverse { tc =>
            resolve(tc.name, tc.arg.map(a => Equivalence.normalize(substMany(binderVars, expandedArgs, a))), env, 0)
          }
          dictTypes = constraints.map(tc => applyTypeConstructor(tc.name, tc.arg))
          elaborated = binders.foldRight(dictTypes.foldRight(rest)(arrowT)) { case ((p, k), acc) => forallTypeT(p, k, acc) }
          (tyApplied, tyAppliedType) = applyTypeArgs(varrType(name, elaborated), elaborated, tyArgs.zip(expandedArgs))
          (withDicts, _) = applyDictArgs(tyApplied, tyAppliedType, dicts)
          result <- replaySpine(withDicts, elems.drop(arity), env, bound)
        } yield result
      case _ =>
        rewriteExpr(head, env, bound).flatMap(rewrittenHead => replaySpine(rewrittenHead, elems, env, bound))
    }
  }

  private def rewriteChild[I](child: TypeRec[I], env: Env, bound: Set[Variable]): EitherS[TypeRec[I]] =
    child.extract match {
      case ExprAnn(_) => rewriteExpr(child.asInstanceOf[TypeRec[Expr]], env, bound).asInstanceOf[EitherS[TypeRec[I]]]
      case _ => Right(child)
    }

  private def rewriteExpr(e: TypeRec[Expr], env: Env, bound: Set[Variable]): EitherS[TypeRec[Expr]] = e.project match {
    case AST.Var(name) if !bound.contains(name) && env.constrains.contains(name) => rewriteSpine(e, env, bound)
    case AST.App(_, _) | AST.TyApp(_, _) => rewriteSpine(e, env, bound)
    case AST.Abs(variable, types, body) =>
      rewriteExpr(body, env, bound + variable).map(b => HCofree(e.extract, AST.Abs(variable, types, b)))
    case AST.Let(variable, types, value, body) =>
      (rewriteExpr(value, env, bound), rewriteExpr(body, env, bound + variable))
        .mapN((v, b) => HCofree(e.extract, AST.Let(variable, types, v, b)))
    case AST.LetRec(variable, types, value, body) =>
      (rewriteExpr(value, env, bound + variable), rewriteExpr(body, env, bound + variable))
        .mapN((v, b) => HCofree(e.extract, AST.LetRec(variable, types, v, b)))
    case AST.Match(scrutinee, cases) =>
      (rewriteExpr(scrutinee, env, bound), cases.traverse(rewriteCase(_, env, bound)))
        .mapN((s, cs) => HCofree(e.extract, AST.Match(s, cs)))
    case AST.Fold(scrutinee, resultType, cases) =>
      (rewriteExpr(scrutinee, env, bound), cases.traverse(rewriteCase(_, env, bound)))
        .mapN((s, cs) => HCofree(e.extract, AST.Fold(s, resultType, cs)))
    case _ =>
      e.project.htraverse([x] => (child: TypeRec[x]) => rewriteChild(child, env, bound)).map(node => HCofree(e.extract, node))
  }

  private def rewriteCase(matchCase: MatchCase[TypeRec], env: Env, bound: Set[Variable]): EitherS[MatchCase[TypeRec]] =
    rewriteExpr(matchCase.body, env, bound ++ matchCase.binders)
      .map(body => MatchCase(matchCase.constructor, matchCase.binders, body))
}
