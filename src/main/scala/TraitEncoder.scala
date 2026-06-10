package com.yuuki14202028

import cats.syntax.all.*

import scala.annotation.tailrec

object TraitEncoder {

  private type EitherS[A] = Either[String, A]

  private def invariant(msg: String): String = s"Compiler invariant violation: $msg"

  private enum SpineElem {
    case TyArg(arg: TypeRec[Type], resultType: TypeRec[Type])
    case ValArg(arg: TypeRec[Expr], resultType: TypeRec[Type])
  }

  def encode(program: TypeRec[AST.Program.type]): EitherS[TypeRec[AST.Program.type]] = program.project match {
    case AST.Program(decls) =>
      val env = program.extract match {
        case ProgramAnn(e) => e
      }
      decls.toVector
        .flatTraverse(decl => encodeDecl(decl, env).map(_.toVector))
        .map(encodedDecls => programT(encodedDecls, env))
  }

  private def encodeDecl(decl: TypeRec[Decl], env: Env): EitherS[Seq[TypeRec[Decl]]] = decl.project match {
    case AST.TopTrait(name, params, _, _) => encodeTrait(name, params, env)
    case AST.TopImpl(name, target, methods) => encodeImpl(name, target, methods, env).map(Seq(_))
    case AST.TopLet(variable, types, value) =>
      rewriteExpr(value, env, Set.empty).map(rewritten => Seq(topLetT(variable, types, rewritten)))
    case AST.TopLetRec(variable, types, value) =>
      rewriteExpr(value, env, Set.empty).map(rewritten => Seq(topLetRecT(variable, types, rewritten)))
    case _ => Right(Seq(decl))
  }

  /** trait C[p̄] { def mᵢ: τᵢ }
   * data C[p̄] = | MkC(τ₁)…(τₙ)
   * let mᵢ : ∀p̄. C[p̄] → τᵢ = Λp̄. λ$dict: C[p̄]. match $dict with | MkC(x̄) -> xᵢ
   */
  private def encodeTrait(name: TypeVariable, params: Seq[(TypeVariable, Kind)], env: Env): EitherS[Seq[TypeRec[Decl]]] =
    env.traits.get(name).toRight(invariant(s"trait ${name.name} is missing from ProgramAnn")).map { traitDef =>
      val ctorName = dictionaryConstructor(name)
      val sigs = traitDef.methods
      val dictDecl = topDataT(name, params, Seq(DataConstructor[TypeRec](ctorName, sigs.map(_._2))))
      val dictType = applyTypeConstructor(name, params.map { case (p, _) => typeVarT(p) })
      val dictVar = Variable("$dict")
      val binders = sigs.indices.map(i => Variable(s"$$field_$i"))
      val projections = sigs.zipWithIndex.map { case ((methodName, sig), index) =>
        val matchE = matchExprT(sig, varrType(dictVar, dictType), Seq(MatchCase(ctorName, binders, varrType(binders(index), sig))))
        val lam = absT(dictVar, arrowT(dictType, sig), dictType, matchE)
        val value = params.foldRight(lam) { case ((p, k), acc) => tyAbsT(p, forallTypeT(p, k, typeOf(acc)), k, acc) }
        topLetT(methodName, typeOf(value), value)
      }
      dictDecl +: projections
    }

  // impl C[T] { def mᵢ = eᵢ } ⟶ let $inst_C_h : C[T] = MkC[T](e₁)…(eₙ)  (eᵢ は trait 宣言順)
  private def encodeImpl(traitName: TypeVariable, target: TypeRec[Type], methods: Seq[MethodImpl[TypeRec]], env: Env): EitherS[TypeRec[Decl]] = for {
    traitDef <- env.traits.get(traitName).toRight(invariant(s"trait ${traitName.name} is missing from ProgramAnn"))
    expandedTarget <- TAnalyser.expandType(target, env)
    headApp <- typeConstructorHead(expandedTarget).toRight(invariant(s"impl ${traitName.name}: target is not a type constructor"))
    inst <- env.instances.get(instanceKey(traitName, headApp._1))
      .toRight(invariant(s"instance ${traitName.name}[${headApp._1}] is missing from ProgramAnn"))
    orderedBodies <- traitDef.methods.traverse { case (methodName, _) =>
      methods.collectFirst { case m if m.name == methodName => m.body }
        .toRight(invariant(s"impl ${traitName.name}[${headApp._1}] is missing method ${methodName.name}"))
    }
    rewrittenBodies <- orderedBodies.traverse(body => rewriteExpr(body, env, Set.empty))
  } yield {
    val (paramVar, paramKind) = traitDef.param.head
    val fields = traitDef.methods.map(_._2)
    val specialized = fields.map(field => substType(paramVar, inst.target, field))
    val dictType = applyTypeConstructor(traitName, Seq(inst.target))
    val ctorResult = applyTypeConstructor(traitName, Seq(typeVarT(paramVar)))
    val ctorType = forallTypeT(paramVar, paramKind, fields.foldRight(ctorResult)(arrowT))
    val ctorRef = varrType(dictionaryConstructor(traitName), ctorType)
    val tyApplied = tyAppT(specialized.foldRight(dictType)(arrowT), ctorRef, target)
    val value = rewrittenBodies.zipWithIndex.foldLeft(tyApplied) { case (acc, (body, index)) =>
      appT(specialized.drop(index + 1).foldRight(dictType)(arrowT), acc, body)
    }
    topLetT(inst.dictName, dictType, value)
  }

  // 制約 C[T̄] を満たす辞書式を返す（Phase 1: head 一致の impl のみ）
  private def resolve(traitName: TypeVariable, args: Seq[TypeRec[Type]], env: Env): EitherS[TypeRec[Expr]] = {
    def noInstance = s"No instance for ${traitName.name}[${args.map(_.show).mkString("][")}]"
    for {
      headArg <- args.headOption.toRight(noInstance)
      headApp <- typeConstructorHead(headArg).toRight(noInstance)
      inst <- env.instances.get(instanceKey(traitName, headApp._1)).toRight(noInstance)
    } yield varrType(inst.dictName, applyTypeConstructor(traitName, args))
  }

  private def elaboratedMethodType(traitName: TypeVariable, traitDef: TraitDef, sig: TypeRec[Type]): TypeRec[Type] = {
    val dictType = applyTypeConstructor(traitName, traitDef.param.map { case (p, _) => typeVarT(p) })
    traitDef.param.foldRight(arrowT(dictType, sig)) { case ((p, k), acc) => forallTypeT(p, k, acc) }
  }

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

  // メソッド呼び出しのスパイン: trait パラメータ分の型適用直後に resolve(C[T̄]) を挿入する（§5.3, §6）
  private def rewriteSpine(e: TypeRec[Expr], env: Env, bound: Set[Variable]): EitherS[TypeRec[Expr]] = {
    val (head, elems) = collectSpine(e)
    head.project match {
      case AST.Var(method) if !bound.contains(method) && env.constrains.contains(method) =>
        for {
          constraint <- env.constrains(method).headOption.toRight(invariant(s"method ${method.name} has no constraint"))
          traitName = constraint.name
          traitDef <- env.traits.get(traitName).toRight(invariant(s"trait ${traitName.name} is missing from ProgramAnn"))
          arity = traitDef.param.length
          tyArgs <- {
            val prefix = elems.take(arity).collect { case SpineElem.TyArg(arg, _) => arg }
            Either.cond(
              prefix.length == arity,
              prefix,
              s"Ambiguous constraint: method ${method.name} requires $arity type application(s) to resolve ${traitName.name}"
            )
          }
          expandedArgs = tyArgs.map(arg => TAnalyser.expandType(arg, env).getOrElse(arg))
          dict <- resolve(traitName, expandedArgs, env)
          sig <- traitDef.methods.collectFirst { case (name, s) if name == method => s }
            .toRight(invariant(s"method ${method.name} is missing from trait ${traitName.name}"))
          inserted <- {
            val elaborated = elaboratedMethodType(traitName, traitDef, sig)
            tyArgs.zip(expandedArgs)
              .foldLeftM((varrType(method, elaborated), elaborated)) { case ((expr, current), (argSyntax, argExpanded)) =>
                destructForAllK(current).toRight(invariant(s"method ${method.name}: elaborated type is not polymorphic")).map {
                  case (v, _, body) =>
                    val next = substType(v, argExpanded, body)
                    (tyAppT(next, expr, argSyntax), next)
                }
              }
              .flatMap { case (expr, current) =>
                destructArrow(current).toRight(invariant(s"method ${method.name}: elaborated type does not take a dictionary"))
                  .map { case (_, after) => appT(after, expr, dict) }
              }
          }
          result <- replaySpine(inserted, elems.drop(arity), env, bound)
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
    case AST.Var(method) if !bound.contains(method) && env.constrains.contains(method) =>
      Left(s"Ambiguous constraint: method ${method.name} requires explicit type application to resolve its dictionary")
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
