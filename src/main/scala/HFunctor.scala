package com.yuuki14202028

import cats.{Applicative, Id}
import cats.syntax.all._

type ~>[F[_], G[_]] = [X] => F[X] => G[X]

trait HFunctor[H[_[_], _]] {
  def map[R[_], S[_], I](fa: H[R, I])(f: R ~> S): H[S, I]
}

extension [H[_[_], _], R[_], I](self: H[R, I])(using hf: HFunctor[H]) {
  def hmap[S[_]](f: R ~> S): H[S, I] = hf.map(self)(f)
}

trait HTraverse[H[_[_], _]] extends HFunctor[H] {
  def traverse[G[_] : Applicative, R[_], S[_], I](fa: H[R, I])(f: [x] => R[x] => G[S[x]]): G[H[S, I]]

  override def map[R[_], S[_], I](fa: H[R, I])(f: R ~> S): H[S, I] =
    traverse[Id, R, S, I](fa)([x] => (r: R[x]) => f(r): Id[S[x]])
}

extension [H[_[_], _], R[_], I](self: H[R, I])(using ht: HTraverse[H]) {
  def htraverse[G[_]: Applicative, S[_]](f: [x] => R[x] => G[S[x]]): G[H[S, I]] = ht.traverse(self)(f)
}

private def traverseConstraint[G[_]: Applicative, R[_], S[_]](c: Constraint[R])(f: [x] => R[x] => G[S[x]]): G[Constraint[S]] =
  (c.arg.traverse(a => f(a)), c.premises.traverse(traverseConstraint(_)(f))).mapN(Constraint[S](c.name, _, c.params, _, c.span))

extension [R[_]](self: Constraint[R]) {
  def hmap[S[_]](f: R ~> S): Constraint[S] = traverseConstraint[Id, R, S](self)([x] => (r: R[x]) => f(r): Id[S[x]])
}

private def traverseEvidence[G[_]: Applicative, R[_], S[_]](e: Evidence[R])(f: [x] => R[x] => G[S[x]]): G[Evidence[S]] = e match {
  case Evidence.Given(constraint) =>
    traverseConstraint(constraint)(f).map(Evidence.Given(_))
  case Evidence.Instance(traitName, heads, typeArgs, context) =>
    (typeArgs.traverse(a => f(a)), context.traverse(traverseEvidence(_)(f))).mapN(Evidence.Instance(traitName, heads, _, _))
  case Evidence.Super(traitName, index, of) =>
    traverseEvidence(of)(f).map(Evidence.Super(traitName, index, _))
  case Evidence.Apply(of, typeArgs, premises) =>
    (traverseEvidence(of)(f), typeArgs.traverse(a => f(a)), premises.traverse(traverseEvidence(_)(f))).mapN(Evidence.Apply(_, _, _))
  case Evidence.Abs(constraint, body) =>
    (traverseConstraint(constraint)(f), traverseEvidence(body)(f)).mapN(Evidence.Abs(_, _))
  case Evidence.Explicit(dictionary) =>
    f(dictionary).map(Evidence.Explicit(_))
}

private def traversePattern[G[_]: Applicative, R[_], S[_]](p: Pattern[R])(f: [x] => R[x] => G[S[x]]): G[Pattern[S]] = p match {
  case Pattern.Wildcard() => Applicative[G].pure(Pattern.Wildcard[S]())
  case Pattern.Bind(name) => Applicative[G].pure(Pattern.Bind[S](name))
  case Pattern.Constructor(name, args, siblings) =>
    val siblingsG = siblings.traverse(s => s.fields.traverse(field => f(field)).map(ConstructorShape[S](s.name, _)))
    (args.traverse(traversePattern(_)(f)), siblingsG).mapN(Pattern.Constructor[S](name, _, _))
  case Pattern.Literal(value, equals) => equals.traverse(e => f(e)).map(Pattern.Literal[S](value, _))
  case Pattern.Or(alternatives) => alternatives.traverse(traversePattern(_)(f)).map(Pattern.Or[S](_))
  case Pattern.Located(span, inner) => traversePattern(inner)(f).map(Pattern.Located[S](span, _))
}

extension [R[_]](self: Evidence[R]) {
  def hmap[S[_]](f: R ~> S): Evidence[S] = traverseEvidence[Id, R, S](self)([x] => (r: R[x]) => f(r): Id[S[x]])
}

given HTraverse[AST] with {
  def traverse[G[_]: Applicative, R[_], S[_], I](fa: AST[R, I])(f: [x] => R[x] => G[S[x]]): G[AST[S, I]] = fa match {
    case AST.Program(decls) =>
      decls.traverse(d => f(d)).map(AST.Program(_))
    case AST.TopLet(v, types, value) =>
      (f(types), f(value)).mapN(AST.TopLet(v, _, _))
    case AST.TopRec(decls) =>
      decls.traverse(d => f(d)).map(AST.TopRec(_))
    case AST.TopImport(path) =>
      Applicative[G].pure(AST.TopImport(path))
    case AST.TopType(v, params, alias) =>
      f(alias).map(AST.TopType(v, params, _))
    case AST.TopData(v, params, ctors) =>
      ctors.traverse { c =>
        c.fields.traverse(field => f(field)).map(fs => DataConstructor[S](c.name, fs))
      }.map(AST.TopData(v, params, _))
    case AST.TopTrait(v, params, supers, methods) =>
      val supersG = supers.traverse(traverseConstraint(_)(f))
      val methodsG = methods.traverse { m =>
        (f(m.sig), m.body.traverse(b => f(b))).mapN((sig, body) => MethodSig[S](m.name, sig, body))
      }
      (supersG, methodsG).mapN(AST.TopTrait(v, params, _, _))
    case AST.TopImpl(v, params, targets, context, methods, superEvidence) =>
      val contextG = context.traverse(traverseConstraint(_)(f))
      val methodsG = methods.traverse { m =>
        (m.sig.traverse(s => f(s)), f(m.body)).mapN((sig, body) => MethodImpl[S](m.name, sig, body))
      }
      val superEvidenceG = superEvidence.traverse(traverseEvidence(_)(f))
      (targets.traverse(t => f(t)), contextG, methodsG, superEvidenceG).mapN(AST.TopImpl(v, params, _, _, _, _))
    case AST.TopDerive(traitName, target) =>
      Applicative[G].pure(AST.TopDerive(traitName, target))
    case AST.TopModule(name, decls) =>
      decls.traverse(d => f(d)).map(AST.TopModule(name, _))
    case AST.TopOpen(path) =>
      Applicative[G].pure(AST.TopOpen(path))
    case AST.TopOperator(op, fixity, function) =>
      Applicative[G].pure(AST.TopOperator(op, fixity, function))
    case AST.TopModified(modifier, decl) =>
      f(decl).map(AST.TopModified(modifier, _))
    case AST.Abs(v, types, body) =>
      (f(types), f(body)).mapN(AST.Abs(v, _, _))
    case AST.TyAbs(v, k, body) =>
      f(body).map(AST.TyAbs(v, k, _))
    case AST.InstAbs(constraint, body) =>
      (traverseConstraint(constraint)(f), f(body)).mapN(AST.InstAbs(_, _))
    case AST.Let(v, types, vall, body) =>
      (f(types), f(vall), f(body)).mapN(AST.Let(v, _, _, _))
    case AST.LetPattern(pattern, types, value, body) =>
      (traversePattern(pattern)(f), f(types), f(value), f(body)).mapN(AST.LetPattern(_, _, _, _))
    case AST.LetRec(bindings, body) =>
      val bindingsG = bindings.traverse(b => (f(b.types), f(b.value)).mapN(RecBinding[S](b.variable, _, _)))
      (bindingsG, f(body)).mapN(AST.LetRec(_, _))
    case AST.TypeLet(v, params, alias, body) =>
      (f(alias), f(body)).mapN(AST.TypeLet(v, params, _, _))
    case AST.DataLet(v, params, ctors, body) =>
      val ctorsG = ctors.traverse { c =>
        c.fields.traverse(field => f(field)).map(fs => DataConstructor[S](c.name, fs))
      }
      (ctorsG, f(body)).mapN(AST.DataLet(v, params, _, _))
    case AST.DataLetRec(datas, body) =>
      val datasG = datas.traverse { d =>
        d.constructors.traverse(c => c.fields.traverse(field => f(field)).map(fs => DataConstructor[S](c.name, fs)))
          .map(DataBinding[S](d.variable, d.params, _))
      }
      (datasG, f(body)).mapN(AST.DataLetRec(_, _))
    case AST.PatternMatch(scrut, cases) =>
      val casesG = cases.traverse(c => (traversePattern(c.pattern)(f), c.guard.traverse(g => f(g)), f(c.body)).mapN(PatternCase[S](_, _, _, c.span)))
      (f(scrut), casesG).mapN(AST.PatternMatch(_, _))
    case AST.Match(scrut, cases) =>
      val casesG = cases.traverse { c =>
        f(c.body).map(b => MatchCase[S](c.constructor, c.binders, b))
      }
      (f(scrut), casesG).mapN(AST.Match(_, _))
    case AST.Fold(scrut, resultType, cases) =>
      val casesG = cases.traverse { c =>
        f(c.body).map(b => MatchCase[S](c.constructor, c.binders, b))
      }
      (f(scrut), f(resultType), casesG).mapN(AST.Fold(_, _, _))
    case AST.App(func, arg) =>
      (f(func), f(arg)).mapN(AST.App(_, _))
    case AST.TyApp(func, arg) =>
      (f(func), f(arg)).mapN(AST.TyApp(_, _))
    case AST.InstApp(func, evidence) =>
      (f(func), traverseEvidence(evidence)(f)).mapN(AST.InstApp(_, _))
    case AST.Using(dictionary, body) =>
      (f(dictionary), f(body)).mapN(AST.Using(_, _))
    case AST.Instance(traitName, targets, methods, superEvidence) =>
      val methodsG = methods.traverse { m =>
        (m.sig.traverse(s => f(s)), f(m.body)).mapN((sig, body) => MethodImpl[S](m.name, sig, body))
      }
      (targets.traverse(t => f(t)), methodsG, superEvidence.traverse(traverseEvidence(_)(f))).mapN(AST.Instance(traitName, _, _, _))
    case AST.Summon(constraint, evidence) =>
      (traverseConstraint(constraint)(f), evidence.traverse(traverseEvidence(_)(f))).mapN(AST.Summon(_, _))
    case AST.Foreign(v, types) =>
      f(types).map(AST.Foreign(v, _))
    case AST.Var(v) => Applicative[G].pure(AST.Var(v))
    case AST.Num(v, t) => Applicative[G].pure(AST.Num(v, t))
    case AST.Char(v) => Applicative[G].pure(AST.Char(v))
    case AST.StringLit(v) => Applicative[G].pure(AST.StringLit(v))
    case AST.StrInterp(parts) =>
      parts.traverse(part => f(part)).map(AST.StrInterp(_))
    case AST.Context(monad, steps, result, ops) =>
      val stepsG = steps.traverse[G, ContextStep[S]] {
        case ContextStep.Bind(name, annotation, value) => (f(annotation), f(value)).mapN(ContextStep.Bind[S](name, _, _))
        case ContextStep.Let(name, annotation, value) => (f(annotation), f(value)).mapN(ContextStep.Let[S](name, _, _))
        case ContextStep.Rec(bindings) =>
          bindings.traverse(b => (f(b.types), f(b.value)).mapN(RecBinding[S](b.variable, _, _))).map(ContextStep.Rec[S](_))
        case ContextStep.BindPattern(pattern, annotation, value) =>
          (traversePattern(pattern)(f), f(annotation), f(value)).mapN(ContextStep.BindPattern[S](_, _, _))
        case ContextStep.LetPattern(pattern, annotation, value) =>
          (traversePattern(pattern)(f), f(annotation), f(value)).mapN(ContextStep.LetPattern[S](_, _, _))
      }
      val opsG = ops.traverse(o => (f(o.pure), f(o.flatMap)).mapN(ContextOps[S](_, _)))
      (f(monad), stepsG, f(result), opsG).mapN(AST.Context(_, _, _, _))
    case AST.Bool(v) => Applicative[G].pure(AST.Bool(v))
    case AST.UnitLit() => Applicative[G].pure(AST.UnitLit())
    case AST.Block(discarded, result) =>
      (discarded.traverse(e => f(e)), result.traverse(e => f(e))).mapN(AST.Block(_, _))
    case AST.OpSeq(first, rest) =>
      (f(first), rest.traverse(step => f(step.operand).map(OpStep(step.op, step.span, _)))).mapN(AST.OpSeq(_, _))
    case AST.BinOp(op, l, r, resolved) =>
      (f(l), f(r), resolved.traverse(e => f(e))).mapN(AST.BinOp(op, _, _, _))
    case AST.Intrinsic(op, args) =>
      args.traverse(arg => f(arg)).map(AST.Intrinsic(op, _))
    case AST.UnaryOp(op, t, resolved) =>
      (f(t), resolved.traverse(e => f(e))).mapN(AST.UnaryOp(op, _, _))
    case AST.If(c, t, e) =>
      (f(c), f(t), f(e)).mapN(AST.If(_, _, _))
    case AST.Primitive(n) => Applicative[G].pure(AST.Primitive(n))
    case AST.TypeVar(v) => Applicative[G].pure(AST.TypeVar(v))
    case AST.Arrow(from, to) =>
      (f(from), f(to)).mapN(AST.Arrow(_, _))
    case AST.ForAll(v, k, body) =>
      f(body).map(AST.ForAll(v, k, _))
    case AST.InstArrow(constraint, to) =>
      (traverseConstraint(constraint)(f), f(to)).mapN(AST.InstArrow(_, _))
    case AST.TypeApp(func, arg) =>
      (f(func), f(arg)).mapN(AST.TypeApp(_, _))
    case AST.TypeAbs(v, k, body) =>
      f(body).map(AST.TypeAbs(v, k, _))
  }
}

type Algebra[H[_[_], _], A[_]] = [x] => H[A, x] => A[x]

final case class ParaChild[A[_], B[_], I](original: A[I], result: B[I])
type Para[A[_], B[_]] = [I] =>> ParaChild[A, B, I]

type RAlgebra[H[_[_], _], A[_], B[_]] =
  [x] => H[Para[A, B], x] => B[x]
type ApoCoalgebra[H[_[_], _], B[_]] =
  [x] => B[x] => Either[HFix[H, x], H[[y] =>> Either[HFix[H, y], B[y]], x]]

def apo[H[_[_], _], B[_], I]
       (seed: B[I])(coalg: ApoCoalgebra[H, B])
       (using hf: HFunctor[H]): HFix[H, I] = {
  coalg(seed) match {
    case Left(done) => done
    case Right(layer) =>
      val tail = layer.hmap([Y] => e => e match {
        case Left(tree) => tree
        case Right(s) => apo(s)(coalg)
      })
      HFix(tail)
  }
}

extension [H[_[_], _], I](self: HFix[H, I]) {
  def cata[A[_]](alg: Algebra[H, A])(using hf: HFunctor[H]): A[I] = {
    val mapped = self.unfix.hmap([X] => (child: HFix[H, X]) => child.cata(alg))
    alg(mapped)
  }

  def para[B[_]](alg: RAlgebra[H, [y] =>> HFix[H, y], B])(using hf: HFunctor[H]): B[I] = {
    val mapped = self.unfix.hmap[Para[[y] =>> HFix[H, y], B]]([X] => child => ParaChild(child, child.para(alg)))
    alg(mapped)
  }
}
