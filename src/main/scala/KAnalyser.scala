package com.yuuki14202028

import Check.{ask, fail, guard, lift}

object KAnalyser {

  private def ok(k: Kind): Check[Kind] = Check.pure(k)

  private def primitiveKind(name: String): EitherS[Kind] =
    BuiltinTypes.arity(name) match {
      case Some(n) => Right(arityKind(n))
      case None => Left(CompileError.UndefinedPrimitive(name))
    }

  private def arityKind(n: Int): Kind =
    (0 until n).foldLeft(Kind.Star: Kind)((acc, _) => Kind.Arrow(Kind.Star, acc))

  private def dataKind(params: Seq[(TypeVariable, Kind)]): Kind =
    params.foldRight(Kind.Star: Kind) { case ((_, k), acc) => Kind.Arrow(k, acc) }

  private val alg: RAlgebra[TypedAST, TypeRec, ConstI[Check[Kind]]] = [x] =>
    (he: TypedAST[[y] =>> (TypeRec[y], Check[Kind]), x]) => he.ast match {
    case AST.Primitive(name) => lift(primitiveKind(name))

    case AST.TypeVar(v) => for {
      env <- ask
      kind <- lift(env.typeVars.get(v) match {
        case Some(k) => Right(k)
        case None => env.dataTypes.get(v).map(d => dataKind(d.params))
          .orElse(env.typeAliases.get(v).map(a => dataKind(a.params)))
          .toRight(CompileError.UndefinedTypeVariable(v))
      })
    } yield kind

    case AST.Arrow(from, to) => for {
      kf <- from._2
      kt <- to._2
      _ <- guard(kf == Kind.Star, CompileError.ArrowKindNotStar(ArrowSide.Lhs, kf, from._1))
      _ <- guard(kt == Kind.Star, CompileError.ArrowKindNotStar(ArrowSide.Rhs, kt, to._1))
    } yield Kind.Star

    case AST.ForAll(v, k, body) => for {
      kb <- body._2.local((e: Env) => e.copy(typeVars = e.typeVars + (v -> k)))
      _ <- guard(kb == Kind.Star, CompileError.ForAllBodyKindNotStar(kb, body._1))
    } yield Kind.Star

    case AST.TypeAbs(v, k, body) =>
      body._2.local((e: Env) => e.copy(typeVars = e.typeVars + (v -> k))).map(kb => Kind.Arrow(k, kb))

    case AST.TypeApp(function, argument) => for {
      kf <- function._2
      ka <- argument._2
      result <- kf match {
        case Kind.Arrow(k1, k2) if k1 == ka => ok(k2)
        case Kind.Arrow(k1, _) =>
          fail[Kind](CompileError.KindMismatchInTypeApp(k1, ka, Some(argument._1)))
        case other =>
          fail[Kind](CompileError.CannotApplyKind(other, function._1))
      }
    } yield result

    case _ => sys.error("Compiler invariant violation: non-type node encountered in Kinding")
  }

  def kindOf(t: TypeRec[Type]): Check[Kind] =
    t.para(alg)
}