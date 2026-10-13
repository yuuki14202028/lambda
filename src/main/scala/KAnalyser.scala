package com.yuuki14202028

import cats.syntax.all.*
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
    (he: TypedAST[Para[TypeRec, ConstI[Check[Kind]]], x]) => he.ast match {
    case AST.Primitive(name) => lift(primitiveKind(name))

    case AST.TypeVar(v) => for {
      env <- ask
      kind <- lift(env.typeVars.get(v) match {
        case Some(k) => Right(k)
        case None => env.dataTypes.get(v).map(d => dataKind(d.params))
          .orElse(env.typeAliases.get(v).map(a => dataKind(a.params)))
          .orElse(env.abstractTypes.get(v).map(a => dataKind(a.params)))
          .toRight(CompileError.UndefinedTypeVariable(v))
      })
    } yield kind

    case AST.Arrow(from, to) => for {
      kf <- from.result
      kt <- to.result
      _ <- guard(kf == Kind.Star, CompileError.ArrowKindNotStar(ArrowSide.Lhs, kf, from.original))
      _ <- guard(kt == Kind.Star, CompileError.ArrowKindNotStar(ArrowSide.Rhs, kt, to.original))
    } yield Kind.Star

    case AST.ForAll(v, k, body) => for {
      kb <- body.result.local((e: Env) => e.copy(typeVars = e.typeVars + (v -> k)))
      _ <- guard(kb == Kind.Star, CompileError.ForAllBodyKindNotStar(kb, body.original))
    } yield Kind.Star

    case AST.InstArrow(constraint, to) => for {
      _ <- checkConstraint(constraint.hmap([y] => (child: Para[TypeRec, ConstI[Check[Kind]]][y]) => child.original), None)
      kt <- to.result
      _ <- guard(kt == Kind.Star, CompileError.ArrowKindNotStar(ArrowSide.Rhs, kt, to.original))
    } yield Kind.Star

    case AST.TypeAbs(v, k, body) =>
      body.result.local((e: Env) => e.copy(typeVars = e.typeVars + (v -> k))).map(kb => Kind.Arrow(k, kb))

    case AST.TypeApp(function, argument) => for {
      kf <- function.result
      ka <- argument.result
      result <- kf match {
        case Kind.Arrow(k1, k2) if k1 == ka => ok(k2)
        case Kind.Arrow(k1, _) =>
          fail[Kind](CompileError.KindMismatchInTypeApp(k1, ka, Some(argument.original)))
        case other =>
          fail[Kind](CompileError.CannotApplyKind(other, function.original))
      }
    } yield result

    case _ => sys.error("Compiler invariant violation: non-type node encountered in Kinding")
  }

  def kindOf(t: TypeRec[Type]): Check[Kind] =
    t.para(alg)

  // 制約 C[τ̄] の検査: trait の存在・引数の数・各引数のカインド一致。site はエラー表示用の宣言
  def checkConstraint(constraint: Constraint[TypeRec], site: Option[DeclRef]): Check[Unit] = (for {
    kinds <- constraint.arg.traverse(kindOf)
    _ <- checkConstraintKinds(constraint.name, constraint.arg.zip(kinds), site)
    _ <- constraint.premises.traverse_(checkConstraint(_, site))
  } yield ()).local((e: Env) => e.copy(typeVars = e.typeVars ++ constraint.params))

  private def checkConstraintKinds(name: TypeVariable, args: Seq[(TypeRec[Type], Kind)], site: Option[DeclRef]): Check[Unit] = for {
    env <- ask
    traitDef <- lift(env.traits.get(name).toRight(CompileError.UndefinedTrait(name, site)))
    _ <- guard(args.length == traitDef.param.length, CompileError.ConstraintArityMismatch(site, name, traitDef.param.length, args.length))
    _ <- args.zip(traitDef.param).traverse_ { case ((arg, argKind), (_, kind)) =>
      guard(argKind == kind, CompileError.ConstraintKindMismatch(site, name, arg, argKind, kind))
    }
  } yield ()
}
