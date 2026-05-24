package com.yuuki14202028

import cats.data.ReaderT

object KAnalyser {

  private type EitherS[A] = Either[String, A]
  private type KC[I] = ReaderT[EitherS, Env, Kind]

  private def guard(cond: Boolean, msg: => String): ReaderT[EitherS, Env, Unit] =
    ReaderT.liftF(Either.cond(cond, (), msg))
  private def lift[A](e: EitherS[A]): ReaderT[EitherS, Env, A] = ReaderT.liftF(e)
  private val ask: ReaderT[EitherS, Env, Env] = ReaderT.ask[EitherS, Env]
  private def ok(k: Kind): ReaderT[EitherS, Env, Kind] = ReaderT.pure(k)

  private def primitiveKind(name: String): EitherS[Kind] =
    BuiltinTypes.arity(name) match {
      case Some(n) => Right(arityKind(n))
      case None => Left(s"Primitive type $name is not defined")
    }

  private def arityKind(n: Int): Kind =
    (0 until n).foldLeft(Kind.Star: Kind)((acc, _) => Kind.Arrow(Kind.Star, acc))

  private def dataKind(params: Seq[(TypeVariable, Kind)]): Kind =
    params.foldRight(Kind.Star: Kind) { case ((_, k), acc) => Kind.Arrow(k, acc) }

  private val alg: HCofreeParaAlgebra[AST, TypeAnn, KC] = [x] => (_, node) => node match {
    case AST.Primitive(name) => lift(primitiveKind(name))

    case AST.TypeVar(v) => for {
      env <- ask
      kind <- lift(env.typeVars.get(v) match {
        case Some(k) => Right(k)
        case None =>
          env.dataTypes.get(v).map(d => dataKind(d.params))
            .orElse(env.typeAliases.get(v).map(a => dataKind(a.params)))
            .toRight(s"Type variable ${v.name} is not defined")
      })
    } yield kind

    case AST.Arrow(from, to) => for {
      kf <- from._2
      kt <- to._2
      _ <- guard(kf == Kind.Star, s"Arrow LHS must have kind *, got ${kf.show}: ${from._1.show}")
      _ <- guard(kt == Kind.Star, s"Arrow RHS must have kind *, got ${kt.show}: ${to._1.show}")
    } yield Kind.Star

    case AST.ForAll(v, k, body) => for {
      kb <- body._2.local((e: Env) => e.copy(typeVars = e.typeVars + (v -> k)))
      _ <- guard(kb == Kind.Star, s"∀ body must have kind *, got ${kb.show}: ${body._1.show}")
    } yield Kind.Star

    case AST.TypeAbs(v, k, body) =>
      body._2.local((e: Env) => e.copy(typeVars = e.typeVars + (v -> k))).map(kb => Kind.Arrow(k, kb))

    case AST.TypeApp(function, argument) => for {
      kf <- function._2
      ka <- argument._2
      result <- kf match {
        case Kind.Arrow(k1, k2) if k1 == ka => ok(k2)
        case Kind.Arrow(k1, _) =>
          lift[Kind](Left(s"Kind mismatch in type application: expected ${k1.show}, got ${ka.show} (argument ${argument._1.show})"))
        case other =>
          lift[Kind](Left(s"Cannot apply a type of kind ${other.show}: ${function._1.show}"))
      }
    } yield result

    case _ => lift[Kind](Left("Unreachable: non-type node encountered in Kinding"))
  }

  def kindOf(t: TypeRec[Type], env: Env): Either[String, Kind] =
    t.paraAnn(alg).run(env)
}