package com.yuuki14202028

import cats.data.ReaderT

type EitherS[A] = Either[CompileError, A]
type Check[A] = ReaderT[EitherS, Env, A]

object Check {
  def pure[A](a: A): Check[A] = ReaderT.pure(a)
  def fail[A](error: CompileError): Check[A] = ReaderT.liftF(Left(error))
  def guard(cond: Boolean, error: => CompileError): Check[Unit] = ReaderT.liftF(Either.cond(cond, (), error))
  def lift[A](e: EitherS[A]): Check[A] = ReaderT.liftF(e)
  val ask: Check[Env] = ReaderT.ask[EitherS, Env]
}

type ConstI[A] = [I] =>> A
