package com.yuuki14202028

import cats.syntax.all.*

object ContextDesugar {

  private type EitherS[A] = Either[String, A]
  private type Desugared[I] = EitherS[TypeRec[I]]

  private val flatMapMethod = Variable("flatMap")
  private val pureMethod = Variable("pure")
  private val monadKind = Kind.Arrow(Kind.Star, Kind.Star)

  def desugar(program: TypeRec[AST.Program.type]): EitherS[TypeRec[AST.Program.type]] = program.project match {
    case AST.Program(decls) =>
      val env = program.extract match {
        case ProgramAnn(e) => e
      }
      decls.toVector.traverse[EitherS, TypeRec[Decl]](_.cata(desugarAlg(env.values))).map(programT(_, env))
  }

  private def desugarAlg(values: Map[Variable, TypeRec[Type]]): Algebra[TypedAST, Desugared] = [x] => he => he.ast match {
    case AST.Context(monad, bindings, result) => for {
      desugaredMonad <- monad
      desugaredBindings <- bindings.toList.traverse { b =>
        (b.annotation, b.value).mapN(ContextBinding[TypeRec](b.name, _, _, b.monadic, b.recursive))
      }
      desugaredResult <- result
      chain <- desugarContext(values, desugaredMonad, desugaredBindings, desugaredResult)
    } yield chain
    case node => node.htraverse([y] => (child: Desugared[y]) => child).map(HCofree(he.ann, _))
  }

  def desugarContext(
      values: Map[Variable, TypeRec[Type]],
      monad: TypeRec[Type],
      bindings: Seq[ContextBinding[TypeRec]],
      result: TypeRec[Expr]
  ): EitherS[TypeRec[Expr]] = {
    val resultType = typeOf(result)
    for {
      retChain <- for {
        retRef <- methodRef(values, pureMethod)
        retM <- applyTypeArg(retRef, monad, monadKind)
        retR <- applyTypeArg(retM, resultType, Kind.Star)
        applied <- applyArg(retR, result)
      } yield applied
      chain <- bindings.foldRight(Right(retChain): EitherS[TypeRec[Expr]]) { (b, accE) =>
        accE.flatMap { acc =>
          if (b.monadic) for {
            bindRef <- methodRef(values, flatMapMethod)
            bindM <- applyTypeArg(bindRef, monad, monadKind)
            bindA <- applyTypeArg(bindM, b.annotation, Kind.Star)
            bindR <- applyTypeArg(bindA, resultType, Kind.Star)
            bindValue <- applyArg(bindR, b.value)
            continuation = absT(b.name, arrowT(b.annotation, typeOf(acc)), b.annotation, acc)
            applied <- applyArg(bindValue, continuation)
          } yield applied
          else if (b.recursive) Right(letRecT(b.name, typeOf(acc), b.annotation, b.value, acc))
          else Right(letT(b.name, typeOf(acc), b.annotation, b.value, acc))
        }
      }
    } yield chain
  }

  private def methodRef(values: Map[Variable, TypeRec[Type]], name: Variable): EitherS[TypeRec[Expr]] =
    values.get(name)
      .toRight(s"context: method `${name.name}` is not defined")
      .map(t => varrType(name, t))

  private def applyArg(function: TypeRec[Expr], argument: TypeRec[Expr]): EitherS[TypeRec[Expr]] =
    destructArrow(typeOf(function)) match {
      case Some((from, to)) if Equivalence.beta(from, typeOf(argument)) => Right(appT(to, function, argument))
      case Some((from, _)) => Left(s"context: type mismatch: expected ${from.show}, actual ${typeOf(argument).show}")
      case None => Left(s"context: not a function: ${typeOf(function).show}")
    }

  private def applyTypeArg(function: TypeRec[Expr], argument: TypeRec[Type], argKind: Kind): EitherS[TypeRec[Expr]] =
    destructForAllK(typeOf(function)) match {
      case Some((variable, expectedKind, bodyType)) =>
        if (expectedKind == argKind) Right(tyAppT(substType(variable, argument, bodyType), function, argument))
        else Left(s"context: kind mismatch in type application: expected ${expectedKind.show}, got ${argKind.show}")
      case None => Left(s"context: not a polymorphic function: ${typeOf(function).show}")
    }
}
