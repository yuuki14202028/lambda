package com.yuuki14202028

import cats.syntax.traverse._
import cats.data.ReaderT
import cats.implicits.catsSyntaxTuple2Semigroupal

object Analyser {

  type Env = Map[Variable, Rec[Type]]
  type EitherS[A] = Either[String, A]
  type TC[I] = ReaderT[EitherS, Env, TypeRec[I]]

  private def expect(expected: Rec[Type], actual: Rec[Type]): EitherS[Unit] = {
    Either.cond(expected == actual, (), s"Type mismatch: expected $expected, actual $actual")
  }

  private def expectNumeric(actual: Rec[Type]): EitherS[Unit] = {
    Either.cond(isNumericType(actual), (), s"Type mismatch: expected Int or Char, actual $actual")
  }

  private val tcAlg: Algebra[AST, TC] = [x] => (node: AST[TC, x]) => node match {

    case AST.Program(body) => body.traverse(identity).map(programT)

    case AST.Abs(variable, types, body) => for {
      typedTypes <- types
      paramType = eraseAnn[Type](typedTypes)
      typedBody <- body.local((env: Env) => env + (variable -> paramType))
      resultType = arrow(paramType, typeOf(typedBody))
    } yield absT(variable, resultType, typedTypes, typedBody)

    case AST.Let(variable, types, value, body) => for {
      typedTypes <- types
      declaredType = eraseAnn[Type](typedTypes)
      typedValue <- value
      _ <- ReaderT.liftF(expect(declaredType, typeOf(typedValue)))
      typedBody <- body.local((env: Env) => env + (variable -> declaredType))
      resultType = typeOf(typedBody)
    } yield letT(variable, resultType, typedTypes, typedValue, typedBody)

    case AST.App(function, argument) => for {
      typedFunction <- function
      typedArgument <- argument
      resultType <- ReaderT.liftF[EitherS, Env, Rec[Type]] {
        destructArrow(typeOf(typedFunction)) match {
          case Some((from, to)) if from == typeOf(typedArgument) => Right(to)
          case Some((from, _)) => Left(s"Type mismatch: expected $from, actual ${typeOf(typedArgument)}")
          case None => Left(s"Not a function: ${typeOf(typedFunction)}")
        }
      }
    } yield appT(resultType, typedFunction, typedArgument)

    case AST.Foreign(value) => ReaderT.pure(foreignT(value, foreignType))

    case AST.Var(value) => for {
      env <- ReaderT.ask[EitherS, Env]
      t <- ReaderT.liftF[EitherS, Env, Rec[Type]](
        env.get(value).toRight(s"Variable $value is not defined")
      )
    } yield varrType(value, t)

    case AST.Num(value) => ReaderT.pure(numT(value, intType))
    case AST.Char(value) => ReaderT.pure(charT(value, charType))

    case AST.BinOp(op, left, right) => for {
      typedLeft <- left
      typedRight <- right
      leftType = typeOf(typedLeft)
      rightType = typeOf(typedRight)
      _ <- ReaderT.liftF(expectNumeric(leftType))
      _ <- ReaderT.liftF(expect(leftType, rightType))
    } yield binopT(op, leftType, typedLeft, typedRight)

    case AST.UnaryOp(UnaryOps.Neg, body) => for {
      typedBody <- body
      bodyType = typeOf(typedBody)
      _ <- ReaderT.liftF(expectNumeric(bodyType))
    } yield unopT(UnaryOps.Neg, bodyType, typedBody)

    case AST.If(cond, thenBranch, elseBranch) => for {
      typedCond <- cond
      typedThen <- thenBranch
      typedElse <- elseBranch
      condType = typeOf(typedCond)
      thenType = typeOf(typedThen)
      elseType = typeOf(typedElse)
      _ <- ReaderT.liftF(expectNumeric(condType))
      _ <- ReaderT.liftF(expect(thenType, elseType))
    } yield ifT(thenType, typedCond, typedThen, typedElse)

    case AST.Primitive("Int") => ReaderT.pure(primitiveT("Int"))
    case AST.Primitive("Char") => ReaderT.pure(primitiveT("Char"))
    case AST.Primitive(name) => ReaderT.liftF(Left(s"Primitive type $name is not defined"))
    case AST.Arrow(from, to) => (from, to).mapN(arrowT)
  }

  def validate(prog: Rec[AST.Program.type]): EitherS[TypeRec[AST.Program.type]] = {
    prog.cata(tcAlg).run(Map.empty)
  }

}
