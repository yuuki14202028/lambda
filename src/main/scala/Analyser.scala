package com.yuuki14202028

import cats.syntax.traverse._
import cats.data.ReaderT
import cats.implicits.catsSyntaxTuple2Semigroupal

object Analyser {

  case class Env(values: Map[Variable, Rec[Type]], typeVars: Set[TypeVariable])
  type EitherS[A] = Either[String, A]
  type TC[I] = ReaderT[EitherS, Env, TypeRec[I]]

  private def expect(expected: Rec[Type], actual: Rec[Type]): EitherS[Unit] = {
    Either.cond(sameType(expected, actual), (), s"Type mismatch: expected ${expected.show}, actual ${actual.show}")
  }

  private def expectNumeric(actual: Rec[Type]): EitherS[Unit] = {
    Either.cond(isNumericType(actual), (), s"Type mismatch: expected Int or Char, actual $actual")
  }

  private def expectEquatable(actual: Rec[Type]): EitherS[Unit] = {
    Either.cond(isEquatableType(actual), (), s"Type mismatch: expected Int, Char, or Bool, actual $actual")
  }

  private val tcAlg: Algebra[AST, TC] = [x] => (node: AST[TC, x]) => node match {

    case AST.Program(body) => body.traverse(identity).map(programT)

    case AST.Abs(variable, types, body) => for {
      typedTypes <- types
      paramType = eraseAnn[Type](typedTypes)
      typedBody <- body.local((env: Env) => env.copy(values = env.values + (variable -> paramType)))
      resultType = arrow(paramType, typeOf(typedBody))
    } yield absT(variable, resultType, typedTypes, typedBody)

    case AST.TyAbs(variable, body) => for {
      env <- ReaderT.ask[EitherS, Env]
      _ <- ReaderT.liftF[EitherS, Env, Unit](
        Either.cond(!env.typeVars.contains(variable), (), s"Type variable ${variable.name} is already defined")
      )
      typedBody <- body.local((env: Env) => env.copy(typeVars = env.typeVars + variable))
      resultType = forallType(variable, typeOf(typedBody))
    } yield tyAbsT(variable, resultType, typedBody)

    case AST.Let(variable, types, value, body) => for {
      typedTypes <- types
      declaredType = eraseAnn[Type](typedTypes)
      typedValue <- value
      _ <- ReaderT.liftF(expect(declaredType, typeOf(typedValue)))
      typedBody <- body.local((env: Env) => env.copy(values = env.values + (variable -> declaredType)))
      resultType = typeOf(typedBody)
    } yield letT(variable, resultType, typedTypes, typedValue, typedBody)

    case AST.LetRec(variable, types, value, body) => for {
      typedTypes <- types
      declaredType = eraseAnn[Type](typedTypes)
      typedValue <- value.local((env: Env) => env.copy(values = env.values + (variable -> declaredType)))
      _ <- ReaderT.liftF(expect(declaredType, typeOf(typedValue)))
      typedBody <- body.local((env: Env) => env.copy(values = env.values + (variable -> declaredType)))
      resultType = typeOf(typedBody)
    } yield letRecT(variable, resultType, typedTypes, typedValue, typedBody)

    case AST.App(function, argument) => for {
      typedFunction <- function
      typedArgument <- argument
      resultType <- ReaderT.liftF[EitherS, Env, Rec[Type]] {
        destructArrow(typeOf(typedFunction)) match {
          case Some((from, to)) if sameType(from, typeOf(typedArgument)) => Right(to)
          case Some((from, _)) => Left(s"Type mismatch: expected ${from.show}, actual ${typeOf(typedArgument).show}")
          case None => Left(s"Not a function: ${typeOf(typedFunction).show}")
        }
      }
    } yield appT(resultType, typedFunction, typedArgument)

    case AST.TyApp(function, argument) => for {
      typedFunction <- function
      typedArgument <- argument
      argumentType = eraseAnn[Type](typedArgument)
      resultType <- ReaderT.liftF[EitherS, Env, Rec[Type]] {
        destructForAll(typeOf(typedFunction)) match {
          case Some((variable, bodyType)) => Right(substType(variable, argumentType, bodyType))
          case None => Left(s"Not a polymorphic function: ${typeOf(typedFunction).show}")
        }
      }
    } yield tyAppT(resultType, typedFunction, typedArgument)

    case AST.Foreign(value) => ReaderT.pure(foreignT(value, foreignType))

    case AST.Var(value) => for {
      env <- ReaderT.ask[EitherS, Env]
      t <- ReaderT.liftF[EitherS, Env, Rec[Type]](
        env.values.get(value).toRight(s"Variable $value is not defined")
      )
    } yield varrType(value, t)

    case AST.Num(value) => ReaderT.pure(numT(value, intType))
    case AST.Char(value) => ReaderT.pure(charT(value, charType))
    case AST.Bool(value) => ReaderT.pure(boolT(value, boolType))

    case AST.BinOp(op, left, right) => for {
      typedLeft <- left
      typedRight <- right
      leftType = typeOf(typedLeft)
      rightType = typeOf(typedRight)
      _ <- ReaderT.liftF(op match {
        case BinOps.Add | BinOps.Sub | BinOps.Mul | BinOps.Div => expectNumeric(leftType)
        case BinOps.Eq | BinOps.Neq => expectEquatable(leftType)
        case BinOps.Lt | BinOps.Leq | BinOps.Gt | BinOps.Geq => expectNumeric(leftType)
      })
      _ <- ReaderT.liftF(expect(leftType, rightType))
      resultType = op match {
        case BinOps.Add | BinOps.Sub | BinOps.Mul | BinOps.Div => leftType
        case BinOps.Eq | BinOps.Neq | BinOps.Lt | BinOps.Leq | BinOps.Gt | BinOps.Geq => boolType
      }
    } yield binopT(op, resultType, typedLeft, typedRight)

    case AST.UnaryOp(op, body) => for {
      typedBody <- body
      bodyType = typeOf(typedBody)
      _ <- ReaderT.liftF(op match {
        case UnaryOps.Neg => expectNumeric(bodyType)
        case UnaryOps.Not => expect(boolType, bodyType)
      })
    } yield unopT(op, bodyType, typedBody)

    case AST.If(cond, thenBranch, elseBranch) => for {
      typedCond <- cond
      typedThen <- thenBranch
      typedElse <- elseBranch
      condType = typeOf(typedCond)
      thenType = typeOf(typedThen)
      elseType = typeOf(typedElse)
      _ <- ReaderT.liftF(expect(boolType, condType))
      _ <- ReaderT.liftF(expect(thenType, elseType))
    } yield ifT(thenType, typedCond, typedThen, typedElse)

    case AST.Primitive("Int") => ReaderT.pure(primitiveT("Int"))
    case AST.Primitive("Char") => ReaderT.pure(primitiveT("Char"))
    case AST.Primitive("Bool") => ReaderT.pure(primitiveT("Bool"))
    case AST.Primitive(name) => ReaderT.liftF(Left(s"Primitive type $name is not defined"))
    case AST.TypeVar(variable) => for {
      env <- ReaderT.ask[EitherS, Env]
      _ <- ReaderT.liftF[EitherS, Env, Unit](
        Either.cond(env.typeVars.contains(variable), (), s"Type variable ${variable.name} is not defined")
      )
    } yield typeVarT(variable)
    case AST.Arrow(from, to) => (from, to).mapN(arrowT)
    case AST.ForAll(variable, body) => for {
      env <- ReaderT.ask[EitherS, Env]
      _ <- ReaderT.liftF[EitherS, Env, Unit](
        Either.cond(!env.typeVars.contains(variable), (), s"Type variable ${variable.name} is already defined")
      )
      typedBody <- body.local((env: Env) => env.copy(typeVars = env.typeVars + variable))
    } yield forallTypeT(variable, typedBody)
  }

  def validate(prog: Rec[AST.Program.type]): EitherS[TypeRec[AST.Program.type]] = {
    prog.cata(tcAlg).run(Env(Map.empty, Set.empty))
  }

}
