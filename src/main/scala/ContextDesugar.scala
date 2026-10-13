package com.yuuki14202028

import cats.syntax.all.*

/** context ブロックの脱糖。型検査が解決した pure[M] / flatMap[M]（ContextOps）を使って
 * 束縛を flatMap の連鎖へ、結果を pure へ置き換える。名前や制約の解決は行わない。
 */
object ContextDesugar {

  private def invariant(msg: String): Nothing = sys.error(s"Compiler invariant violation: $msg")

  val flatMapMethod: Variable = Variable("flatMap")
  val pureMethod: Variable = Variable("pure")
  val monadKind: Kind = Kind.Arrow(Kind.Star, Kind.Star)

  def desugar(program: TypeRec[AST.Program.type]): TypeRec[AST.Program.type] = program.cata(desugarAlg)

  private val desugarAlg: Algebra[TypedAST, TypeRec] = [x] => he => he.ast match {
    case AST.Context(_, steps, result, ops) =>
      val resolved = ops.getOrElse(invariant("context block reached ContextDesugar without resolved methods"))
      desugarContext(resolved, steps, result)
        .fold(err => invariant(s"context block failed to desugar after validation: ${err.render}"), identity)
    case node => HCofree(he.ann, node)
  }

  // 型検査からも呼ばれ、ブロック全体の型と束縛の型エラーをここで確定する
  def desugarContext(
      ops: ContextOps[TypeRec],
      steps: Seq[ContextStep[TypeRec]],
      result: TypeRec[Expr]
  ): EitherS[TypeRec[Expr]] = {
    val resultType = typeOf(result)
    for {
      retChain <- applyTypeArg(ops.pure, resultType, Kind.Star).flatMap(applyArg(_, result))
      chain <- steps.foldRight(Right(retChain): EitherS[TypeRec[Expr]]) { (step, accE) =>
        accE.flatMap { acc =>
          step match {
            case ContextStep.Bind(name, annotation, value) => for {
              bindA <- applyTypeArg(ops.flatMap, annotation, Kind.Star)
              bindR <- applyTypeArg(bindA, resultType, Kind.Star)
              bindValue <- applyArg(bindR, value)
              applied <- applyArg(bindValue, mkAbs(name, annotation, acc))
            } yield applied
            case ContextStep.Let(name, annotation, value) => Right(letT(name, typeOf(acc), annotation, value, acc))
            case ContextStep.Rec(bindings) => Right(letRecGroupT(bindings, typeOf(acc), acc))
          }
        }
      }
    } yield chain
  }

  private def applyArg(function: TypeRec[Expr], argument: TypeRec[Expr]): EitherS[TypeRec[Expr]] =
    destructArrow(typeOf(function)) match {
      case Some((from, to)) if Equivalence.alpha(from, typeOf(argument)) => Right(appT(to, function, argument))
      case Some((from, _)) => Left(CompileError.InContext(CompileError.TypeMismatch(from, typeOf(argument))))
      case None => Left(CompileError.InContext(CompileError.NotAFunction(typeOf(function))))
    }

  def applyTypeArg(function: TypeRec[Expr], argument: TypeRec[Type], argKind: Kind): EitherS[TypeRec[Expr]] =
    destructForAllK(typeOf(function)) match {
      case Some((variable, expectedKind, bodyType)) =>
        if (expectedKind == argKind) Right(tyAppT(Equivalence.normalize(substType(variable, argument, bodyType)), function, argument))
        else Left(CompileError.InContext(CompileError.KindMismatchInTypeApp(expectedKind, argKind, None)))
      case None => Left(CompileError.InContext(CompileError.NotPolymorphic(typeOf(function))))
    }
}
