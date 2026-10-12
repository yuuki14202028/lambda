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
    case AST.Context(_, bindings, result, ops) =>
      val resolved = ops.getOrElse(invariant("context block reached ContextDesugar without resolved methods"))
      desugarContext(resolved, bindings, result)
        .fold(err => invariant(s"context block failed to desugar after validation: ${err.render}"), identity)
    case node => HCofree(he.ann, node)
  }

  // 型検査からも呼ばれ、ブロック全体の型と束縛の型エラーをここで確定する
  def desugarContext(
      ops: ContextOps[TypeRec],
      bindings: Seq[ContextBinding[TypeRec]],
      result: TypeRec[Expr]
  ): EitherS[TypeRec[Expr]] = {
    val resultType = typeOf(result)
    for {
      retChain <- applyTypeArg(ops.pure, resultType, Kind.Star).flatMap(applyArg(_, result))
      chain <- bindings.foldRight(Right(retChain): EitherS[TypeRec[Expr]]) { (b, accE) =>
        accE.flatMap { acc =>
          if (b.monadic) for {
            bindA <- applyTypeArg(ops.flatMap, b.annotation, Kind.Star)
            bindR <- applyTypeArg(bindA, resultType, Kind.Star)
            bindValue <- applyArg(bindR, b.value)
            applied <- applyArg(bindValue, mkAbs(b.name, b.annotation, acc))
          } yield applied
          else if (b.recursive) Right(letRecT(b.name, typeOf(acc), b.annotation, b.value, acc))
          else Right(letT(b.name, typeOf(acc), b.annotation, b.value, acc))
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
