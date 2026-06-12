package com.yuuki14202028

import cats.syntax.all.*

import java.nio.file.{Files, Path, Paths}

object ImportResolver {

  private def withLamExtension(path: String): String =
    if (path.endsWith(".lam")) path else s"$path.lam"

  private def resolvePath(baseDir: Path, importPath: String): Path = {
    val path =
      if (importPath.startsWith("std/")) Paths.get("stdlib").resolve(withLamExtension(importPath.stripPrefix("std/")))
      else baseDir.resolve(withLamExtension(importPath))
    path.normalize.toAbsolutePath
  }

  private def resolveDecl(decl: Rec[Decl], baseDir: Path, seen: Set[Path]): EitherS[(Vector[Rec[Decl]], Set[Path])] =
    decl.unfix match {
      case AST.TopImport(importPath) => {
        val path = resolvePath(baseDir, importPath)
        if (seen.contains(path)) Right(Vector.empty -> seen)
        else resolveFile(path, seen + path)
      }
      case _ => Right(Vector(decl) -> seen)
    }

  private def resolveFile(path: Path, seen: Set[Path]): EitherS[(Vector[Rec[Decl]], Set[Path])] = for {
    src <- try Right(Files.readString(path)) catch {
      case e: Exception => Left(CompileError.ImportReadFailure(path, e))
    }
    ast <- ParserAST.programParser.parseAll(src).left.map(err => CompileError.ImportParseFailure(path, err))
    result <- ast.unfix match {
      case AST.Program(decls) => resolveDecls(decls.toVector, path.getParent, seen)
    }
  } yield result

  private def resolveDecls(decls: Vector[Rec[Decl]], baseDir: Path, seen: Set[Path]): EitherS[(Vector[Rec[Decl]], Set[Path])] =
    decls.foldLeftM(Vector.empty[Rec[Decl]] -> seen) {
      case ((resolved, currentSeen), decl) =>
        resolveDecl(decl, baseDir, currentSeen).map { case (newDecls, nextSeen) =>
          (resolved ++ newDecls) -> nextSeen
        }
    }

  def resolve(prog: Rec[AST.Program.type], sourcePath: Path): EitherS[Rec[AST.Program.type]] =
    prog.unfix match {
      case AST.Program(decls) =>
        resolveDecls(decls.toVector, sourcePath.toAbsolutePath.getParent, Set.empty)
          .map { case (resolved, _) => program(resolved) }
    }
}
