package com.yuuki14202028

import cats.syntax.all.*

import java.nio.file.{Files, Path, Paths}

/** import の解決。`import "path"` を、そのファイルの宣言の列に置き換える。
 *
 * top-level の import は、同じファイルを二度展開しない（循環していてもよい）。
 * module の中の import は、その module の中に宣言を展開する（module ごとに独立に展開するので、
 * 同じファイルを別の module にも top-level にも取り込める）。ただし、展開中のファイルを
 * module の中から取り込むと終わらないので、エラーにする。
 */
object ImportResolver {

  private def withLamExtension(path: String): String =
    if (path.endsWith(".lam")) path else s"$path.lam"

  private def resolvePath(baseDir: Path, importPath: String): Path = {
    val path =
      if (importPath.startsWith("std/")) Paths.get("stdlib").resolve(withLamExtension(importPath.stripPrefix("std/")))
      else baseDir.resolve(withLamExtension(importPath))
    path.normalize.toAbsolutePath
  }

  // seen: この並び（top-level または 1 つの module の中）ですでに展開したファイル。ancestors: 展開の途中にあるファイル
  private type Resolved = EitherS[(Vector[IndexedRec[Decl]], Set[Path])]

  private def resolveDecl(decl: IndexedRec[Decl], baseDir: Path, seen: Set[Path], ancestors: Set[Path]): Resolved =
    decl.project match {
      case AST.TopImport(importPath) =>
        val path = resolvePath(baseDir, importPath)
        if (seen.contains(path)) Right(Vector.empty -> seen)
        else resolveFile(path, seen + path, ancestors)
      case AST.TopModule(name, decls) =>
        // module の中は独立に展開する。外で展開済みのファイルも、この module の中にもう一度取り込める
        resolveDecls(decls.toVector, baseDir, Set.empty, ancestors, inModule = true).map { case (resolved, _) =>
          Vector(HCofree(decl.extract, AST.TopModule[IndexedRec, Decl](name, resolved))) -> seen
        }
      case AST.TopModified(modifier, inner) =>
        resolveDecl(inner, baseDir, seen, ancestors).map { case (resolved, nextSeen) =>
          resolved.map(d => HCofree(decl.extract, AST.TopModified[IndexedRec, Decl](modifier, d))) -> nextSeen
        }
      case _ => Right(Vector(decl) -> seen)
    }

  private def resolveFile(path: Path, seen: Set[Path], ancestors: Set[Path]): Resolved = for {
    src <- try Right(Files.readString(path)) catch {
      case e: Exception => Left(CompileError.ImportReadFailure(path, e))
    }
    ast <- ParserAST.programParser.parseAll(src).left.map(err => CompileError.ImportParseFailure(path, err))
    result <- ast.project match {
      case AST.Program(decls) => resolveDecls(decls.toVector, path.getParent, seen, ancestors + path, inModule = false)
    }
  } yield result

  private def resolveDecls(decls: Vector[IndexedRec[Decl]], baseDir: Path, seen: Set[Path], ancestors: Set[Path], inModule: Boolean): Resolved =
    decls.foldLeftM(Vector.empty[IndexedRec[Decl]] -> seen) {
      case ((resolved, currentSeen), decl) =>
        val cyclic = decl.project match {
          case AST.TopImport(importPath) if inModule => Option(resolvePath(baseDir, importPath)).filter(ancestors.contains)
          case _ => None
        }
        cyclic match {
          case Some(path) => Left(CompileError.ImportCycleInModule(path).at(decl.extract))
          case None => resolveDecl(decl, baseDir, currentSeen, ancestors).map { case (newDecls, nextSeen) =>
            (resolved ++ newDecls) -> nextSeen
          }
        }
    }

  def resolve(prog: IndexedRec[AST.Program.type], sourcePath: Path): EitherS[IndexedRec[AST.Program.type]] =
    prog.project match {
      case AST.Program(decls) =>
        val source = sourcePath.toAbsolutePath.normalize
        resolveDecls(decls.toVector, source.getParent, Set.empty, Set(source), inModule = false)
          .map { case (resolved, _) => programI(resolved) }
    }
}
