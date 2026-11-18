package com.yuuki14202028

import java.nio.file.{Files, Path, Paths}

/** import の解決。`import "path"` を、そのファイルの宣言の並びに置き換える。
 *
 * `std/…` は同梱の stdlib/ を、それ以外は取り込む側のファイルからの相対パスを読む。
 * top-level では、同じファイルは一度だけ取り込む。module の中の import は、その module の中に取り込む
 * （module ごとに別に数えるので、同じファイルを別の module にも top-level にも取り込める）。
 */
object Imports {
  import Surface.Decl

  private def fail(error: CompileError): Nothing = throw CompileFailure(error)

  def expand(declarations: List[Decl], file: String): List[Decl] = {
    val source = Paths.get(file).toAbsolutePath.normalize
    new Imports().expand(declarations, source.getParent, Set(source), inModule = false)
  }

  private def locate(directory: Path, written: String): Path = {
    val name = if (written.endsWith(".lam")) written else s"$written.lam"
    val path = if (written.startsWith("std/")) Paths.get("stdlib").resolve(name.stripPrefix("std/")) else directory.resolve(name)
    path.toAbsolutePath.normalize
  }
}

private final class Imports {
  import Imports.*
  import Surface.Decl

  /** seen: この並び（top-level か、1 つの module の中）ですでに取り込んだファイル。decls を順に処理しながら増える */
  private var seen = Set.empty[Path]

  /** ancestors: 取り込みの途中にあるファイル */
  def expand(declarations: List[Decl], directory: Path, ancestors: Set[Path], inModule: Boolean): List[Decl] = declarations.flatMap {
    case Decl.Import(written, span) =>
      val path = locate(directory, written)
      if (seen(path)) Nil
      // 取り込みの途中のファイルを module の中から取り込むと、終わらない
      else if (inModule && ancestors(path)) fail(CompileError.ImportCycle(written, span))
      else {
        seen += path
        val text = try Files.readString(path) catch { case _: java.io.IOException => fail(CompileError.ImportNotFound(display(path), span)) }
        expand(Parser.parse(text, display(path)), path.getParent, ancestors + path, inModule)
      }
    case Decl.Module(name, members, span) => List(Decl.Module(name, new Imports().expand(members, directory, ancestors, inModule = true), span))
    case Decl.Private(inner, span) => expand(List(inner), directory, ancestors, inModule).map(Decl.Private(_, span))
    case other => List(other)
  }

  /** エラーの表示に使うファイル名（作業ディレクトリからの相対パス） */
  private def display(path: Path): String = {
    val base = Paths.get("").toAbsolutePath
    if (path.startsWith(base)) base.relativize(path).toString else path.toString
  }
}
