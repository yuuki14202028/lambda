package com.yuuki14202028

import java.nio.file.{Files, Path, Paths}

/** docs/language.md のコード例が、書いてあるとおりにコンパイルできることを確かめる。
 * ```lam で始まるコード例を 1 つずつ、先頭に import "std/prelude" を付けてコンパイルする（main がなければ足す）。
 */
class DocSuite extends munit.FunSuite {

  private val document: Path = Paths.get("docs/language.md")

  // (開始行, コード)
  private def snippets: Seq[(Int, String)] = {
    val lines = Files.readString(document).linesIterator.toVector
    val starts = lines.zipWithIndex.collect { case (line, index) if line.trim == "```lam" => index }
    starts.map { start =>
      val body = lines.drop(start + 1).takeWhile(_.trim != "```")
      (start + 1, body.mkString("\n"))
    }
  }

  private def compile(code: String): Either[String, Unit] = {
    val withMain = if (code.linesIterator.exists(_.startsWith("let main("))) code else code + "\n\nlet main(): i32 = 0\n"
    val src = "import \"std/prelude\"\n\n" + withMain
    val result = for {
      ast <- ParserAST.programParser.parseAll(src).left.map(CompileError.ParseFailure.apply)
      resolved <- ImportResolver.resolve(ast, Paths.get("docs/snippet.lam"))
      typed <- TAnalyser.validate(resolved)
    } yield Compiler.desugar(typed)
    result.left.map(Diagnostic.render(_, Sources("snippet.lam", src))).map(_ => ())
  }

  test("文書にコード例がある") {
    assert(snippets.length > 20, clue(snippets.length))
  }

  snippets.foreach { case (line, code) =>
    test(s"docs/language.md の ${line} 行目のコード例") {
      compile(code) match {
        case Right(()) => ()
        case Left(error) => fail(s"$error\n\n$code")
      }
    }
  }
}
