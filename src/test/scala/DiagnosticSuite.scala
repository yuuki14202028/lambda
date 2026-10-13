package com.yuuki14202028

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** エラー表示のテスト。src/test/diagnostics の各 .lam をコンパイルし、表示されるエラーを同名の .txt と比べる。
 *
 * 表示を変えたときは、UPDATE_DIAGNOSTICS=1 を付けて DiagnosticSuite を実行して で .txt を書き直し、差分を確認する。
 */
class DiagnosticSuite extends munit.FunSuite {

  private val directory: Path = Paths.get("src/test/diagnostics")
  private val update: Boolean = sys.env.contains("UPDATE_DIAGNOSTICS")

  private def diagnose(path: Path): String = {
    val src = Files.readString(path)
    val result = for {
      ast <- ParserAST.programParser.parseAll(src).left.map(CompileError.ParseFailure.apply)
      resolved <- ImportResolver.resolve(ast, path)
      typed <- TAnalyser.validate(resolved)
    } yield typed
    result.fold(Diagnostic.render(_, Sources(path.getFileName.toString, src)), _ => "(no error)")
  }

  private val cases: Seq[Path] = Files.list(directory).iterator().asScala.filter(_.toString.endsWith(".lam")).toSeq.sortBy(_.toString)

  cases.foreach { path =>
    val name = path.getFileName.toString.stripSuffix(".lam")
    test(name) {
      val expectedPath = directory.resolve(s"$name.txt")
      val actual = diagnose(path) + "\n"
      if (update) Files.writeString(expectedPath, actual)
      assert(Files.exists(expectedPath), s"missing $expectedPath (run with UPDATE_DIAGNOSTICS=1 to create it)")
      assertNoDiff(actual, Files.readString(expectedPath))
    }
  }

  test("エラーのあるプログラムだけを集めている") {
    cases.foreach(path => assert(diagnose(path) != "(no error)", clue(path)))
  }

  test("色を付けても、色を除けば同じ表示になる") {
    val path = directory.resolve("type_annotation.lam")
    val src = Files.readString(path)
    val error = ParserAST.programParser.parseAll(src).left.map(CompileError.ParseFailure.apply).flatMap(TAnalyser.validate).left.toOption.get
    val sources = Sources("type_annotation.lam", src)
    val colored = Diagnostic.render(error, sources, color = true)
    assert(colored.contains("\u001b["), clue(colored))
    assertEquals(colored.replaceAll("\u001b\\[[0-9;]*m", ""), Diagnostic.render(error, sources))
  }
}
