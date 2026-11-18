package com.yuuki14202028

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** エラー表示のテスト。src/test/diagnostics の各 .lam をコンパイルし、表示されるエラーを同名の .txt と比べる。
 *
 * 表示を変えたときは、UPDATE_DIAGNOSTICS=1 を付けて DiagnosticSuite を実行して .txt を書き直し、差分を確認する。
 */
class DiagnosticSuite extends munit.FunSuite {

  private val directory: Path = Paths.get("src/test/diagnostics")
  private val update: Boolean = sys.env.contains("UPDATE_DIAGNOSTICS")

  private def diagnose(path: Path): String = {
    val source = Files.readString(path)
    Compiler.compile(source, path.toString).fold(Diagnostics.render(_, path.toString, source), _ => "(no error)")
      .replace(s"$directory/", "")
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
}
