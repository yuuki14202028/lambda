package com.yuuki14202028

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*

/** .lam をコンパイル → clang でリンク → 実行し、標準出力と終了コードを確かめる。
 * ARM64 macOS と clang が必要なので、それ以外の環境ではスキップする。
 */
class EndToEndSuite extends munit.FunSuite {

  override val munitTimeout: Duration = 2.minutes

  private val supported: Boolean =
    System.getProperty("os.name").toLowerCase.contains("mac") && System.getProperty("os.arch") == "aarch64"

  private val workDir: Path = Files.createTempDirectory("lambda-e2e")

  private def exec(command: String*): (Int, String) = {
    val process = new ProcessBuilder(command*).redirectErrorStream(true).start()
    val output = new String(process.getInputStream.readAllBytes(), "UTF-8")
    if (!process.waitFor(60, TimeUnit.SECONDS)) {
      process.destroyForcibly()
      fail(s"timed out: ${command.mkString(" ")}")
    }
    (process.exitValue(), output)
  }

  private def run(source: String): (Int, String) = {
    val srcPath = Paths.get(source)
    val name = srcPath.getFileName.toString.stripSuffix(".lam")
    val src = Files.readString(srcPath)
    val asm = Compiler.compile(src, srcPath) match {
      case Right(asm) => asm
      case Left(err) => fail(Diagnostic.render(err, Sources(source, src)))
    }
    val asmPath = workDir.resolve(s"$name.s")
    val binPath = workDir.resolve(name)
    Files.writeString(asmPath, asm)
    val (linkExit, linkOutput) = exec("clang", "-arch", "arm64", asmPath.toString, "runtime/ffi.c", "-o", binPath.toString)
    assertEquals(linkExit, 0, clue(linkOutput))
    exec(binPath.toString)
  }

  private def program(source: String, stdout: String, exitCode: Int = 0)(using munit.Location): Unit =
    test(source) {
      assume(supported, "requires ARM64 macOS")
      assertEquals(run(source), (exitCode, stdout))
    }

  // examples/ のサンプルは、期待する出力を同名の .expected に置く
  private def example(name: String)(using munit.Location): Unit =
    test(s"examples/$name.lam") {
      assume(supported, "requires ARM64 macOS")
      assertEquals(run(s"examples/$name.lam"), (0, Files.readString(Paths.get(s"examples/$name.expected"))))
    }

  example("calculator")
  example("transformer")
  example("ranking")
  example("json")

  program(
    "main.lam",
    """v + v = <2, 4>
      |double = <2, 4>, 42, abab
      |equal = true
      |compute = Some(9), None
      |classify = [zero, negative, small, large]
      |squares = [1, 4, 9, 16], sum = 30
      |dot = 100
      |""".stripMargin
  )
  // 失敗したテストの番号が終了コードになる
  program("stdlib_test.lam", "")
  program("src/test/lam/traverse.lam", "6\n-1\n7\n-1\n6\n-7\n4\n100\n-1\n")
  program("src/test/lam/operators.lam", "14\n5\n26\n4\n321\n512\n18\n321\n6\n1\n12\n24\n")
  program("src/test/lam/dot_notation.lam", "3\n1\n60\n1\n6\n6\n9\n3\n")
  program("src/test/lam/impl_rec.lam", "3\n4\n")
  program("src/test/lam/tail_calls.lam", "1000000\n2050477040\n1\n42\n7\n")
  program("src/test/lam/mutual_rec.lam", "1\n0\n0\n10\n4\n7\n100\n")
  program("src/test/lam/quantified.lam", "6\n18\n60\n6\n42\n7\n6\n")
  program("src/test/lam/patterns.lam", "100\n101\n42\n2\n0\n12\n5\n-1\n32\n1000\n9\n-1\n-2\n2\n0\n7\n" +
    "10\n20\n30\n40\n1\n2\n0\n1\n2\n3\n3\n7\n8\n1\n2\n3\n")
  program("src/test/lam/default_methods.lam", "1\n0\n0\n1\n0\n1\n9\n1\n")
  program("src/test/lam/explicit_instances.lam", "1\n2\n1\n2\n3\n9\n0\n2\n9\n2\n")
  program("src/test/lam/show.lam", "42\nfalse\nx\ntext\n[1, 2, 3]\nSome((1, green))\nRight([red])\nsum of [1, 2, 3] is 6\nno newline\n")
  program("src/test/lam/modules.lam", "2\n10\n1\n2\n70\n7\n204\n9\n5\n8\n")
}
