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
      case Left(err) => fail(err.render(source, cats.parse.LocationMap(src)))
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

  program("main.lam", "result = Just(9)\n")
  // 失敗したテストの番号が終了コードになる
  program("stdlib_test.lam", "")
  program("src/test/lam/traverse.lam", "6\n-1\n")
  program("src/test/lam/impl_rec.lam", "3\n4\n")
  program("src/test/lam/tail_calls.lam", "1000000\n2050477040\n1\n42\n7\n")
  program("src/test/lam/mutual_rec.lam", "1\n0\n0\n10\n4\n7\n100\n")
  program("src/test/lam/modules.lam", "2\n10\n1\n2\n70\n7\n204\n9\n")
}
