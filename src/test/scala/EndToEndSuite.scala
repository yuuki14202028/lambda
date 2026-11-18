package com.yuuki14202028

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*

/** .lam のプログラムを最後まで通し、標準出力と終了コードを確かめる。
 * インタプリタで実行し、ARM64 macOS では、アセンブリを clang でリンクして実行した結果も同じであることを確かめる。
 */
class EndToEndSuite extends munit.FunSuite {

  override val munitTimeout: Duration = 3.minutes

  private val native: Boolean =
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

  private def compile(file: String): Lambda.Program = {
    val source = Files.readString(Paths.get(file))
    Compiler.compile(source, file) match {
      case Right(program) => program
      case Left(errors) => fail(Diagnostics.render(errors, file, source))
    }
  }

  private def runNative(file: String, program: Lambda.Program): (Int, String) = {
    val name = Paths.get(file).getFileName.toString.stripSuffix(".lam")
    val assembly = workDir.resolve(s"$name.s")
    val binary = workDir.resolve(name)
    Files.writeString(assembly, Generator.generate(program))
    val (linkExit, linkOutput) = exec("clang", "-arch", "arm64", assembly.toString, "runtime/ffi.c", "-o", binary.toString)
    assertEquals(linkExit, 0, clue(linkOutput))
    exec(binary.toString)
  }

  private def program(file: String, stdout: String, exitCode: Int = 0)(using munit.Location): Unit = {
    test(s"$file (interpreter)") {
      val result = Interpreter.run(compile(file))
      assertEquals((result.exitCode, result.output), (exitCode, stdout))
    }
    test(s"$file (native)") {
      assume(native, "requires ARM64 macOS")
      assertEquals(runNative(file, compile(file)), (exitCode, stdout))
    }
  }

  // examples/ のサンプルは、期待する出力を同名の .expected に置く
  private def example(name: String)(using munit.Location): Unit =
    program(s"examples/$name.lam", Files.readString(Paths.get(s"examples/$name.expected")))

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
  program("src/test/lam/stdlib_extra.lam",
    "6 24 2\nSome(9) None Some(2)\n([3, 4, 5], [1, 2])\n[a, bb, ccc] Some(2) [1, 2, 3] [1, 0, 2, 0, 3]\n" +
      "[a|b||c] [hi] true Some(3) a+b+c\nababab HELLO hello cba true\n" +
      "{a: 1, b: 2, c: 3} 3 Some(2) None true\n{b: 2, c: 3, d: 4} {a: 10, b: 20, c: 30} [a, b, c] [1, 2, 3] {a: 101, b: 2, c: 3}\n" +
      "200 8 Some(22500) 100\n{1, 2, 3} 3 true {2, 3, 5} {1, 2, 3, 9} {2, 3} {2, 3}\n" +
      "Ok(2) Err(boom) Ok(6) 0 true None Err(4)\nOk(2) Err(none)\n" +
      "{1, 2, 3} {a: 3, b: 2} empty one 5 two 3 many\n(1, (true, x)) x1true (1, c, 2.5)\n")
  program("src/test/lam/derive_values.lam",
    "Green\ntrue false true false true\nMkStudent(name = alice, math = 90)\ntrue false true true\n" +
      "[MkStudent(name = alice, math = 90), MkStudent(name = bob, math = 70)]\nNode(Leaf, 1, Node(Leaf, 2, Leaf))\n" +
      "true false true false\nMkWrap([1, 2], Some(3))\ntrue\ntrue false\n")
  program("src/test/lam/records.lam", "3\n4\n3\n7\n24\n3\n7\n4\n2\n3\n9\n1\n12\n5\n6\n6\n8\n1\n")
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
