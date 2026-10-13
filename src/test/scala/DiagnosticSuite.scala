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

  private def errorsOf(src: String): Seq[String] =
    ParserAST.programParser.parseAll(src).left.map(CompileError.ParseFailure.apply).flatMap(TAnalyser.validate)
      .fold(_.all.map(_.render), _ => Seq.empty)

  test("互いに無関係な宣言のエラーは、ソースの順にすべて報告される") {
    val src =
      """let a: i32 = true
        |let b: bool = 1
        |let main(): i32 = 0
        |""".stripMargin
    assertEquals(errorsOf(src), Seq("Type mismatch: expected i32, actual bool", "Type mismatch: expected bool, actual i32"))
  }

  test("失敗した宣言を使う側のエラーは報告されない") {
    val src =
      """let a: i32 = true
        |let b: i32 = a
        |let c: i32 = b
        |let main(): i32 = c
        |""".stripMargin
    assertEquals(errorsOf(src), Seq("Type mismatch: expected i32, actual bool"))
  }

  test("失敗した data / trait / impl / module を使う側のエラーも報告されない") {
    val src =
      """data Broken = { MkBroken(Missing) }
        |let b: Broken = MkBroken(1)
        |trait Show[a] { def show(x: a): Missing }
        |impl Show[i32] { def show = λx: i32. x }
        |trait Size[a] { def size(x: a): i32 }
        |impl Size[i32] { def size = λx: i32. true }
        |let n: i32 = size[i32](1)
        |module M { let x: i32 = true }
        |let y: i32 = M.x
        |let main(): i32 = 0
        |""".stripMargin
    assertEquals(errorsOf(src), Seq(
      "Type variable Missing is not defined",
      "Type variable Missing is not defined",
      "Method type mismatch in impl Size[i32]: size must have type i32, actual bool",
      "Type mismatch: expected i32, actual bool"
    ))
  }

  test("失敗した宣言と無関係なエラーは、その後でも報告される") {
    val src =
      """let a: i32 = true
        |let b: i32 = a
        |let c: i32 = nowhere
        |let main(): i32 = 0
        |""".stripMargin
    assertEquals(errorsOf(src), Seq("Type mismatch: expected i32, actual bool", "Variable nowhere is not defined"))
  }

  test("main が失敗していても、main がないとは報告しない") {
    assertEquals(errorsOf("let main(): i32 = true\n"), Seq("Type mismatch: expected i32, actual bool"))
  }

  test("module の中のエラーも、すべて報告される") {
    val src =
      """module M {
        |  let a: i32 = true
        |  let b: bool = 1
        |  let c: i32 = a
        |}
        |let main(): i32 = 0
        |""".stripMargin
    assertEquals(errorsOf(src), Seq("Type mismatch: expected i32, actual bool", "Type mismatch: expected bool, actual i32"))
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
