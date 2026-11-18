package com.yuuki14202028

/** テストから .lam のソースを扱うための入口 */
object Lam {
  private val file = "test.lam"

  /** コンパイルエラー（なければ空）。表示のために付けた情報（関係する位置・手がかり）は外す */
  def errors(source: String): List[CompileError] = Compiler.compile(source, file).fold(_.map(_.core), _ => Nil)

  /** エラーの 1 行の説明 */
  def messages(source: String): List[String] = errors(source).map(_.message)

  /** コンパイルしてインタプリタで実行する。コンパイルできなければ、エラーの表示を付けて失敗する */
  def run(source: String)(using munit.Location): Interpreter.Result = Compiler.compile(source, file) match {
    case Right(program) => Interpreter.run(program)
    case Left(found) => munit.Assertions.fail(Diagnostics.render(found, file, source))
  }

  def exitCode(source: String)(using munit.Location): Int = run(source).exitCode
  def output(source: String)(using munit.Location): String = run(source).output
}
