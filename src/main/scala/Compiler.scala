package com.yuuki14202028

import java.nio.file.{Files, Paths}

/** ソースから、型のないラムダ式まで:
 *
 *   字句解析 → 構文解析 (Surface) → import の展開 → 名前解決 (Resolved) → 型検査 (Typed)
 *     → 糖衣を外す (Core。ここまで型つきで、CoreCheck が検査する) → ラムダへ符号化 (Lambda)
 *
 * その先は、インタプリタ（Interpreter）か、ARM64 のアセンブリ（Generator）。
 */
object Compiler {

  /** file はソースのファイル名（import の相対パスの基準と、エラーの表示に使う） */
  def check(source: String, file: String = "main.lam"): Either[List[CompileError], Typed.Program] =
    try {
      val (nameErrors, resolved) = Resolve.program(Imports.expand(Parser.parse(source, file), file))
      // 名前のエラーがあっても、解決できた宣言の型検査は行って、エラーをまとめて報告する
      Declarations.program(resolved) match {
        case Right(typed) if nameErrors.isEmpty => Right(typed)
        case Right(_) => Left(nameErrors)
        case Left(typeErrors) => Left((nameErrors ++ typeErrors).sortBy(error => (error.span.file, error.span.start)))
      }
    } catch { case failure: CompileFailure => Left(failure.errors) }

  def core(source: String, file: String = "main.lam"): Either[List[CompileError], Core.Program] = check(source, file).map { typed =>
    val lowered = Lower.program(typed)
    CoreCheck.program(lowered)
    lowered
  }

  def compile(source: String, file: String = "main.lam"): Either[List[CompileError], Lambda.Program] = core(source, file).map(Encode.program)
}

/** run <src.lam>: コンパイルしてインタプリタで実行する。run <src.lam> <out.s>: ARM64 のアセンブリを書き出す */
@main
def main(args: String*): Unit = {
  val path = args.headOption.getOrElse("main.lam")
  val source = Files.readString(Paths.get(path))
  Compiler.compile(source, path) match {
    case Left(errors) =>
      Console.err.println(Diagnostics.render(errors, path, source))
      sys.exit(1)
    case Right(program) => args.lift(1) match {
      case Some(output) =>
        Option(Paths.get(output).getParent).foreach(Files.createDirectories(_))
        Files.writeString(Paths.get(output), Generator.generate(program))
      case None =>
        val result = Interpreter.run(program)
        print(result.output)
        sys.exit(result.exitCode)
    }
  }
}
