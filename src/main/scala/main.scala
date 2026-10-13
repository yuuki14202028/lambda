package com.yuuki14202028

import java.nio.file.{Files, Path, Paths}

object Compiler {

  /** ソースをアセンブリへコンパイルする。trace には脱糖前後のプログラムの表示が渡される */
  def compile(src: String, srcPath: Path, trace: String => Unit = _ => ()): Either[CompileError, String] = for {
    ast <- ParserAST.programParser.parseAll(src).left.map(CompileError.ParseFailure.apply)
    resolved <- ImportResolver.resolve(ast, srcPath)
    _ = trace(eraseIndex(resolved).show)
    typed <- TAnalyser.validate(resolved)
    encoded = ChurchEncoder.encode(TraitEncoder.encode(ContextDesugar.desugar(Deriver.desugar(PatternDesugar.desugar(ModuleDesugar.desugar(typed))))))
    _ = trace(eraseAnn(encoded).show)
  } yield Generator.generate(encoded)
}

@main
def main(args: String*): Unit = {
  val srcPath = args.headOption.map(Paths.get(_)).getOrElse(Paths.get("main.lam"))
  val asmPath = args.drop(1).headOption.map(Paths.get(_)).getOrElse(Paths.get("build/out.s"))
  val src = Files.readString(srcPath)

  // 脱糖前後のプログラムの表示は、コンパイルできたときだけ出す（エラーの表示を流さないため）
  val trace = Vector.newBuilder[String]
  Compiler.compile(src, srcPath, trace += _) match {
    case Left(err) =>
      // 端末に出すときだけ色を付ける
      val color = System.console() != null && sys.env.get("NO_COLOR").isEmpty
      Console.err.println(Diagnostic.render(err, Sources(srcPath.toString, src), color))
      sys.exit(1)
    case Right(asm) =>
      trace.result().foreach(println)
      val outDir = asmPath.getParent
      if (outDir != null) Files.createDirectories(outDir)
      Files.writeString(asmPath, asm)
  }
}
