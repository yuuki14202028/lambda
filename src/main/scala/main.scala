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

  Compiler.compile(src, srcPath, println) match {
    case Left(err) => Console.err.println(err.render(srcPath.toString, cats.parse.LocationMap(src)))
    case Right(asm) =>
      val outDir = asmPath.getParent
      if (outDir != null) Files.createDirectories(outDir)
      Files.writeString(asmPath, asm)
  }
}
