package com.yuuki14202028

import java.nio.file.{Files, Paths}

@main
def main(args: String*): Unit = {
  val srcPath = args.headOption.map(Paths.get(_)).getOrElse(Paths.get("main.lam"))
  val asmPath = args.drop(1).headOption.map(Paths.get(_)).getOrElse(Paths.get("build/out.s"))
  val src = Files.readString(srcPath)

  val result = for {
    ast <- ParserAST.programParser.parseAll(src).left.map(CompileError.ParseFailure.apply)
    resolved <- ImportResolver.resolve(ast, srcPath)
    _ = println(eraseIndex(resolved).show)
    typed <- TAnalyser.validate(resolved)
    contextFree <- ContextDesugar.desugar(typed)
    desugared <- TraitEncoder.encode(contextFree)
    encoded = ChurchEncoder.encode(desugared)
    _ = println(eraseAnn(encoded).show)
  } yield encoded

  result match {
    case Left(err) => Console.err.println(err.render(srcPath.toString, cats.parse.LocationMap(src)))
    case Right(encoded) => {
      val asm = Generator.generate(encoded)
      val outDir = asmPath.getParent
      if (outDir != null) Files.createDirectories(outDir)
      Files.writeString(asmPath, asm)
    }
  }
}
