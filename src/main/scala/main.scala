package com.yuuki14202028

import java.nio.file.{Files, Paths}

@main
def main(args: String*): Unit = {
  val srcPath = args.headOption.map(Paths.get(_)).getOrElse(Paths.get("main.lam"))
  val asmPath = args.drop(1).headOption.map(Paths.get(_)).getOrElse(Paths.get("build/out.s"))
  val src = Files.readString(srcPath)

  val result = for {
    ast <- ParserAST.programParser.parseAll(src).left.map(err => s"Parse error: $err")
    resolved <- ImportResolver.resolve(ast, srcPath)
    _ = println(resolved.show)
    typed <- TAnalyser.validate(resolved).left.map(err => s"Type error: $err")
    encoded <- ChurchEncoder.encode(typed).left.map(err => s"Encode error: $err")
    _ = println(eraseAnn(encoded).show)
  } yield encoded

  result match {
    case Left(err) => Console.err.println(err)
    case Right(encoded) => {
      val asm = Generator.generate(encoded)
      val outDir = asmPath.getParent
      if (outDir != null) Files.createDirectories(outDir)
      Files.writeString(asmPath, asm)
    }
  }
}
