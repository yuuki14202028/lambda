package com.yuuki14202028

import java.nio.file.{Files, Paths}

@main
def main(args: String*): Unit = {
  val srcPath = args.headOption.map(Paths.get(_)).getOrElse(Paths.get("main.lam"))
  val asmPath = args.drop(1).headOption.map(Paths.get(_)).getOrElse(Paths.get("build/out.s"))
  val src = Files.readString(srcPath)

  ParserAST.programParser.parseAll(src) match {
    case Left(err) =>
      Console.err.println(s"Parse error: $err")
      sys.exit(1)

    case Right(ast) =>
      println(ast.show)
      val typed = Analyser.validate(ast) match {
        case Left(err) =>
          Console.err.println(s"Type error: $err")
          sys.exit(1)
        case Right(typed) => typed
      }
      val encoded = ChurchEncoder.encode(typed) match {
        case Left(err) =>
          Console.err.println(s"Encode error: $err")
          sys.exit(1)
        case Right(encoded) => encoded
      }
      println(eraseAnn(encoded).show)
      val asm = Generator.generate(encoded)
      val outDir = asmPath.getParent
      if (outDir != null) Files.createDirectories(outDir)
      Files.writeString(asmPath, asm)
  }
}
