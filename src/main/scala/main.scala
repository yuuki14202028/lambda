package com.yuuki14202028

import java.nio.file.{Files, Paths}

@main
def main(): Unit = {
  val srcPath = Paths.get("main.lam")
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
      val outDir = Paths.get("build")
      Files.createDirectories(outDir)
      Files.writeString(outDir.resolve("out.s"), asm)
  }
}
