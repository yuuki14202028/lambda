package com.yuuki14202028

import java.nio.file.{Files, Path, Paths}

object Compiler {

  /** 型検査の後の脱糖。順序に意味がある: context の束縛のパターンは ContextDesugar が match にするので、
   * PatternDesugar はその後に走る。演算子とドット記法は制約の根拠を含む関数呼び出しになるので、TraitEncoder より前 */
  def desugar(typed: TypeRec[AST.Program.type]): TypeRec[AST.Program.type] =
    ChurchEncoder.encode(TraitEncoder.encode(OperatorDesugar.desugar(SelectDesugar.desugar(
      PatternDesugar.desugar(ContextDesugar.desugar(Deriver.desugar(RecordDesugar.desugar(ModuleDesugar.desugar(typed)))))
    ))))

  /** ソースをアセンブリへコンパイルする。trace には脱糖前後のプログラムの表示が渡される */
  def compile(src: String, srcPath: Path, trace: String => Unit = _ => ()): Either[CompileError, String] = for {
    ast <- ParserAST.programParser.parseAll(src).left.map(CompileError.ParseFailure.apply)
    resolved <- ImportResolver.resolve(ast, srcPath)
    _ = trace(eraseIndex(resolved).show)
    typed <- TAnalyser.validate(resolved)
    encoded = desugar(typed)
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
