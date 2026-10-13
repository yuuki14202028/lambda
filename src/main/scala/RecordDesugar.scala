package com.yuuki14202028

/** 名前付きフィールドの脱糖。構成子が 1 つの data C(name: T)… に、フィールドを読む関数（アクセサ）を生成する。
 * アクセサの名前と型は型検査が Env に登録済みで（型の module のメンバー）、ここでは本体を作るだけ。
 *
 *   data D[ā] = { C(x: T)(y: U) }
 *     ↦  let D.x: ∀ā. D[ā] → T = Λā. λ$record: D[ā]. match $record { C($x)($y) => $x }
 *        let D.y: ∀ā. D[ā] → U = …
 *
 * s.x は型検査が D.x の呼び出しと決める（SelectDesugar）ので、アクセサは普通の関数でよい。
 * ModuleDesugar の後に走る（宣言は平らで、抽象型は表現に読み替え済み）。
 */
object RecordDesugar {

  private def invariant(msg: String): Nothing = sys.error(s"Compiler invariant violation: $msg")

  def desugar(program: TypeRec[AST.Program.type]): TypeRec[AST.Program.type] = program.project match {
    case AST.Program(decls) =>
      val env = program.extract match {
        case ProgramAnn(e) => e
      }
      programT(decls.flatMap(decl => decl +: accessors(env, decl)), env)
  }

  private def accessors(env: Env, decl: TypeRec[Decl]): Seq[TypeRec[Decl]] = decl.project match {
    case AST.TopData(variable, _, constructors) if constructors.exists(_.named) => dataAccessors(env, variable, constructors)
    // rec の組は同じ種類の宣言だけを持つので、アクセサは組の後ろに置く
    case AST.TopRec(members) => members.flatMap(accessors(env, _))
    case _ => Seq.empty
  }

  private def dataAccessors(env: Env, owner: TypeVariable, constructors: Seq[DataConstructor[TypeRec]]): Seq[TypeRec[Decl]] = {
    val dataDef = env.dataTypes.getOrElse(owner, invariant(s"data type ${owner.name} is not defined"))
    val (written, constructor) = (constructors, dataDef.constructors) match {
      case (Seq(w), Seq(c)) => (w, c)
      case _ => invariant(s"data type ${owner.name} has named fields but not exactly one constructor")
    }
    val receiverType = applyTypeConstructor(owner, dataDef.paramVars.map(typeVarT))
    val record = Variable("$record")
    val binders = constructor.fields.indices.map(index => Variable(s"$$field_$index"))
    written.names.zipWithIndex.collect { case (Some(field), index) =>
      val read = matchExprT(
        constructor.fields(index),
        varrType(record, receiverType),
        Seq(MatchCase[TypeRec](constructor.name, binders, varrType(binders(index), constructor.fields(index))))
      )
      val value = dataDef.params.foldRight(mkAbs(record, receiverType, read)) { case ((v, k), body) => mkTyAbs(v, k, body) }
      topLetT(fieldAccessorName(owner, field), typeOf(value), value)
    }
  }
}
