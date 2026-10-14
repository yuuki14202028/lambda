package com.yuuki14202028

/** 名前付きフィールドの脱糖。構成子が 1 つの data C(name: T)… に、フィールドを読む関数（アクセサ）を生成し、
 * 名前付きの構築 C[T̄](y = e₁, x = e₂) を位置の適用に直す。
 * アクセサの名前と型は型検査が Env に登録済みで（型の module のメンバー）、ここでは本体を作るだけ。
 *
 *   data D[ā] = { C(x: T)(y: U) }
 *     ↦  let D.x: ∀ā. D[ā] → T = Λā. λ$record: D[ā]. match $record { C($x)($y) => $x }
 *        let D.y: ∀ā. D[ā] → U = …
 *
 * s.x は型検査が D.x の呼び出しと決める（SelectDesugar）ので、アクセサは普通の関数でよい。
 * 名前付きの構築は、書かれた順に値を評価するよう、順番がフィールドの順と違えば let で先に束縛する:
 *
 *   C[T̄](y = e₁, x = e₂)  ↦  let $named_0 = e₁ in let $named_1 = e₂ in C[T̄]($named_1)($named_0)
 *
 * ModuleDesugar の後に走る（宣言は平らで、抽象型は表現に読み替え済み）。
 */
object RecordDesugar {

  private def invariant(msg: String): Nothing = sys.error(s"Compiler invariant violation: $msg")

  def desugar(program: TypeRec[AST.Program.type]): TypeRec[AST.Program.type] = program.project match {
    case AST.Program(decls) =>
      val env = program.extract match {
        case ProgramAnn(e) => e
      }
      val constructed = programT(decls, env).cata(constructAlg)
      constructed.project match {
        case AST.Program(desugared) => programT(desugared.flatMap(decl => decl +: accessors(env, decl)), env)
      }
  }

  private val constructAlg: Algebra[TypedAST, TypeRec] = [x] => he => (he.ann, he.ast) match {
    case (ExprAnn(_), AST.NamedApp(constructor, arguments, positions)) =>
      val byField = arguments.zip(positions).sortBy(_._2).map(_._1.value)
      if (positions == positions.sorted) byField.foldLeft(constructor)(mkApp)
      else {
        val bound = arguments.indices.map(index => Variable(s"$$named_$index"))
        val inFieldOrder = positions.zipWithIndex.sortBy(_._1).map { case (_, index) => varrType(bound(index), typeOf(arguments(index).value)) }
        val applied = inFieldOrder.foldLeft(constructor)(mkApp)
        arguments.zip(bound).foldRight(applied) { case ((argument, variable), body) =>
          letT(variable, typeOf(body), typeOf(argument.value), argument.value, body)
        }
      }
    case (ann, node) => HCofree(ann, node)
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
