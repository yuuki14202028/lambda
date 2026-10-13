package com.yuuki14202028

/** module の脱糖。名前は型検査が完全な名前（M.x）に解決済みなので、ここでは
 *   - module / private / opaque / open を外して、宣言を平らな列に戻す
 *   - 封印された opaque type（Env.abstractTypes）を、その表現へ読み替える
 * だけを行う。以降のフェーズに module と抽象型は残らない。
 */
object ModuleDesugar {

  def desugar(program: TypeRec[AST.Program.type]): TypeRec[AST.Program.type] = program.project match {
    case AST.Program(decls) =>
      val env = program.extract match {
        case ProgramAnn(e) => e
      }
      val flat = decls.flatMap(flatten)
      if (env.abstractTypes.isEmpty) programT(flat, env)
      else {
        val reveal = Reveal(env.abstractTypes)
        programT(flat.map(decl => reveal(decl)), reveal.env(env))
      }
  }

  private def flatten(decl: TypeRec[Decl]): Seq[TypeRec[Decl]] = decl.project match {
    case AST.TopModule(_, decls) => decls.flatMap(flatten)
    case AST.TopModified(_, inner) => flatten(inner)
    case AST.TopOpen(_) => Seq.empty
    case _ => Seq(decl)
  }

  private class Reveal(abstractTypes: Map[TypeVariable, TypeAlias]) {

    // 表現が別の抽象型に言及していることがあるので、表現も読み替えておく（別名と同じく循環はない）
    private lazy val representations: Map[TypeVariable, TypeRec[Type]] = abstractTypes.map { case (name, alias) =>
      name -> alias.params.foldRight(typeOf(alias.body)) { case ((p, k), body) => typeAbsT(p, k, body) }
    }

    private val revealAlg: Algebra[TypedAST, TypeRec] = [x] => he => he.ast match {
      case AST.TypeVar(variable) if abstractTypes.contains(variable) => representations(variable)
      case node => HCofree(he.ann, node)
    }

    def typeOf(t: TypeRec[Type]): TypeRec[Type] = Equivalence.normalize(t.cata(revealAlg))

    private def constraint(c: TypeConstraint): TypeConstraint = c.hmap([y] => (child: TypeRec[y]) => apply(child))

    def apply[I](t: TypeRec[I]): TypeRec[I] = {
      val children = t.project.hmap([y] => (child: TypeRec[y]) => apply(child))
      (t.extract, t.project, children) match {
        case (TypeAnn, _, _) => typeOf(t)
        case (ExprAnn(exprType), _, revealed) => HCofree(ExprAnn(typeOf(exprType)), revealed)
        // impl のターゲットはインスタンスを引く鍵（辞書の名前の元）なので、抽象型の名前のまま残す
        case (ann, AST.TopImpl(_, _, targets, _, _, _), AST.TopImpl(name, params, _, context, methods, superEvidence)) =>
          HCofree(ann, AST.TopImpl(name, params, targets, context, methods, superEvidence))
        case (ann, _, revealed) => HCofree(ann, revealed)
      }
    }

    def env(e: Env): Env = {
      def constructor(c: ConstructorDef): ConstructorDef = c.copy(fields = c.fields.map(typeOf))
      e.copy(
        values = e.values.view.mapValues(typeOf).toMap,
        typeAliases = (e.typeAliases ++ abstractTypes).view.mapValues(alias => alias.copy(body = typeOf(alias.body))).toMap,
        dataTypes = e.dataTypes.view.mapValues(d => d.copy(constructors = d.constructors.map(constructor))).toMap,
        constructors = e.constructors.view.mapValues(constructor).toMap,
        traits = e.traits.view.mapValues { t =>
          t.copy(methods = t.methods.map { case (name, sig) => name -> typeOf(sig) }, supers = t.supers.map(constraint))
        }.toMap,
        instances = e.instances.view.mapValues(i => i.copy(targets = i.targets.map(typeOf), context = i.context.map(constraint))).toMap,
        abstractTypes = Map.empty
      )
    }
  }
}
