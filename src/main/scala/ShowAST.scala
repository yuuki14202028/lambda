package com.yuuki14202028

type ShowResult[I] = String

private def showKindedParam(param: (TypeVariable, Kind)): String = param match {
  case (v, Kind.Star) => s"[${v.name}]"
  case (v, k) => s"[${v.name}: ${k.show}]"
}

private def showConstraints(constraints: Seq[Constraint[ShowResult]]): String = constraints match {
  case Seq() => ""
  case _ => constraints.map(showConstraint).mkString(" with ", ", ", "")
}

private def showConstraint(c: Constraint[ShowResult]): String = {
  val binders = c.params.map {
    case (v, Kind.Star) => s"∀${v.name}. "
    case (v, k) => s"∀(${v.name}: ${k.show}). "
  }.mkString
  val premises = if (c.premises.isEmpty) "" else c.premises.map(showConstraint).mkString("", ", ", " ⇒ ")
  s"$binders$premises${c.name.name}${c.arg.map(a => s"[$a]").mkString}"
}

private def showEvidence(e: Evidence[ShowResult]): String = e match {
  case Evidence.Given(constraint) => s"given ${showConstraint(constraint)}"
  case Evidence.Instance(traitName, heads, typeArgs, context) =>
    val args = typeArgs.map(a => s"[$a]").mkString
    val ctx = context.map(c => s"(${showEvidence(c)})").mkString
    s"${instanceDictionaryName(traitName, heads).name}$args$ctx"
  case Evidence.Super(traitName, index, of) => s"${superDictionaryName(traitName, index).name}(${showEvidence(of)})"
  case Evidence.Apply(of, typeArgs, premises) =>
    s"(${showEvidence(of)})${typeArgs.map(a => s"[$a]").mkString}${premises.map(p => s"(${showEvidence(p)})").mkString}"
  case Evidence.Abs(constraint, body) => s"λ[${showConstraint(constraint)}]. ${showEvidence(body)}"
}

private def showRecBindings(bindings: Seq[RecBinding[ShowResult]]): String = {
  val lets = bindings.map(b => s"let ${b.variable.name}: ${b.types} = ${b.value}")
  if (lets.length == 1) s"rec ${lets.head}" else s"rec { ${lets.mkString("; ")} }"
}

private def showData(v: TypeVariable, params: Seq[(TypeVariable, Kind)], constructors: Seq[DataConstructor[ShowResult]]): String = {
  val ctorText = constructors.map { ctor =>
    val fields = ctor.fields.map(field => s"($field)").mkString
    s"| ${ctor.name.name}$fields"
  }.mkString(" ")
  s"data ${v.name}${params.map(showKindedParam).mkString} = $ctorText"
}

private def showPattern(p: Pattern[ShowResult]): String = p match {
  case Pattern.Wildcard() => "_"
  case Pattern.Bind(name) => name.name
  case Pattern.Constructor(name, args, _) => s"${name.name}${args.map(a => s"(${showPattern(a)})").mkString}"
  case Pattern.Literal(value, _) => value.shown
}

val showAlg: Algebra[AST, ShowResult] = [x] => node => node match {
  case AST.Program(decls)          => decls.mkString("\n")
  case AST.TopLet(v, types, value) => s"let ${v.name}: $types = $value"
  case AST.TopRec(Seq(decl))       => s"rec $decl"
  case AST.TopRec(decls)           => s"rec {\n${decls.mkString("\n")}\n}"
  case AST.TopImport(path)         => s"import \"$path\""
  case AST.TopType(v, params, alias) =>
    val suffix = params.map(showKindedParam).mkString
    s"type ${v.name}$suffix = $alias"
  case AST.TopData(v, params, constructors) =>
    val suffix = params.map(showKindedParam).mkString
    val ctorText = constructors.map { ctor =>
      val fields = ctor.fields.map(field => s"($field)").mkString
      s"| ${ctor.name.name}$fields"
    }.mkString(" ")
    s"data ${v.name}$suffix = $ctorText"
  case AST.TopTrait(v, params, supers, methods) =>
    val suffix = params.map(showKindedParam).mkString
    val superText = showConstraints(supers)
    val methodText = methods.map(m => s"  def ${m.name.name}: ${m.sig}").mkString("\n")
    s"trait ${v.name}$suffix$superText {\n$methodText\n}"
  case AST.TopImpl(v, params, targets, context, methods, _) =>
    val suffix = params.map(showKindedParam).mkString
    val targetText = targets.map(t => s"[$t]").mkString
    val contextText = showConstraints(context)
    val methodText = methods.map { m =>
      val sigText = m.sig.map(s => s": $s").getOrElse("")
      s"  def ${m.name.name}$sigText = ${m.body}"
    }.mkString("\n")
    s"impl$suffix ${v.name}$targetText$contextText {\n$methodText\n}"
  case AST.TopDerive(traitName, target) => s"derive ${traitName.name}[${target.name}]"
  case AST.TopModule(name, decls)  => s"module $name {\n${decls.mkString("\n")}\n}"
  case AST.TopOpen(path)           => s"open ${path.mkString(".")}"
  case AST.TopModified(modifier, decl) => s"${modifier.keyword} $decl"
  case AST.Abs(v, types, body)     => s"λ${v.name}: $types. $body"
  case AST.TyAbs(v, Kind.Star, body) => s"Λ${v.name}. $body"
  case AST.TyAbs(v, k, body)         => s"Λ(${v.name}: ${k.show}). $body"
  case AST.InstAbs(constraint, body) => s"λ[${showConstraint(constraint)}]. $body"
  case AST.Let(v, types, value, body) => s"let ${v.name}: $types = $value in $body"
  case AST.LetRec(bindings, body)  => s"${showRecBindings(bindings)} in $body"
  case AST.TypeLet(v, params, alias, body) =>
    val suffix = params.map(showKindedParam).mkString
    val head = s"${v.name}$suffix"
    s"type $head = $alias in $body"
  case AST.DataLet(v, params, constructors, body) => s"${showData(v, params, constructors)} in $body"
  case AST.DataLetRec(Seq(d), body) => s"rec ${showData(d.variable, d.params, d.constructors)} in $body"
  case AST.DataLetRec(datas, body) =>
    s"rec { ${datas.map(d => showData(d.variable, d.params, d.constructors)).mkString("; ")} } in $body"
  case AST.PatternMatch(scrutinee, cases) =>
    s"match $scrutinee { ${cases.map(c => s"${showPattern(c.pattern)} => ${c.body}").mkString("; ")} }"
  case AST.Match(scrutinee, cases) =>
    val caseText = cases.map { c =>
      val binders = c.binders.map(binder => s"(${binder.name})").mkString
      s"| ${c.constructor.name}$binders -> ${c.body}"
    }.mkString(" ")
    s"match $scrutinee with $caseText"
  case AST.Fold(scrutinee, resultType, cases) =>
    val caseText = cases.map { c =>
      val binders = c.binders.map(binder => s"(${binder.name})").mkString
      s"| ${c.constructor.name}$binders -> ${c.body}"
    }.mkString(" ")
    s"fold $scrutinee as $resultType with $caseText"
  case AST.App(func, arg)          => s"$func($arg)"
  case AST.TyApp(func, arg)        => s"$func[$arg]"
  case AST.InstApp(func, evidence) => s"$func⟨${showEvidence(evidence)}⟩"
  case AST.Foreign(v, types)       => s"foreign[$types] ${v.name}"
  case AST.Var(v)                  => s"${v.name}"
  case AST.Num(v, t)               => if (t == "i32" || t == "f64" && v.exists(ch => ch == '.' || ch == 'e' || ch == 'E')) v else s"$v$t"
  case AST.Char(v)                 => s"$v"
  case AST.StringLit(v)            => "\"" + v.flatMap {
    case '"' => "\\\""
    case '\\' => "\\\\"
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case '\u0000' => "\\0"
    case ch => ch.toString
  } + "\""
  case AST.StrInterp(parts)        => parts.map(p => s"{$p}").mkString("`", "", "`")
  case AST.Context(monad, steps, result, _) =>
    val bindingText = steps.map {
      case ContextStep.Bind(name, annotation, value) => s"${name.name}: $annotation = $value;"
      case ContextStep.Let(name, annotation, value) => s"let ${name.name}: $annotation = $value;"
      case ContextStep.Rec(bindings) => s"${showRecBindings(bindings)};"
    }
    s"context[$monad] { ${(bindingText :+ result.toString).mkString(" ")} }"
  case AST.Bool(v)                 => s"$v"
  case AST.UnitLit()               => "()"
  case AST.Block(discarded, result) =>
    val body = (discarded.map(_.toString + ";") ++ result.toSeq.map(_.toString)).mkString(" ")
    s"{ $body }"
  case AST.BinOp(op, l, r)         => s"($l $op $r)"
  case AST.Intrinsic(op, args)     => s"intrinsic[$op](${args.mkString(", ")})"
  case AST.UnaryOp(op, t)          => s"$op $t"
  case AST.If(c, t, e)             => s"if $c then $t else $e"
  case AST.Primitive(name)         => name
  case AST.TypeVar(v)              => v.name
  case AST.Arrow(from, to)         => s"$from → $to"
  case AST.InstArrow(constraint, to) => s"[${showConstraint(constraint)}] → $to"
  case AST.ForAll(v, Kind.Star, body) => s"∀${v.name}. $body"
  case AST.ForAll(v, k, body)         => s"∀(${v.name}: ${k.show}). $body"
  case AST.TypeApp(func, arg)         => s"$func[$arg]"
  case AST.TypeAbs(v, Kind.Star, body) => s"λ${v.name}. $body"
  case AST.TypeAbs(v, k, body)         => s"λ(${v.name}: ${k.show}). $body"
}

extension [I](t: Rec[I]) {
  def show: String = t.cata(showAlg)
}

val typedShowAlg: Algebra[TypedAST, ShowResult] = [x] => he => {
  val shown = showAlg(he.ast)
  he.ann match {
    case ProgramAnn(_)     => shown
    case DeclAnn           => shown
    case ExprAnn(exprType) => s"($shown)[${exprType.show}]"
    case TypeAnn           => shown
  }
}

extension [I](t: TypeRec[I]) {
  @scala.annotation.targetName("showAnn")
  def show: String = t.cata(typedShowAlg)
}
