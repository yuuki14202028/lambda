package com.yuuki14202028

type ShowResult[I] = String

private def showKindedParam(param: (TypeVariable, Kind)): String = param match {
  case (v, Kind.Star) => s"[${v.name}]"
  case (v, k) => s"[${v.name}: ${k.show}]"
}

private def showConstraints(constraints: Seq[Constraint[ShowResult]]): String = constraints match {
  case Seq() => ""
  case _ => constraints.map(c => s"${c.name.name}${c.arg.map(a => s"[$a]").mkString}").mkString(" with ", ", ", "")
}

val showAlg: Algebra[AST, ShowResult] = [x] => node => node match {
  case AST.Program(decls)          => decls.mkString("\n")
  case AST.TopLet(v, types, value) => s"let ${v.name}: $types = $value"
  case AST.TopLetRec(v, types, value) => s"let rec ${v.name}: $types = $value"
  case AST.TopImport(path)         => s"import \"$path\""
  case AST.TopType(v, params, alias) =>
    val suffix = params.map(showKindedParam).mkString
    s"type ${v.name}$suffix = $alias"
  case AST.TopData(v, params, constructors, recursive) =>
    val suffix = params.map(showKindedParam).mkString
    val ctorText = constructors.map { ctor =>
      val fields = ctor.fields.map(field => s"($field)").mkString
      s"| ${ctor.name.name}$fields"
    }.mkString(" ")
    s"data ${if (recursive) "rec " else ""}${v.name}$suffix = $ctorText"
  case AST.TopTrait(v, params, supers, methods) =>
    val suffix = params.map(showKindedParam).mkString
    val superText = showConstraints(supers)
    val methodText = methods.map(m => s"  def ${m.name.name}: ${m.sig}").mkString("\n")
    s"trait ${v.name}$suffix$superText {\n$methodText\n}"
  case AST.TopImpl(v, params, targets, context, methods) =>
    val suffix = params.map(showKindedParam).mkString
    val targetText = targets.map(t => s"[$t]").mkString
    val contextText = showConstraints(context)
    val methodText = methods.map { m =>
      val sigText = m.sig.map(s => s": $s").getOrElse("")
      s"  def ${m.name.name}$sigText = ${m.body}"
    }.mkString("\n")
    s"impl$suffix ${v.name}$targetText$contextText {\n$methodText\n}"
  case AST.TopLetWith(v, params, constraints, types, value, recursive) =>
    val suffix = params.map(showKindedParam).mkString
    s"let ${if (recursive) "rec " else ""}${v.name}$suffix: $types${showConstraints(constraints)} = $value"
  case AST.TopDerive(traitName, target) => s"derive ${traitName.name}[${target.name}]"
  case AST.Abs(v, types, body)     => s"λ${v.name}: $types. $body"
  case AST.TyAbs(v, Kind.Star, body) => s"Λ${v.name}. $body"
  case AST.TyAbs(v, k, body)         => s"Λ(${v.name}: ${k.show}). $body"
  case AST.Let(v, types, value, body) => s"let ${v.name}: $types = $value in $body"
  case AST.LetRec(v, types, value, body) => s"let rec ${v.name}: $types = $value in $body"
  case AST.TypeLet(v, params, alias, body) =>
    val suffix = params.map(showKindedParam).mkString
    val head = s"${v.name}$suffix"
    s"type $head = $alias in $body"
  case AST.DataLet(v, params, constructors, body, recursive) =>
    val suffix = params.map(showKindedParam).mkString
    val head = s"${v.name}$suffix"
    val ctorText = constructors.map { ctor =>
      val fields = ctor.fields.map(field => s"($field)").mkString
      s"| ${ctor.name.name}$fields"
    }.mkString(" ")
    s"data ${if (recursive) "rec " else ""}$head = $ctorText in $body"
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
  case AST.Context(monad, bindings, result) =>
    val bindingText = bindings.map { b =>
      if (b.monadic) s"${b.name.name}: ${b.annotation} = ${b.value};"
      else s"let ${if (b.recursive) "rec " else ""}${b.name.name}: ${b.annotation} = ${b.value};"
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
