package com.yuuki14202028

import java.nio.file.{Files, Paths}

/** エラーの表示。CompileError（エラーの中身）から、見出し・ソース上の位置に付ける説明・補足・直し方を組み立て、
 * ソースの該当行と一緒に描く。文言はすべてここにある（CompileError.message は 1 行の要約）。
 *
 *   error: mismatched types
 *    --> main.lam:3:14
 *     |
 *   3 | let x: i32 = true
 *     |              ^^^^ expected `i32`, found `bool`
 *     |        --- expected `i32` because of this type annotation
 *     |
 *     = help: …
 */
object Diagnostics {

  /** ソースの本文。コンパイル対象のファイルは渡された本文を、import されたファイルはディスクから読む */
  final class Sources(mainFile: String, mainText: String) {
    private val cache = scala.collection.mutable.Map.empty[String, Source]

    def apply(file: String): Source = {
      val name = if (file.isEmpty) mainFile else file
      cache.getOrElseUpdate(name, Source(name, if (name == mainFile) mainText else try Files.readString(Paths.get(name)) catch { case _: java.io.IOException => "" }))
    }
  }

  final case class Source(name: String, text: String) {
    private val lineStarts: Vector[Int] = 0 +: text.zipWithIndex.collect { case ('\n', index) => index + 1 }.toVector

    /** 0 始まりの (行, 桁) */
    def position(offset: Int): (Int, Int) = {
      val clamped = offset.max(0).min(text.length)
      val line = lineStarts.lastIndexWhere(_ <= clamped).max(0)
      (line, clamped - lineStarts(line))
    }

    def line(index: Int): String = {
      val start = lineStarts(index)
      val end = if (index + 1 < lineStarts.length) lineStarts(index + 1) - 1 else text.length
      text.substring(start, end.max(start)).stripSuffix("\r")
    }

    def slice(span: Span): String = text.substring(span.start.min(text.length), span.end.min(text.length))
  }

  private final case class Label(span: Span, message: String, primary: Boolean)

  /** label は主な位置に付ける説明 */
  private final case class Description(headline: String, label: String = "", notes: List[String] = Nil, helps: List[String] = Nil)

  // ---- 描画 ----

  /** エラーが複数あれば、ソースの順に並べ、最後に件数を示す */
  def render(errors: List[CompileError], mainFile: String, mainSource: String): String = {
    val sources = new Sources(mainFile, mainSource)
    val rendered = errors.map(render(_, sources))
    if (errors.length <= 1) rendered.mkString else (rendered :+ s"error: ${errors.length} errors").mkString("\n\n")
  }

  private def render(error: CompileError, sources: Sources): String = {
    val description = describe(error, sources, error.hints.nonEmpty)
    val hinted = error.hints.map(hint)
    val primary = Option.when(error.span != Span.none)(Label(error.span, description.label, primary = true))
    val labels = primary.toList ++ error.related.map { case (span, reason) => Label(span, related(reason), primary = false) }
    draw(description.headline, labels, description.notes ++ hinted.flatMap(_._1), description.helps ++ hinted.flatMap(_._2), sources)
  }

  private def draw(headline: String, labels: List[Label], notes: List[String], helps: List[String], sources: Sources): String = {
    final case class Placed(label: Label, file: Source, line: Int, column: Int, width: Int)

    val placed = labels.map { label =>
      val file = sources(label.span.file)
      val (line, column) = file.position(label.span.start)
      val (endLine, endColumn) = file.position(label.span.end.max(label.span.start))
      // 複数行にまたがる範囲は、最初の行の終わりまでを示す
      val width = (if (endLine == line) endColumn - column else file.line(line).length - column).max(1)
      Placed(label, file, line, column, width)
    }
    val gutter = (placed.map(_.line + 1) :+ 1).max.toString.length
    val pad = " " * gutter

    // 主な位置のあるファイルを先に
    val files = placed.sortBy(!_.label.primary).map(_.file).distinct
    val sections = files.zipWithIndex.flatMap { case (file, index) =>
      val inFile = placed.filter(_.file eq file)
      val anchor = inFile.find(_.label.primary).getOrElse(inFile.minBy(p => (p.line, p.column)))
      val location = s"$pad${if (index == 0) "-->" else ":::"} ${file.name}:${anchor.line + 1}:${anchor.column + 1}"
      val lines = inFile.map(_.line).distinct.sorted
      val body = lines.zipWithIndex.flatMap { case (line, i) =>
        val gap = if (i > 0 && lines(i - 1) + 1 < line) List(s"$pad...") else Nil
        val number = (line + 1).toString
        val source = s"${" " * (gutter - number.length)}$number | ${file.line(line).replace('\t', ' ')}"
        // 主な位置を先に、あとは左から
        val marks = inFile.filter(_.line == line).sortBy(p => (!p.label.primary, p.column)).map { p =>
          val underline = (if (p.label.primary) "^" else "-") * p.width
          s"$pad | ${" " * p.column}${if (p.label.message.isEmpty) underline else s"$underline ${p.label.message}"}"
        }
        gap ++ (source :: marks)
      }
      List(location, s"$pad |") ++ body
    }

    // 複数行の補足は、2 行目以降を本文の位置に揃える
    def continued(text: String): String = text.linesIterator.mkString("\n" + " " * (gutter + 9))
    val footer = notes.map(note => s"$pad = note: ${continued(note)}") ++ helps.map(help => s"$pad = help: ${continued(help)}")
    val spacer = if (sections.nonEmpty && footer.nonEmpty) List(s"$pad |") else Nil
    (List(s"error: $headline") ++ sections ++ spacer ++ footer).mkString("\n")
  }

  // ---- 文言 ----

  private def ty(tpe: Type): String = s"`${Type.show(tpe)}`"
  private def name(text: String): String = s"`$text`"
  private def names(texts: List[String]): String = texts.map(name).mkString(", ")
  private def plural(count: Int, noun: String): String = if (count == 1) s"$count $noun" else s"$count ${noun}s"

  private def related(reason: Related): String = reason match {
    case Related.Annotation(expected) => s"expected ${ty(expected)} because of this type annotation"
    case Related.Function(tpe) => s"this function has type ${ty(tpe)}"
    case Related.FirstCase(tpe) => s"the first case has type ${ty(tpe)}"
    case Related.ThenBranch(tpe) => s"this branch has type ${ty(tpe)}"
    case Related.LeftOperand(tpe) => s"the left operand has type ${ty(tpe)}"
    case Related.FirstElement(tpe) => s"the first element has type ${ty(tpe)}"
  }

  /** (補足, 直し方) */
  private def hint(found: Hint): (List[String], List[String]) = found match {
    case Hint.Similar(List(only)) => (Nil, List(s"a similar name exists: ${name(only)}"))
    case Hint.Similar(similar) => (Nil, List(s"similar names exist: ${names(similar)}"))
    case Hint.DeclaredLater(declared) => (
      List(s"${name(declared)} is declared later in the program; a declaration is only visible after it"),
      List(s"move the declaration of ${name(declared)} above this use, or put the declarations that refer to each other in `rec { … }`")
    )
    case Hint.NeedsRec(defined, keyword) => (
      List(s"${name(defined)} refers to itself, and recursion must be marked with `rec`"),
      List(s"write `$keyword` to make ${name(defined)} visible in its own definition")
    )
    case Hint.OpaqueType(opaque) => (
      List(s"${name(opaque)} is an `opaque type`: outside its module it is a different type from its representation"),
      List("use the functions the module exports to create and inspect values of this type")
    )
    case Hint.InstanceNeedsRec(instance) => (
      List(s"${name(instance)} refers to itself, and recursion must be marked with `rec`"),
      List(s"write `rec impl` to make ${name(instance)} visible in its own definition")
    )
    case Hint.InstanceDeclaredLater(instance) => (
      List(
        s"the instance ${name(instance)} is declared later in the same `rec { … }` group",
        "an instance without a `with` context is built immediately, and its dictionary holds the dictionaries of its superclasses, so those must already exist"
      ),
      List(s"move the declaration of ${name(instance)} before this one")
    )
  }

  /** 長い型は位置の説明に入れず、補足に並べて示す */
  private def mismatch(expected: Type, actual: Type): (String, List[String]) = {
    val (e, a) = (Type.show(expected), Type.show(actual))
    if (e.length + a.length <= 60) (s"expected `$e`, found `$a`", Nil)
    else ("this has the wrong type", List(s"expected: `$e`\n   found: `$a`"))
  }

  private val declarationKeywords = "(`let`, `rec`, `data`, `type`, `trait`, `impl`, `derive`, `module`, `open`, `import`)"

  private def syntax(expected: String, found: String): Description = {
    val atDeclaration = expected == "a declaration" || expected == "the end of the declaration"
    val label =
      if (found == "an unclosed comment") "this comment is never closed"
      else if (atDeclaration || expected == "a token") s"unexpected $found"
      else s"expected $expected, found $found"
    val notes =
      if (atDeclaration || (expected == "a token" && found != "an unclosed comment"))
        List(s"a declaration is complete before this point, so a new declaration $declarationKeywords or the end of the file is expected")
      else Nil
    val help = (expected, found) match {
      case (_, "an unclosed comment") => Some("close it with `*/`")
      case (_, "`->`") => Some("functions and cases use the arrows `→` and `=>`")
      case (_, "`#`") => Some("comments are written `// …` (to the end of the line) or `/* … */`")
      case (_, "`operator`") => Some("an operator is declared with its precedence: `infixl 6 + = add` (or `infixr` / `infix`), and `prefix - = neg`")
      case ("`:`", "`=`") => Some("every definition states its type: `let x: T = …`, or `let f(x: T): U = …` for a function")
      case ("a name", "`rec`") => Some("recursion is marked in front of the declaration: write `rec let …`")
      case ("`{`", "`with`") => Some("the cases of a fold are written in braces: `fold e as T { C(x) => … }`")
      case (closing, "the end of the file") if closing.contains("`}`") => Some("a `{` is not closed")
      case ("an expression", keyword) if keyword.startsWith("`") && Lexer.keywords(keyword.stripPrefix("`").stripSuffix("`")) =>
        Some(s"$keyword is a keyword, and the expression before it is not finished")
      case _ => None
    }
    Description("syntax error", label, notes, help.toList)
  }

  /** hasHints: 手がかりが付いている（その説明と重なる補足は出さない） */
  private def describe(error: CompileError, sources: Sources, hasHints: Boolean): Description = {
    import CompileError.*
    def written(span: Span): String = sources(span.file).slice(span)

    error match {
      case Detailed(inner, _, _, _) => describe(inner, sources, hasHints)

      // ---- 構文・import ----
      case Syntax(expected, found, _) => syntax(expected, found)
      case ImportNotFound(path, _) => Description(
        s"cannot read the imported file ${name(path)}", "imported here",
        notes = List("the file does not exist or cannot be read"),
        helps = List("`import \"std/…\"` reads from the bundled `stdlib/` directory; any other path is relative to the importing file"))
      case ImportCycle(path, _) => Description(
        "import cycle through a module", s"this imports ${name(path)}, which is still being imported",
        notes = List("an `import` inside a `module { … }` copies the file's declarations into the module, so a file cannot be imported there while it is itself being read"))

      // ---- 名前 ----
      case UndefinedVariable(variable, _) => Description(s"cannot find value ${name(variable)} in this scope", "not found in this scope")
      case UndefinedType(tpe, _) => Description(s"cannot find type ${name(tpe)} in this scope", "not found in this scope")
      case UndefinedModule(module, _) => Description(
        s"cannot find module ${name(module)}", "no module with this name is visible here",
        notes = List("a module is visible only after its declaration"))
      case PrivateName(private_, isType, _) => Description(
        s"cannot find ${if (isType) "type" else "value"} ${name(private_)} in this scope", "not found in this scope",
        notes = List(s"${name(private_)} exists, but it is `private` (or a constructor of an `opaque data`) and cannot be used outside its module"))
      case UndefinedOperator(operator, isPrefix, _) =>
        val standard = Set("+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "!", "&", "|", "^")
        // +- のような並びは、二項の演算子と前置の演算子を続けて書いたものかもしれない
        val split = Option.when(!isPrefix && operator.length > 1 && standard(operator.init) && Set("-", "!")(operator.last.toString))((operator.init, operator.last.toString))
        Description(
          s"the ${if (isPrefix) "prefix " else ""}operator ${name(operator)} is not declared", "used here",
          notes = List("an operator calls the function it is declared for: after `infixl 6 + = add`, `a + b` means `add(a)(b)`") ++
            split.map { case (binary, prefix) => s"a run of symbols is read as one operator, so `$operator` is not `$binary` followed by the prefix `$prefix`" },
          helps = split.map { case (binary, prefix) => s"put a space between them: `$binary $prefix`" }.toList ++
            Option.when(split.isEmpty && standard(operator))("the standard operators are declared in the standard library: add `import \"std/operators\"`"))
      case AlreadyDefined(defined, _) => Description(s"${name(defined)} is defined more than once", "defined again here")
      case NotAConstructor(constructor, _) => Description(
        s"${name(constructor)} is not a constructor", "used as a constructor pattern",
        notes = List("a name with arguments in a pattern must be a constructor in scope"))
      case OperatorsNotAssociative(left, leftFixity, right, rightFixity, _) =>
        def declared(fixity: Fixity): String = s"`${fixity.assoc match { case Assoc.Left => "infixl"; case Assoc.Right => "infixr"; case Assoc.None => "infix" }} ${fixity.level}`"
        Description(
          s"${name(left)} and ${name(right)} cannot be mixed without parentheses", "the grouping of these operators is not determined",
          notes = List(s"${name(left)} is declared ${declared(leftFixity)} and ${name(right)} is declared ${declared(rightFixity)}: operators of the same precedence group only when both are `infixl` or both are `infixr`"),
          helps = List("add parentheses to say which operator applies first"))
      case OperatorBuiltin(operator, _) => Description(
        s"the operator ${name(operator)} is built in", "cannot be declared",
        notes = List("`&&` and `||` work on `bool` only and evaluate their right operand only when needed, so they are not functions"))
      case OutsideModule(keyword, _) => Description(
        s"`$keyword` is used outside a module", "this declaration is at the top level",
        notes = List(s"`$keyword` limits what a `module { … }` shows to the outside, so it has no meaning at the top level"))
      case MixedRecursiveGroup(_) => Description(
        "a `rec { … }` group mixes different kinds of declarations", "this group",
        notes = List("a group contains only `let`s, only `data`s, or only `impl` / `derive`s"),
        helps = List("split it into one `rec { … }` per kind (data first, then the functions or instances that use it)"))
      case UnknownIntrinsic(intrinsic, _) => Description(
        s"unknown intrinsic ${name(intrinsic)}", "not an intrinsic",
        notes = List("an intrinsic is named `operation_type`, such as `add_i32` or `lt_f64`"))
      case SyntaxNotDeclared(what, declaration, _) =>
        val (explanation, library) = declaration match {
          case "interpolate" => ("`{x}` calls the function declared with `interpolate = f`: `f(x)`", "the standard library declares `interpolate = show`: add `import \"std/show\"`")
          case "literal []" => ("`[x, y]` calls the functions declared with `literal [] = nil, cons`", "the standard library declares `literal [] = Nil, Cons`: add `import \"std/list\"`")
          case _ => (s"`(x, y)` calls the function declared with `$declaration = f`", "the standard library declares `literal (,) = Pair`: add `import \"std/pair\"`")
        }
        Description(s"$what ${if (what.endsWith("s")) "are" else "is"} not declared", "used here", notes = List(explanation), helps = List(library))
      case EmptyListLiteral(_) => Description(
        "an empty list cannot be written as `[]`", "no element to read the type from",
        notes = List("the element type of `[x, y]` is the type of its first element"),
        helps = List("write the empty value with its type argument, for example `Nil[i32]`"))
      case SyntaxPatternNotConstructor(what, _) => Description(
        s"a $what pattern cannot be used here", "this pattern",
        notes = List(s"a $what pattern stands for constructor patterns, so the function declared for the $what syntax must be a constructor"))
      case FieldNamesNotAllowed(data, reason, _) =>
        val (label, note, help) = reason match {
          case FieldNamesReason.SeveralConstructors => (
            "this data type does not have exactly one constructor",
            "a named field `C(name: T)` generates the function `name` that reads the field from any value of the type, so the type must have one constructor",
            "remove the names and read the fields with `match`")
          case FieldNamesReason.LocalData => (
            "this data type is declared inside an expression",
            "a named field generates a function in the module of the type, and a data type declared with `data … in` has no module",
            "declare the data type at the top level, or remove the names")
          case FieldNamesReason.OpaqueData => (
            "this data type is opaque",
            "a named field generates a public function that reads the field, which would show the representation that `opaque` hides",
            "remove the names and write the functions the module should offer")
        }
        Description(s"data type ${name(data)} cannot have named fields", label, List(note), List(help))
      case PatternBindsTwice(variable, _) => Description(s"${name(variable)} is bound more than once in this pattern", "this pattern")
      case AlternativesBindDifferently(variable, _) => Description(
        "the alternatives of `|` bind different variables", s"${name(variable)} is not bound in every alternative",
        notes = List("the case body is the same for every alternative, so each of them must bind the same variables with the same types"))
      case DependsOnFailedDeclaration(_) => Description("this uses a declaration that failed to compile", "used here")

      // ---- 型 ----
      case TypeMismatch(expected, actual, _) =>
        val (label, notes) = mismatch(expected, actual)
        Description("mismatched types", label, notes)
      case KindMismatch(expected, actual, span) => Description(
        "mismatched kinds", s"expected a type of kind `${expected.show}`, found ${name(written(span))} of kind `${actual.show}`",
        helps = Option.when(expected != Kind.Star)(
          "a type constructor is expected here. If this is a bound type variable, give its kind: `∀(f: * → *). …` or `[f: * → *]` (the default kind is `*`)").toList)
      case NotAFunction(actual, _) => Description(
        "this value is applied like a function, but it is not one", s"has type ${ty(actual)}",
        helps = List("check the number of arguments; `f(x)(y)` passes them one at a time"))
      case NotPolymorphic(actual, _) => Description(
        "type arguments are given to a value that takes none", s"has type ${ty(actual)}",
        notes = List("`e[T]` applies a value of type `∀a. …` to the type `T`"),
        helps = List("remove the type argument; inside a trait's default method, the trait's own methods are plain functions and are called without type arguments"))
      case TypeArgumentUndetermined(parameter, function, _) => Description(
        s"the type argument ${name(parameter)} cannot be read from the arguments", s"the function has type ${ty(function)}",
        notes = List("a type argument is read from the types of the value arguments; one that appears in none of the arguments given here must be written"),
        helps = List(s"write it in brackets: `f[T](x)` (a written type argument fills the first type parameter that is not yet known)"))
      case IntrinsicArguments(intrinsic, _) => Description(
        s"`intrinsic[${intrinsic.name}]` cannot be used like this", "this intrinsic",
        notes = List(s"it takes ${plural(intrinsic.op.arity, "argument")} of type ${name(intrinsic.typeName)}, and the operation must exist for that type"))
      case ForeignNotFunction(actual, _) => Description(
        "a foreign value must be a function", s"has type ${ty(actual)}",
        helps = List("a C function without parameters takes `unit`: `foreign[unit → T] name`"))
      case RecursiveValueNotFunction(variable, _) => Description(
        s"${name(variable)} would be read before it is initialized", "this definition",
        notes = List("in a `rec` definition, a value that is not a function is evaluated immediately, while the definitions it refers to are still being created"),
        helps = List(s"make ${name(variable)} a function (`λ…` or `let $variable(…)`), or move it out of the `rec` group"))
      case LocalTypeEscapes(actual, _) => Description(
        "a data type declared inside an expression leaves it", s"the result has type ${ty(actual)}",
        notes = List("a data type declared with `data … in` exists only inside that expression, so the result cannot have that type"))
      case SelectUnknown(member, receiver, None, _, _) => Description(
        s"cannot use ${name("." + member)} on a value of type ${ty(receiver)}",
        s"this type has no module, and no trait method ${name(member)} is in scope",
        notes = List("`x.f` calls the function `f` in the module named after the type of `x`, or the trait method `f` in scope; only data types, opaque types and built-in types have a module"),
        helps = List(s"call the function directly: `$member(…)`"))
      case SelectUnknown(member, receiver, Some(module), members, _) => Description(
        s"module ${name(module)} has no member ${name(member)}", s"this value has type ${ty(receiver)}",
        notes = List(
          s"`x.$member` calls the function `$module.$member` (the type of `x` is ${ty(receiver)}) or the trait method `$member` in scope, and neither exists",
          if (members.isEmpty) s"module ${name(module)} has no members visible here"
          else s"members of ${name(module)}: ${names(members.take(12))}${if (members.length > 12) s", … and ${members.length - 12} more" else ""}"),
        helps = List(s"define it in the module of the type: `module $module { let $member… }`, import the trait that declares it, or call the function directly"))
      case SelectAmbiguous(member, receiver, function, _) => Description(
        s"${name("." + member)} is ambiguous", s"this value has type ${ty(receiver)}",
        notes = List(s"both the module function ${name(function)} and the trait method ${name(member)} are named ${name(member)}"),
        helps = List(s"call one of them directly: `$function(…)` or `$member(…)`"))
      case SelectNotApplicable(function, functionType, receiver, _) => Description(
        s"${name(function)} cannot be called with dot notation here", s"this value has type ${ty(receiver)}",
        notes = List(s"${name(function)} has type ${ty(functionType)}", "`x.f` passes `x` as the first argument of `f`"))
      case NamedArgumentsInvalid(reason, _) => Description(
        "named arguments cannot be used here", reason,
        notes = List("named arguments `C(x = e, …)` give every field of a constructor whose fields all have names, in any order"))
      case MissingFields(constructor, fields, _) => Description(
        s"${name(constructor)} is missing ${plural(fields.length, "field")}: ${names(fields)}", "every field must be given",
        notes = List("named arguments `C(x = e, …)` give every field of a constructor whose fields all have names, in any order"))
      case UnknownField(constructor, field, fields, _) => Description(
        s"${name(constructor)} has no field ${name(field)}", "unknown field", notes = List(s"the fields are ${names(fields)}"))

      // ---- パターン ----
      case NotADataType(actual, _) => Description(
        "a constructor pattern is used on a value that is not a data type", s"the matched value has type ${ty(actual)}")
      case PatternConstructorMismatch(constructor, scrutinee, _) => Description(
        s"the constructor ${name(constructor)} does not belong to the matched type", s"the matched value has type ${ty(scrutinee)}",
        notes = List("a name without arguments in a pattern is a constructor if one with that name is in scope, and a variable otherwise"))
      case ConstructorArity(constructor, expected, actual, _) => Description(
        s"wrong number of arguments in the pattern ${name(constructor)}", s"${name(constructor)} has ${plural(expected, "field")}, but the pattern has $actual",
        helps = List("write one sub-pattern per field; use `_` for the fields you do not need"))
      case NonExhaustiveMatch(example, guarded, _) => Description(
        "this match does not cover every value", s"${name(example)} is not matched",
        notes = Option.when(guarded)("a case with an `if` condition does not count as covering its pattern, because the condition may be false").toList,
        helps = List(s"add a case for ${name(example)}, or a final `_ => …`"))
      case UnreachableCase(index, _) => Description(
        s"case $index of this match can never be reached", "no value reaches this case",
        notes = List("cases are tried from top to bottom, and the earlier cases already match every value this one matches"),
        helps = List("remove the case, or move it above the more general one"))
      case RefutablePattern(example, _) => Description(
        "this pattern does not match every value", s"${name(example)} is not matched",
        notes = List("a pattern in a binding must always succeed, because there is no other case to fall back to"),
        helps = List("use `match` to handle the other cases"))
      case FoldPattern(_) => Description(
        "this is not a case of a fold", "expected a constructor applied to variables",
        notes = List("each case of `fold` names one constructor and binds its fields: `C(x)(rest) => …`; a field of the folded type is bound to its folded result"))

      // ---- trait とインスタンス ----
      case NotAConstraint(actual, _) => Description(
        "this is not a constraint", s"${ty(actual)} is not a trait applied to types",
        notes = List("a constraint is written `C[T]` for a trait `C` (or `∀a. P ⇒ C[T]`)"))
      case ConstraintBinderUnused(parameter, _) => Description(
        s"the type variable ${name(parameter)} is not determined by the constraint", "this constraint",
        notes = List("a quantified constraint `∀a. P ⇒ C[T]` is used by matching `C[T]` against the instance that is needed, so every variable bound by `∀` must appear in `C[T]`"))
      case NoInstance(goal, requiredBy, known, _) =>
        val required = requiredBy.reverse.map { case (wanted, through) => s"required for ${name(wanted)} by ${name(through)}" }
        val declared =
          if (hasHints) Nil
          else if (known.isEmpty) List("no instance of this trait is declared before this point (an instance is visible only after its `impl`)")
          else List(s"instances declared before this point:\n${(known.take(8) ++ Option.when(known.length > 8)(s"… and ${known.length - 8} more")).map("  " + _).mkString("\n")}")
        val forParameter = Type.dataApplication(goal).exists(_._2.exists(_.isInstanceOf[Type.Var])) && requiredBy.isEmpty && !hasHints
        Description(
          s"no instance for ${ty(goal)}", "this needs the instance", required ++ declared,
          Option.when(forParameter)(s"if the type is a type parameter, require the instance from the caller: add `with ${Type.show(goal)}` to the enclosing definition").toList)
      case ResolutionTooDeep(goal, _) => Description(
        s"resolving the instance ${ty(goal)} does not finish", "this needs the instance",
        notes = List("each instance found needs another, larger instance; the search was stopped"))
      case NoConstraintToSatisfy(function, _) => Description(
        "this expression does not take an instance", s"has type ${ty(function)}",
        notes = List("`f(with d)` passes the dictionary `d` for the first constraint in the type of `f`, written `[C[…]] → …`"),
        helps = List("remove `(with …)`"))
      case UsingNotDictionary(actual, _) => Description(
        "`using` expects a dictionary", s"has type ${ty(actual)}",
        notes = List("a dictionary is a value of a trait type, such as `Show[i32]` (or a polymorphic function producing one)"),
        helps = List("build one with the trait's constructor (`MkShow[i32](…)`) or take an existing instance with `summon[Show[i32]]`"))
      case NotATrait(found, _) => Description(s"${name(found)} is not a trait", "an instance can be declared only for a trait")
      case ImplTargetArity(traitName, expected, actual, _) => Description(
        s"wrong number of types for the trait ${name(traitName)}", s"${name(traitName)} takes ${plural(expected, "type")}, but $actual ${if (actual == 1) "is" else "are"} given")
      case InstanceHeadInvalid(target, _) => Description(
        "an instance cannot be declared for this type", s"${ty(target)} does not start with a type constructor",
        notes = List("an instance is found by the outermost type constructor of each type, so that must be a data type, a built-in type or a function type (not a type parameter)"))
      case ImplParameterUnused(parameter, _) => Description(
        s"the type parameter ${name(parameter)} is not used", "this impl",
        notes = List("every type parameter of an impl must appear in the types the instance is for, because it is read from them"))
      case OverlappingInstance(instance, _) => Description(
        s"a second instance of ${name(instance)}", "this instance",
        notes = List("there is at most one instance per trait and outermost type constructor"),
        helps = List("to use a different implementation in one place, build a dictionary and pass it with `(with d)` or `using d in …`"))
      case MissingMethods(instance, methods, _) => Description(
        s"$instance does not define ${names(methods)}", "this impl",
        notes = List("an impl must define every method of the trait that has no default"),
        helps = List(s"add ${methods.map(method => s"`def $method = …`").mkString(", ")}"))
      case ExtraMethod(traitName, method, _) => Description(s"${name(method)} is not a method of the trait ${name(traitName)}", "this definition")
      case MethodTypeMismatch(instance, method, expected, actual, _) =>
        val (label, notes) = mismatch(expected, actual)
        Description(s"the method ${name(method)} of $instance has the wrong type", label, notes)
      case RecursiveInstanceMethodNotFunction(method, _) => Description(
        s"the method ${name(method)} would read an instance before it is initialized", "this method",
        notes = List("an instance without a `with` context is built immediately; a method that is not a function is evaluated at that moment, while the instances of its `rec` group are still being created"),
        helps = List(s"make ${name(method)} a function (`def $method(…)` or `λ…`)"))
      case DeriveUnsupported(traitName, target, reason, _) => Description(s"cannot derive `$traitName[$target]`", reason)
      case DeriveNeeds(traitName, target, goal, _) => Description(
        s"`derive $traitName[$target]` needs ${ty(goal)}", "this derive",
        notes = List(s"the derived instance uses the instance of each field's type, and ${ty(goal)} is not declared before this point"),
        helps = List(s"declare that instance before this derive (for a data type, with `derive`; if the types refer to each other, put the derives in one `rec { … }`), or write the `impl` by hand"))

      case MainMissing(_) => Description("the program has no `main`", helps = List("add `let main(): i32 = …` at the top level; its result is the exit code"))
      case MainTypeInvalid(actual, _) => Description(
        "`main` has the wrong type", "this is the program's entry point",
        notes = List(s"expected: `unit → i32`\n   found: ${ty(actual)}"), helps = List("declare it as `let main(): i32 = …`"))
    }
  }
}
