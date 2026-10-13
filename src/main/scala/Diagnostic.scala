package com.yuuki14202028

import java.nio.file.{Files, Path}

/** エラーの表示。CompileError（エラーの中身）から、見出し・ソース上の位置に付ける説明・補足・直し方を組み立て、
 * ソースの該当行と一緒に描く。文言はすべてここにある（CompileError.render は 1 行の要約）。
 *
 *   error: mismatched types
 *     --> main.lam:3:14
 *      |
 *    3 | let x: i32 = true
 *      |              ^^^^ expected `i32`, found `bool`
 *      |        --- expected because of this type annotation
 *      |
 *      = help: …
 */
final case class Diagnostic(
    headline: String,
    labels: Seq[Diagnostic.Label],
    notes: Seq[String],
    helps: Seq[String]
)

/** ソースの本文。コンパイル対象のファイルは渡された本文を、import されたファイルはディスクから読む */
final class Sources(mainName: String, mainText: String) {
  private val cache = scala.collection.mutable.Map.empty[Path, Sources.File]
  private val main = Sources.File(mainName, mainText)

  def file(path: Option[Path]): Sources.File = path match {
    case None => main
    case Some(p) => cache.getOrElseUpdate(p, Sources.File(Sources.displayName(p), try Files.readString(p) catch { case _: Exception => "" }))
  }
}

object Sources {
  final case class File(name: String, text: String) {
    private val lineStarts: Vector[Int] = 0 +: text.zipWithIndex.collect { case ('\n', i) => i + 1 }.toVector

    // 0 始まりの (行, 桁)
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
  }

  // 作業ディレクトリからの相対パスで示す
  def displayName(path: Path): String = {
    val cwd = java.nio.file.Paths.get("").toAbsolutePath
    val absolute = path.toAbsolutePath.normalize
    if (absolute.startsWith(cwd)) cwd.relativize(absolute).toString else absolute.toString
  }
}

object Diagnostic {

  final case class Label(span: Span, message: String, primary: Boolean)

  // ---- 描画 ----

  def render(error: CompileError, sources: Sources, color: Boolean = false): String = render(of(error, sources), sources, color)

  def render(diagnostic: Diagnostic, sources: Sources, color: Boolean): String = {
    def paint(code: String, text: String): String = if (color) s"\u001b[${code}m$text\u001b[0m" else text
    def red(text: String): String = paint("1;31", text)
    def blue(text: String): String = paint("1;34", text)
    def bold(text: String): String = paint("1", text)

    final case class Placed(label: Label, file: Sources.File, line: Int, column: Int, width: Int)

    val placed = diagnostic.labels.map { label =>
      val file = sources.file(label.span.file)
      val (line, column) = file.position(label.span.start)
      val (endLine, endColumn) = file.position(label.span.end.max(label.span.start))
      // 複数行にまたがる範囲は、最初の行の終わりまでを示す
      val lineLength = file.line(line).length
      val width = (if (endLine == line) endColumn - column else lineLength - column).max(1)
      Placed(label, file, line, column, width)
    }
    val gutter = (placed.map(_.line + 1) :+ 1).max.toString.length
    val pad = " " * gutter
    val bar = blue("|")

    val header = s"${red("error")}${bold(s": ${diagnostic.headline}")}"

    // 主な位置のあるファイルを先に
    val files = placed.sortBy(!_.label.primary).map(_.file).distinct
    val sections = files.zipWithIndex.flatMap { case (file, index) =>
      val inFile = placed.filter(_.file eq file)
      val anchor = inFile.find(_.label.primary).getOrElse(inFile.minBy(p => (p.line, p.column)))
      val arrow = if (index == 0) "-->" else ":::"
      val location = s"$pad${blue(arrow)} ${file.name}:${anchor.line + 1}:${anchor.column + 1}"
      val lines = inFile.map(_.line).distinct.sorted
      val body = lines.zipWithIndex.flatMap { case (line, i) =>
        val gap = if (i > 0 && lines(i - 1) + 1 < line) Seq(s"$pad${blue("...")}") else Seq.empty
        val number = (line + 1).toString
        val source = s"${" " * (gutter - number.length)}${blue(number)} $bar ${file.line(line).replace('\t', ' ')}"
        // 主な位置を先に、あとは左から
        val marks = inFile.filter(_.line == line).sortBy(p => (!p.label.primary, p.column)).map { p =>
          val underline = (if (p.label.primary) "^" else "-") * p.width
          val text = if (p.label.message.isEmpty) underline else s"$underline ${p.label.message}"
          s"$pad $bar ${" " * p.column}${if (p.label.primary) red(text) else blue(text)}"
        }
        gap ++ (source +: marks)
      }
      Seq(location, s"$pad $bar") ++ body
    }

    val footer =
      diagnostic.notes.map(note => s"$pad ${blue("=")} ${bold("note")}: ${indentContinuation(note, gutter)}") ++
        diagnostic.helps.map(help => s"$pad ${blue("=")} ${bold("help")}: ${indentContinuation(help, gutter)}")
    val spacer = if (sections.nonEmpty && footer.nonEmpty) Seq(s"$pad $bar") else Seq.empty

    (Seq(header) ++ sections ++ spacer ++ footer).mkString("\n")
  }

  // 複数行の補足は、2 行目以降を本文の位置に揃える
  private def indentContinuation(text: String, gutter: Int): String =
    text.linesIterator.mkString("\n" + " " * (gutter + 9))

  // ---- CompileError から組み立てる ----

  private final case class Description(headline: String, label: String = "", notes: Seq[String] = Nil, helps: Seq[String] = Nil)

  def of(error: CompileError, sources: Sources): Diagnostic = {
    val core = error.core
    // 構文エラーは、読めなかった字句の範囲を示す
    def syntax(failure: cats.parse.Parser.Error, path: Option[Path]): (Description, Option[Span]) = {
      val (description, width) = syntaxError(failure, sources.file(path))
      (description, Some(Span(failure.failedAtOffset, failure.failedAtOffset + width, path)))
    }
    val (description, position) = core match {
      case CompileError.ParseFailure(failure) => syntax(failure, None)
      case CompileError.ImportParseFailure(path, failure) => syntax(failure, Some(path))
      case other => (describe(other), error.position)
    }
    val hinted = error.hints.map(hint)
    // どのインスタンスの文脈として必要になったか（実際に書かれた制約に向かって並べる）
    val required = error.requirements.map { case (wanted, through) => s"required for `$wanted` by `$through`" }
    Diagnostic(
      headline = description.headline,
      labels = position.map(Label(_, description.label, primary = true)).toSeq ++
        error.related.map { case (span, reason) => Label(span, related(reason), primary = false) },
      notes = description.notes ++ required ++ hinted.flatMap(_._1),
      helps = description.helps ++ hinted.flatMap(_._2)
    )
  }

  private def ty(t: TypeRec[Type]): String = s"`${t.show}`"
  private def kind(k: Kind): String = s"`${k.show}`"
  private def name(n: String): String = s"`$n`"
  private def names(ns: Seq[String]): String = ns.map(name).mkString(", ")
  private def constraint(traitName: TypeVariable, args: Seq[TypeRec[Type]]): String =
    s"${traitName.name}${args.map(a => s"[${a.show}]").mkString}"
  private def plural(n: Int, noun: String): String = if (n == 1) s"$n $noun" else s"$n ${noun}s"

  private def related(reason: Related): String = reason match {
    case Related.ExpectedByAnnotation(expected) => s"expected ${ty(expected)} because of this type annotation"
    case Related.OtherBranch(found) => s"this branch has type ${ty(found)}"
    case Related.FunctionType(found) => s"this function has type ${ty(found)}"
    case Related.FirstCase(found) => s"the first case has type ${ty(found)}"
    case Related.Scrutinee(found) => s"this has type ${ty(found)}"
    case Related.LeftOperand(found) => s"the left operand has type ${ty(found)}"
  }

  // (補足, 直し方)
  private def hint(h: Hint): (Seq[String], Seq[String]) = h match {
    case Hint.SimilarNames(Seq(only)) => (Nil, Seq(s"a similar name exists: ${name(only)}"))
    case Hint.SimilarNames(similar) => (Nil, Seq(s"similar names exist: ${names(similar)}"))
    case Hint.DefinedLater(defined) => (
      Seq(s"${name(defined)} is declared later in the program; a declaration is only visible after it"),
      Seq(s"move the declaration of ${name(defined)} above this use, or put the declarations that refer to each other in `rec { … }`")
    )
    case Hint.DeclareRec(keyword, defined) => (
      Seq(s"${name(defined)} refers to itself, and recursion must be marked with `rec`"),
      Seq(s"write `$keyword` to make ${name(defined)} visible in its own definition")
    )
    case Hint.Instances(wanted, declared, inScope) =>
      val scope = if (inScope.isEmpty) Nil else Seq(s"constraints in scope:\n${inScope.map("  " + _).mkString("\n")}")
      val shown = declared.take(8).map("  " + _) ++ (if (declared.length > 8) Seq(s"  … and ${declared.length - 8} more") else Nil)
      val instances =
        if (declared.isEmpty) Seq(s"no instance of this trait is declared before this point (an instance is visible only after its `impl`)")
        else Seq(s"instances declared before this point:\n${shown.mkString("\n")}")
      (scope ++ instances, Nil)
    case Hint.PrivateMember(member) => (
      Seq(s"${name(member)} exists, but it is `private` (or a constructor of an `opaque data`) and cannot be used outside its module"),
      Nil
    )
    case Hint.OpaqueType(opaque) => (
      Seq(s"${name(opaque)} is an `opaque type`: outside its module it is a different type from its representation"),
      Seq("use the functions the module exports to create and inspect values of this type")
    )
    case Hint.AddConstraint(wanted) => (
      Nil,
      Seq(s"the type is a type parameter, so require the instance from the caller: add `with $wanted` to the enclosing definition")
    )
  }

  // 長い型は見出しの行に入れず、補足に並べて示す
  private def mismatch(expected: TypeRec[Type], actual: TypeRec[Type]): (String, Seq[String]) = {
    val (e, a) = (expected.show, actual.show)
    if (e.length + a.length <= 60) (s"expected `$e`, found `$a`", Nil)
    else ("this has the wrong type", Seq(s"expected: `$e`\n   found: `$a`"))
  }

  private def describe(error: CompileError): Description = {
    import CompileError.*
    error match {
      case At(_, inner) => describe(inner)
      case RelatedTo(inner, _) => describe(inner)
      case Hinted(inner, _) => describe(inner)

      // ---- 構文・import ----
      case ParseFailure(_) | ImportParseFailure(_, _) => Description("syntax error")
      case RequiredBy(inner, _, _) => describe(inner)
      case ImportReadFailure(path, cause) => Description(
        s"cannot read the imported file ${name(Sources.displayName(path))}",
        "imported here",
        notes = Seq(cause match {
          case _: java.nio.file.NoSuchFileException => "the file does not exist"
          case other => s"${other.getClass.getSimpleName}: ${Option(other.getMessage).getOrElse("")}"
        }),
        helps = Seq("`import \"std/…\"` reads from the bundled `stdlib/` directory; any other path is relative to the importing file")
      )
      case UnresolvedImport(path) => Description(s"the import ${name(path)} was not resolved", "this import")
      case ImportCycleInModule(path) => Description(
        s"import cycle through a module",
        s"this imports ${name(Sources.displayName(path))}, which is still being imported",
        notes = Seq("an `import` inside a `module { … }` copies the file's declarations into the module, so a file cannot be imported there while it is itself being read")
      )

      // ---- 名前 ----
      case UndefinedVariable(variable) => Description(s"cannot find value ${name(variable.name)} in this scope", "not found in this scope")
      case UndefinedTypeVariable(variable) => Description(s"cannot find type ${name(variable.name)} in this scope", "not found in this scope")
      case UndefinedPrimitive(primitive) => Description(s"unknown primitive type ${name(primitive)}", "not a primitive type")
      case UndefinedTrait(traitName, site) => Description(
        s"cannot find trait ${name(traitName.name)} in this scope",
        site.fold("not found in this scope")(s => s"used by ${s.shown.toLowerCase}"),
        notes = Seq("a trait is only visible after its declaration")
      )
      case UndefinedModule(path) => Description(s"cannot find module ${name(path.mkString("."))}", "no module with this name is visible here",
        notes = Seq("a module can be opened only after it is closed; inside a module, its members are already visible without `open`"))
      case ModifierOutsideModule(modifier) => Description(
        s"${name(modifier.keyword)} is only allowed inside a module",
        "this declaration is not inside a `module { … }`",
        helps = Seq(s"`${modifier.keyword}` controls what a module shows to the outside; wrap the declarations in `module Name { … }` or remove the modifier")
      )
      case MixedRecursiveGroup => Description(
        "a `rec { … }` group mixes different kinds of declarations",
        "this group",
        notes = Seq("a group contains only `let`s, only `data`s, or only `impl` / `derive`s"),
        helps = Seq("split it into one `rec { … }` per kind (data first, then the functions or instances that use it)")
      )
      case NoConstraintToSatisfy(actual) => Description(
        "this expression does not take an instance",
        s"has type ${ty(actual)}",
        notes = Seq("`f(with d)` passes the dictionary `d` for the first constraint in the type of `f`, written `[C[…]] → …`"),
        helps = Seq("remove `(with …)`, or apply the type arguments first: `f[T](with d)`")
      )
      case NotADictionary(actual) => Description(
        "`using` expects a dictionary",
        s"has type ${ty(actual)}",
        notes = Seq("a dictionary is a value of a trait type, such as `Show[i32]` (or a polymorphic function producing one)"),
        helps = Seq("build one with the trait's constructor (`MkShow[i32](…)`) or take an existing instance with `summon[Show[i32]]`")
      )
      case DefaultMethodTypeMismatch(traitName, method, expected, actual) =>
        val (label, notes) = mismatch(expected, actual)
        Description(s"the default of ${name(method.name)} does not match its signature", label, notes)
      case QuantifiedParamUnused(traitName, param) => Description(
        s"type variable ${name(param.name)} is not determined by the constraint",
        s"${name(param.name)} does not appear in the arguments of `${traitName.name}[…]`",
        notes = Seq("a quantified constraint is used like an instance: its type variables are found by matching the arguments after `⇒`")
      )
      case AlreadyDefined(kindOfName, defined) => Description(
        s"${kindOfName.shown.toLowerCase} ${name(defined)} is defined more than once", "already defined in this scope")
      case ParamAlreadyDefined(owner) => Description(
        s"a ${owner.paramNoun} of ${owner.shown.toLowerCase} shadows a type that is already in scope", "this declaration")
      case DuplicateParams(owner) => Description(s"${owner.shown.toLowerCase} repeats a ${owner.paramNoun} name", "this declaration")
      case MemberAlreadyDefined(owner, member) => Description(
        s"a ${member.singular} of ${owner.shown.toLowerCase} is already defined", "this declaration",
        notes = Seq(s"${member.singular} names share one namespace with values, so they must be unique in their scope"),
        helps = Seq("rename it, or put the declaration in its own `module`"))
      case DictionaryCtorAlreadyDefined(traitName, ctorName) => Description(
        s"the name ${name(ctorName.name)} is needed for the dictionary of trait ${name(traitName.name)}", "this trait",
        notes = Seq(s"a trait `C` also defines the dictionary constructor `MkC`"))
      case DuplicateMembers(owner, member) => Description(s"${owner.shown.toLowerCase} repeats a ${member.singular} name", "this declaration")

      // ---- 型 ----
      case TypeMismatch(expected, actual) =>
        val (label, notes) = mismatch(expected, actual)
        Description("mismatched types", label, notes)
      case ExpectedNumeric(actual) => Description("mismatched types", s"expected a numeric type, found ${ty(actual)}")
      case ExpectedEquatable(actual) => Description("mismatched types", s"expected a numeric type, `char` or `bool`, found ${ty(actual)}")
      case NotAFunction(actual) => actual.project match {
        case AST.ForAll(variable, _, _) => Description(
          "missing type arguments",
          s"has type ${ty(actual)}",
          notes = Seq("type arguments are always written out; they are not inferred from the value arguments"),
          helps = Seq(s"give a type for `${variable.name}` in brackets before the value arguments: `f[T](x)`"))
        case AST.InstArrow(c, _) => Description(
          "this value still expects an instance",
          s"has type ${ty(actual)}",
          helps = Seq(s"pass a dictionary for `${showTypeConstraint(c)}` with `(with d)`, or remove the `(with …)` before it so that the instance is resolved"))
        case _ => Description(
          "this value is applied like a function, but it is not one",
          s"has type ${ty(actual)}",
          helps = Seq("check the number of arguments; `f(x)(y)` passes them one at a time"))
      }
      case NotPolymorphic(actual) => Description(
        "type arguments are given to a value that takes none",
        s"has type ${ty(actual)}",
        notes = Seq("`e[T]` applies a value of type `∀a. …` to the type `T`"),
        helps = Seq("remove the type argument; inside a trait's default method, the trait's own methods are plain functions and are called without type arguments")
      )
      case NotADataType(actual) => Description(
        "this value cannot be taken apart by constructor patterns",
        s"has type ${ty(actual)}, which is not a data type")
      case DataArityMismatch(data, expected, actual) => Description(
        s"wrong number of type arguments for ${name(data)}", s"expected ${plural(expected, "type argument")}, found $actual")
      case ForeignNotFunction(t) => Description(
        "a foreign value must be a function", s"has type ${ty(t)}",
        helps = Seq("C functions without arguments take `unit`: `foreign[unit → i32] name`"))
      case NumericLiteralUnknown(typeName) => Description(s"unknown numeric type ${name(typeName)}", "this literal")
      case RecursiveDataNotDeclared(data) => Description(
        s"data type ${name(data.name)} refers to itself",
        "this declaration",
        notes = Seq("recursion must be marked with `rec`"),
        helps = Seq(s"write `rec data ${data.name} …`")
      )
      case RecursiveValueNotFunction(variable) => Description(
        s"${name(variable.name)} would be read before it is initialized",
        "this definition",
        notes = Seq("in a `rec` definition, a value that is not a function is evaluated immediately, while the definitions it refers to are still being created"),
        helps = Seq(s"make ${name(variable.name)} a function (`λ…` or `let ${variable.name}(…)`), or move it out of the `rec` group")
      )

      // ---- カインド ----
      case KindNotStar(t, k) => Description(
        s"expected a type of values, but ${ty(t)} still needs type arguments", s"has kind ${kind(k)}, expected `*`")
      case KindMismatchInTypeApp(expected, actual, argument) => Description(
        "mismatched kinds in a type application",
        s"expected a type of kind ${kind(expected)}, found ${argument.fold("one")(ty)} of kind ${kind(actual)}",
        helps = kindMismatchHelp(expected, actual)
      )
      case CannotApplyKind(k, function) => Description(
        s"${ty(function)} does not take type arguments", s"has kind ${kind(k)}")
      case ArrowKindNotStar(side, k, operand) => Description(
        "the two sides of `→` must be types of values", s"${ty(operand)} has kind ${kind(k)}, expected `*`")
      case ForAllBodyKindNotStar(k, body) => Description(
        "the body of `∀` must be a type of values", s"${ty(body)} has kind ${kind(k)}, expected `*`")

      // ---- 演算子・組み込み ----
      case OperatorUnresolvable(op, operandType) => Description(
        s"operator `$op` cannot be used on ${ty(operandType)}", "this operand's type has no name to look an operator up by")
      case OperatorUndefined(op, operandType, function) => Description(
        s"operator `$op` is not defined for ${ty(operandType)}",
        s"needs a function named ${name(function.name)}",
        notes = Seq(s"`a $op b` is a call to the function ${name(function.name)}, chosen from the type of the left operand"),
        helps = operandType.project match {
          case AST.Primitive(_) => Seq("the operators for the built-in types are defined in the standard library: add `import \"std/operators\"`")
          case _ => Seq(s"define the operator for this type: `let ${function.name}(x: ${operandType.show})(y: ${operandType.show}): … = …`")
        }
      )
      case OperatorNotAFunction(function, fnType) => Description(
        s"the operator function ${name(function.name)} is not a function", s"${name(function.name)} has type ${ty(fnType)}")
      case OperatorNotBinary(function, fnType) => Description(
        s"the operator function ${name(function.name)} does not take two arguments", s"${name(function.name)} has type ${ty(fnType)}")
      case IntrinsicArityMismatch(op, actual) =>
        val expected = op match { case _: BinOps => 2; case _: UnaryOps => 1 }
        Description(s"wrong number of arguments for `intrinsic[$op]`", s"expected ${plural(expected, "argument")}, found $actual")
      case IntrinsicOperandInvalid(op, requirement) => Description(
        s"`intrinsic[$op]` cannot be used at this type", s"requires ${requirement.shown} operands")
      case IntrinsicUnsupported(op) => Description(
        s"there is no intrinsic for `$op`", "this intrinsic",
        helps = Seq("short-circuit operators are ordinary functions taking a `unit → bool` thunk; see `stdlib/operators.lam`"))

      // ---- match / fold ----
      case DuplicateCases(matchKind) => Description(s"${matchKind.shown.toLowerCase} has two cases for the same constructor", "this expression")
      case DuplicateBinders(matchKind, constructor) => Description(
        s"the case ${name(constructor.name)} binds the same name twice", "this expression")
      case NonExhaustive(matchKind, missing, invalid) => Description(
        s"this ${matchKind.shown.toLowerCase} does not have exactly one case per constructor",
        "this expression",
        notes = (if (missing.isEmpty) Nil else Seq(s"missing: ${names(missing.map(_.name))}")) ++
          (if (invalid.isEmpty) Nil else Seq(s"not constructors of this type: ${names(invalid.map(_.name))}"))
      )
      case BinderArityMismatch(constructor, expected, actual) => Description(
        s"wrong number of arguments in the pattern ${name(constructor.name)}",
        s"${name(constructor.name)} has ${plural(expected, "field")}, but the pattern has $actual",
        helps = Seq("write one sub-pattern per field; use `_` for the fields you do not need"))
      case EmptyMatch => Description("a match needs at least one case", "this match")
      case NonExhaustivePatterns(example, guarded) => Description(
        "this match does not cover every value",
        s"${name(example)} is not matched",
        notes = if (guarded) Seq("a case with an `if` condition does not count as covering its pattern, because the condition may be false") else Nil,
        helps = Seq(s"add a case for ${name(example)}, or a final `_ => …`")
      )
      case UnreachableCase(index) => Description(
        s"case $index of this match can never be reached",
        "no value reaches this case",
        notes = Seq("cases are tried from top to bottom, and the earlier cases already match every value this one matches"),
        helps = Seq("remove the case, or move it above the more general one"))
      case NotAConstructor(constructor) => Description(
        s"${name(constructor.name)} is not a constructor", "used as a constructor in this pattern",
        notes = Seq("a name with arguments in a pattern must be a constructor of the matched data type"))
      case OrPatternBindingMismatch(first, other) => Description(
        "the alternatives of a `|` pattern bind different variables",
        "in this pattern",
        notes = Seq(s"one alternative binds (${first.map(_.name).mkString(", ")}), another binds (${other.map(_.name).mkString(", ")})",
          "the body is shared, so every alternative must bind the same variables with the same types"))
      case PatternConstructorMismatch(constructor, scrutinee) => Description(
        s"the constructor ${name(constructor.name)} does not belong to the matched type",
        s"the matched value has type ${ty(scrutinee)}",
        notes = Seq("a name without arguments in a pattern is a constructor if one with that name is in scope, and a variable otherwise"))

      // ---- trait / impl ----
      case TraitNeedsParameter(traitName) => Description(
        s"trait ${name(traitName.name)} has no type parameter", "this trait",
        helps = Seq(s"a trait describes operations on a type: `trait ${traitName.name}[a] { … }`"))
      case ConstraintArityMismatch(site, traitName, expected, actual) => Description(
        s"wrong number of type arguments for trait ${name(traitName.name)}",
        s"expected ${plural(expected, "type argument")}, found $actual")
      case ConstraintKindMismatch(site, traitName, arg, actual, expected) => Description(
        s"mismatched kinds in a constraint on ${name(traitName.name)}",
        s"${ty(arg)} has kind ${kind(actual)}, but the trait's parameter has kind ${kind(expected)}",
        helps = kindMismatchHelp(expected, actual))
      case ImplTargetArity(traitName, expected, actual) => Description(
        s"wrong number of type arguments for trait ${name(traitName.name)}",
        s"expected ${plural(expected, "type argument")}, found $actual")
      case ImplKindMismatch(traitName, target, paramKind, targetKind) => Description(
        s"${ty(target)} has the wrong kind to be an instance of ${name(traitName.name)}",
        s"has kind ${kind(targetKind)}, but the trait's parameter has kind ${kind(paramKind)}",
        helps = if (paramKind != Kind.Star && targetKind == Kind.Star)
          Seq(s"the trait is about type constructors: write the type without its last argument (for example `impl ${traitName.name}[List]`, not `[List[a]]`)")
        else if (paramKind == Kind.Star) Seq("the trait is about complete types: apply the type constructor to its arguments (for example `impl[a] … [List[a]]`)")
        else Nil)
      case InstanceHeadInvalid(traitName, target) => Description(
        "an instance must be for a named type", s"${ty(target)} does not start with a type constructor")
      case InstanceHeadTypeParameter(impl) => Description(
        "an instance cannot be for a bare type parameter", "this impl",
        notes = Seq("instances are found by the outermost type constructor, so `impl[a] C[a]` would match every type"))
      case ImplParamUnused(impl) => Description(
        "a type parameter of this impl does not appear in the instance's type", "this impl",
        notes = Seq("the type parameters are found by matching the instance's type, so each of them must occur in it"))
      case OverlappingInstance(traitName, heads) => Description(
        s"a second instance of `${traitName.name}${heads.map(h => s"[$h]").mkString}`", "this instance",
        notes = Seq("there is at most one instance per trait and outermost type constructor"),
        helps = Seq("to use a different implementation in one place, build a dictionary and pass it with `(with d)` or `using d in …`"))
      case MissingMethods(impl, methods) => Description(
        s"${impl.shown} does not define ${names(methods.map(_.name))}", "this impl",
        notes = Seq("an impl must define every method of the trait that has no default"),
        helps = Seq(s"add ${methods.map(m => s"`def ${m.name} = …`").mkString(", ")}"))
      case ExtraMethods(impl, methods) => Description(
        s"${impl.shown} defines ${names(methods.map(_.name))}, which the trait does not declare", "this impl")
      case MethodBodyTypeMismatch(impl, method, expected, actual) =>
        val (label, notes) = mismatch(expected, actual)
        Description(s"the method ${name(method.name)} of ${impl.shown} has the wrong type", label, notes)
      case MethodSigTypeMismatch(impl, method, declared, expected) =>
        val (label, notes) = mismatch(expected, declared)
        Description(s"the signature of ${name(method.name)} in ${impl.shown} differs from the trait's", label,
          notes :+ "the signature in an impl is optional; when written, it must be the trait's signature at the instance's type")
      case RecursiveInstanceMethodNotFunction(impl, method) => Description(
        s"the method ${name(method.name)} would read ${impl.shown} before it is initialized", "this impl",
        notes = Seq("the instance has no `with` context, so its dictionary is built immediately; a method that is not a function is evaluated while the dictionary is still being created"),
        helps = Seq(s"make ${name(method.name)} a function (`λ…`)"))

      // ---- derive ----
      case DeriveUnsupportedTrait(traitName) => Description(
        s"trait ${name(traitName.name)} cannot be derived", "this derive",
        notes = Seq("`derive` supports `Functor`, `Foldable` and `Traversable` with their standard signatures (see `stdlib/`)"))
      case DeriveTargetNotData(target) => Description(s"${name(target.name)} is not a data type", "this derive")
      case DeriveTraitSigMismatch(traitName, method, expected, actual) =>
        Description(s"`derive ${traitName.name}` needs the standard signature of ${name(method.name)}", "this derive",
          Seq(s"expected: `${expected.show}`\n   found: `${actual.show}`"))
      case DeriveKindMismatch(traitName, target) => Description(
        s"${name(target.name)} has no last type parameter to derive `${traitName.name}` over", "this derive",
        notes = Seq("the instance is derived over the last type parameter, which must have kind `*`"))
      case DeriveNegativeOccurrence(traitName, target, constructor, field) => Description(
        s"cannot derive `${traitName.name}` for ${name(target.name)}", "this derive",
        notes = Seq(s"in the constructor ${name(constructor.name)}, the type parameter appears as a function argument in the field ${ty(field)}",
          "a value of that type consumes the parameter instead of containing it, so there is nothing to map over"))
      case DeriveUnsupportedField(traitName, target, constructor, field) => Description(
        s"cannot derive `${traitName.name}` for ${name(target.name)}", "this derive",
        notes = Seq(s"the field ${ty(field)} of the constructor ${name(constructor.name)} uses the type parameter in a way `derive` does not handle"),
        helps = Seq("write the `impl` by hand"))
      case DeriveFieldInstanceMissing(traitName, target, constructor, head) => Description(
        s"`derive ${traitName.name}[${target.name}]` needs an instance for ${name(head)}", "this derive",
        notes = Seq(s"the constructor ${name(constructor.name)} has a field built with ${name(head)}, and `${traitName.name}[$head]` is not declared before this point"),
        helps = Seq(s"declare `derive ${traitName.name}[$head]` first; if the two types refer to each other, put both derives in one `rec { … }`"))

      // ---- 制約の解決 ----
      case NoInstance(traitName, args) => Description(
        s"no instance for `${constraint(traitName, args)}`", "this needs the instance")
      case ResolutionDepthExceeded(traitName, args) => Description(
        s"instance resolution for `${constraint(traitName, args)}` does not finish", "while resolving the constraints of this expression",
        notes = Seq("resolving an instance needed another instance more than 64 levels deep; the contexts of the instances probably require each other"))

      // ---- context ----
      case ContextMonadKindInvalid(monad, k) => Description(
        "`context` needs a type constructor", s"${ty(monad)} has kind ${kind(k)}, expected `* → *`")
      case ContextMethodMissing(method) => Description(
        s"`context` blocks need the method ${name(method.name)}", "this block",
        notes = Seq("a `context[M] { … }` block is rewritten to calls of `pure` and `flatMap`"),
        helps = Seq("add `import \"std/monad\"` (and an instance of `Monad` for the type)"))
      case InContext(inner) =>
        val d = describe(inner)
        d.copy(notes = d.notes :+ "this arose while rewriting a `context` block to `pure` / `flatMap`")

      // ---- 文字列埋め込み ----
      case StrInterpShowMissing(partType) => Description(
        s"cannot embed a value of type ${ty(partType)} in a string", "this string",
        notes = Seq("values other than strings are converted with the method `show`, which is not defined here"),
        helps = Seq("declare `trait Show[a] { def show(x: a): foreign.C.String }` and an instance for the type"))
      case StrInterpShowNotPolymorphic(actual) => Description("`show` cannot be used for string interpolation", s"`show` has type ${ty(actual)}, expected `∀a. …`")
      case StrInterpShowKindInvalid(k) => Description("`show` cannot be used for string interpolation", s"its type parameter has kind ${kind(k)}, expected `*`")
      case StrInterpShowNotFunction(actual) => Description("`show` cannot be used for string interpolation", s"`show` has type ${ty(actual)}, which is not a function")
      case StrInterpShowParamMismatch(expected, partType) => Description(
        s"cannot embed a value of type ${ty(partType)} in a string", s"`show` takes ${ty(expected)}")
      case StrInterpShowResultMismatch(expected, actual) => Description(
        "`show` cannot be used for string interpolation", s"`show` returns ${ty(actual)}, expected ${ty(expected)}")

      // ---- エントリポイント ----
      case MainMissing => Description(
        "the program has no `main`",
        helps = Seq("add `let main(): i32 = …` at the top level; its result is the exit code"))
      case MainTypeInvalid(actual) => Description(
        "`main` has the wrong type",
        "this is the program's entry point",
        notes = Seq(s"expected: `unit → i32`\n   found: `${actual.show}`"),
        helps = Seq("declare it as `let main(): i32 = …`"))
    }
  }

  private def kindMismatchHelp(expected: Kind, actual: Kind): Seq[String] =
    if (expected != Kind.Star && actual == Kind.Star)
      Seq(s"a type constructor is expected here. If this is a bound type variable, give its kind: `∀(f: ${expected.show}). …` or `[f: ${expected.show}]` (the default kind is `*`)")
    else if (expected == Kind.Star) Seq("a complete type is expected here: apply the type constructor to its arguments")
    else Nil

  // ---- 構文エラー ----

  // (説明, 読めなかった字句の幅)
  private def syntaxError(failure: cats.parse.Parser.Error, file: Sources.File): (Description, Int) = {
    import cats.parse.Parser.Expectation
    val offset = failure.failedAtOffset
    val rest = file.text.drop(offset)
    // 読めなかった字句: 名前や数はひとまとまり、記号は 1 文字
    val token =
      if (rest.trim.isEmpty) ""
      else if (rest.head.isLetterOrDigit || rest.head == '_') rest.takeWhile(c => c.isLetterOrDigit || c == '_')
      else if (rest.head.isWhitespace) ""
      else rest.take(1)
    val found = if (rest.trim.isEmpty) "the end of the file" else if (token.isEmpty) "whitespace" else name(token)

    val raw = failure.expected.toList.flatMap {
      case Expectation.OneOfStr(_, strs) => strs.map(name)
      case Expectation.InRange(_, lo, hi) =>
        val chars = (lo to hi).filterNot(_.isWhitespace)
        // 文字の範囲は、名前・数・個々の記号として示す
        (if (chars.exists(_.isLetter)) Seq("a name") else Nil) ++
          (if (chars.exists(_.isDigit)) Seq("a number") else Nil) ++
          chars.filterNot(c => c.isLetterOrDigit || c == '_').take(4).map(c => if (c == '`') "a backtick" else name(c.toString))
      case Expectation.EndOfString(_, _) => Seq("the end of the file")
      case Expectation.FailWith(_, message) => Seq(message)
      case _ => Nil
    }.distinct
    // 式が始まる位置なら、始まりうる字句を並べる代わりに「式」と言う
    val startsExpression = raw.contains("`true`") && raw.contains("`false`")
    val closers = Set("`)`", "`]`", "`}`", "`=`", "`,`", "`;`", "`=>`", "`in`", "`then`", "`else`", "`:`", "`.`", "`⇒`", "the end of the file")
    val expected = if (startsExpression) "an expression" +: raw.filter(closers) else raw
    val onlyEnd = expected == Seq("the end of the file")
    val label =
      if (onlyEnd || expected.isEmpty) s"unexpected $found"
      else if (expected.length <= 6) s"expected ${orList(expected)}, found $found"
      else s"unexpected $found"
    val notes =
      if (onlyEnd) Seq("a declaration is complete before this point, so a new declaration (`let`, `rec`, `data`, `type`, `trait`, `impl`, `derive`, `module`, `open`, `import`) or the end of the file is expected")
      else if (expected.length > 6) Seq(s"expected one of: ${expected.mkString(", ")}")
      else Nil

    val (line, _) = file.position(offset)
    val text = file.line(line)
    val previous = if (line > 0) file.line(line - 1) else ""
    def has(pattern: String): Boolean = pattern.r.findFirstIn(text).nonEmpty
    val helps = Seq(
      Option.when(has("""\blet\s+rec\b"""))("recursion is marked in front of the declaration: write `rec let …`"),
      Option.when(has("""\bdata\s+rec\b"""))("recursion is marked in front of the declaration: write `rec data …`"),
      Option.when(has("""\bimpl\s+rec\b"""))("recursion is marked in front of the declaration: write `rec impl …`"),
      Option.when(has("""--|#""") || rest.startsWith("*/"))("comments are written `// …` (to the end of the line) or `/* … */`"),
      Option.when(file.text.sliding(2).count(_ == "/*") > file.text.sliding(2).count(_ == "*/"))("a `/*` comment is not closed"),
      Option.when(has("""\bfold\b.*\bwith\b"""))("the cases of a fold are written in braces: `fold e as T { C(x) => … }`"),
      Option.when(has("""^\s*(rec\s+)?let\s+\w+(\[[^\]]*\])*(\([^)]*\))*\s*="""))("every definition states its type: `let x: T = …`, or `let f(x: T): U = …` for a function"),
      Option.when(token.nonEmpty && Set("let", "rec", "data", "type", "trait", "impl", "derive", "module", "open", "import", "in", "then", "else", "with", "as", "match", "fold", "if")(token) && startsExpression)(
        s"`$token` is a keyword, and the expression before it is not finished"),
      Option.when(has("""->"""))("functions and cases use the arrows `→` and `=>`"),
      Option.when(has("""\\""") && !has("\""))("a function is written `λx: T. body`"),
      Option.when(has("""^\s*\|""") || previous.trim.endsWith("with"))("the cases of a match are written in braces, one per line: `match e { C(x) => … }`"),
      Option.when(rest.trim.isEmpty && file.text.count(_ == '{') > file.text.count(_ == '}'))("a `{` is not closed")
    ).flatten

    (Description("syntax error", label, notes, helps), token.length.max(1))
  }

  private def orList(items: Seq[String]): String = items match {
    case Seq(only) => only
    case init :+ last => s"${init.mkString(", ")} or $last"
    case _ => ""
  }
}
