package com.yuuki14202028

/** エラーに関係する、別の位置（表示では - の下線になる） */
enum Related {
  case Annotation(expected: Type)
  case Function(tpe: Type)
  case FirstCase(tpe: Type)
  case ThenBranch(tpe: Type)
  case LeftOperand(tpe: Type)
  case FirstElement(tpe: Type)
}

/** エラーが起きた場所の環境からわかる、直し方の手がかり */
enum Hint {
  case Similar(names: List[String])
  case DeclaredLater(name: String)
  // keyword は rec let / rec data
  case NeedsRec(name: String, keyword: String)
  case OpaqueType(name: String)
  case InstanceNeedsRec(instance: String)
  case InstanceDeclaredLater(instance: String)
}

enum FieldNamesReason {
  case SeveralConstructors, LocalData, OpaqueData
}

/** ユーザーに直せるエラー。表示用の文字列ではなく、型や名前を持つ（表示は Diagnostics） */
enum CompileError {
  def span: Span

  // エラーに、関係する位置と手がかりを付けたもの
  case Detailed(error: CompileError, addedRelated: List[(Span, Related)], addedHints: List[Hint], span: Span)

  case Syntax(expected: String, found: String, span: Span)
  case ImportNotFound(path: String, span: Span)
  case ImportCycle(path: String, span: Span)

  // ---- 名前解決 ----
  case UndefinedVariable(name: String, span: Span)
  case UndefinedType(name: String, span: Span)
  case UndefinedModule(name: String, span: Span)
  case UndefinedOperator(operator: String, isPrefix: Boolean, span: Span)
  case PrivateName(name: String, isType: Boolean, span: Span)
  case AlreadyDefined(name: String, span: Span)
  case NotAConstructor(name: String, span: Span)
  case OperatorsNotAssociative(left: String, leftFixity: Fixity, right: String, rightFixity: Fixity, span: Span)
  case OperatorBuiltin(operator: String, span: Span)
  case MixedRecursiveGroup(span: Span)
  case OutsideModule(keyword: String, span: Span)
  case UnknownIntrinsic(name: String, span: Span)
  // 文字列埋め込み・リテラル・組に、関数が宣言されていない（declaration はその宣言の書き出し）
  case SyntaxNotDeclared(what: String, declaration: String, span: Span)
  case EmptyListLiteral(span: Span)
  case SyntaxPatternNotConstructor(what: String, span: Span)
  case FieldNamesNotAllowed(data: String, reason: FieldNamesReason, span: Span)
  case PatternBindsTwice(name: String, span: Span)
  case AlternativesBindDifferently(name: String, span: Span)

  // すでに失敗した宣言を使っている（原因は報告済みなので、これ自体は報告しない）
  case DependsOnFailedDeclaration(span: Span)

  // ---- 型 ----
  case TypeMismatch(expected: Type, actual: Type, span: Span)
  case KindMismatch(expected: Kind, actual: Kind, span: Span)
  case NotAFunction(actual: Type, span: Span)
  case NotPolymorphic(actual: Type, span: Span)
  case TypeArgumentUndetermined(parameter: String, function: Type, span: Span)
  case IntrinsicArguments(intrinsic: Intrinsic, span: Span)
  case ForeignNotFunction(actual: Type, span: Span)
  case RecursiveValueNotFunction(name: String, span: Span)
  case LocalTypeEscapes(actual: Type, span: Span)
  // module は受け手の型の module の名前（なければ None）、members はそのメンバー
  case SelectUnknown(name: String, receiver: Type, module: Option[String], members: List[String], span: Span)
  case SelectAmbiguous(name: String, receiver: Type, function: String, span: Span)
  case SelectNotApplicable(function: String, functionType: Type, receiver: Type, span: Span)
  case NamedArgumentsInvalid(reason: String, span: Span)
  case MissingFields(constructor: String, fields: List[String], span: Span)
  case UnknownField(constructor: String, field: String, fields: List[String], span: Span)

  // ---- パターン ----
  case NotADataType(actual: Type, span: Span)
  case PatternConstructorMismatch(constructor: String, scrutinee: Type, span: Span)
  case ConstructorArity(constructor: String, expected: Int, actual: Int, span: Span)
  case NonExhaustiveMatch(example: String, guarded: Boolean, span: Span)
  case UnreachableCase(index: Int, span: Span)
  case RefutablePattern(example: String, span: Span)
  case FoldPattern(span: Span)

  // ---- trait とインスタンス ----
  case NotAConstraint(actual: Type, span: Span)
  case ConstraintBinderUnused(parameter: String, span: Span)
  // requiredBy は、この制約に至るまでの（必要とした制約, それを必要としたインスタンス）。外側が先頭。known は宣言済みのインスタンス
  case NoInstance(goal: Type, requiredBy: List[(String, String)], known: List[String], span: Span)
  case MethodTypeMismatch(instance: String, method: String, expected: Type, actual: Type, span: Span)
  case DeriveNeeds(traitName: String, target: String, goal: Type, span: Span)
  case ResolutionTooDeep(goal: Type, span: Span)
  case NoConstraintToSatisfy(function: Type, span: Span)
  case UsingNotDictionary(actual: Type, span: Span)
  case NotATrait(name: String, span: Span)
  case ImplTargetArity(traitName: String, expected: Int, actual: Int, span: Span)
  case InstanceHeadInvalid(target: Type, span: Span)
  case ImplParameterUnused(parameter: String, span: Span)
  case OverlappingInstance(instance: String, span: Span)
  case MissingMethods(instance: String, methods: List[String], span: Span)
  case ExtraMethod(traitName: String, method: String, span: Span)
  case RecursiveInstanceMethodNotFunction(method: String, span: Span)
  case DeriveUnsupported(traitName: String, target: String, reason: String, span: Span)

  case MainMissing(span: Span)
  case MainTypeInvalid(actual: Type, span: Span)

  /** 付けた情報を外した、エラーそのもの */
  def core: CompileError = this match {
    case Detailed(error, _, _, _) => error.core
    case other => other
  }

  def related: List[(Span, Related)] = this match {
    case Detailed(error, added, _, _) => error.related ++ added
    case _ => Nil
  }

  def hints: List[Hint] = this match {
    case Detailed(error, _, added, _) => error.hints ++ added
    case _ => Nil
  }

  def withRelated(at: Span, reason: Related): CompileError = Detailed(this, List(at -> reason), Nil, span)
  def withHints(added: List[Hint]): CompileError = if (added.isEmpty) this else Detailed(this, Nil, added, span)

  /** 1 行の説明 */
  def message: String = this match {
    case Detailed(error, _, _, _) => error.message
    case Syntax(expected, found, _) => s"syntax error: expected $expected, found $found"
    case ImportNotFound(path, _) => s"cannot read the imported file `$path`"
    case ImportCycle(path, _) => s"`$path` is imported inside a module while it is itself being imported"
    case UndefinedVariable(name, _) => s"cannot find value `$name` in this scope"
    case UndefinedType(name, _) => s"cannot find type `$name` in this scope"
    case UndefinedModule(name, _) => s"cannot find module `$name` in this scope"
    case UndefinedOperator(operator, isPrefix, _) => s"the ${if (isPrefix) "prefix " else ""}operator `$operator` is not declared"
    case PrivateName(name, _, _) => s"`$name` is private"
    case AlreadyDefined(name, _) => s"`$name` is defined more than once"
    case NotAConstructor(name, _) => s"`$name` is not a constructor"
    case OperatorsNotAssociative(left, _, right, _, _) => s"`$left` and `$right` cannot be mixed without parentheses"
    case OperatorBuiltin(operator, _) => s"the operator `$operator` is built in and cannot be declared"
    case OutsideModule(keyword, _) => s"`$keyword` can be used only inside a module"
    case MixedRecursiveGroup(_) => "a `rec { … }` group must contain declarations of one kind: `let`s, `data`s, or `impl`s and `derive`s"
    case UnknownIntrinsic(name, _) => s"unknown intrinsic `$name`"
    case SyntaxNotDeclared(what, declaration, _) => s"$what cannot be used here: no `$declaration = …` declaration is in scope"
    case EmptyListLiteral(_) => "an empty list literal has no element to read its type from"
    case SyntaxPatternNotConstructor(what, _) => s"a $what pattern can be used only when the declared function is a constructor"
    case FieldNamesNotAllowed(data, _, _) => s"data type `$data` cannot have named fields"
    case PatternBindsTwice(name, _) => s"`$name` is bound more than once in this pattern"
    case AlternativesBindDifferently(name, _) => s"every alternative of `|` must bind the same variables, but `$name` is not bound in all of them"
    case DependsOnFailedDeclaration(_) => "this uses a declaration that failed to compile"
    case TypeMismatch(expected, actual, _) => s"mismatched types: expected `${Type.show(expected)}`, found `${Type.show(actual)}`"
    case KindMismatch(expected, actual, _) => s"mismatched kinds: expected `${expected.show}`, found `${actual.show}`"
    case NotAFunction(actual, _) => s"this is applied like a function, but it has type `${Type.show(actual)}`"
    case NotPolymorphic(actual, _) => s"type arguments are given to a value of type `${Type.show(actual)}`, which takes none"
    case TypeArgumentUndetermined(parameter, function, _) =>
      s"the type argument `$parameter` of `${Type.show(function)}` cannot be read from the arguments; write it as `[…]`"
    case IntrinsicArguments(intrinsic, _) => s"`intrinsic[${intrinsic.name}]` cannot be used with these arguments"
    case ForeignNotFunction(actual, _) => s"a foreign value must be a function, but it has type `${Type.show(actual)}`"
    case RecursiveValueNotFunction(name, _) => s"`$name` would be read before it is initialized: a `rec` value that refers to its group must be a function"
    case LocalTypeEscapes(actual, _) => s"the result type `${Type.show(actual)}` mentions a data type declared inside the expression"
    case SelectUnknown(name, receiver, _, _, _) => s"no function `$name` for a value of type `${Type.show(receiver)}`"
    case SelectAmbiguous(name, receiver, function, _) => s"`.$name` is ambiguous: both the module function `$function` and the trait method `$name` apply"
    case SelectNotApplicable(function, functionType, receiver, _) =>
      s"`$function` of type `${Type.show(functionType)}` cannot take a value of type `${Type.show(receiver)}` as its first argument"
    case NamedArgumentsInvalid(reason, _) => s"invalid named arguments: $reason"
    case MissingFields(constructor, fields, _) => s"`$constructor` is missing ${fields.map(f => s"`$f`").mkString(", ")}"
    case UnknownField(constructor, field, _, _) => s"`$constructor` has no field `$field`"
    case MethodTypeMismatch(instance, method, expected, actual, _) =>
      s"the method `$method` of $instance has the wrong type: expected `${Type.show(expected)}`, found `${Type.show(actual)}`"
    case DeriveNeeds(traitName, target, goal, _) => s"`derive $traitName[$target]` needs `${Type.show(goal)}`"
    case NotADataType(actual, _) => s"a value of type `${Type.show(actual)}` cannot be taken apart by constructor patterns"
    case PatternConstructorMismatch(constructor, scrutinee, _) => s"the constructor `$constructor` does not belong to `${Type.show(scrutinee)}`"
    case ConstructorArity(constructor, expected, actual, _) => s"`$constructor` has $expected field(s), but the pattern has $actual"
    case NonExhaustiveMatch(example, _, _) => s"this match does not cover every value: `$example` is not matched"
    case UnreachableCase(index, _) => s"case $index of this match can never be reached"
    case RefutablePattern(example, _) => s"this pattern does not match every value: `$example` is not matched"
    case FoldPattern(_) => "a case of `fold` must be a constructor applied to variables"
    case ConstraintBinderUnused(parameter, _) => s"the type variable `$parameter` bound by `∀` does not appear in the constraint it leads to"
    case NotAConstraint(actual, _) => s"`${Type.show(actual)}` is not a constraint: a constraint is a trait applied to types"
    case NoInstance(goal, _, _, _) => s"no instance for `${Type.show(goal)}`"
    case ResolutionTooDeep(goal, _) => s"resolving the instance `${Type.show(goal)}` does not finish"
    case NoConstraintToSatisfy(function, _) => s"a dictionary is passed with `(with …)`, but `${Type.show(function)}` has no constraint left to satisfy"
    case UsingNotDictionary(actual, _) => s"`using` needs a dictionary, but this has type `${Type.show(actual)}`"
    case NotATrait(name, _) => s"`$name` is not a trait"
    case ImplTargetArity(traitName, expected, actual, _) => s"the trait `$traitName` takes $expected type argument(s), but $actual are given"
    case InstanceHeadInvalid(target, _) => s"an instance cannot be declared for `${Type.show(target)}`"
    case ImplParameterUnused(parameter, _) => s"the type parameter `$parameter` does not appear in the types the instance is for"
    case OverlappingInstance(instance, _) => s"overlapping instances: `$instance` is already declared"
    case MissingMethods(instance, methods, _) => s"$instance does not define ${methods.map(m => s"`$m`").mkString(", ")}"
    case ExtraMethod(traitName, method, _) => s"`$method` is not a method of the trait `$traitName`"
    case RecursiveInstanceMethodNotFunction(method, _) => s"the method `$method` would read an instance of its `rec` group before it is initialized: it must be a function"
    case DeriveUnsupported(traitName, target, reason, _) => s"cannot derive `$traitName` for `$target`: $reason"
    case MainMissing(_) => "the program has no `main`"
    case MainTypeInvalid(actual, _) => s"`main` must have type `unit → i32`, but it has type `${Type.show(actual)}`"
  }
}

/** ユーザーエラーで検査を打ち切るための例外。宣言の境界で捕まえて、エラーの一覧に集める */
final class CompileFailure(val errors: List[CompileError]) extends RuntimeException(errors.head.message) {
  def error: CompileError = errors.head

  /** 各エラーを f で置き換えたもの */
  def map(f: CompileError => CompileError): CompileFailure = new CompileFailure(errors.map(f))
}

object CompileFailure {
  def apply(error: CompileError): CompileFailure = new CompileFailure(List(error))

  /** items をすべて検査する。失敗したものがあれば、そのエラーをまとめて投げる */
  def collect[A, B](items: List[A])(check: A => B): List[B] = {
    val results = items.map(item => try Right(check(item)) catch { case failure: CompileFailure => Left(failure.errors) })
    val errors = results.collect { case Left(found) => found }.flatten
    if (errors.nonEmpty) throw new CompileFailure(errors)
    results.collect { case Right(value) => value }
  }
}
