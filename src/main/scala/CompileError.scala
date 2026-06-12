package com.yuuki14202028

import java.nio.file.Path

enum DeclRef {
  case TraitDecl(name: TypeVariable)
  case ImplDecl(traitName: TypeVariable, heads: Seq[String])
  case LetDecl(name: Variable)
  case DataDecl(name: TypeVariable)
  case TypeAliasDecl(name: TypeVariable)

  def shown: String = this match {
    case TraitDecl(n) => s"Trait ${n.name}"
    case ImplDecl(t, heads) => s"impl ${t.name}${heads.map(h => s"[$h]").mkString}"
    case LetDecl(n) => s"let ${n.name}"
    case DataDecl(n) => s"Data type ${n.name}"
    case TypeAliasDecl(n) => s"Type alias ${n.name}"
  }

  def paramNoun: String = this match {
    case ImplDecl(_, _) | LetDecl(_) => "type parameter"
    case _ => "parameter"
  }
}

enum NameKind {
  case TypeVariableName, TypeAliasName, DataTypeName, TraitName

  def shown: String = this match {
    case TypeVariableName => "Type variable"
    case TypeAliasName => "Type alias"
    case DataTypeName => "Data type"
    case TraitName => "Trait"
  }
}

enum MemberKind {
  case ConstructorMember
  case MethodMember

  def singular: String = this match {
    case ConstructorMember => "constructor"
    case MethodMember => "method"
  }
}

enum MatchKindRef {
  case MatchExpr, FoldExpr

  def shown: String = this match {
    case MatchExpr => "Match"
    case FoldExpr => "Fold"
  }
}

enum ArrowSide {
  case Lhs, Rhs

  def shown: String = this match {
    case Lhs => "LHS"
    case Rhs => "RHS"
  }
}

enum OperandRequirement {
  case Numeric, Integer, Equatable, NumericOrBool, Bool

  def shown: String = this match {
    case Numeric => "numeric"
    case Integer => "integer"
    case Equatable => "equatable"
    case NumericOrBool => "numeric or bool"
    case Bool => "bool"
  }
}

/** ユーザー起因のコンパイルエラー。エラーの種類ごとに case を持ち、
 * 表示用の文字列は持たず構造化された情報（型・名前・カインド等）だけを運ぶ。
 * 表示は render が一元的に行う（将来の多言語対応は render の差し替えで行う）。
 */
enum CompileError {
  case At(offset: Int, error: CompileError)

  // ---- 構文・モジュール ----
  case ParseFailure(error: cats.parse.Parser.Error)
  case ImportReadFailure(path: Path, cause: Exception)
  case ImportParseFailure(path: Path, error: cats.parse.Parser.Error)
  case UnresolvedImport(path: String)

  // ---- 名前解決 ----
  case UndefinedVariable(name: Variable)
  case UndefinedTypeVariable(name: TypeVariable)
  case UndefinedPrimitive(name: String)
  case UndefinedTrait(name: TypeVariable, site: Option[DeclRef])
  case UndefinedDataConstructor(dataName: TypeVariable)
  case AlreadyDefined(kind: NameKind, name: String)
  case ParamAlreadyDefined(owner: DeclRef)
  case DuplicateParams(owner: DeclRef)
  case MemberAlreadyDefined(owner: DeclRef, member: MemberKind)
  case DictionaryCtorAlreadyDefined(traitName: TypeVariable, ctorName: Variable)
  case DuplicateMembers(owner: DeclRef, member: MemberKind)

  // ---- 型 ----
  case TypeMismatch(expected: TypeRec[Type], actual: TypeRec[Type])
  case ExpectedNumeric(actual: TypeRec[Type])
  case ExpectedEquatable(actual: TypeRec[Type])
  case NotAFunction(actual: TypeRec[Type])
  case NotPolymorphic(actual: TypeRec[Type])
  case NotADataType(actual: TypeRec[Type])
  case DataArityMismatch(name: String, expected: Int, actual: Int)
  case ForeignNotFunction(t: TypeRec[Type])
  case NumericLiteralUnknown(typeName: String)
  case RecursiveDataNotDeclared(name: TypeVariable)

  // ---- カインド ----
  case KindNotStar(t: TypeRec[Type], kind: Kind)
  case KindMismatchInTypeApp(expected: Kind, actual: Kind, argument: Option[TypeRec[Type]])
  case CannotApplyKind(kind: Kind, function: TypeRec[Type])
  case ArrowKindNotStar(side: ArrowSide, kind: Kind, operand: TypeRec[Type])
  case ForAllBodyKindNotStar(kind: Kind, body: TypeRec[Type])

  // ---- 演算子・組み込み ----
  case OperatorUnresolvable(op: BinOps | UnaryOps, operandType: TypeRec[Type])
  case OperatorUndefined(op: BinOps | UnaryOps, operandType: TypeRec[Type], function: Variable)
  case OperatorNotAFunction(function: Variable, fnType: TypeRec[Type])
  case OperatorNotBinary(function: Variable, fnType: TypeRec[Type])
  case IntrinsicArityMismatch(op: BinOps | UnaryOps, actual: Int)
  case IntrinsicOperandInvalid(op: BinOps | UnaryOps, requirement: OperandRequirement)
  case IntrinsicUnsupported(op: BinOps)

  // ---- match / fold ----
  case DuplicateCases(kind: MatchKindRef)
  case DuplicateBinders(kind: MatchKindRef, constructor: Variable)
  case NonExhaustive(kind: MatchKindRef, missing: Seq[Variable], invalid: Seq[Variable])
  case BinderArityMismatch(constructor: Variable, expected: Int, actual: Int)
  case EmptyMatch

  // ---- trait / impl / where ----
  case TraitNeedsParameter(name: TypeVariable)
  case DefaultMethodsUnsupported(name: TypeVariable)
  case ConstraintArityMismatch(site: DeclRef, name: TypeVariable, expected: Int, actual: Int)
  case ConstraintKindMismatch(site: DeclRef, name: TypeVariable, arg: TypeRec[Type], actual: Kind, expected: Kind)
  case ImplTargetArity(traitName: TypeVariable, expected: Int, actual: Int)
  case ImplKindMismatch(traitName: TypeVariable, target: TypeRec[Type], paramKind: Kind, targetKind: Kind)
  case InstanceHeadInvalid(traitName: TypeVariable, target: TypeRec[Type])
  case InstanceHeadTypeParameter(impl: DeclRef)
  case ImplParamUnused(impl: DeclRef)
  case OverlappingInstance(traitName: TypeVariable, heads: Seq[String])
  case MissingMethods(impl: DeclRef, methods: Seq[Variable])
  case ExtraMethods(impl: DeclRef, methods: Seq[Variable])
  case MethodBodyTypeMismatch(impl: DeclRef, method: Variable, expected: TypeRec[Type], actual: TypeRec[Type])
  case MethodSigTypeMismatch(impl: DeclRef, method: Variable, declared: TypeRec[Type], expected: TypeRec[Type])
  case EmptyWhereClause(name: Variable)
  case WhereLeadingForalls(name: Variable, expected: Int, got: Int)
  case WherePolymorphic(name: Variable)

  // ---- 辞書解決（TraitEncoder） ----
  case NoInstance(name: TypeVariable, args: Seq[TypeRec[Type]])
  case ResolutionDepthExceeded(name: TypeVariable, args: Seq[TypeRec[Type]])
  case AmbiguousConstraint(name: Variable, arity: Int)

  // ---- context 記法 ----
  case ContextMonadKindInvalid(monad: TypeRec[Type], kind: Kind)
  case ContextMethodMissing(name: Variable)
  case InContext(error: CompileError)

  // ---- 文字列埋め込み ----
  case StrInterpShowMissing(partType: TypeRec[Type])
  case StrInterpShowNotPolymorphic(actual: TypeRec[Type])
  case StrInterpShowKindInvalid(kind: Kind)
  case StrInterpShowNotFunction(actual: TypeRec[Type])
  case StrInterpShowParamMismatch(expected: TypeRec[Type], partType: TypeRec[Type])
  case StrInterpShowResultMismatch(expected: TypeRec[Type], actual: TypeRec[Type])

  // ---- エントリポイント ----
  case MainMissing
  case MainTypeInvalid(actual: TypeRec[Type])

  def at(offset: Int): CompileError = this match {
    case At(_, _) => this
    case _ => At(offset, this)
  }

  def render: String = this match {
    case At(_, error) => error.render

    case ParseFailure(error) => s"Parse error: expected ${expectationsShown(error.expected)}"
    case ImportReadFailure(path, cause) => s"Import error: $path: ${cause.getMessage}"
    case ImportParseFailure(path, error) =>
      s"Import error: parse error in $path: expected ${expectationsShown(error.expected)}"
    case UnresolvedImport(path) => s"Unresolved import: $path"

    case UndefinedVariable(name) => s"Variable ${name.name} is not defined"
    case UndefinedTypeVariable(name) => s"Type variable ${name.name} is not defined"
    case UndefinedPrimitive(name) => s"Primitive type $name is not defined"
    case UndefinedTrait(name, site) =>
      site.fold(s"Trait ${name.name} is not defined")(s => s"${s.shown}: trait ${name.name} is not defined")
    case UndefinedDataConstructor(dataName) => s"Constructor for data type ${dataName.name} is not defined"
    case AlreadyDefined(kind, name) => s"${kind.shown} $name is already defined"
    case ParamAlreadyDefined(owner) => s"${owner.shown} has a ${owner.paramNoun} that is already defined"
    case DuplicateParams(owner) => s"${owner.shown} has duplicate ${owner.paramNoun}s"
    case MemberAlreadyDefined(owner, member) => s"${owner.shown} has a ${member.singular} that is already defined"
    case DictionaryCtorAlreadyDefined(traitName, ctorName) =>
      s"Trait ${traitName.name}: dictionary constructor ${ctorName.name} is already defined"
    case DuplicateMembers(owner, member) => s"${owner.shown} has duplicate ${member.singular}s"

    case TypeMismatch(expected, actual) => s"Type mismatch: expected ${expected.show}, actual ${actual.show}"
    case ExpectedNumeric(actual) => s"Type mismatch: expected numeric, actual ${actual.show}"
    case ExpectedEquatable(actual) => s"Type mismatch: expected numeric, char, or bool, actual ${actual.show}"
    case NotAFunction(actual) => s"Not a function: ${actual.show}"
    case NotPolymorphic(actual) => s"Not a polymorphic function: ${actual.show}"
    case NotADataType(actual) => s"Not a data type: ${actual.show}"
    case DataArityMismatch(name, expected, actual) => s"Data type $name expects $expected arguments, got $actual"
    case ForeignNotFunction(t) => s"Foreign function must have at least one argument: ${t.show}"
    case NumericLiteralUnknown(typeName) => s"Numeric literal type $typeName is not defined"
    case RecursiveDataNotDeclared(name) => s"Recursive data type ${name.name} must be declared with data rec"

    case KindNotStar(t, kind) => s"Type ${t.show} has kind ${kind.show}, expected *"
    case KindMismatchInTypeApp(expected, actual, argument) =>
      val suffix = argument.fold("")(arg => s" (argument ${arg.show})")
      s"Kind mismatch in type application: expected ${expected.show}, got ${actual.show}$suffix"
    case CannotApplyKind(kind, function) => s"Cannot apply a type of kind ${kind.show}: ${function.show}"
    case ArrowKindNotStar(side, kind, operand) => s"Arrow ${side.shown} must have kind *, got ${kind.show}: ${operand.show}"
    case ForAllBodyKindNotStar(kind, body) => s"∀ body must have kind *, got ${kind.show}: ${body.show}"

    case OperatorUnresolvable(op, operandType) => s"Cannot resolve operator $op for type ${operandType.show}"
    case OperatorUndefined(op, operandType, function) =>
      s"Operator $op is not defined for type ${operandType.show}; expected function ${function.name}"
    case OperatorNotAFunction(function, fnType) => s"Operator function ${function.name} must be a function: ${fnType.show}"
    case OperatorNotBinary(function, fnType) => s"Operator function ${function.name} must take two arguments: ${fnType.show}"
    case IntrinsicArityMismatch(op, actual) => op match {
      case _: BinOps => s"Binary intrinsic $op expects 2 arguments, got $actual"
      case _: UnaryOps => s"Unary intrinsic $op expects 1 argument, got $actual"
    }
    case IntrinsicOperandInvalid(op, requirement) =>
      val noun = op match {
        case _: BinOps => "operands"
        case _: UnaryOps => "operand"
      }
      s"Intrinsic $op requires ${requirement.shown} $noun"
    case IntrinsicUnsupported(op) => s"Intrinsic $op is not supported; define it as an operator function"

    case DuplicateCases(kind) => s"${kind.shown} has duplicate cases"
    case DuplicateBinders(kind, constructor) => s"${kind.shown} case ${constructor.name} has duplicate binders"
    case NonExhaustive(kind, missing, invalid) =>
      s"Non-exhaustive or invalid ${kind.shown.toLowerCase}: missing ${missing.map(_.name).mkString(", ")}, invalid ${invalid.map(_.name).mkString(", ")}"
    case BinderArityMismatch(constructor, expected, actual) =>
      s"Constructor ${constructor.name} expects $expected binders, got $actual"
    case EmptyMatch => "Match must have at least one case"

    case TraitNeedsParameter(name) => s"Trait ${name.name} must have at least one parameter"
    case DefaultMethodsUnsupported(name) => s"Trait ${name.name}: default method implementations are not supported yet"
    case ConstraintArityMismatch(site, name, expected, actual) =>
      s"${site.shown}: constraint ${name.name} expects $expected type arguments, got $actual"
    case ConstraintKindMismatch(site, name, arg, actual, expected) =>
      s"${site.shown}: constraint ${name.name} argument ${arg.show} has kind ${actual.show}, expected ${expected.show}"
    case ImplTargetArity(traitName, expected, actual) =>
      s"impl ${traitName.name}: trait expects $expected type argument(s), got $actual"
    case ImplKindMismatch(traitName, target, paramKind, targetKind) =>
      s"Kind mismatch in impl ${traitName.name}[${target.show}]: trait parameter has kind ${paramKind.show}, target has kind ${targetKind.show}"
    case InstanceHeadInvalid(traitName, target) =>
      s"impl ${traitName.name}[${target.show}]: instance head must be a type constructor"
    case InstanceHeadTypeParameter(impl) => s"${impl.shown}: instance head must be a type constructor, not a type parameter"
    case ImplParamUnused(impl) => s"${impl.shown}: every type parameter must occur in the instance target"
    case OverlappingInstance(traitName, heads) =>
      s"Overlapping instance: ${traitName.name}${heads.map(h => s"[$h]").mkString} is already defined"
    case MissingMethods(impl, methods) => s"Missing method in ${impl.shown}: ${methods.map(_.name).mkString(", ")}"
    case ExtraMethods(impl, methods) => s"Extra method in ${impl.shown}: ${methods.map(_.name).mkString(", ")}"
    case MethodBodyTypeMismatch(impl, method, expected, actual) =>
      s"Method type mismatch in ${impl.shown}: ${method.name} must have type ${expected.show}, actual ${actual.show}"
    case MethodSigTypeMismatch(impl, method, declared, expected) =>
      s"Method type mismatch in ${impl.shown}: ${method.name} is declared as ${declared.show}, expected ${expected.show}"
    case EmptyWhereClause(name) => s"let ${name.name}: where clause must not be empty"
    case WhereLeadingForalls(name, expected, got) =>
      s"let ${name.name}: a where-constrained function must bind its type parameters as leading ∀s (expected $expected leading ∀ binders, got $got)"
    case WherePolymorphic(name) =>
      s"let ${name.name}: a where-constrained function cannot have a polymorphic type beyond its declared type parameters"

    case NoInstance(name, args) => s"No instance for ${constraintShown(name, args)}"
    case ResolutionDepthExceeded(name, args) =>
      s"Instance resolution depth limit exceeded while resolving ${constraintShown(name, args)} (possible cycle)"
    case AmbiguousConstraint(name, arity) =>
      s"Ambiguous constraint: ${name.name} requires $arity type application(s) to resolve its dictionaries"

    case ContextMonadKindInvalid(monad, kind) =>
      s"context: ${monad.show} has kind ${kind.show}, expected ${Kind.Arrow(Kind.Star, Kind.Star).show}"
    case ContextMethodMissing(name) => s"context: method `${name.name}` is not defined"
    case InContext(error) => s"context: ${error.render}"

    case StrInterpShowMissing(partType) =>
      s"String interpolation: cannot embed ${partType.show}; method `show` is not defined"
    case StrInterpShowNotPolymorphic(actual) => s"String interpolation: `show` must be polymorphic, actual ${actual.show}"
    case StrInterpShowKindInvalid(kind) => s"String interpolation: `show` type parameter must have kind *, actual ${kind.show}"
    case StrInterpShowNotFunction(actual) => s"String interpolation: `show` must be a function, actual ${actual.show}"
    case StrInterpShowParamMismatch(expected, partType) =>
      s"String interpolation: `show` cannot accept ${partType.show}, expected ${expected.show}"
    case StrInterpShowResultMismatch(expected, actual) =>
      s"String interpolation: `show` must return ${expected.show}, actual ${actual.show}"

    case MainMissing => "Top-level main is not defined"
    case MainTypeInvalid(actual) => s"Top-level main must have type unit → i32, actual ${actual.show}"
  }

  def render(sourceName: String, locations: cats.parse.LocationMap): String = {
    def withCaret(offset: Int, message: String): String =
      locations.toCaret(offset) match {
        case Some(caret) =>
          val line = locations.getLine(caret.line).getOrElse("")
          s"$sourceName:${caret.line + 1}:${caret.col + 1}: $message\n  $line\n  ${" " * caret.col}^"
        case None => message
      }
    this match {
      case At(offset, error) => withCaret(offset, error.render)
      case ParseFailure(error) => withCaret(error.failedAtOffset, render)
      case _ => render
    }
  }

  private def constraintShown(name: TypeVariable, args: Seq[TypeRec[Type]]): String =
    s"${name.name}${args.map(a => s"[${a.show}]").mkString}"

  private def expectationsShown(expected: cats.data.NonEmptyList[cats.parse.Parser.Expectation]): String = {
    import cats.parse.Parser.Expectation
    expected.toList.map {
      case Expectation.OneOfStr(_, strs) => strs.map(s => s"`$s`").mkString(", ")
      case Expectation.InRange(_, lo, hi) => if (lo == hi) s"'$lo'" else s"'$lo'..'$hi'"
      case Expectation.EndOfString(_, _) => "end of input"
      case Expectation.StartOfString(_) => "start of input"
      case Expectation.FailWith(_, message) => message
      case Expectation.WithContext(context, _) => context
      case other => other.toString
    }.distinct.mkString(", ")
  }
}
