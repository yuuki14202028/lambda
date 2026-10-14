package com.yuuki14202028

sealed trait NodeType
case object ExprNode extends NodeType
case object TypeNode extends NodeType
case object DeclNode extends NodeType

type Expr = ExprNode.type
type Type = TypeNode.type
type Decl = DeclNode.type

enum AST[R[_], I] {
  case Program(decls: Seq[R[Decl]]) extends AST[R, Program.type]
  case TopLet(variable: Variable, types: R[Type], value: R[Expr]) extends AST[R, Decl]
  case TopImport(path: String) extends AST[R, Decl]
  case TopType(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], alias: R[Type]) extends AST[R, Decl]
  case TopData(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], constructors: Seq[DataConstructor[R]]) extends AST[R, Decl]
  case TopTrait(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], supers: Seq[Constraint[R]], methods: Seq[MethodSig[R]]) extends AST[R, Decl]
  case TopImpl(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], targets: Seq[R[Type]], context: Seq[Constraint[R]], methods: Seq[MethodImpl[R]], superEvidence: Seq[Evidence[R]]) extends AST[R, Decl]
  case TopDerive(traitName: TypeVariable, target: TypeVariable) extends AST[R, Decl]
  // rec: 包んだ宣言（let / data / impl のどれか一種類）が互いに、また自身から見える。宣言が 1 つなら自己再帰
  case TopRec(decls: Seq[R[Decl]]) extends AST[R, Decl]
  // operator + = add / prefix operator - = neg: 演算子が指す多相関数の宣言
  // infixl 6 + = add（fixity が None なら前置: prefix - = neg）
  case TopOperator(op: String, fixity: Option[Fixity], function: Variable) extends AST[R, Decl]
  // interpolate = show / literal [] = Nil, Cons: 構文（文字列埋め込み・リストリテラル）が呼ぶ関数の宣言
  case TopSyntax(hook: SyntaxHook, functions: Seq[Variable]) extends AST[R, Decl]
  case TopModule(name: String, decls: Seq[R[Decl]]) extends AST[R, Decl]
  case TopOpen(path: Seq[String]) extends AST[R, Decl]
  case TopModified(modifier: Modifier, decl: R[Decl]) extends AST[R, Decl]
  case Abs(variable: Variable, types: R[Type], body: R[Expr]) extends AST[R, Expr]
  case TyAbs(variable: TypeVariable, kind: Kind, body: R[Expr]) extends AST[R, Expr]
  case InstAbs(constraint: Constraint[R], body: R[Expr]) extends AST[R, Expr]
  case Let(variable: Variable, types: R[Type], value: R[Expr], body: R[Expr]) extends AST[R, Expr]
  case LetRec(bindings: Seq[RecBinding[R]], body: R[Expr]) extends AST[R, Expr]
  // let パターン: T = e in body: 値を分解して束縛する（分岐が 1 つの match。パターンはすべての値に当たるものに限る）
  case LetPattern(pattern: Pattern[R], types: R[Type], value: R[Expr], body: R[Expr]) extends AST[R, Expr]
  case TypeLet(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], alias: R[Type], body: R[Expr]) extends AST[R, Expr]
  case DataLet(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], constructors: Seq[DataConstructor[R]], body: R[Expr]) extends AST[R, Expr]
  case DataLetRec(datas: Seq[DataBinding[R]], body: R[Expr]) extends AST[R, Expr]
  // match e { パターン => 本体 … }。型検査の後、PatternDesugar が下の Match（構成子 1 段の分岐）と if / let へ展開する
  case PatternMatch(scrutinee: R[Expr], cases: Seq[PatternCase[R]]) extends AST[R, Expr]
  case Match(scrutinee: R[Expr], cases: Seq[MatchCase[R]]) extends AST[R, Expr]
  case Fold(scrutinee: R[Expr], resultType: R[Type], cases: Seq[MatchCase[R]]) extends AST[R, Expr]
  case App(function: R[Expr], argument: R[Expr]) extends AST[R, Expr]
  case TyApp(function: R[Expr], argument: R[Type]) extends AST[R, Expr]
  // パーサが読んだ . を含む名前 a.b.c[T]。module の修飾か、値へのドット記法かは、型検査がスコープで決める（型検査後には残らない）
  case Path(name: Variable, typeArgs: Seq[R[Type]]) extends AST[R, Expr]
  // x.f[T]: x の型の module にある関数 f の呼び出し。resolved は、型検査が決めた関数（型引数と制約を適用済み）
  case Select(receiver: R[Expr], name: String, typeArgs: Seq[R[Type]], resolved: Option[R[Expr]]) extends AST[R, Expr]
  // C[T](x = e, …): 名前付きフィールドを名前で渡す構築。positions は、型検査が決めた各引数のフィールドの位置（書かれた順。検査前は空）
  case NamedApp(constructor: R[Expr], arguments: Seq[NamedArg[R]], positions: Seq[Int]) extends AST[R, Expr]
  case InstApp(function: R[Expr], evidence: Evidence[R]) extends AST[R, Expr]
  // impl C[τ̄] { def m = e … }（式）: 辞書の値を作る。インスタンスとしては登録しない。省略したメソッドはデフォルトで埋まり、
  // スーパークラスの辞書はその位置で解決する（superEvidence は型検査が埋める）
  case Instance(traitName: TypeVariable, targets: Seq[R[Type]], methods: Seq[MethodImpl[R]], superEvidence: Seq[Evidence[R]]) extends AST[R, Expr]
  // using d in e: e の中では、辞書の値 d をその型の制約のインスタンスとして解決に使う
  case Using(dictionary: R[Expr], body: R[Expr]) extends AST[R, Expr]
  // summon[C[τ̄]]: 制約を解決して得た辞書そのもの（型は C[τ̄]）。evidence は型検査が埋める
  case Summon(constraint: Constraint[R], evidence: Option[Evidence[R]]) extends AST[R, Expr]
  case Foreign(value: Variable, types: R[Type]) extends AST[R, Expr]
  case Var(value: Variable) extends AST[R, Expr]
  case Num(value: String, typeName: String) extends AST[R, Expr]
  case Char(value: scala.Char) extends AST[R, Expr]
  case StringLit(value: String) extends AST[R, Expr]
  // `a = {x}`: 文字列と埋め込みの並び。埋め込みの resolved は、型検査が決めた「interpolate に宣言された関数を x の型で具体化したもの」
  case StrInterp(parts: Seq[InterpPart[R]]) extends AST[R, Expr]
  case Bool(value: Boolean) extends AST[R, Expr]
  case UnitLit() extends AST[R, Expr]
  case Block(discarded: Seq[R[Expr]], result: Option[R[Expr]]) extends AST[R, Expr]
  case Context(monad: R[Type], steps: Seq[ContextStep[R]], result: R[Expr], ops: Option[ContextOps[R]]) extends AST[R, Expr]
  // a ⊕ b: ⊕ に宣言された関数 f を、a の型で具体化して呼ぶ（f[T](a)(b)）。resolved は型検査が埋める f[T]。
  // && と || は bool 専用の組み込みで、resolved を持たない
  // パーサが読んだ演算子の並び a ⊕ b ⊗ c。優先順位は宣言（スコープに従う）で決まるので、型検査が BinOp の木に組む
  case OpSeq(first: R[Expr], rest: Seq[OpStep[R]]) extends AST[R, Expr]
  case BinOp(op: String, left: R[Expr], right: R[Expr], resolved: Option[R[Expr]]) extends AST[R, Expr]
  case Intrinsic(op: IntrinsicOps, args: Seq[R[Expr]]) extends AST[R, Expr]
  case UnaryOp(op: String, body: R[Expr], resolved: Option[R[Expr]]) extends AST[R, Expr]
  case If(cond: R[Expr], thenBranch: R[Expr], elseBranch: R[Expr]) extends AST[R, Expr]
  case Primitive(name: String) extends AST[R, Type]
  case TypeVar(value: TypeVariable) extends AST[R, Type]
  case Arrow(from: R[Type], to: R[Type]) extends AST[R, Type]
  case ForAll(variable: TypeVariable, kind: Kind, body: R[Type]) extends AST[R, Type]
  case InstArrow(constraint: Constraint[R], to: R[Type]) extends AST[R, Type]
  case TypeApp(function: R[Type], argument: R[Type]) extends AST[R, Type]
  case TypeAbs(variable: TypeVariable, kind: Kind, body: R[Type]) extends AST[R, Type]
}

/** module の中の宣言に付ける修飾子。
 *   private: 宣言を module の外から見えなくする
 *   opaque:  type は名前だけを公開し（外では中身のわからない型になる）、data は構成子を隠す
 */
enum Modifier {
  case Private, Opaque

  def keyword: String = this match {
    case Private => "private"
    case Opaque => "opaque"
  }
}

// rec let / rec { let …  let … } の束縛。値からは組の全員が見える
case class RecBinding[R[_]](variable: Variable, types: R[Type], value: R[Expr])
// fieldNames は fields と同じ長さ（名前のないフィールドは None）。名前付きのフィールド C(name: T) は、型の module にアクセサを生成する
case class DataConstructor[R[_]](name: Variable, fields: Seq[R[Type]], fieldNames: Seq[Option[String]] = Nil) {
  def names: Seq[Option[String]] = if (fieldNames.isEmpty) fields.map(_ => None) else fieldNames
  def named: Boolean = fieldNames.exists(_.nonEmpty)
}
case class MatchCase[R[_]](constructor: Variable, binders: Seq[Variable], body: R[Expr])

/** match のパターン。パーサは引数のない名前を Constructor(x, Nil, Nil) として読み、
 * 型検査が「スコープ中の構成子でなければ変数の束縛」と決めて Bind に直す。
 */
enum Pattern[R[_]] {
  case Wildcard()                                                                            // _
  case Bind(name: Variable)                                                                  // x
  case Constructor(name: Variable, args: Seq[Pattern[R]], siblings: Seq[ConstructorShape[R]]) // C(p)(q)。siblings は型検査が埋める
  case Literal(value: LiteralValue, equals: Option[R[Expr]])                                 // 1 / 'a' / true / "s"。equals は型検査が解決した ==
  case Or(alternatives: Seq[Pattern[R]])                                                     // p | q。どの選択肢も同じ変数を同じ型で束縛する
  case Located(span: Span, pattern: Pattern[R])                                              // ソース上の位置（エラー表示用。型検査が外す）
}

// スクルーティニーのデータ型の構成子と、型引数を適用したフィールドの型（脱糖が Env なしで分岐を組み立てるため）
case class ConstructorShape[R[_]](name: Variable, fields: Seq[R[Type]])

enum LiteralValue {
  case Num(value: String, typeName: String)
  case Char(value: scala.Char)
  case Bool(value: Boolean)
  case Str(value: String)

  def shown: String = this match {
    case Num(value, _) => value
    case Char(value) => s"'$value'"
    case Bool(value) => value.toString
    case Str(value) => "\"" + value + "\""
  }
}

// guard は `パターン if 条件 => 本体` の条件。偽なら後続の分岐を試す。span はパターンの位置（エラー表示用）
case class PatternCase[R[_]](pattern: Pattern[R], guard: Option[R[Expr]], body: R[Expr], span: Option[Span] = None)

def patternVariables[R[_]](pattern: Pattern[R]): Seq[Variable] = pattern match {
  case Pattern.Bind(name) => Seq(name)
  case Pattern.Constructor(_, args, _) => args.flatMap(patternVariables)
  case Pattern.Or(alternatives) => alternatives.headOption.toSeq.flatMap(patternVariables)
  case Pattern.Located(_, inner) => patternVariables(inner)
  case Pattern.Wildcard() | Pattern.Literal(_, _) => Seq.empty
}
// rec data / rec { data …  data … } のデータ型。フィールドからは組の全員が見える
case class DataBinding[R[_]](variable: TypeVariable, params: Seq[(TypeVariable, Kind)], constructors: Seq[DataConstructor[R]])

// context ブロックの中の束縛
enum ContextStep[R[_]] {
  case Bind(name: Variable, annotation: R[Type], value: R[Expr]) // x: T = e;          モナドの中身を束縛する
  case Let(name: Variable, annotation: R[Type], value: R[Expr])  // let x: T = e;
  case Rec(bindings: Seq[RecBinding[R]])                         // rec let …;  /  rec { let …  let … };
  case BindPattern(pattern: Pattern[R], annotation: R[Type], value: R[Expr]) // パターン: T = e;     モナドの中身を分解して束縛する
  case LetPattern(pattern: Pattern[R], annotation: R[Type], value: R[Expr])  // let パターン: T = e;
}
// context[M] が使う pure[M] / flatMap[M]。型検査が名前と制約を解決して埋める（パース直後は None）
case class ContextOps[R[_]](pure: R[Expr], flatMap: R[Expr])

/** 制約 ∀params. premises ⇒ name[arg]。params と premises が空なら単純な制約 C[τ̄]。
 * 量化された制約（`∀a. Show[a] ⇒ Show[f[a]]`）は「その場で使えるインスタンス」で、前提にも量化された制約を書ける。
 * 辞書の型は ∀params. premises の辞書… → name[arg]（文脈付きインスタンスの辞書と同じ形）。
 */
case class Constraint[R[_]](
    name: TypeVariable,
    arg: Seq[R[Type]],
    params: Seq[(TypeVariable, Kind)] = Nil,
    premises: Seq[Constraint[R]] = Nil,
    span: Option[Span] = None // ソースに書かれた制約の位置（エラー表示用。型検査が作る制約にはない）
) {
  def quantified: Boolean = params.nonEmpty || premises.nonEmpty
  def head: Constraint[R] = Constraint(name, arg)
}

/** 制約は Lean 4 と同じく「辞書 C[τ̄] を受け取る、インスタンス解決で埋まる引数」として表す:
 *   型 InstArrow  `[C[τ̄]] → τ`   / 抽象 InstAbs `λ[C[τ̄]]. e` / 適用 InstApp（型検査が挿入）
 * trait C は辞書のデータ型でもあるので、TraitEncoder はこれらを通常の → / λ / 適用に置き換えるだけでよい。
 * 引数の位置には trait の適用しか置けないので、型ではなく Constraint として持つ。
 *
 * Evidence は制約 C[τ̄] が成り立つ根拠。型検査が Resolver で導出して InstApp / TopImpl に記録し、
 * TraitEncoder が辞書式へ翻訳する。
 */
enum Evidence[R[_]] {
  case Given(constraint: Constraint[R])                                                                        // スコープ中の制約（with 節・impl の文脈）
  case Instance(traitName: TypeVariable, heads: Seq[String], typeArgs: Seq[R[Type]], context: Seq[Evidence[R]]) // インスタンス（typeArgs は impl の型パラメータへの束縛）
  case Super(traitName: TypeVariable, index: Int, of: Evidence[R])                                             // traitName[τ̄] の根拠 of から index 番目のスーパークラスを射影
  case Apply(of: Evidence[R], typeArgs: Seq[R[Type]], premises: Seq[Evidence[R]])                              // 量化された制約の根拠 of を、型引数と前提の根拠で具体化する
  case Abs(constraint: Constraint[R], body: Evidence[R])                                                       // 量化された制約の根拠。body では constraint の前提がスコープに入る
  case Explicit(dictionary: R[Expr])                                                                           // f(with d): 解決せず、辞書の値 d を直接渡す
}
case class MethodSig[R[_]](name: Variable, sig: R[Type], body: Option[R[Expr]])
case class MethodImpl[R[_]](name: Variable, sig: Option[R[Type]], body: R[Expr])

enum BinOps {
  case Add, Sub, Mul, Div, Mod,
    Eq, Neq, Lt, Leq, Gt, Geq,
    And, Or, Xor,
    ShortAnd, ShortOr

  override def toString: String = this match {
    case BinOps.Add => "+"
    case BinOps.Sub => "-"
    case BinOps.Mul => "*"
    case BinOps.Div => "/"
    case BinOps.Mod => "%"
    case BinOps.Eq  => "=="
    case BinOps.Neq => "!="
    case BinOps.Lt  => "<"
    case BinOps.Leq => "<="
    case BinOps.Gt  => ">"
    case BinOps.Geq => ">="
    case BinOps.And => "&"
    case BinOps.Or  => "|"
    case BinOps.Xor => "^"
    case BinOps.ShortAnd => "&&"
    case BinOps.ShortOr  => "||"
  }
}

enum IntrinsicOps {
  case BinOp(op: BinOps, operandType: String)
  case UnaryOp(op: UnaryOps, operandType: String)
}

// 演算子の並びの 1 歩: 演算子（とその位置）と、右の被演算子
case class OpStep[R[_]](op: String, span: Span, operand: R[Expr])

enum InterpPart[R[_]] {
  case Text(value: String)
  case Embed(value: R[Expr], resolved: Option[R[Expr]])
}

// 構文が呼ぶ関数の宣言の種類
enum SyntaxHook {
  case Interpolate // interpolate = f: `{x}` は f[T](x)（T は x の型）
  case ListLiteral // literal [] = nil, cons: [x, y] は cons[T](x)(cons[T](y)(nil[T]))（T は最初の要素の型）

  def keyword: String = this match {
    case Interpolate => "interpolate"
    case ListLiteral => "literal []"
  }
}

// 名前付きの構築の引数 name = value（span は名前の位置）
case class NamedArg[R[_]](name: String, span: Span, value: R[Expr])

// 二項演算子の結合性と優先順位（0〜9。大きいほど強く結び付く）
enum Assoc {
  case Left, Right, Non
  def keyword: String = this match {
    case Assoc.Left => "infixl"
    case Assoc.Right => "infixr"
    case Assoc.Non => "infix"
  }
}
case class Fixity(assoc: Assoc, level: Int)

// 演算子に宣言された関数（fixity が None なら前置）
case class OperatorDef(function: Variable, fixity: Option[Fixity])

object Operators {
  val shortAnd = "&&"
  val shortOr = "||"
  // && と || は bool 専用の組み込み。宣言できず、優先順位も決まっている
  val builtin: Map[String, Fixity] = Map(shortAnd -> Fixity(Assoc.Right, 3), shortOr -> Fixity(Assoc.Right, 2))
  // 演算子の宣言を引く鍵（前置は二項と区別する）
  def prefixKey(op: String): String = s"prefix $op"
}

enum UnaryOps {
  case Neg, Not
  override def toString: String = this match {
    case UnaryOps.Neg => "-"
    case UnaryOps.Not => "!"
  }
}

case class Variable(name: String)

case class TypeVariable(name: String)

object BuiltinTypes {
  val Ptr: String = "foreign.C.Ptr"
  val CString: String = "foreign.C.String"
  val VoidPtr: String = "foreign.C.VoidPtr"

  val primitiveArities: Map[String, Int] = Map(
    "i8" -> 0,
    "i16" -> 0,
    "i32" -> 0,
    "i64" -> 0,
    "u8" -> 0,
    "u16" -> 0,
    "u32" -> 0,
    "u64" -> 0,
    "f32" -> 0,
    "f64" -> 0,
    "isize" -> 0,
    "usize" -> 0,
    "char" -> 0,
    "bool" -> 0,
    "unit" -> 0,
    CString -> 0,
    VoidPtr -> 0,
    Ptr -> 1
  )

  val integerTypes: Set[String] =
    Set("i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "isize", "usize")

  val floatTypes: Set[String] =
    Set("f32", "f64")

  val numericTypes: Set[String] =
    integerTypes ++ floatTypes

  val equatableTypes: Set[String] =
    numericTypes ++ Set("char", "bool")

  def isPrimitive(name: String): Boolean =
    primitiveArities.contains(name)

  def arity(name: String): Option[Int] =
    primitiveArities.get(name)
}

type Rec[I] = HFix[AST, I]
/** ソース上の範囲（start 以上 end 未満の文字位置）。file は import されたファイル（None はコンパイル対象のファイル） */
case class Span(start: Int, end: Int, file: Option[java.nio.file.Path] = None) {
  def to(other: Span): Span = Span(start, other.end.max(end), file)
}

type IndexedRec[I] = HCofree[AST, ConstI[Span], I]
type IndexedAST[R[_], x] = HCofreeT[ConstI[Span], AST, R, x]

def programI(decls: Seq[IndexedRec[Decl]]): IndexedRec[AST.Program.type] = HCofree(Span(0, 0), AST.Program(decls))

val eraseIndex: IndexedRec ~> Rec =
  [I] => (t: IndexedRec[I]) => HFix(t.project.hmap(eraseIndex))

def program(decls: Seq[Rec[Decl]]): Rec[AST.Program.type] = HFix(AST.Program(decls))
def topLet(variable: Variable, types: Rec[Type], value: Rec[Expr]): Rec[Decl] = HFix(AST.TopLet(variable, types, value))
def topRec(decls: Seq[Rec[Decl]]): Rec[Decl] = HFix(AST.TopRec(decls))
def topLetRec(variable: Variable, types: Rec[Type], value: Rec[Expr]): Rec[Decl] = topRec(Seq(topLet(variable, types, value)))
def topImport(path: String): Rec[Decl] = HFix(AST.TopImport(path))
def topType(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], alias: Rec[Type]): Rec[Decl] =
  HFix(AST.TopType(variable, params, alias))
def topData(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], constructors: Seq[DataConstructor[Rec]]): Rec[Decl] =
  HFix(AST.TopData(variable, params, constructors))
def topTrait(v: TypeVariable, p: Seq[(TypeVariable, Kind)], s: Seq[Constraint[Rec]], m: Seq[MethodSig[Rec]]): Rec[Decl] = HFix(AST.TopTrait(v, p, s, m))
def topImpl(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], targets: Seq[Rec[Type]], context: Seq[Constraint[Rec]], methods: Seq[MethodImpl[Rec]]): Rec[Decl] =
  HFix(AST.TopImpl(variable, params, targets, context, methods, Seq.empty))
def topDerive(traitName: TypeVariable, target: TypeVariable): Rec[Decl] = HFix(AST.TopDerive(traitName, target))
def topModule(name: String, decls: Seq[Rec[Decl]]): Rec[Decl] = HFix(AST.TopModule(name, decls))
def topOpen(path: Seq[String]): Rec[Decl] = HFix(AST.TopOpen(path))
def topModified(modifier: Modifier, decl: Rec[Decl]): Rec[Decl] = HFix(AST.TopModified(modifier, decl))
def abs(variable: Variable, types: Rec[Type], body: Rec[Expr]): Rec[Expr] = HFix(AST.Abs(variable, types, body))
def tyAbs(variable: TypeVariable, kind: Kind, body: Rec[Expr]): Rec[Expr] = HFix(AST.TyAbs(variable, kind, body))
def instAbs(constraint: Constraint[Rec], body: Rec[Expr]): Rec[Expr] = HFix(AST.InstAbs(constraint, body))
def let(variable: Variable, types: Rec[Type], value: Rec[Expr], body: Rec[Expr]): Rec[Expr] = HFix(AST.Let(variable, types, value, body))
def letRec(variable: Variable, types: Rec[Type], value: Rec[Expr], body: Rec[Expr]): Rec[Expr] =
  HFix(AST.LetRec(Seq(RecBinding[Rec](variable, types, value)), body))
def typeLet(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], alias: Rec[Type], body: Rec[Expr]): Rec[Expr] =
  HFix(AST.TypeLet(variable, params, alias, body))
def dataLet(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], constructors: Seq[DataConstructor[Rec]], body: Rec[Expr]): Rec[Expr] =
  HFix(AST.DataLet(variable, params, constructors, body))
def matchExpr(scrutinee: Rec[Expr], cases: Seq[MatchCase[Rec]]): Rec[Expr] =
  HFix(AST.Match(scrutinee, cases))
def foldExpr(scrutinee: Rec[Expr], resultType: Rec[Type], cases: Seq[MatchCase[Rec]]): Rec[Expr] =
  HFix(AST.Fold(scrutinee, resultType, cases))
def app(function: Rec[Expr], argument: Rec[Expr]): Rec[Expr] = HFix(AST.App(function, argument))
def tyApp(function: Rec[Expr], argument: Rec[Type]): Rec[Expr] = HFix(AST.TyApp(function, argument))
def foreign(variable: Variable, types: Rec[Type]): Rec[Expr] = HFix(AST.Foreign(variable, types))
def varr(variable: Variable): Rec[Expr] = HFix(AST.Var(variable))
def num(value: String, typeName: String): Rec[Expr] = HFix(AST.Num(value, typeName))
def char(value: scala.Char): Rec[Expr] = HFix(AST.Char(value))
def stringLit(value: String): Rec[Expr] = HFix(AST.StringLit(value))
def strInterp(parts: Seq[InterpPart[Rec]]): Rec[Expr] = HFix(AST.StrInterp(parts))
def strInterpT(t: TypeRec[Type], parts: Seq[InterpPart[TypeRec]]): TypeRec[Expr] = HCofree(ExprAnn(t), AST.StrInterp(parts))
def bool(value: Boolean): Rec[Expr] = HFix(AST.Bool(value))
def unitLit: Rec[Expr] = HFix(AST.UnitLit())
def block(discarded: Seq[Rec[Expr]], result: Option[Rec[Expr]]): Rec[Expr] = HFix(AST.Block(discarded, result))
def contextExpr(monad: Rec[Type], steps: Seq[ContextStep[Rec]], result: Rec[Expr]): Rec[Expr] =
  HFix(AST.Context(monad, steps, result, None))
def binop(op: String, left: Rec[Expr], right: Rec[Expr]): Rec[Expr] = HFix(AST.BinOp(op, left, right, None))
def intrinsic(op: IntrinsicOps, args: Seq[Rec[Expr]]): Rec[Expr] = HFix(AST.Intrinsic(op, args))
def unop(op: String, body: Rec[Expr]): Rec[Expr] = HFix(AST.UnaryOp(op, body, None))
def iff(cond: Rec[Expr], thenBranch: Rec[Expr], elseBranch: Rec[Expr]): Rec[Expr] = HFix(AST.If(cond, thenBranch, elseBranch))
def primitive(name: String): Rec[Type] = HFix(AST.Primitive(name))
def typeVar(variable: TypeVariable): Rec[Type] = HFix(AST.TypeVar(variable))
def arrow(from: Rec[Type], to: Rec[Type]): Rec[Type] = HFix(AST.Arrow(from, to))
def forallType(variable: TypeVariable, kind: Kind, body: Rec[Type]): Rec[Type] = HFix(AST.ForAll(variable, kind, body))
def instArrow(constraint: Constraint[Rec], to: Rec[Type]): Rec[Type] = HFix(AST.InstArrow(constraint, to))
def typeApp(function: Rec[Type], argument: Rec[Type]): Rec[Type] = HFix(AST.TypeApp(function, argument))
def typeAbs(variable: TypeVariable, kind: Kind, body: Rec[Type]): Rec[Type] = HFix(AST.TypeAbs(variable, kind, body))

def intType: Rec[Type] = primitive("i32")
def charType: Rec[Type] = primitive("char")
def stringType: Rec[Type] = primitive(BuiltinTypes.CString)
def boolType: Rec[Type] = primitive("bool")
def unitType: Rec[Type] = primitive("unit")

type TypeRec[I] = HCofree[AST, TypeAnn, I]
type TypedAST[R[_], x] = HCofreeT[TypeAnn, AST, R, x]

extension[R[_], x](self: TypedAST[R, x]) {
  def ann: TypeAnn[x] = self.ask
  def ast: AST[R, x] = self.lower
}

private val eraseAnn: TypeRec ~> Rec =
  [I] => (t: TypeRec[I]) => HFix(t.project.hmap(eraseAnn))

def programT(decls: Seq[TypeRec[Decl]], env: Env = Env.empty): TypeRec[AST.Program.type] =
  HCofree(ProgramAnn(env), AST.Program(decls))
def topLetT(variable: Variable, types: TypeRec[Type], value: TypeRec[Expr]): TypeRec[Decl] =
  HCofree(DeclAnn, AST.TopLet(variable, types, value))
def topRecT(decls: Seq[TypeRec[Decl]]): TypeRec[Decl] =
  HCofree(DeclAnn, AST.TopRec(decls))
def topLetRecT(variable: Variable, types: TypeRec[Type], value: TypeRec[Expr]): TypeRec[Decl] =
  topRecT(Seq(topLetT(variable, types, value)))
def topImportT(path: String): TypeRec[Decl] =
  HCofree(DeclAnn, AST.TopImport(path))
def topTypeT(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], alias: TypeRec[Type]): TypeRec[Decl] =
  HCofree(DeclAnn, AST.TopType(variable, params, alias))
def topDataT(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], constructors: Seq[DataConstructor[TypeRec]]): TypeRec[Decl] =
  HCofree(DeclAnn, AST.TopData(variable, params, constructors))
def topTraitT(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], supers: Seq[Constraint[TypeRec]], methods: Seq[MethodSig[TypeRec]]): TypeRec[Decl] =
  HCofree(DeclAnn, AST.TopTrait(variable, params, supers, methods))
def topImplT(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], targets: Seq[TypeRec[Type]], context: Seq[Constraint[TypeRec]], methods: Seq[MethodImpl[TypeRec]], superEvidence: Seq[Evidence[TypeRec]]): TypeRec[Decl] =
  HCofree(DeclAnn, AST.TopImpl(variable, params, targets, context, methods, superEvidence))
def topDeriveT(traitName: TypeVariable, target: TypeVariable): TypeRec[Decl] =
  HCofree(DeclAnn, AST.TopDerive(traitName, target))
def topOperatorT(op: String, fixity: Option[Fixity], function: Variable): TypeRec[Decl] =
  HCofree(DeclAnn, AST.TopOperator(op, fixity, function))
def topSyntaxT(hook: SyntaxHook, functions: Seq[Variable]): TypeRec[Decl] =
  HCofree(DeclAnn, AST.TopSyntax(hook, functions))
def topModuleT(name: String, decls: Seq[TypeRec[Decl]]): TypeRec[Decl] =
  HCofree(DeclAnn, AST.TopModule(name, decls))
def topOpenT(path: Seq[String]): TypeRec[Decl] =
  HCofree(DeclAnn, AST.TopOpen(path))
def topModifiedT(modifier: Modifier, decl: TypeRec[Decl]): TypeRec[Decl] =
  HCofree(DeclAnn, AST.TopModified(modifier, decl))
def absT(variable: Variable, t: TypeRec[Type], types: TypeRec[Type], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.Abs(variable, types, body))
def tyAbsT(variable: TypeVariable, t: TypeRec[Type], kind: Kind, body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.TyAbs(variable, kind, body))
def instAbsT(t: TypeRec[Type], constraint: Constraint[TypeRec], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.InstAbs(constraint, body))
def letT(variable: Variable, t: TypeRec[Type], types: TypeRec[Type], value: TypeRec[Expr], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.Let(variable, types, value, body))
def letRecT(variable: Variable, t: TypeRec[Type], types: TypeRec[Type], value: TypeRec[Expr], body: TypeRec[Expr]): TypeRec[Expr] =
  letRecGroupT(Seq(RecBinding[TypeRec](variable, types, value)), t, body)
def letPatternT(t: TypeRec[Type], pattern: Pattern[TypeRec], types: TypeRec[Type], value: TypeRec[Expr], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.LetPattern(pattern, types, value, body))
def letRecGroupT(bindings: Seq[RecBinding[TypeRec]], t: TypeRec[Type], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.LetRec(bindings, body))
def typeLetT(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], t: TypeRec[Type], alias: TypeRec[Type], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.TypeLet(variable, params, alias, body))
def dataLetT(variable: TypeVariable, params: Seq[(TypeVariable, Kind)], t: TypeRec[Type], constructors: Seq[DataConstructor[TypeRec]], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.DataLet(variable, params, constructors, body))
def dataLetRecT(datas: Seq[DataBinding[TypeRec]], t: TypeRec[Type], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.DataLetRec(datas, body))
def patternMatchT(t: TypeRec[Type], scrutinee: TypeRec[Expr], cases: Seq[PatternCase[TypeRec]]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.PatternMatch(scrutinee, cases))
def matchExprT(t: TypeRec[Type], scrutinee: TypeRec[Expr], cases: Seq[MatchCase[TypeRec]]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.Match(scrutinee, cases))
def foldExprT(t: TypeRec[Type], scrutinee: TypeRec[Expr], resultType: TypeRec[Type], cases: Seq[MatchCase[TypeRec]]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.Fold(scrutinee, resultType, cases))
def appT(t: TypeRec[Type], function: TypeRec[Expr], argument: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.App(function, argument))
def tyAppT(t: TypeRec[Type], function: TypeRec[Expr], argument: TypeRec[Type]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.TyApp(function, argument))
def selectT(t: TypeRec[Type], receiver: TypeRec[Expr], name: String, typeArgs: Seq[TypeRec[Type]], resolved: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.Select(receiver, name, typeArgs, Some(resolved)))
def namedAppT(t: TypeRec[Type], constructor: TypeRec[Expr], arguments: Seq[NamedArg[TypeRec]], positions: Seq[Int]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.NamedApp(constructor, arguments, positions))
def instAppT(t: TypeRec[Type], function: TypeRec[Expr], evidence: Evidence[TypeRec]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.InstApp(function, evidence))
def instanceT(t: TypeRec[Type], traitName: TypeVariable, targets: Seq[TypeRec[Type]], methods: Seq[MethodImpl[TypeRec]], superEvidence: Seq[Evidence[TypeRec]]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.Instance(traitName, targets, methods, superEvidence))
def usingT(t: TypeRec[Type], dictionary: TypeRec[Expr], body: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.Using(dictionary, body))
def summonT(t: TypeRec[Type], constraint: Constraint[TypeRec], evidence: Evidence[TypeRec]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.Summon(constraint, Some(evidence)))
def foreignT(variable: Variable, t: TypeRec[Type], types: TypeRec[Type]): TypeRec[Expr] = HCofree(ExprAnn(t), AST.Foreign(variable, types))
def varrType(variable: Variable, t: TypeRec[Type]): TypeRec[Expr] = HCofree(ExprAnn(t), AST.Var(variable))
def numT(value: String, typeName: String, t: TypeRec[Type]): TypeRec[Expr] = HCofree(ExprAnn(t), AST.Num(value, typeName))
def charT(value: scala.Char, t: TypeRec[Type]): TypeRec[Expr] = HCofree(ExprAnn(t), AST.Char(value))
def stringLitT(value: String, t: TypeRec[Type]): TypeRec[Expr] = HCofree(ExprAnn(t), AST.StringLit(value))
def boolT(value: Boolean, t: TypeRec[Type]): TypeRec[Expr] = HCofree(ExprAnn(t), AST.Bool(value))
def unitLitT(t: TypeRec[Type]): TypeRec[Expr] = HCofree(ExprAnn(t), AST.UnitLit())
def blockT(t: TypeRec[Type], discarded: Seq[TypeRec[Expr]], result: Option[TypeRec[Expr]]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.Block(discarded, result))
def contextExprT(t: TypeRec[Type], monad: TypeRec[Type], steps: Seq[ContextStep[TypeRec]], result: TypeRec[Expr], ops: ContextOps[TypeRec]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.Context(monad, steps, result, Some(ops)))
def binopT(op: String, t: TypeRec[Type], left: TypeRec[Expr], right: TypeRec[Expr], resolved: Option[TypeRec[Expr]]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.BinOp(op, left, right, resolved))
def intrinsicT(op: IntrinsicOps, t: TypeRec[Type], args: Seq[TypeRec[Expr]]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.Intrinsic(op, args))
def unopT(op: String, t: TypeRec[Type], body: TypeRec[Expr], resolved: Option[TypeRec[Expr]]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.UnaryOp(op, body, resolved))
def ifT(t: TypeRec[Type], cond: TypeRec[Expr], thenBranch: TypeRec[Expr], elseBranch: TypeRec[Expr]): TypeRec[Expr] =
  HCofree(ExprAnn(t), AST.If(cond, thenBranch, elseBranch))
def primitiveT(name: String): TypeRec[Type] = HCofree(TypeAnn, AST.Primitive(name))
def typeVarT(variable: TypeVariable): TypeRec[Type] = HCofree(TypeAnn, AST.TypeVar(variable))
def arrowT(from: TypeRec[Type], to: TypeRec[Type]): TypeRec[Type] = HCofree(TypeAnn, AST.Arrow(from, to))
def forallTypeT(variable: TypeVariable, kind: Kind, body: TypeRec[Type]): TypeRec[Type] = HCofree(TypeAnn, AST.ForAll(variable, kind, body))
def instArrowT(constraint: Constraint[TypeRec], to: TypeRec[Type]): TypeRec[Type] = HCofree(TypeAnn, AST.InstArrow(constraint, to))
def typeAppT(function: TypeRec[Type], argument: TypeRec[Type]): TypeRec[Type] = HCofree(TypeAnn, AST.TypeApp(function, argument))
def typeAbsT(variable: TypeVariable, kind: Kind, body: TypeRec[Type]): TypeRec[Type] = HCofree(TypeAnn, AST.TypeAbs(variable, kind, body))

def intTypeT: TypeRec[Type] = primitiveT("i32")
def charTypeT: TypeRec[Type] = primitiveT("char")
def stringTypeT: TypeRec[Type] = primitiveT(BuiltinTypes.CString)
def boolTypeT: TypeRec[Type] = primitiveT("bool")
def unitTypeT: TypeRec[Type] = primitiveT("unit")

def typeOf(expr: TypeRec[Expr]): TypeRec[Type] = expr.extract match {
  case ExprAnn(t) => t
}

def destructArrow(t: TypeRec[Type]): Option[(TypeRec[Type], TypeRec[Type])] = PartialFunction.condOpt(t.project) {
  case AST.Arrow(from, to) => (from, to)
}

def destructForAllK(t: TypeRec[Type]): Option[(TypeVariable, Kind, TypeRec[Type])] = PartialFunction.condOpt(t.project) {
  case AST.ForAll(variable, kind, body) => (variable, kind, body)
}

def destructInstArrow(t: TypeRec[Type]): Option[(Constraint[TypeRec], TypeRec[Type])] = PartialFunction.condOpt(t.project) {
  case AST.InstArrow(constraint, to) => (constraint, to)
}

// ---- 子の型から注釈を導出する構築関数（脱糖フェーズ用。型が合わないのは前段のバグ） ----

def mkAbs(variable: Variable, paramType: TypeRec[Type], body: TypeRec[Expr]): TypeRec[Expr] =
  absT(variable, arrowT(paramType, typeOf(body)), paramType, body)

def mkTyAbs(variable: TypeVariable, kind: Kind, body: TypeRec[Expr]): TypeRec[Expr] =
  tyAbsT(variable, forallTypeT(variable, kind, typeOf(body)), kind, body)

def mkApp(function: TypeRec[Expr], argument: TypeRec[Expr]): TypeRec[Expr] = destructArrow(typeOf(function)) match {
  case Some((_, to)) => appT(to, function, argument)
  case None => sys.error(s"Compiler invariant violation: expected a function type, got ${typeOf(function).show}")
}

def mkTyApp(function: TypeRec[Expr], argument: TypeRec[Type]): TypeRec[Expr] = destructForAllK(typeOf(function)) match {
  case Some((variable, _, body)) => tyAppT(Equivalence.normalize(substType(variable, argument, body)), function, argument)
  case None => sys.error(s"Compiler invariant violation: expected a polymorphic type, got ${typeOf(function).show}")
}

def freeTypeVars(t: Rec[Type]): Set[TypeVariable] = {
  val alg: Algebra[AST, ConstI[Set[TypeVariable]]] = [x] => node => node match {
    case AST.TypeVar(v) => Set(v)
    case AST.ForAll(v, _, body) => body - v
    case AST.InstArrow(constraint, to) =>
      def free(c: Constraint[ConstI[Set[TypeVariable]]]): Set[TypeVariable] =
        (c.arg.foldLeft(Set.empty[TypeVariable])(_ ++ _) ++ c.premises.flatMap(free)) -- c.params.map(_._1)
      to ++ free(constraint)
    case AST.Arrow(from, to) => from ++ to
    case AST.TypeApp(f, a) => f ++ a
    case AST.TypeAbs(v, _, body) => body - v
    case _ => Set.empty
  }
  t.cata(alg)
}
@scala.annotation.targetName("freeTypeVarsAnn")
def freeTypeVars(t: TypeRec[Type]): Set[TypeVariable] = freeTypeVars(eraseAnn(t))

def freeVars(expr: Rec[Expr]): Set[Variable] = {
  // 根拠の中の式は、明示的に渡された辞書だけ
  def inEvidence(evidence: Evidence[ConstI[Set[Variable]]]): Set[Variable] = evidence match {
    case Evidence.Explicit(dictionary) => dictionary
    case Evidence.Given(_) => Set.empty
    case Evidence.Instance(_, _, _, context) => context.flatMap(inEvidence).toSet
    case Evidence.Super(_, _, of) => inEvidence(of)
    case Evidence.Apply(of, _, premises) => inEvidence(of) ++ premises.flatMap(inEvidence)
    case Evidence.Abs(_, body) => inEvidence(body)
  }
  val alg: Algebra[AST, ConstI[Set[Variable]]] = [x] => node => node match {
    case AST.Var(variable) => Set(variable)
    case AST.Abs(variable, _, body) => body - variable
    case AST.TyAbs(_, _, body) => body
    case AST.InstAbs(_, body) => body
    case AST.InstApp(function, evidence) => function ++ inEvidence(evidence)
    case AST.Summon(_, evidence) => evidence.fold(Set.empty[Variable])(inEvidence)
    case AST.Using(dictionary, body) => dictionary ++ body
    case AST.Instance(_, _, methods, superEvidence) =>
      methods.foldLeft(Set.empty[Variable])(_ ++ _.body) ++ superEvidence.flatMap(inEvidence)
    case AST.Let(variable, _, value, body) => value ++ (body - variable)
    case AST.LetRec(bindings, body) => bindings.foldLeft(body)(_ ++ _.value) -- bindings.map(_.variable)
    case AST.LetPattern(pattern, _, value, body) => value ++ (body -- patternVariables(pattern))
    case AST.TypeLet(_, _, _, body) => body
    case AST.DataLet(_, _, _, body) => body
    case AST.DataLetRec(_, body) => body
    case AST.PatternMatch(scrutinee, cases) =>
      def inPattern(p: Pattern[ConstI[Set[Variable]]]): Set[Variable] = p match {
        case Pattern.Constructor(_, args, _) => args.flatMap(inPattern).toSet
        case Pattern.Literal(_, equals) => equals.getOrElse(Set.empty)
        case Pattern.Or(alternatives) => alternatives.flatMap(inPattern).toSet
        case Pattern.Located(_, inner) => inPattern(inner)
        case Pattern.Wildcard() | Pattern.Bind(_) => Set.empty
      }
      scrutinee ++ cases.foldLeft(Set.empty[Variable]) { (acc, c) =>
        acc ++ inPattern(c.pattern) ++ ((c.body ++ c.guard.getOrElse(Set.empty)) -- patternVariables(c.pattern))
      }
    case AST.Match(scrutinee, cases) =>
      scrutinee ++ cases.foldLeft(Set.empty[Variable]) { (acc, c) =>
        acc ++ (c.body -- c.binders.toSet)
      }
    case AST.Fold(scrutinee, _, cases) =>
      scrutinee ++ cases.foldLeft(Set.empty[Variable]) { (acc, c) =>
        acc ++ (c.body -- c.binders.toSet)
      }
    case AST.App(function, argument) => function ++ argument
    case AST.TyApp(function, _) => function
    case AST.Path(name, _) => Set(name)
    case AST.Select(receiver, _, _, resolved) => receiver ++ resolved.getOrElse(Set.empty)
    case AST.NamedApp(constructor, arguments, _) => arguments.foldLeft(constructor)(_ ++ _.value)
    case AST.Foreign(_, _) => Set.empty
    case AST.Block(discarded, result) => discarded.foldLeft(Set.empty[Variable])(_ ++ _) ++ result.getOrElse(Set.empty)
    case AST.BinOp(_, left, right, resolved) => left ++ right ++ resolved.getOrElse(Set.empty)
    case AST.OpSeq(first, rest) => first ++ rest.flatMap(_.operand)
    case AST.Intrinsic(_, args) => args.foldLeft(Set.empty[Variable])(_ ++ _)
    case AST.StrInterp(parts) => parts.foldLeft(Set.empty[Variable]) {
      case (acc, InterpPart.Embed(value, resolved)) => acc ++ value ++ resolved.getOrElse(Set.empty)
      case (acc, InterpPart.Text(_)) => acc
    }
    case AST.Context(_, steps, result, ops) =>
      val body = steps.foldRight(result) {
        case (ContextStep.Bind(name, _, value), acc) => value ++ (acc - name)
        case (ContextStep.Let(name, _, value), acc) => value ++ (acc - name)
        case (ContextStep.Rec(bindings), acc) => bindings.foldLeft(acc)(_ ++ _.value) -- bindings.map(_.variable)
        case (ContextStep.BindPattern(pattern, _, value), acc) => value ++ (acc -- patternVariables(pattern))
        case (ContextStep.LetPattern(pattern, _, value), acc) => value ++ (acc -- patternVariables(pattern))
      }
      ops.fold(body)(o => body ++ o.pure ++ o.flatMap)
    case AST.UnaryOp(_, body, resolved) => body ++ resolved.getOrElse(Set.empty)
    case AST.If(cond, thenBranch, elseBranch) => cond ++ thenBranch ++ elseBranch
    case AST.Num(_, _) | AST.Char(_) | AST.StringLit(_) | AST.Bool(_) | AST.UnitLit() => Set.empty
    case _ => Set.empty
  }
  expr.cata(alg)
}
@scala.annotation.targetName("freeVarsAnn")
def freeVars(expr: TypeRec[Expr]): Set[Variable] = freeVars(eraseAnn(expr))

def freshTypeVariable(base: TypeVariable, used: Set[TypeVariable]): TypeVariable = {
  Iterator.from(0)
    .map {
      case 0 => TypeVariable(s"${base.name}'")
      case n => TypeVariable(s"${base.name}'$n")
    }
    .find(v => !used.contains(v))
    .get
}

def substType(target: TypeVariable, replace: TypeRec[Type], in: TypeRec[Type]): TypeRec[Type] = {

  def renameTypeVar(from: TypeVariable, to: TypeVariable, in: TypeRec[Type]): TypeRec[Type] = {
    val algebra: Algebra[TypedAST, TypeRec] = [x] => he => he match {
      case HCofreeT(_, AST.TypeVar(variable)) if variable == from => typeVarT(to)
      case HCofreeT(_, AST.ForAll(variable, kind, body)) if variable == from => forallTypeT(to, kind, body)
      case HCofreeT(_, AST.TypeAbs(variable, kind, body)) if variable == from => typeAbsT(to, kind, body)
      case HCofreeT(_, AST.InstArrow(constraint, body)) =>
        // 引数は子として付け替え済み。束縛子（前提の中のものも）を付け替える
        def binders(c: Constraint[TypeRec]): Constraint[TypeRec] =
          c.copy(params = c.params.map { case (p, k) => (if (p == from) to else p, k) }, premises = c.premises.map(binders))
        instArrowT(binders(constraint), body)
      case HCofreeT(ann, ast) => HCofree(ann, ast)
    }
    in.cata(algebra)
  }

  val replaceFreeVars = freeTypeVars(replace)

  def rebind(variable: TypeVariable, body: TypeRec[Type]): (TypeVariable, TypeRec[Type]) = {
    if (replaceFreeVars.contains(variable)) {
      val fresh = freshTypeVariable(variable, replaceFreeVars ++ freeTypeVars(body) + target + variable)
      (fresh, renameTypeVar(variable, fresh, body))
    } else (variable, body)
  }

  // 制約を型 [制約] → unit として扱い、型の操作をそのまま使う
  def constraintAsType(c: Constraint[TypeRec]): TypeRec[Type] = instArrowT(c, unitTypeT)
  def constraintOfType(t: TypeRec[Type]): Constraint[TypeRec] = t.project match {
    case AST.InstArrow(c, _) => c
    case _ => sys.error("Compiler invariant violation: expected a constraint type")
  }

  def rebindConstraint(constraint: Constraint[TypeRec]): Constraint[TypeRec] =
    constraint.params.map(_._1).filter(replaceFreeVars.contains).foldLeft(constraint) { (c, param) =>
      val body = c.copy(params = Nil)
      val fresh = freshTypeVariable(param, replaceFreeVars ++ freeTypeVars(constraintAsType(body)) ++ c.params.map(_._1) + target)
      constraintOfType(renameTypeVar(param, fresh, constraintAsType(body)))
        .copy(params = c.params.map { case (p, k) => (if (p == param) fresh else p, k) })
    }

  val coalg: ApoCoalgebra[TypedAST, TypeRec] = [x] => seed => seed.project match {
    case AST.TypeVar(variable) if variable == target => Left(replace)
    case AST.ForAll(variable, _, _) if variable == target => Left(seed)
    case AST.TypeAbs(variable, _, _) if variable == target => Left(seed)
    case AST.ForAll(variable, kind, body) =>
      val (binder, newBody) = rebind(variable, body)
      Right(HCofreeT(seed.extract, AST.ForAll(binder, kind, Right(newBody))))
    case AST.TypeAbs(variable, kind, body) =>
      val (binder, newBody) = rebind(variable, body)
      Right(HCofreeT(seed.extract, AST.TypeAbs(binder, kind, Right(newBody))))
    case AST.Arrow(from, to) =>
      Right(HCofreeT(seed.extract, AST.Arrow(Right(from), Right(to))))
    case AST.TypeApp(function, argument) =>
      Right(HCofreeT(seed.extract, AST.TypeApp(Right(function), Right(argument))))
    case AST.InstArrow(constraint, to) =>
      type Pending[y] = Either[TypeRec[y], TypeRec[y]]
      def unchanged(c: Constraint[TypeRec]): Constraint[Pending] =
        Constraint[Pending](c.name, c.arg.map(Left(_)), c.params, c.premises.map(unchanged))
      // 量化された制約のパラメータは束縛子: target を隠し、replace の自由変数と衝突するなら付け替える（前提の中でも同じ）
      def substituted(c: Constraint[TypeRec]): Constraint[Pending] =
        if (c.params.exists(_._1 == target)) unchanged(c)
        else {
          val rebound = rebindConstraint(c)
          Constraint[Pending](rebound.name, rebound.arg.map(Right(_)), rebound.params, rebound.premises.map(substituted))
        }
      Right(HCofreeT(seed.extract, AST.InstArrow(substituted(constraint), Right(to))))
    case _ => Left(seed)
  }

  apo(in)(coalg)
}

// 同時代入。素朴な逐次代入では先行する引数の中の変数名が後続のパラメータ名と一致したとき
// 二重に置換されてしまうため、いったん fresh な変数へ退避してから代入する
def substMany(params: Seq[TypeVariable], args: Seq[TypeRec[Type]], in: TypeRec[Type]): TypeRec[Type] = {
  val used = (args.flatMap(freeTypeVars) ++ freeTypeVars(in) ++ params).toSet
  val staging = params.foldLeft((List.empty[TypeVariable], used)) { case ((acc, taken), param) =>
    val fresh = freshTypeVariable(param, taken)
    (fresh :: acc, taken + fresh)
  }._1.reverse
  val staged = params.zip(staging).foldLeft(in) { case (acc, (param, fresh)) => substType(param, typeVarT(fresh), acc) }
  staging.zip(args).foldLeft(staged) { case (acc, (fresh, arg)) => substType(fresh, arg, acc) }
}

def isNumericType(t: TypeRec[Type]): Boolean = t.project match {
  case AST.Primitive(name) => BuiltinTypes.numericTypes.contains(name)
  case _ => false
}

def isEquatableType(t: TypeRec[Type]): Boolean = t.project match {
  case AST.Primitive(name) => BuiltinTypes.equatableTypes.contains(name)
  case _ => false
}

def collectTypeApps(t: TypeRec[Type]): (TypeRec[Type], Seq[TypeRec[Type]]) = {
  type Collected[I] = (TypeRec[I], Seq[TypeRec[Type]])
  val alg: RAlgebra[TypedAST, TypeRec, Collected] = [x] => he => he match {
    case HCofreeT(_, AST.TypeApp(function, argument)) =>
      val (head, args) = function.result
      (head, args :+ argument.original)
    case HCofreeT(ann, node) => (HCofree(ann, paraOriginals(node)), Seq.empty)
  }
  t.para(alg)
}

def isDataApplicationOf(t: Rec[Type], owner: TypeVariable): Boolean = {
  val alg: Algebra[AST, ConstI[Boolean]] = [x] => node => node match {
    case AST.TypeVar(variable) => variable == owner
    case AST.TypeApp(headIsOwner, _) => headIsOwner
    case _ => false
  }
  t.cata(alg)
}
@scala.annotation.targetName("isDataApplicationOfAnn")
def isDataApplicationOf(t: TypeRec[Type], owner: TypeVariable): Boolean = isDataApplicationOf(eraseAnn(t), owner)

def containsDataApplicationOf(t: Rec[Type], owner: TypeVariable): Boolean = {
  val alg: Algebra[AST, ConstI[Boolean]] = [x] => node => node match {
    case AST.TypeVar(variable) => variable == owner
    case AST.Arrow(from, to) => from || to
    case AST.ForAll(_, _, body) => body
    case AST.InstArrow(constraint, to) =>
      def mentioned(c: Constraint[ConstI[Boolean]]): Boolean = c.arg.exists(identity) || c.premises.exists(mentioned)
      to || mentioned(constraint)
    case AST.TypeAbs(_, _, body) => body
    case AST.TypeApp(function, argument) => function || argument
    case _ => false
  }
  t.cata(alg)
}
@scala.annotation.targetName("containsDataApplicationOfAnn")
def containsDataApplicationOf(t: TypeRec[Type], owner: TypeVariable): Boolean = containsDataApplicationOf(eraseAnn(t), owner)

def dataTypeApplication[D]
                       (t: TypeRec[Type], dataTypes: Map[TypeVariable, D])
                       (paramsOf: D => Seq[TypeVariable]): Either[CompileError, (TypeVariable, D, Seq[TypeRec[Type]])] = {
  val (head, args) = collectTypeApps(t)
  head.project match {
    case AST.TypeVar(variable) => dataTypes.get(variable) match {
      case Some(dataDef) if paramsOf(dataDef).length == args.length => Right((variable, dataDef, args))
      case Some(dataDef) => Left(CompileError.DataArityMismatch(variable.name, paramsOf(dataDef).length, args.length))
      case None => Left(CompileError.NotADataType(t))
    }
    case _ => Left(CompileError.NotADataType(t))
  }
}

// 名前付きフィールド field のアクセサ（型 owner の module のメンバー）
def fieldAccessorName(owner: TypeVariable, field: String): Variable = Variable(s"${owner.name}.$field")

def applyTypeConstructor(head: TypeVariable, args: Seq[TypeRec[Type]]): TypeRec[Type] =
  args.foldLeft(typeVarT(head)) { (acc, arg) => typeAppT(acc, arg) }

def typeConstructorHead(t: TypeRec[Type]): Option[(String, Seq[TypeRec[Type]])] = {
  val (head, args) = collectTypeApps(t)
  head.project match {
    case AST.TypeVar(variable) => Some((variable.name, args))
    case AST.Primitive(name) => Some((name, args))
    case _ => None
  }
}
