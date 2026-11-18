package com.yuuki14202028

/** ソースの中の範囲。file は、そのソースを読んだファイル */
final case class Span(start: Int, end: Int, file: String) {
  def to(other: Span): Span = copy(end = end.max(other.end))
}

object Span {
  val none: Span = Span(0, 0, "")
}

/** 束縛ごとに 1 つ作る名前。同じ綴りでも別の束縛なら別の Symbol（同一性で比べる） */
final class Symbol(val name: String) {
  override def toString: String = name
}

enum Kind {
  case Star
  case Arrow(from: Kind, to: Kind)

  def show: String = this match {
    case Star => "*"
    case Arrow(from: Arrow, to) => s"(${from.show}) → ${to.show}"
    case Arrow(from, to) => s"${from.show} → ${to.show}"
  }
}

enum Literal {
  case Int(text: String, typeName: String)
  case Float(text: String, typeName: String)
  case Bool(value: Boolean)
  case Char(value: scala.Char)
  case Str(value: String)
  case Unit
}

enum Assoc {
  case Left, Right, None
}

final case class Fixity(assoc: Assoc, level: Int)

object Primitives {
  val string = "foreign.C.String"

  val integers: Set[String] = Set("i8", "i16", "i32", "i64", "isize", "u8", "u16", "u32", "u64", "usize")
  val floats: Set[String] = Set("f32", "f64")
  val numbers: Set[String] = integers ++ floats

  /** 組み込みの型と、型引数の数 */
  val arity: Map[String, Int] =
    (numbers ++ Set("char", "bool", "unit", string, "foreign.C.VoidPtr")).map(_ -> 0).toMap + ("foreign.C.Ptr" -> 1)
}

enum PrimOp {
  case Add, Sub, Mul, Div, Mod, Eq, Neq, Lt, Leq, Gt, Geq, And, Or, Xor, Neg, Not

  def arity: Int = this match {
    case Neg | Not => 1
    case _ => 2
  }
}

/** intrinsic[add_i32] のような組み込み演算。名前は「演算_型」 */
final case class Intrinsic(op: PrimOp, typeName: String) {
  def name: String = s"${op.toString.toLowerCase}_$typeName"
}

object Intrinsic {
  private val ops: Map[String, PrimOp] = PrimOp.values.map(op => op.toString.toLowerCase -> op).toMap

  def parse(name: String): Option[Intrinsic] = name.split("_", 2) match {
    case Array(op, typeName) if Primitives.arity.contains(typeName) => ops.get(op).map(Intrinsic(_, typeName))
    case _ => None
  }
}
