package com.yuuki14202028

object StandardLibrary {
  private val binaryNames: Map[String, BinOps] = Map(
    "add" -> BinOps.Add,
    "sub" -> BinOps.Sub,
    "mul" -> BinOps.Mul,
    "div" -> BinOps.Div,
    "mod" -> BinOps.Mod,
    "eq" -> BinOps.Eq,
    "neq" -> BinOps.Neq,
    "lt" -> BinOps.Lt,
    "leq" -> BinOps.Leq,
    "gt" -> BinOps.Gt,
    "geq" -> BinOps.Geq
  )

  private val unaryNames: Map[String, UnaryOps] = Map(
    "neg" -> UnaryOps.Neg,
    "not" -> UnaryOps.Not
  )

  private def opName(op: BinOps): String = op match {
    case BinOps.Add => "add"
    case BinOps.Sub => "sub"
    case BinOps.Mul => "mul"
    case BinOps.Div => "div"
    case BinOps.Mod => "mod"
    case BinOps.Eq  => "eq"
    case BinOps.Neq => "neq"
    case BinOps.Lt  => "lt"
    case BinOps.Leq => "leq"
    case BinOps.Gt  => "gt"
    case BinOps.Geq => "geq"
  }

  private def opName(op: UnaryOps): String = op match {
    case UnaryOps.Neg => "neg"
    case UnaryOps.Not => "not"
  }

  def binaryOperatorName(op: BinOps, typeName: String): Variable =
    Variable(s"__${opName(op)}_$typeName")

  def unaryOperatorName(op: UnaryOps, typeName: String): Variable =
    Variable(s"__${opName(op)}_$typeName")

  def intrinsicOp(name: String): Option[IntrinsicOps] = {
    val parts = name.split("_", 2)
    Option.when(parts.length == 2)(parts(0) -> parts(1)).flatMap { case (op, typeName) =>
      binaryNames.get(op).map(IntrinsicOps.BinOp(_, typeName))
        .orElse(unaryNames.get(op).map(IntrinsicOps.UnaryOp(_, typeName)))
    }
  }
}
