package com.yuuki14202028

sealed trait Kind {
  def show: String = this match {
    case Kind.Star => "*"
    case Kind.Arrow(from, to) =>
      val left = from match {
        case Kind.Arrow(_, _) => s"(${from.show})"
        case _ => from.show
      }
      s"$left → ${to.show}"
  }
}

object Kind {
  case object Star extends Kind
  case class Arrow(from: Kind, to: Kind) extends Kind
}
