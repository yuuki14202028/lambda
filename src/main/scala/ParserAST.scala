package com.yuuki14202028

import cats.parse.{Numbers, Parser, Parser0}
import cats.parse.Rfc5234.alpha

object ParserAST {
  private val sp: Parser0[Unit]  = Parser.charIn(" \t\n\r").rep0.void
  private val sp1: Parser0[Unit] = Parser.charIn(" \t").rep0.void
  private val identifier: Parser[String] = alpha.rep.map(_.toList.mkString)

  lazy val expr: Parser[Rec[Expr]] = Parser.defer(absP | letP )

  lazy val absP: Parser[Rec[Expr]] = {
    val name = Parser.string("λ") *> sp *> identifier
    val body = sp *> Parser.char('.') *> sp *> Parser.defer(expr)
    (name ~ body).map { case (n, b) => abs(Variable(n), b) }
  }

  lazy val letP: Parser[Rec[Expr]] = {
    val name = Parser.string("let") *> sp *> identifier
    val value = sp *> Parser.char('=') *> sp *> expr
    val body = sp *> Parser.string("in") *> sp *> Parser.defer(expr)
    (name ~ value ~ body).map { case ((name, value), body) =>
      let(Variable(name), value, body)
    }
  }

   private val addOp: Parser[BinOps] =
    Parser.char('+').as(BinOps.Add) | Parser.char('-').as(BinOps.Sub)

  private val mulOp: Parser[BinOps] =
    Parser.char('*').as(BinOps.Mul) | Parser.char('/').as(BinOps.Div)

  lazy val additive: Parser[Rec[Expr]] = {
    val mul  = Parser.defer(multiplicative)
    val tail = (sp.with1.soft *> addOp ~ (sp.with1 *> mul)).rep0
    (mul ~ tail).map { case (init, ops) =>
      ops.foldLeft(init) { case (acc, (op, r)) => binop(op, acc, r) }
    }
  }

  lazy val multiplicative: Parser[Rec[Expr]] = {
    val un   = Parser.defer(unaryP)
    val tail = (sp.with1.soft *> mulOp ~ (sp.with1 *> un)).rep0
    (un ~ tail).map { case (init, ops) =>
      ops.foldLeft(init) { case (acc, (op, r)) => binop(op, acc, r) }
    }
  }

  lazy val unaryP: Parser[Rec[Expr]] = {
    val neg = (Parser.char('-') *> sp *> Parser.defer(unaryP)).map(b => unop(UnaryOps.Neg, b))
    neg | Parser.defer(appP)
  }

  lazy val appP: Parser[Rec[Expr]] = {
    val arg = Parser.char('(') *> sp *> Parser.defer(expr) <* sp <* Parser.char(')')
    (atom ~ arg.rep0).map { case (f, args) => args.foldLeft(f)((acc, a) => app(acc, a)) }
  }

  lazy val atom: Parser[Rec[Expr]] = {
    val parens = Parser.char('(') *> sp *> Parser.defer(expr) <* sp <* Parser.char(')')
    Parser.defer(numP | varP | parens)
  }

  val varP: Parser[Rec[Expr]] =
    identifier.map(n => varr(Variable(n)))

  val numP: Parser[Rec[Expr]] =
    Numbers.digits.map(_.toInt).map(num)

  val programParser: Parser0[Rec[AST.Program.type]] = {
    val sep: Parser[Unit] = (sp1.with1 *> Parser.charIn("\n;").rep <* sp).void
    (sp *> expr.repSep(sep) <* sp).map(nel => program(nel.toList))
  }
}