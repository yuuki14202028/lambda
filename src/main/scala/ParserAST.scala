package com.yuuki14202028

import cats.parse.{Numbers, Parser, Parser0}
import cats.parse.Rfc5234.{alpha, digit}
object ParserAST {
  private val sp: Parser0[Unit]  = Parser.charIn(" \t\n\r").rep0.void
  private val sp1: Parser0[Unit] = Parser.charIn(" \t").rep0.void
  private val identStart: Parser[Char] = alpha
  private val identChar: Parser[Char] =
    alpha | digit | Parser.char('_').as('_')
  private val identifier: Parser[String] =
    (identStart ~ identChar.rep0).map { case (head, tail) =>
      (head :: tail.toList).mkString
    }

  lazy val expr: Parser[Rec[Expr]] = Parser.defer(absP | letRecP.backtrack | letP | ifP | equitive)

  lazy val absP: Parser[Rec[Expr]] = {
    val name = Parser.string("λ") *> sp *> identifier
    val types = sp *> Parser.char(':') *> sp *> typeP
    val body = sp *> Parser.char('.') *> sp *> Parser.defer(expr)
    (name ~ types ~ body).map { case ((n, t), b) => abs(Variable(n), t, b) }
  }

  lazy val letP: Parser[Rec[Expr]] = {
    val name = Parser.string("let") *> sp *> identifier
    val types = sp *> Parser.char(':') *> sp *> typeP
    val value = sp *> Parser.char('=') *> sp *> expr
    val body = sp *> Parser.string("in") *> sp *> Parser.defer(expr)
    (name ~ types ~ value ~ body).map { case (((name, types), value), body) =>
      let(Variable(name), types, value, body)
    }
  }

  lazy val letRecP: Parser[Rec[Expr]] = {
    val gap = Parser.charIn(" \t").rep.void
    val name = Parser.string("let") *> gap *> Parser.string("rec") *> gap *> identifier
    val types = sp *> Parser.char(':') *> sp *> typeP
    val value = sp *> Parser.char('=') *> sp *> expr
    val body = sp *> Parser.string("in") *> sp *> Parser.defer(expr)
    (name ~ types ~ value ~ body).map { case (((name, types), value), body) =>
      letRec(Variable(name), types, value, body)
    }
  }

  lazy val typeP: Parser[Rec[Type]] = {
    val arrowTail = sp.with1.soft *> Parser.char('→') *> sp *> Parser.defer(typeP)
    (primitiveP ~ arrowTail.?).map {
      case (from, Some(to)) => arrow(from, to)
      case (t, None) => t
    }
  }

  private val primitiveP: Parser[Rec[Type]] = {
    Parser.string("Int").as(primitive("Int")) | Parser.string("Char").as(primitive("Char"))
  }

  lazy val ifP: Parser[Rec[Expr]] = {
    val cond = Parser.string("if") *> sp *> expr
    val trueBranch = sp *> Parser.string("then") *> sp *> expr
    val elseBranch = sp *> Parser.string("else") *> sp *> expr
    (cond ~ trueBranch ~ elseBranch).map { case ((cond, tr), el) =>
      iff(cond, tr, el)
    }
  }

  private val eqOp: Parser[BinOps] =
    Parser.string("==").as(BinOps.Eq) | Parser.string("!=").as(BinOps.Neq) |
    Parser.string("<=").as(BinOps.Leq) | Parser.string("<").as(BinOps.Lt) |
    Parser.string(">=").as(BinOps.Geq) | Parser.string(">").as(BinOps.Gt)

  private val addOp: Parser[BinOps] =
    Parser.char('+').as(BinOps.Add) | Parser.char('-').as(BinOps.Sub)

  private val mulOp: Parser[BinOps] =
    Parser.char('*').as(BinOps.Mul) | Parser.char('/').as(BinOps.Div)

  lazy val equitive: Parser[Rec[Expr]] = {
    val eq  = Parser.defer(additive)
    val tail = (sp.with1.soft *> eqOp ~ (sp.with1 *> eq)).rep0
    (eq ~ tail).map { case (init, ops) =>
      ops.foldLeft(init) { case (acc, (op, r)) => binop(op, acc, r) }
    }
  }

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
    Parser.defer(numP | charP | foreignP | varP | parens)
  }

  val foreignP: Parser[Rec[Expr]] =
    (Parser.string("foreign") *> (sp1.with1 *> identifier)).map(n => foreign(Variable(n)))

  val varP: Parser[Rec[Expr]] =
    identifier.map(n => varr(Variable(n)))

  val numP: Parser[Rec[Expr]] =
    Numbers.digits.map(_.toInt).map(num)

  val charP: Parser[Rec[Expr]] =
    Parser.char('\'') *> alpha.map(char) <* Parser.char('\'')

  val programParser: Parser0[Rec[AST.Program.type]] = {
    val sep: Parser[Unit] = (sp1.with1 *> Parser.charIn("\n;").rep <* sp).void
    (sp *> expr.repSep(sep) <* sp).map(nel => program(nel.toList))
  }
}
