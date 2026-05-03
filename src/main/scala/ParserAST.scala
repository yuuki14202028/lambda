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

  lazy val expr: Parser[Rec[Expr]] =
    Parser.defer(typeLetP | tyAbsP | absP | letSeriesP | ifP | equitive)

  private lazy val letSeriesP: Parser[Rec[Expr]] =
    Parser.defer(letRecFunP.backtrack | letRecPolyP.backtrack | letFunP.backtrack | letPolyP.backtrack | letRecP.backtrack | letP)

  lazy val tyAbsP: Parser[Rec[Expr]] = {
    val name = Parser.string("Λ") *> sp *> identifier
    val body = sp *> Parser.char('.') *> sp *> Parser.defer(expr)
    (name ~ body).map { case (n, b) => tyAbs(TypeVariable(n), b) }
  }

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

  private def functionType(params: List[(String, Rec[Type])], returnType: Rec[Type]): Rec[Type] =
    params.map(_._2).foldRight(returnType)(arrow)

  private def functionValue(params: List[(String, Rec[Type])], value: Rec[Expr]): Rec[Expr] =
    params.foldRight(value) { case ((name, types), body) => abs(Variable(name), types, body) }

  private def polymorphicType(params: List[TypeVariable], bodyType: Rec[Type]): Rec[Type] =
    params.foldRight(bodyType)(forallType)

  private def polymorphicValue(params: List[TypeVariable], value: Rec[Expr]): Rec[Expr] =
    params.foldRight(value)(tyAbs)

  private lazy val typeParamsP: Parser[List[TypeVariable]] = {
    val param = Parser.char('[') *> sp *> identifier <* sp <* Parser.char(']')
    param.rep.map(_.toList.map(TypeVariable(_)))
  }

  private lazy val functionParamsP: Parser[List[(String, Rec[Type])]] = {
    val param = Parser.char('(') *> sp *> ((identifier <* sp <* Parser.char(':') <* sp) ~ typeP) <* sp <* Parser.char(')')
    param.rep.map(_.toList)
  }

  lazy val letFunP: Parser[Rec[Expr]] = {
    val name = Parser.string("let") *> sp *> identifier
    val typeParams = (sp *> typeParamsP).?.map(_.getOrElse(Nil))
    val params = sp *> functionParamsP
    val returnType = sp *> Parser.char(':') *> sp *> typeP
    val value = sp *> Parser.char('=') *> sp *> expr
    val body = sp *> Parser.string("in") *> sp *> Parser.defer(expr)
    (name ~ typeParams ~ params ~ returnType ~ value ~ body).map { case (((((name, typeParams), params), returnType), value), body) =>
      val valueType = polymorphicType(typeParams, functionType(params, returnType))
      val valueExpr = polymorphicValue(typeParams, functionValue(params, value))
      let(Variable(name), valueType, valueExpr, body)
    }
  }

  lazy val letPolyP: Parser[Rec[Expr]] = {
    val name = Parser.string("let") *> sp *> identifier
    val typeParams = sp *> typeParamsP
    val returnType = sp *> Parser.char(':') *> sp *> typeP
    val value = sp *> Parser.char('=') *> sp *> expr
    val body = sp *> Parser.string("in") *> sp *> Parser.defer(expr)
    (name ~ typeParams ~ returnType ~ value ~ body).map { case ((((name, typeParams), returnType), value), body) =>
      let(Variable(name), polymorphicType(typeParams, returnType), polymorphicValue(typeParams, value), body)
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

  lazy val letRecFunP: Parser[Rec[Expr]] = {
    val gap = Parser.charIn(" \t").rep.void
    val name = Parser.string("let") *> gap *> Parser.string("rec") *> gap *> identifier
    val typeParams = (sp *> typeParamsP).?.map(_.getOrElse(Nil))
    val params = sp *> functionParamsP
    val returnType = sp *> Parser.char(':') *> sp *> typeP
    val value = sp *> Parser.char('=') *> sp *> expr
    val body = sp *> Parser.string("in") *> sp *> Parser.defer(expr)
    (name ~ typeParams ~ params ~ returnType ~ value ~ body).map { case (((((name, typeParams), params), returnType), value), body) =>
      val valueType = polymorphicType(typeParams, functionType(params, returnType))
      val valueExpr = polymorphicValue(typeParams, functionValue(params, value))
      letRec(Variable(name), valueType, valueExpr, body)
    }
  }

  lazy val letRecPolyP: Parser[Rec[Expr]] = {
    val gap = Parser.charIn(" \t").rep.void
    val name = Parser.string("let") *> gap *> Parser.string("rec") *> gap *> identifier
    val typeParams = sp *> typeParamsP
    val returnType = sp *> Parser.char(':') *> sp *> typeP
    val value = sp *> Parser.char('=') *> sp *> expr
    val body = sp *> Parser.string("in") *> sp *> Parser.defer(expr)
    (name ~ typeParams ~ returnType ~ value ~ body).map { case ((((name, typeParams), returnType), value), body) =>
      letRec(Variable(name), polymorphicType(typeParams, returnType), polymorphicValue(typeParams, value), body)
    }
  }

  lazy val typeLetP: Parser[Rec[Expr]] = {
    val gap = Parser.charIn(" \t").rep.void
    val name = Parser.string("type") *> gap *> identifier
    val param = Parser.char('[') *> sp *> identifier <* sp <* Parser.char(']')
    val params = param.rep0
    val alias = sp *> Parser.char('=') *> sp *> typeP
    val body = sp *> Parser.string("in") *> sp *> Parser.defer(expr)
    (name ~ params ~ alias ~ body).map { case (((name, params), alias), body) =>
      typeLet(TypeVariable(name), params.map(TypeVariable(_)), alias, body)
    }
  }

  lazy val typeP: Parser[Rec[Type]] = Parser.defer(forAllP | arrowTypeP)

  lazy val forAllP: Parser[Rec[Type]] = {
    val name = Parser.string("∀") *> sp *> identifier
    val body = sp *> Parser.char('.') *> sp *> Parser.defer(typeP)
    (name ~ body).map { case (n, b) => forallType(TypeVariable(n), b) }
  }

  lazy val arrowTypeP: Parser[Rec[Type]] = {
    val arrowTail = sp.with1.soft *> Parser.char('→') *> sp *> Parser.defer(typeP)
    (typeAppP ~ arrowTail.?).map {
      case (from, Some(to)) => arrow(from, to)
      case (t, None) => t
    }
  }

  private lazy val typeAppP: Parser[Rec[Type]] = {
    val typeArg = Parser.char('[') *> sp *> Parser.defer(typeP) <* sp <* Parser.char(']')
    (typeAtomP ~ typeArg.rep0).map { case (init, args) =>
      args.foldLeft(init)(typeApp)
    }
  }

  private lazy val typeAtomP: Parser[Rec[Type]] = {
    val parens = Parser.char('(') *> sp *> Parser.defer(typeP) <* sp <* Parser.char(')')
    primitiveP | typeVarP | parens
  }

  private lazy val primitiveP: Parser[Rec[Type]] = {
    Parser.string("Int").as(primitive("Int")) |
    Parser.string("Char").as(primitive("Char")) |
    Parser.string("Bool").as(primitive("Bool")) |
    Parser.string("Unit").as(unitType) |
    (Parser.char('(') *> sp *> Parser.char(')')).as(unitType).backtrack
  }

  private lazy val typeVarP: Parser[Rec[Type]] =
    identifier.map(n => typeVar(TypeVariable(n)))

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
    val not = (Parser.char('!') *> sp *> Parser.defer(unaryP)).map(b => unop(UnaryOps.Not, b))
    neg | not | Parser.defer(appP)
  }

  lazy val appP: Parser[Rec[Expr]] = {
    type Postfix = Either[Rec[Expr], Rec[Type]]
    val exprArg: Parser[Postfix] =
      (Parser.char('(') *> sp *> Parser.defer(expr) <* sp <* Parser.char(')')).map(Left(_))
    val typeArg: Parser[Postfix] =
      (Parser.char('[') *> sp *> Parser.defer(typeP) <* sp <* Parser.char(']')).map(Right(_))
    (atom ~ (exprArg | typeArg).rep0).map { case (f, args) =>
      args.foldLeft(f) {
        case (acc, Left(a)) => app(acc, a)
        case (acc, Right(t)) => tyApp(acc, t)
      }
    }
  }

  lazy val atom: Parser[Rec[Expr]] = {
    val parens = Parser.char('(') *> sp *> Parser.defer(expr) <* sp <* Parser.char(')')
    Parser.defer(blockP | unitP.backtrack | numP | charP | boolP | foreignP | varP | parens)
  }

  lazy val blockP: Parser[Rec[Expr]] = {
    val semi = sp *> Parser.char(';') <* sp
    val discarded = (Parser.defer(expr) <* semi).backtrack.rep0
    val result = Parser.defer(expr).?
    (Parser.char('{') *> sp *> discarded ~ result <* sp <* Parser.char('}')).map {
      case (discarded, result) =>
        block(discarded.toList, result)
    }
  }

  val foreignP: Parser[Rec[Expr]] =
    (Parser.string("foreign") *> (sp1.with1 *> identifier)).map(n => foreign(Variable(n)))

  val varP: Parser[Rec[Expr]] =
    identifier.map(n => varr(Variable(n)))

  val numP: Parser[Rec[Expr]] =
    Numbers.digits.map(_.toInt).map(num)

  val charP: Parser[Rec[Expr]] =
    Parser.char('\'') *> alpha.map(char) <* Parser.char('\'')

  val boolP: Parser[Rec[Expr]] =
    Parser.string("true").as(bool(true)) | Parser.string("false").as(bool(false))

  val unitP: Parser[Rec[Expr]] =
    (Parser.char('(') *> sp *> Parser.char(')')).as(unitLit)

  val programParser: Parser0[Rec[AST.Program.type]] = {
    val sep: Parser[Unit] = (sp1.with1 *> Parser.charIn("\n;").rep <* sp).void
    (sp *> expr.repSep(sep) <* sp).map(nel => program(nel.toList))
  }
}
