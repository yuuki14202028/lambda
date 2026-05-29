package com.yuuki14202028

import cats.parse.{Numbers, Parser, Parser0}
import cats.parse.Rfc5234.{alpha, digit}
object ParserAST {
  private val sp: Parser0[Unit]  = Parser.charIn(" \t\n\r").rep0.void
  private val sp1: Parser0[Unit] = Parser.charIn(" \t").rep0.void
  private val gap: Parser[Unit] = Parser.charIn(" \t\n\r").rep.void
  private val identStart: Parser[Char] = alpha | Parser.char('_').as('_')
  private val identChar: Parser[Char] =
    alpha | digit | Parser.char('_').as('_')
  private val identifier: Parser[String] =
    (identStart ~ identChar.rep0).map { case (head, tail) =>
      (head :: tail.toList).mkString
    }
  private val typeIdentifier: Parser[String] =
    (identifier ~ (Parser.char('.') *> identifier).backtrack.rep0).map { case (head, tail) =>
      (head :: tail.toList).mkString(".")
    }
  private val importPath: Parser[String] = {
    val plain = Parser.charWhere(ch => ch != '"' && ch != '\n' && ch != '\r')
    Parser.char('"') *> plain.rep0.map(_.toList.mkString) <* Parser.char('"')
  }

  lazy val expr: Parser[Rec[Expr]] =
    Parser.defer(dataLetP | foldP.backtrack | matchP | typeLetP | tyAbsP | absP | letExprP | ifP | logicalOr)


  lazy val tyAbsP: Parser[Rec[Expr]] = {
    val plain = identifier.map(n => (TypeVariable(n), Kind.Star: Kind))
    val annotated = (Parser.char('(') *> sp *> identifier ~ (sp *> Parser.char(':') *> sp *> kindP) <* sp <* Parser.char(')'))
      .map { case (n, k) => (TypeVariable(n), k) }
    val nameKind = Parser.string("Λ") *> sp *> (annotated.backtrack | plain)
    val body = sp *> Parser.char('.') *> sp *> Parser.defer(expr)
    (nameKind ~ body).map { case ((v, k), b) => tyAbs(v, k, b) }
  }

  lazy val kindP: Parser[Kind] = Parser.defer(kindArrowP)

  private lazy val kindArrowP: Parser[Kind] = {
    val arrowTail = sp.with1.soft *> Parser.string("→") *> sp *> Parser.defer(kindP)
    (kindAtomP ~ arrowTail.?).map {
      case (from, Some(to)) => Kind.Arrow(from, to)
      case (k, None) => k
    }
  }

  private lazy val kindAtomP: Parser[Kind] = {
    val star = Parser.char('*').as(Kind.Star: Kind)
    val parens = Parser.char('(') *> sp *> Parser.defer(kindP) <* sp <* Parser.char(')')
    star | parens
  }

  lazy val absP: Parser[Rec[Expr]] = {
    val name = Parser.string("λ") *> sp *> identifier
    val types = sp *> Parser.char(':') *> sp *> typeP
    val body = sp *> Parser.char('.') *> sp *> Parser.defer(expr)
    (name ~ types ~ body).map { case ((n, t), b) => abs(Variable(n), t, b) }
  }

  private def functionType(params: List[(String, Rec[Type])], returnType: Rec[Type]): Rec[Type] =
    params.map(_._2).foldRight(returnType)(arrow)

  private def functionValue(params: List[(String, Rec[Type])], value: Rec[Expr]): Rec[Expr] =
    params.foldRight(value) { case ((name, types), body) => abs(Variable(name), types, body) }

  private def polymorphicType(params: List[(TypeVariable, Kind)], bodyType: Rec[Type]): Rec[Type] =
    params.foldRight(bodyType) { case ((v, k), body) => forallType(v, k, body) }

  private def polymorphicValue(params: List[(TypeVariable, Kind)], value: Rec[Expr]): Rec[Expr] =
    params.foldRight(value) { case ((v, k), body) => tyAbs(v, k, body) }

  private lazy val kindedTypeParamP: Parser[(TypeVariable, Kind)] = {
    val kindAnn = (sp *> Parser.char(':') *> sp *> kindP).?
    (Parser.char('[') *> sp *> identifier ~ kindAnn <* sp <* Parser.char(']')).map {
      case (n, k) => (TypeVariable(n), k.getOrElse(Kind.Star))
    }
  }

  private lazy val typeParamsP: Parser[List[(TypeVariable, Kind)]] =
    kindedTypeParamP.rep.map(_.toList)

  private lazy val functionParamsP: Parser[List[(String, Rec[Type])]] = {
    val namedParam = ((identifier <* sp <* Parser.char(':') <* sp) ~ typeP)
    val unitParam = Parser.char('(') *> sp *> Parser.char(')').as(("_", unitType))
    val param = unitParam.backtrack | (Parser.char('(') *> sp *> namedParam <* sp <* Parser.char(')'))
    param.rep.map(_.toList)
  }

  private lazy val letExprP: Parser[Rec[Expr]] = {
    Parser.defer((binding ~ inBody).map { case ((recursive, v, t, value), body) =>
      if (recursive) letRec(v, t, value, body) else let(v, t, value, body)
    })
  }

  private lazy val topLetP: Parser[Rec[Decl]] = {
    binding.map { case (recursive, v, t, value) =>
      if (recursive) topLetRec(v, t, value) else topLet(v, t, value)
    }
  }

  private val letHead: Parser[(Boolean, String)] = {
    val ws = Parser.charIn(" \t").rep.void
    val recName = (ws *> Parser.string("rec") *> ws *> identifier).map(n => (true, n))
    val plainName = (sp *> identifier).map(n => (false, n))
    Parser.string("let") *> (recName.backtrack | plainName)
  }

  private lazy val inBody: Parser0[Rec[Expr]] =
    sp *> Parser.string("in") *> sp *> Parser.defer(expr)

  private lazy val typeAnnP: Parser0[Rec[Type]] = sp *> Parser.char(':') *> sp *> typeP
  private lazy val valueP: Parser0[Rec[Expr]]   = sp *> Parser.char('=') *> sp *> expr

  private lazy val binding: Parser[(Boolean, Variable, Rec[Type], Rec[Expr])] = {
    val typeParams = (sp.with1.soft *> typeParamsP).?.map(_.getOrElse(Nil))
    val params = (sp.with1.soft *> functionParamsP).?.map(_.getOrElse(Nil))
    (letHead ~ typeParams ~ params ~ typeAnnP ~ valueP).map {
      case (((((recursive, name), typeParams), params), returnType), value) =>
        (recursive, Variable(name),
          polymorphicType(typeParams, functionType(params, returnType)),
          polymorphicValue(typeParams, functionValue(params, value)))
    }
  }

  private lazy val topImportP: Parser[Rec[Decl]] =
    (Parser.string("import") *> gap *> importPath).map(topImport)

  private lazy val typeBinding: Parser[(TypeVariable, List[(TypeVariable, Kind)], Rec[Type])] = {
    val name = Parser.string("type") *> gap *> identifier
    val params = kindedTypeParamP.rep0
    val alias = sp *> Parser.char('=') *> sp *> typeP
    (name ~ params ~ alias).map { case ((name, params), alias) =>
      (TypeVariable(name), params, alias)
    }
  }

  lazy val typeLetP: Parser[Rec[Expr]] =
    (typeBinding ~ inBody).map { case ((v, params, alias), body) =>
      typeLet(v, params, alias, body)
    }

  private lazy val topTypeP: Parser[Rec[Decl]] =
    typeBinding.map { case (v, params, alias) => topType(v, params, alias) }

  private lazy val dataConstructorP: Parser[DataConstructor[[x] =>> Rec[x]]] = {
    val name = Parser.char('|') *> sp *> identifier
    val field = Parser.char('(') *> sp *> typeP <* sp <* Parser.char(')')
    (name ~ field.rep0).map { case (name, fields) =>
      DataConstructor(Variable(name), fields)
    }
  }

  private lazy val dataBinding: Parser[(Boolean, TypeVariable, List[(TypeVariable, Kind)], List[DataConstructor[[x] =>> Rec[x]]])] = {
    val recursive = Parser.string("data") *> gap *> (Parser.string("rec").as(true) <* gap).?.map(_.getOrElse(false))
    val name = identifier
    val params = kindedTypeParamP.rep0
    val constructors = sp *> Parser.char('=') *> sp *> dataConstructorP.repSep(gap)
    (recursive ~ name ~ params ~ constructors).map { case (((recursive, name), params), constructors) =>
      (recursive, TypeVariable(name), params, constructors.toList)
    }
  }

  lazy val dataLetP: Parser[Rec[Expr]] = (dataBinding ~ inBody).map { case ((recursive, v, params, constructors), body) =>
    dataLet(v, params, constructors, body, recursive)
  }

  private lazy val topDataP: Parser[Rec[Decl]] = dataBinding.map { case (recursive, v, params, constructors) =>
    topData(v, params, constructors, recursive)
  }

  private lazy val topDeclP: Parser[Rec[Decl]] =
    Parser.defer(topImportP.backtrack | topDataP.backtrack | topTypeP.backtrack | topLetP)

  lazy val typeP: Parser[Rec[Type]] = Parser.defer(forAllP | typeLambdaP | arrowTypeP)

  lazy val typeLambdaP: Parser[Rec[Type]] = {
    val plain = identifier.map(n => (TypeVariable(n), Kind.Star))
    val annotated = (Parser.char('(') *> sp *> identifier ~ (sp *> Parser.char(':') *> sp *> kindP) <* sp <* Parser.char(')'))
      .map { case (n, k) => (TypeVariable(n), k) }
    val nameKind = Parser.string("λ") *> sp *> (annotated.backtrack | plain)
    val body = sp *> Parser.char('.') *> sp *> Parser.defer(typeP)
    (nameKind ~ body).map { case ((v, k), b) => typeAbs(v, k, b) }
  }

  lazy val forAllP: Parser[Rec[Type]] = {
    val plain = identifier.map(n => (TypeVariable(n), Kind.Star))
    val annotated = (Parser.char('(') *> sp *> identifier ~ (sp *> Parser.char(':') *> sp *> kindP) <* sp <* Parser.char(')'))
      .map { case (n, k) => (TypeVariable(n), k) }
    val nameKind = Parser.string("∀") *> sp *> (annotated.backtrack | plain)
    val body = sp *> Parser.char('.') *> sp *> Parser.defer(typeP)
    (nameKind ~ body).map { case ((v, k), b) => forallType(v, k, b) }
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
    val unit = (Parser.char('(') *> sp *> Parser.char(')')).as(unitType).backtrack
    unit | namedTypeP | parens
  }

  private lazy val namedTypeP: Parser[Rec[Type]] =
    typeIdentifier.map { name =>
      if (BuiltinTypes.isPrimitive(name)) primitive(name)
      else typeVar(TypeVariable(name))
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

  private lazy val matchCaseP: Parser[MatchCase[[x] =>> Rec[x]]] = {
    val name = Parser.char('|') *> sp *> identifier
    val binder = Parser.char('(') *> sp *> identifier <* sp <* Parser.char(')')
    val body = sp *> Parser.string("->") *> sp *> Parser.defer(expr)
    ((name ~ binder.rep0) ~ body).map { case ((name, binders), body) =>
      MatchCase(Variable(name), binders.map(Variable(_)), body)
    }
  }

  lazy val matchP: Parser[Rec[Expr]] = {
    val scrutinee = Parser.string("match") *> gap *> expr
    val cases = sp *> Parser.string("with") *> sp *> matchCaseP.repSep(gap)
    (scrutinee ~ cases).map { case (scrutinee, cases) =>
      matchExpr(scrutinee, cases.toList)
    }
  }

  lazy val foldP: Parser[Rec[Expr]] = {
    val scrutinee = Parser.string("fold") *> gap *> expr
    val resultType = sp *> Parser.string("as") *> sp *> typeP
    val cases = sp *> Parser.string("with") *> sp *> matchCaseP.repSep(gap)
    (scrutinee ~ resultType ~ cases).map { case ((scrutinee, resultType), cases) =>
      foldExpr(scrutinee, resultType, cases.toList)
    }
  }

  private val eqOp: Parser[BinOps] =
    Parser.string("==").as(BinOps.Eq) | Parser.string("!=").as(BinOps.Neq) |
    Parser.string("<=").as(BinOps.Leq) | Parser.string("<").as(BinOps.Lt) |
    Parser.string(">=").as(BinOps.Geq) | Parser.string(">").as(BinOps.Gt)

  private val addOp: Parser[BinOps] =
    Parser.char('+').as(BinOps.Add) | Parser.char('-').as(BinOps.Sub)

  private val mulOp: Parser[BinOps] =
    Parser.char('*').as(BinOps.Mul) | Parser.char('/').as(BinOps.Div) | Parser.char('%').as(BinOps.Mod)

  private val bitAndOp: Parser[BinOps] =
    (Parser.char('&') <* Parser.not(Parser.char('&'))).backtrack.as(BinOps.And)

  private val caseStartLookahead: Parser[Unit] =
    (Parser.char('|') *> sp *> identifier *> sp *> (Parser.char('(').void | Parser.string("->").void)).void

  private val bitOrOp: Parser[BinOps] =
    (Parser.not(caseStartLookahead.backtrack).with1 *> Parser.char('|') <* Parser.not(Parser.char('|'))).backtrack.as(BinOps.Or)

  private val xorOp: Parser[BinOps] =
    Parser.char('^').as(BinOps.Xor)

  lazy val logicalOr: Parser[Rec[Expr]] = {
    val and = Parser.defer(logicalAnd)
    val tail = (sp.with1.soft *> Parser.string("||").as(BinOps.ShortOr) ~ (sp.with1 *> and)).rep0
    (and ~ tail).map { case (init, ops) =>
      ops.foldLeft(init) { case (acc, (op, r)) => binop(op, acc, r) }
    }
  }

  lazy val logicalAnd: Parser[Rec[Expr]] = {
    val bitOr = Parser.defer(bitwiseOr)
    val tail = (sp.with1.soft *> Parser.string("&&").as(BinOps.ShortAnd) ~ (sp.with1 *> bitOr)).rep0
    (bitOr ~ tail).map { case (init, ops) =>
      ops.foldLeft(init) { case (acc, (op, r)) => binop(op, acc, r) }
    }
  }

  lazy val bitwiseOr: Parser[Rec[Expr]] = {
    val xor = Parser.defer(bitwiseXor)
    val tail = (sp.with1.soft *> bitOrOp ~ (sp.with1 *> xor)).rep0
    (xor ~ tail).map { case (init, ops) =>
      ops.foldLeft(init) { case (acc, (op, r)) => binop(op, acc, r) }
    }
  }

  lazy val bitwiseXor: Parser[Rec[Expr]] = {
    val and = Parser.defer(bitwiseAnd)
    val tail = (sp.with1.soft *> xorOp ~ (sp.with1 *> and)).rep0
    (and ~ tail).map { case (init, ops) =>
      ops.foldLeft(init) { case (acc, (op, r)) => binop(op, acc, r) }
    }
  }

  lazy val bitwiseAnd: Parser[Rec[Expr]] = {
    val eq = Parser.defer(equitive)
    val tail = (sp.with1.soft *> bitAndOp ~ (sp.with1 *> eq)).rep0
    (eq ~ tail).map { case (init, ops) =>
      ops.foldLeft(init) { case (acc, (op, r)) => binop(op, acc, r) }
    }
  }

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
    val unitArg: Parser[Postfix] =
      (Parser.char('(') *> sp *> Parser.char(')')).as(Left(unitLit)).backtrack
    val exprArg: Parser[Postfix] =
      (Parser.char('(') *> sp *> Parser.defer(expr) <* sp <* Parser.char(')')).map(Left(_))
    val typeArg: Parser[Postfix] =
      (Parser.char('[') *> sp *> Parser.defer(typeP) <* sp <* Parser.char(']')).map(Right(_))
    (atom ~ (unitArg | exprArg | typeArg).rep0).map { case (f, args) =>
      args.foldLeft(f) {
        case (acc, Left(a)) => app(acc, a)
        case (acc, Right(t)) => tyApp(acc, t)
      }
    }
  }

  lazy val atom: Parser[Rec[Expr]] = {
    val parens = Parser.char('(') *> sp *> Parser.defer(expr) <* sp <* Parser.char(')')
    Parser.defer(blockP | unitP.backtrack | numP | charP | stringP | boolP | foreignP | intrinsicP | varP | parens)
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

  val foreignP: Parser[Rec[Expr]] = {
    val types = Parser.char('[') *> sp *> typeP <* sp <* Parser.char(']')
    (Parser.string("foreign") *> types ~ (sp1.with1 *> identifier)).map {
      case (t, n) => foreign(Variable(n), t)
    }
  }

  val intrinsicP: Parser[Rec[Expr]] = {
    val name = Parser.char('[') *> sp *> identifier <* sp <* Parser.char(']')
    val op = name.map(n => StandardLibrary.intrinsicOp(n).getOrElse(sys.error(s"Unknown intrinsic $n")))
    val arg = Parser.char('(') *> sp *> Parser.defer(expr) <* sp <* Parser.char(')')
    (Parser.string("intrinsic") *> op ~ arg.rep).map { case (op, args) =>
      intrinsic(op, args.toList)
    }
  }

  val varP: Parser[Rec[Expr]] =
    identifier.map(n => varr(Variable(n)))

  private val intSuffixP: Parser[String] =
    Parser.string("isize").as("isize") |
    Parser.string("usize").as("usize") |
    Parser.string("i8").as("i8") |
    Parser.string("i16").as("i16") |
    Parser.string("i32").as("i32") |
    Parser.string("i64").as("i64") |
    Parser.string("u8").as("u8") |
    Parser.string("u16").as("u16") |
    Parser.string("u32").as("u32") |
    Parser.string("u64").as("u64")

  private val floatSuffixP: Parser[String] =
    Parser.string("f32").as("f32") | Parser.string("f64").as("f64")

  val numP: Parser[Rec[Expr]] = {
    val digits = Numbers.digits
    val exponent = Parser.charIn("eE") ~ Parser.charIn("+-").? ~ digits
    val floatValue = (digits ~ Parser.char('.') ~ digits ~ exponent.?).string
    val intValue = digits
    val floatLit = (floatValue ~ floatSuffixP.?).map { case (value, suffix) => num(value, suffix.getOrElse("f64")) }
    val intLit = (intValue ~ intSuffixP.?).map { case (value, suffix) => num(value, suffix.getOrElse("i32")) }
    floatLit.backtrack | intLit
  }

  val charP: Parser[Rec[Expr]] =
    Parser.char('\'') *> alpha.map(char) <* Parser.char('\'')

  val stringP: Parser[Rec[Expr]] = {
    val escaped = Parser.char('\\') *> (
      Parser.char('"').as('"') |
      Parser.char('\\').as('\\') |
      Parser.char('n').as('\n') |
      Parser.char('r').as('\r') |
      Parser.char('t').as('\t') |
      Parser.char('0').as('\u0000')
    )
    val plain = Parser.charWhere(ch => ch != '"' && ch != '\\' && ch != '\n' && ch != '\r')
    (Parser.char('"') *> (escaped | plain).rep0 <* Parser.char('"')).map(chars => stringLit(chars.toList.mkString))
  }

  val boolP: Parser[Rec[Expr]] =
    Parser.string("true").as(bool(true)) | Parser.string("false").as(bool(false))

  val unitP: Parser[Rec[Expr]] =
    (Parser.char('(') *> sp *> Parser.char(')')).as(unitLit)

  val programParser: Parser0[Rec[AST.Program.type]] = {
    val sep: Parser[Unit] = (sp1.with1 *> Parser.charIn("\n;").rep <* sp).void
    sp *> topDeclP.repSep(sep).map(decls => program(decls.toList)) <* sp
  }
}
