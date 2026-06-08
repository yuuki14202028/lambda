package com.yuuki14202028

import cats.parse.{Numbers, Parser, Parser0}
import cats.parse.Rfc5234.{alpha, digit}

object ParserAST {

  given Conversion[String, Parser[Unit]] = Parser.string
  given Conversion[Char, Parser[Unit]] = Parser.char

  private val sp: Parser0[Unit] = Parser.charIn(" \t\n\r").rep0.void
  private val sp1: Parser0[Unit] = Parser.charIn(" \t").rep0.void
  private val gap: Parser[Unit] = Parser.charIn(" \t\n\r").rep.void

  private val identStart: Parser[Char] = alpha | Parser.char('_').as('_')
  private val identChar: Parser[Char] = alpha | digit | '_'.as('_')
  private val identifier: Parser[String] = (identStart ~ identChar.rep0).map(_ :: _).map(_.mkString)
  private val typeIdentifier: Parser[String] =
    (identifier ~ ('.' *> identifier).backtrack.rep0).map(_ :: _).map(_.mkString("."))
  private val importPath: Parser[String] = {
    '"' *> Parser.charWhere(ch => ch != '"' && ch != '\n' && ch != '\r').rep0.map(_.mkString) <* '"'
  }

  lazy val expr: Parser[Rec[Expr]] =
    Parser.defer(dataLetP | foldP.backtrack | matchP | typeLetP | tyAbsP | absP | letExprP | ifP | logicalOr)


  lazy val tyAbsP: Parser[Rec[Expr]] = {
    val plain = identifier.map(n => (TypeVariable(n), Kind.Star))
    val annotated = '(' *> sp *> identifier.map(TypeVariable.apply) ~ (sp *> ':' *> sp *> kindP) <* sp <* ')'
    val nameKind = 'Λ' *> sp *> (annotated.backtrack | plain)
    val body = sp *> '.' *> sp *> Parser.defer(expr)
    (nameKind ~ body).map { case ((v, k), b) => tyAbs(v, k, b) }
  }

  lazy val kindP: Parser[Kind] = Parser.defer(kindArrowP)

  private lazy val kindArrowP: Parser[Kind] = {
    val arrowTail = sp.with1.soft *> '→' *> sp *> Parser.defer(kindP)
    (kindAtomP ~ arrowTail.?).map {
      case (from, Some(to)) => Kind.Arrow(from, to)
      case (k, None) => k
    }
  }

  private lazy val kindAtomP: Parser[Kind] = {
    val star = '*'.as(Kind.Star)
    val parens = '(' *> sp *> Parser.defer(kindArrowP) <* sp <* ')'
    star | parens
  }

  lazy val absP: Parser[Rec[Expr]] = {
    val name = 'λ' *> sp *> identifier
    val types = sp *> ':' *> sp *> typeP
    val body = sp *> '.' *> sp *> Parser.defer(expr)
    (name ~ types ~ body).map { case ((n, t), b) => abs(Variable(n), t, b) }
  }

  private def functionType(params: Seq[(String, Rec[Type])], returnType: Rec[Type]): Rec[Type] =
    params.map(_._2).foldRight(returnType)(arrow)

  private def functionValue(params: Seq[(String, Rec[Type])], value: Rec[Expr]): Rec[Expr] =
    params.foldRight(value) { case ((name, types), body) => abs(Variable(name), types, body) }

  private def polymorphicType(params: Seq[(TypeVariable, Kind)], bodyType: Rec[Type]): Rec[Type] =
    params.foldRight(bodyType) { case ((v, k), body) => forallType(v, k, body) }

  private def polymorphicValue(params: Seq[(TypeVariable, Kind)], value: Rec[Expr]): Rec[Expr] =
    params.foldRight(value) { case ((v, k), body) => tyAbs(v, k, body) }

  private lazy val typeParamP: Parser[(TypeVariable, Kind)] = {
    val kindAnn = (sp *> ':' *> sp *> kindP).?
    (Parser.char('[') *> sp *> identifier ~ kindAnn <* sp <* Parser.char(']')).map {
      case (n, k) => (TypeVariable(n), k.getOrElse(Kind.Star))
    }
  }

  private lazy val functionParamsP: Parser0[Seq[(String, Rec[Type])]] = {
    val namedParam = (identifier <* sp <* ':' <* sp) ~ typeP
    val unitParam = '(' *> sp *> ')'.as(("_", unitType))
    val param = unitParam.backtrack | ('(' *> sp *> namedParam <* sp <* ')')
    param.rep0
  }

  private lazy val letExprP: Parser[Rec[Expr]] = {
    Parser.defer((binding ~ inBody).map {
      case ((true, v, t, value), body) => letRec(v, t, value, body)
      case ((false, v, t, value), body) => let(v, t, value, body)
    })
  }

  private lazy val topLetP: Parser[Rec[Decl]] = {
    binding.map {
      case (true, v, t, value) => topLetRec(v, t, value)
      case (false, v, t, value) => topLet(v, t, value)
    }
  }

  private val letHead: Parser[(Boolean, String)] = {
    val head = "let" *> sp1 *> ("rec" <* sp1).as(true).?.map(_.getOrElse(false))
    head ~ identifier
  }

  private lazy val inBody: Parser0[Rec[Expr]] = sp *> "in" *> sp *> Parser.defer(expr)

  private lazy val typeAnnP: Parser0[Rec[Type]] = sp *> ':' *> sp *> typeP
  private lazy val valueP: Parser0[Rec[Expr]]   = sp *> '=' *> sp *> expr

  private lazy val binding: Parser[(Boolean, Variable, Rec[Type], Rec[Expr])] = {
    val typeParams = sp *> typeParamP.rep0
    val params = sp *> functionParamsP
    (letHead ~ typeParams ~ params ~ typeAnnP ~ valueP).map {
      case (((((recursive, name), typeParams), params), returnType), value) => (
        recursive,
        Variable(name),
        polymorphicType(typeParams, functionType(params, returnType)),
        polymorphicValue(typeParams, functionValue(params, value))
      )
    }
  }

  private lazy val topImportP: Parser[Rec[Decl]] =
    ("import" *> gap *> importPath).map(topImport)

  private lazy val typeBinding: Parser[(TypeVariable, Seq[(TypeVariable, Kind)], Rec[Type])] = {
    val name = "type" *> gap *> identifier
    val params = typeParamP.rep0
    val alias = sp *> '=' *> sp *> typeP
    (name ~ params ~ alias).map { case ((name, params), alias) =>
      (TypeVariable(name), params, alias)
    }
  }

  lazy val typeLetP: Parser[Rec[Expr]] =
    (typeBinding ~ inBody).map { case ((v, params, alias), body) =>
      typeLet(v, params, alias, body)
    }

  private lazy val topTypeP: Parser[Rec[Decl]] = typeBinding.map(topType)

  private lazy val dataConstructorP: Parser[DataConstructor[[x] =>> Rec[x]]] = {
    val name = '|' *> sp *> identifier.map(Variable.apply)
    val field = '(' *> sp *> typeP <* sp <* ')'
    (name ~ field.rep0).map(DataConstructor.apply)
  }

  private lazy val dataBinding: Parser[(Boolean, TypeVariable, Seq[(TypeVariable, Kind)], Seq[DataConstructor[[x] =>> Rec[x]]])] = {
    val recursive = "data" *> gap *> ("rec".as(true) <* gap).?.map(_.getOrElse(false))
    val constructors = sp *> '=' *> sp *> dataConstructorP.repSep(gap)
    (recursive ~ identifier ~ typeParamP.rep0 ~ constructors).map { case (((recursive, name), params), constructors) =>
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
    val annotated = '(' *> sp *> identifier.map(TypeVariable.apply) ~ (sp *> ':' *> sp *> kindP) <* sp <* ')'
    val nameKind = 'λ' *> sp *> (annotated.backtrack | plain)
    val body = sp *> '.' *> sp *> Parser.defer(typeP)
    (nameKind ~ body).map { case ((v, k), b) => typeAbs(v, k, b) }
  }

  lazy val forAllP: Parser[Rec[Type]] = {
    val plain = identifier.map(n => (TypeVariable(n), Kind.Star))
    val annotated = '(' *> sp *> identifier.map(TypeVariable.apply) ~ (sp *> ':' *> sp *> kindP) <* sp <* ')'
    val nameKind = '∀' *> sp *> (annotated.backtrack | plain)
    val body = sp *> '.' *> sp *> Parser.defer(typeP)
    (nameKind ~ body).map { case ((v, k), b) => forallType(v, k, b) }
  }

  lazy val arrowTypeP: Parser[Rec[Type]] = {
    val arrowTail = sp.with1.soft *> '→' *> sp *> Parser.defer(typeP)
    (typeAppP ~ arrowTail.?).map {
      case (from, Some(to)) => arrow(from, to)
      case (t, None) => t
    }
  }

  private lazy val typeAppP: Parser[Rec[Type]] = {
    val typeArg = '[' *> sp *> Parser.defer(typeP) <* sp <* ']'
    (typeAtomP ~ typeArg.rep0).map { case (init, args) =>
      args.foldLeft(init)(typeApp)
    }
  }

  private lazy val typeAtomP: Parser[Rec[Type]] = {
    val parens = '(' *> sp *> Parser.defer(typeP) <* sp <* ')'
    val unit = ('(' *> sp *> ')').as(unitType).backtrack
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
    val cond = "if" *> gap *> expr
    val trueBranch = sp *> "then" *> gap *> expr
    val elseBranch = sp *> "else" *> gap *> expr
    (cond ~ trueBranch ~ elseBranch).map { case ((cond, tr), el) =>
      iff(cond, tr, el)
    }
  }

  private lazy val matchCaseP: Parser[MatchCase[[x] =>> Rec[x]]] = {
    val name = '|' *> sp *> identifier.map(Variable.apply)
    val binder = '(' *> sp *> identifier.map(Variable.apply) <* sp <* ')'
    val body = sp *> "->" *> sp *> Parser.defer(expr)
    ((name ~ binder.rep0) ~ body).map { case ((name, binders), body) =>
      MatchCase(name, binders, body)
    }
  }

  lazy val matchP: Parser[Rec[Expr]] = {
    val scrutinee = "match" *> gap *> expr
    val cases = sp *> "with" *> gap *> matchCaseP.repSep(gap)
    (scrutinee ~ cases).map { case (scrutinee, cases) =>
      matchExpr(scrutinee, cases.toList)
    }
  }

  lazy val foldP: Parser[Rec[Expr]] = {
    val scrutinee = "fold" *> gap *> expr
    val resultType = sp *> "as" *> gap *> typeP
    val cases = sp *> "with" *> gap *> matchCaseP.repSep(gap)
    (scrutinee ~ resultType ~ cases).map { case ((scrutinee, resultType), cases) =>
      foldExpr(scrutinee, resultType, cases.toList)
    }
  }

  private val eqOp: Parser[BinOps] =
    "==".as(BinOps.Eq) | "!=".as(BinOps.Neq) |
    "<=".as(BinOps.Leq) | "<".as(BinOps.Lt) |
    ">=".as(BinOps.Geq) | ">".as(BinOps.Gt)

  private val addOp: Parser[BinOps] = '+'.as(BinOps.Add) | '-'.as(BinOps.Sub)

  private val mulOp: Parser[BinOps] = '*'.as(BinOps.Mul) | '/'.as(BinOps.Div) | '%'.as(BinOps.Mod)

  private val bitAndOp: Parser[BinOps] =
    ('&' <* Parser.not(Parser.char('&'))).backtrack.as(BinOps.And)

  private val caseStartLookahead: Parser[Unit] =
    '|' *> sp *> identifier *> sp *> ('('.void | "->".void)

  private val bitOrOp: Parser[BinOps] =
    (Parser.not(caseStartLookahead.backtrack).with1 *> '|' <* Parser.not(Parser.char('|'))).backtrack.as(BinOps.Or)

  private val xorOp: Parser[BinOps] = '^'.as(BinOps.Xor)

  lazy val logicalOr: Parser[Rec[Expr]] = {
    val and = Parser.defer(logicalAnd)
    val tail = (sp.with1.soft *> "||".as(BinOps.ShortOr) ~ (sp.with1 *> and)).rep0
    (and ~ tail).map { case (init, ops) =>
      ops.foldLeft(init) { case (acc, (op, r)) => binop(op, acc, r) }
    }
  }

  lazy val logicalAnd: Parser[Rec[Expr]] = {
    val bitOr = Parser.defer(bitwiseOr)
    val tail = (sp.with1.soft *> "&&".as(BinOps.ShortAnd) ~ (sp.with1 *> bitOr)).rep0
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
    val neg = ('-' *> sp *> Parser.defer(unaryP)).map(b => unop(UnaryOps.Neg, b))
    val not = ('!' *> sp *> Parser.defer(unaryP)).map(b => unop(UnaryOps.Not, b))
    neg | not | Parser.defer(appP)
  }

  lazy val appP: Parser[Rec[Expr]] = {
    type Postfix = Either[Rec[Expr], Rec[Type]]
    val unitArg: Parser[Postfix] =
      ('(' *> sp *> ')').as(Left(unitLit)).backtrack
    val exprArg: Parser[Postfix] =
      ('(' *> sp *> Parser.defer(expr) <* sp <* ')').map(Left.apply)
    val typeArg: Parser[Postfix] =
      ('[' *> sp *> Parser.defer(typeP) <* sp <* ']').map(Right.apply)
    (atom ~ (unitArg | exprArg | typeArg).rep0).map { case (f, args) =>
      args.foldLeft(f) {
        case (acc, Left(a)) => app(acc, a)
        case (acc, Right(t)) => tyApp(acc, t)
      }
    }
  }

  lazy val atom: Parser[Rec[Expr]] = {
    val parens = '(' *> sp *> Parser.defer(expr) <* sp <* ')'
    Parser.defer(blockP | unitP.backtrack | numP | charP | stringP | boolP | foreignP | intrinsicP | varP | parens)
  }

  lazy val blockP: Parser[Rec[Expr]] = {
    val semi = sp *> ';' <* sp
    val discarded = (Parser.defer(expr) <* semi).backtrack.rep0
    val result = Parser.defer(expr).?
    ('{' *> sp *> discarded ~ result <* sp <* '}').map(block)
  }

  val foreignP: Parser[Rec[Expr]] = {
    val types = '[' *> sp *> typeP <* sp <* ']'
    ("foreign" *> types ~ (sp1.with1 *> identifier.map(Variable.apply))).map(_.swap).map(foreign)
  }

  val intrinsicP: Parser[Rec[Expr]] = {
    val name = '[' *> sp *> identifier <* sp <* ']'
    val op = name.map(n => StandardLibrary.intrinsicOp(n).getOrElse(sys.error(s"Unknown intrinsic $n")))
    val arg = '(' *> sp *> Parser.defer(expr) <* sp <* ')'
    ("intrinsic" *> op ~ arg.rep).map { case (op, args) =>
      intrinsic(op, args.toList)
    }
  }

  val varP: Parser[Rec[Expr]] = identifier.map(n => varr(Variable(n)))

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
    val floatValue = (digits ~ '.' ~ digits ~ exponent.?).string
    val intValue = digits
    val floatLit = (floatValue ~ floatSuffixP.?).map { case (value, suffix) => num(value, suffix.getOrElse("f64")) }
    val intLit = (intValue ~ intSuffixP.?).map { case (value, suffix) => num(value, suffix.getOrElse("i32")) }
    floatLit.backtrack | intLit
  }

  val charP: Parser[Rec[Expr]] = '\'' *> alpha.map(char) <* '\''

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
    ('"' *> (escaped | plain).rep0 <* '"').map(chars => stringLit(chars.mkString))
  }

  val boolP: Parser[Rec[Expr]] = "true".as(bool(true)) | "false".as(bool(false))

  val unitP: Parser[Rec[Expr]] = ('(' *> sp *> ')').as(unitLit)

  val programParser: Parser0[Rec[AST.Program.type]] = {
    val sep = (sp1.with1 *> Parser.charIn("\n;").rep <* sp).void
    sp *> topDeclP.repSep(sep).map(decls => program(decls.toList)) <* sp
  }
}
