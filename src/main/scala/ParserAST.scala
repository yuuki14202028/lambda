package com.yuuki14202028

import cats.parse.{Numbers, Parser, Parser0}
import cats.parse.Rfc5234.{alpha, digit}

object ParserAST {

  given Conversion[String, Parser[Unit]] = Parser.string
  given Conversion[Char, Parser[Unit]] = Parser.char

  private val sp: Parser0[Unit] = Parser.charIn(" \t\n\r").rep0.void
  private val sp1: Parser0[Unit] = Parser.charIn(" \t").rep0.void
  private val gap: Parser[Unit] = Parser.charIn(" \t\n\r").rep.void

  private def spaced(sep: Parser[Unit]): Parser[Unit] = sp.with1 *> sep <* sp

  extension [A](p: Parser[A]) {
    private infix def -*>[B](right: Parser0[B]): Parser[B] = p *> sp *> right
    private infix def -+>[B](right: Parser0[B]): Parser[B] = p *> gap *> right
    private infix def <*-[B](close: Parser0[B]): Parser[A] = p <* sp <* close
    private def parens: Parser[A] = '(' -*> p <*- ')'
    private def brackets: Parser[A] = '[' -*> p <*- ']'
  }

  private val unitParens: Parser[Unit] = '(' -*> ')'

  private def opLevel(op: Parser[(Rec[Expr], Rec[Expr]) => Rec[Expr]], next: => Parser[Rec[Expr]], atomicStep: Boolean = false): Parser[Rec[Expr]] = {
    val operand = Parser.defer(next)
    val step = op ~ (sp.with1 *> operand)
    val tail = (sp.with1.soft *> (if (atomicStep) step.backtrack else step)).rep0
    (operand ~ tail).map { case (init, ops) =>
      ops.foldLeft(init) { case (acc, (f, r)) => f(acc, r) }
    }
  }

  private def binaryLevel(op: Parser[BinOps], next: => Parser[Rec[Expr]]): Parser[Rec[Expr]] =
    opLevel(op.map(o => binop(o, _, _)), next)

  private def arrowChain[A](atom: Parser[A], whole: => Parser[A])(make: (A, A) => A): Parser[A] = {
    val arrowTail = sp.with1.soft *> '→' -*> Parser.defer(whole)
    (atom ~ arrowTail.?).map {
      case (from, Some(to)) => make(from, to)
      case (a, None) => a
    }
  }

  private def binderP[A](sigil: Char, body: => Parser[A])(make: (TypeVariable, Kind, A) => A): Parser[A] = {
    val plain = identifier.map(n => (TypeVariable(n), Kind.Star))
    val annotated = (identifier.map(TypeVariable.apply) ~ (spaced(':') *> kindP)).parens
    val nameKind = Parser.char(sigil) -*> (annotated.backtrack | plain)
    (nameKind ~ (spaced('.') *> Parser.defer(body))).map { case ((v, k), b) => make(v, k, b) }
  }

  private val identStart: Parser[Char] = alpha | Parser.char('_').as('_')
  private val identChar: Parser[Char] = alpha | digit | '_'.as('_')
  private val identifier: Parser[String] = (identStart ~ identChar.rep0).map(_ :: _).map(_.mkString)
  private val typeIdentifier: Parser[String] =
    (identifier ~ ('.' *> identifier).backtrack.rep0).map(_ :: _).map(_.mkString("."))
  private val importPath: Parser[String] = {
    '"' *> Parser.charWhere(ch => ch != '"' && ch != '\n' && ch != '\r').rep0.map(_.mkString) <* '"'
  }

  lazy val expr: Parser[Rec[Expr]] =
    Parser.defer(dataLetP | foldP.backtrack | matchP | typeLetP | tyAbsP | absP | contextP.backtrack | letExprP | ifP | logicalOr)


  lazy val tyAbsP: Parser[Rec[Expr]] = binderP('Λ', expr)(tyAbs)

  lazy val kindP: Parser[Kind] = Parser.defer(kindArrowP)

  private lazy val kindArrowP: Parser[Kind] = arrowChain(kindAtomP, kindP)(Kind.Arrow.apply)

  private lazy val kindAtomP: Parser[Kind] = {
    val star = '*'.as(Kind.Star)
    star | Parser.defer(kindArrowP).parens
  }

  lazy val absP: Parser[Rec[Expr]] = {
    val name = 'λ' -*> identifier
    ((name ~ (spaced(':') *> typeP)) ~ (spaced('.') *> Parser.defer(expr))).map {
      case ((n, t), b) => abs(Variable(n), t, b)
    }
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
    val kindAnn = (spaced(':') *> kindP).?
    (identifier ~ kindAnn).brackets.map {
      case (n, k) => (TypeVariable(n), k.getOrElse(Kind.Star))
    }
  }

  private lazy val functionParamsP: Parser0[Seq[(String, Rec[Type])]] = {
    val namedParam = identifier ~ (spaced(':') *> typeP)
    val unitParam = unitParens.as(("_", unitType))
    val param = unitParam.backtrack | namedParam.parens
    param.rep0
  }

  private lazy val letExprP: Parser[Rec[Expr]] = {
    Parser.defer((binding ~ inBody).map {
      case ((true, v, t, value), body) => letRec(v, t, value, body)
      case ((false, v, t, value), body) => let(v, t, value, body)
    })
  }

  private lazy val constraintP: Parser[Constraint[Rec]] =
    (identifier.map(TypeVariable.apply) ~ typeP.brackets.rep0).map(Constraint.apply).brackets

  private lazy val whereClauseP: Parser0[Seq[Constraint[Rec]]] =
    (spaced("where") *> (constraintP ~ (sp.with1.soft *> constraintP).backtrack.rep0)).backtrack.?.map {
      case Some((h, t)) => h :: t
      case None => Nil
    }

  private lazy val topLetP: Parser[Rec[Decl]] = {
    val typeParams = sp *> typeParamP.rep0
    val params = sp *> functionParamsP
    (letHead ~ typeParams ~ params ~ whereClauseP ~ typeAnnP ~ valueP).map {
      case ((((((recursive, name), typeParams), params), constraints), returnType), value) =>
        val fullType = polymorphicType(typeParams, functionType(params, returnType))
        val fullValue = polymorphicValue(typeParams, functionValue(params, value))
        if (constraints.isEmpty) {
          if (recursive) topLetRec(Variable(name), fullType, fullValue)
          else topLet(Variable(name), fullType, fullValue)
        } else topLetWhere(Variable(name), typeParams, constraints, fullType, fullValue, recursive)
    }
  }

  private val letHead: Parser[(Boolean, String)] = {
    val head = "let" *> sp1 *> ("rec" <* sp1).as(true).?.map(_.getOrElse(false))
    head ~ identifier
  }

  private lazy val inBody: Parser0[Rec[Expr]] = spaced("in") *> Parser.defer(expr)

  private lazy val typeAnnP: Parser0[Rec[Type]] = spaced(':') *> typeP
  private lazy val valueP: Parser0[Rec[Expr]] = spaced('=') *> expr

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

  private lazy val topImportP: Parser[Rec[Decl]] = ("import" -+> importPath).map(topImport)

  private lazy val typeBinding: Parser[(TypeVariable, Seq[(TypeVariable, Kind)], Rec[Type])] = {
    val name = "type" -+> identifier
    val params = typeParamP.rep0
    (name ~ params ~ (spaced('=') *> typeP)).map { case ((name, params), alias) =>
      (TypeVariable(name), params, alias)
    }
  }

  lazy val typeLetP: Parser[Rec[Expr]] = (typeBinding ~ inBody).map { case ((v, params, alias), body) =>
    typeLet(v, params, alias, body)
  }

  private lazy val topTypeP: Parser[Rec[Decl]] = typeBinding.map(topType)

  private lazy val dataConstructorP: Parser[DataConstructor[[x] =>> Rec[x]]] = {
    val name = '|' -*> identifier.map(Variable.apply)
    val field = typeP.parens
    (name ~ field.rep0).map(DataConstructor.apply)
  }

  private lazy val dataBinding: Parser[(Boolean, TypeVariable, Seq[(TypeVariable, Kind)], Seq[DataConstructor[[x] =>> Rec[x]]])] = {
    val recursive = "data" -+> ("rec".as(true) <* gap).?.map(_.getOrElse(false))
    val constructors = spaced('=') *> dataConstructorP.repSep(gap)
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

  lazy val traitP: Parser[Rec[Decl]] = {
    val head = "trait" -+> identifier.map(TypeVariable.apply)
    val typeParams = sp *> typeParamP.rep0
    val params = sp *> functionParamsP
    val supers = whereClauseP
    val method = ((("def" -+> identifier.map(Variable.apply)) ~ typeParams ~ params ~ typeAnnP).map {
      case (((name, typeParams), params), returnType) =>
        val sig = polymorphicType(typeParams, functionType(params, returnType))
        MethodSig(name, sig, None)
    } <* sp).rep
    val body = sp.with1 *> ('{' -*> method <*- '}')
    (head ~ typeParams ~ supers ~ body).map {
      case (((f, t), s), m) => topTrait(f, t, s, m.toList)
    }
  }

  lazy val implP: Parser[Rec[Decl]] = {
    val implParams = ("impl" *> sp *> typeParamP.rep.map(_.toList)).backtrack | ("impl" *> gap).as(List.empty[(TypeVariable, Kind)])
    val head = implParams ~ (sp.with1 *> identifier.map(TypeVariable.apply))
    val targets = sp *> typeP.brackets.rep.map(_.toList)
    val typeParams = sp *> typeParamP.rep0
    val params = sp *> functionParamsP
    val returnAnn = typeAnnP.backtrack.?
    val method = ((("def" -+> identifier.map(Variable.apply)) ~ typeParams ~ params ~ returnAnn ~ valueP).map {
      case ((((name, typeParams), params), returnType), value) =>
        val body = polymorphicValue(typeParams, functionValue(params, value))
        val sig = returnType.map(rt => polymorphicType(typeParams, functionType(params, rt)))
        MethodImpl(name, sig, body)
    } <* sp).rep
    val body = sp.with1 *> ('{' -*> method <*- '}')
    (head ~ targets ~ whereClauseP ~ body).map {
      case ((((implParams, name), targets), context), methods) => topImpl(name, implParams, targets, context, methods.toList)
    }
  }

  private lazy val topDeclP: Parser[Rec[Decl]] =
    Parser.defer(topImportP.backtrack | topDataP.backtrack | topTypeP.backtrack | traitP.backtrack | implP.backtrack | topLetP)

  lazy val typeP: Parser[Rec[Type]] = Parser.defer(forAllP | typeLambdaP | arrowTypeP)

  lazy val typeLambdaP: Parser[Rec[Type]] = binderP('λ', typeP)(typeAbs)

  lazy val forAllP: Parser[Rec[Type]] = binderP('∀', typeP)(forallType)

  lazy val arrowTypeP: Parser[Rec[Type]] = arrowChain(typeAppP, typeP)(arrow)

  private lazy val typeAppP: Parser[Rec[Type]] = {
    val typeArg = Parser.defer(typeP).brackets
    (typeAtomP ~ typeArg.rep0).map { case (init, args) =>
      args.foldLeft(init)(typeApp)
    }
  }

  private lazy val typeAtomP: Parser[Rec[Type]] = {
    val unit = unitParens.as(unitType).backtrack
    unit | namedTypeP | Parser.defer(typeP).parens
  }

  private lazy val namedTypeP: Parser[Rec[Type]] =
    typeIdentifier.map { name =>
      if (BuiltinTypes.isPrimitive(name)) primitive(name)
      else typeVar(TypeVariable(name))
    }

  private lazy val typeVarP: Parser[Rec[Type]] =
    identifier.map(n => typeVar(TypeVariable(n)))

  lazy val ifP: Parser[Rec[Expr]] = {
    val cond = "if" -+> expr
    val trueBranch = sp.with1 *> ("then" -+> expr)
    val elseBranch = sp.with1 *> ("else" -+> expr)
    (cond ~ trueBranch ~ elseBranch).map { case ((cond, tr), el) =>
      iff(cond, tr, el)
    }
  }

  private lazy val matchCaseP: Parser[MatchCase[[x] =>> Rec[x]]] = {
    val name = '|' -*> identifier.map(Variable.apply)
    val binder = identifier.map(Variable.apply).parens
    val body = spaced("->") *> Parser.defer(expr)
    ((name ~ binder.rep0) ~ body).map { case ((name, binders), body) =>
      MatchCase(name, binders, body)
    }
  }

  lazy val matchP: Parser[Rec[Expr]] = {
    val scrutinee = "match" -+> expr
    val cases = sp.with1 *> ("with" -+> matchCaseP.repSep(gap))
    (scrutinee ~ cases).map { case (scrutinee, cases) =>
      matchExpr(scrutinee, cases.toList)
    }
  }

  lazy val contextP: Parser[Rec[Expr]] = {
    val monad = "context" -*> Parser.defer(typeP).brackets
    val monadicBinding = (identifier.map(Variable.apply) ~ typeAnnP ~ valueP <* spaced(';')).backtrack.map {
      case ((name, types), value) => ContextBinding[[x] =>> Rec[x]](name, types, value, monadic = true)
    }
    val pureLet = (binding <* spaced(';')).backtrack.map { case (recursive, name, types, value) =>
      ContextBinding[[x] =>> Rec[x]](name, types, value, monadic = false, recursive)
    }
    val bindings = (pureLet | monadicBinding).rep0
    val body = '{' -*> (bindings.with1 ~ Parser.defer(expr)) <*- '}'
    (monad ~ (sp.with1 *> body)).map { case (m, (bs, result)) => contextExpr(m, bs, result) }
  }

  lazy val foldP: Parser[Rec[Expr]] = {
    val scrutinee = "fold" -+> expr
    val resultType = sp.with1 *> ("as" -+> typeP)
    val cases = sp.with1 *> ("with" -+> matchCaseP.repSep(gap))
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
    '|' -*> identifier -*> ('('.void | "->".void)

  private val bitOrOp: Parser[BinOps] =
    (Parser.not(caseStartLookahead.backtrack).with1 *> '|' <* Parser.not(Parser.char('|'))).backtrack.as(BinOps.Or)

  private val xorOp: Parser[BinOps] = '^'.as(BinOps.Xor)

  private val exprKeywords: Set[String] = Set(
    "in", "then", "else", "with", "as", "where", "context",
    "let", "rec", "if", "match", "fold", "data", "type",
    "import", "trait", "impl", "def", "true", "false",
    "foreign", "intrinsic"
  )

  private val infixIdentOp: Parser[(Rec[Expr], Rec[Expr]) => Rec[Expr]] =
    identifier.filter(n => !exprKeywords(n)).map { n => (l, r) =>
      app(app(varr(Variable(n)), l), r)
    }

  lazy val logicalOr: Parser[Rec[Expr]]      = binaryLevel("||".as(BinOps.ShortOr), logicalAnd)
  lazy val logicalAnd: Parser[Rec[Expr]]     = binaryLevel("&&".as(BinOps.ShortAnd), bitwiseOr)
  lazy val bitwiseOr: Parser[Rec[Expr]]      = binaryLevel(bitOrOp, bitwiseXor)
  lazy val bitwiseXor: Parser[Rec[Expr]]     = binaryLevel(xorOp, bitwiseAnd)
  lazy val bitwiseAnd: Parser[Rec[Expr]]     = binaryLevel(bitAndOp, equitive)
  lazy val equitive: Parser[Rec[Expr]]       = binaryLevel(eqOp, additive)
  lazy val additive: Parser[Rec[Expr]]       = binaryLevel(addOp, multiplicative)
  lazy val multiplicative: Parser[Rec[Expr]] = binaryLevel(mulOp, infixApp)
  lazy val infixApp: Parser[Rec[Expr]]       = opLevel(infixIdentOp, unaryP, atomicStep = true)

  lazy val unaryP: Parser[Rec[Expr]] = {
    val neg = ('-' -*> Parser.defer(unaryP)).map(b => unop(UnaryOps.Neg, b))
    val not = ('!' -*> Parser.defer(unaryP)).map(b => unop(UnaryOps.Not, b))
    neg | not | Parser.defer(appP)
  }

  lazy val appP: Parser[Rec[Expr]] = {
    type Postfix = Either[Rec[Expr], Rec[Type]]
    val unitArg: Parser[Postfix] = unitParens.as(Left(unitLit)).backtrack
    val exprArg: Parser[Postfix] = Parser.defer(expr).parens.map(Left.apply)
    val typeArg: Parser[Postfix] = Parser.defer(typeP).brackets.map(Right.apply)
    (atom ~ (unitArg | exprArg | typeArg).rep0).map { case (f, args) =>
      args.foldLeft(f) {
        case (acc, Left(a)) => app(acc, a)
        case (acc, Right(t)) => tyApp(acc, t)
      }
    }
  }

  lazy val atom: Parser[Rec[Expr]] =
    Parser.defer(blockP | unitP.backtrack | numP | charP | stringP | interpStringP | boolP | foreignP | intrinsicP | varP | Parser.defer(expr).parens)

  lazy val blockP: Parser[Rec[Expr]] = {
    val discarded = (Parser.defer(expr) <* spaced(';')).backtrack.rep0
    val result = Parser.defer(expr).?
    ('{' -*> discarded ~ result <*- '}').map(block)
  }

  val foreignP: Parser[Rec[Expr]] = {
    ("foreign" *> typeP.brackets ~ (sp1.with1 *> identifier.map(Variable.apply))).map(_.swap).map(foreign)
  }

  val intrinsicP: Parser[Rec[Expr]] = {
    val name = identifier.brackets
    val op = name.map(n => StandardLibrary.intrinsicOp(n).getOrElse(sys.error(s"Unknown intrinsic $n")))
    val arg = Parser.defer(expr).parens
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
    val floatLit = (floatValue ~ floatSuffixP.?).map { case (value, suffix) => num(value, suffix.getOrElse("f64")) }
    val intLit = (digits ~ intSuffixP.?).map { case (value, suffix) => num(value, suffix.getOrElse("i32")) }
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

  val interpStringP: Parser[Rec[Expr]] = {
    val escaped = Parser.char('\\') *> (
      Parser.char('`').as('`') |
      Parser.char('{').as('{') |
      Parser.char('}').as('}') |
      Parser.char('\\').as('\\') |
      Parser.char('n').as('\n') |
      Parser.char('r').as('\r') |
      Parser.char('t').as('\t') |
      Parser.char('0').as('\u0000')
    )
    val plain = Parser.charWhere(ch => ch != '`' && ch != '{' && ch != '\\' && ch != '\n' && ch != '\r')
    val literalPart: Parser[Either[String, Rec[Expr]]] =
      (escaped | plain).rep.map(chars => Left(chars.toList.mkString))
    val exprPart: Parser[Either[String, Rec[Expr]]] =
      ('{' -*> Parser.defer(expr) <*- '}').map(Right.apply)
    ('`' *> (literalPart | exprPart).rep0 <* '`').map { parts =>
      if (parts.forall(_.isLeft)) stringLit(parts.collect { case Left(s) => s }.mkString)
      else strInterp(parts.map {
        case Left(s) => stringLit(s)
        case Right(e) => e
      })
    }
  }

  val boolP: Parser[Rec[Expr]] = "true".as(bool(true)) | "false".as(bool(false))

  val unitP: Parser[Rec[Expr]] = unitParens.as(unitLit)

  val programParser: Parser0[Rec[AST.Program.type]] = {
    val sep = (sp1.with1 *> Parser.charIn("\n;").rep.void <* sp)
    sp *> topDeclP.repSep(sep).map(decls => program(decls.toList)) <* sp
  }
}