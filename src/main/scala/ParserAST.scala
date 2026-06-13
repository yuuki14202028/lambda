package com.yuuki14202028

import cats.parse.{Numbers, Parser, Parser0}
import cats.parse.Rfc5234.{alpha, digit}
import scala.language.implicitConversions

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

  private def at[I](offset: Int, node: AST[IndexedRec, I]): IndexedRec[I] = HCofree(offset, node)

  extension [I](p: Parser[AST[IndexedRec, I]]) {
    private def indexed: Parser[IndexedRec[I]] = {
      (Parser.index.with1 ~ p).map { case (offset, node) => at(offset, node) }
    }
  }

  private val unitParens: Parser[Unit] = '(' -*> ')'

  private def opLevel(op: Parser[(IndexedRec[Expr], IndexedRec[Expr]) => IndexedRec[Expr]], next: => Parser[IndexedRec[Expr]], atomicStep: Boolean = false): Parser[IndexedRec[Expr]] = {
    val operand = Parser.defer(next)
    val step = op ~ (sp.with1 *> operand)
    val tail = (sp.with1.soft *> (if (atomicStep) step.backtrack else step)).rep0
    (operand ~ tail).map { case (init, ops) =>
      ops.foldLeft(init) { case (acc, (f, r)) => f(acc, r) }
    }
  }

  private def binaryLevel(op: Parser[BinOps], next: => Parser[IndexedRec[Expr]]): Parser[IndexedRec[Expr]] =
    opLevel(op.map(o => (l, r) => at(l.extract, AST.BinOp(o, l, r))), next)

  private def arrowChain[A](atom: Parser[A], whole: => Parser[A])(make: (A, A) => A): Parser[A] = {
    val arrowTail = sp.with1.soft *> '→' -*> Parser.defer(whole)
    (atom ~ arrowTail.?).map {
      case (from, Some(to)) => make(from, to)
      case (a, None) => a
    }
  }

  private def binderP[I](sigil: Char, body: => Parser[IndexedRec[I]])(make: (TypeVariable, Kind, IndexedRec[I]) => AST[IndexedRec, I]): Parser[IndexedRec[I]] = {
    val plain = identifier.map(n => (TypeVariable(n), Kind.Star))
    val annotated = (identifier.map(TypeVariable.apply) ~ (spaced(':') *> kindP)).parens
    val nameKind = Parser.char(sigil) -*> (annotated.backtrack | plain)
    (nameKind ~ (spaced('.') *> Parser.defer(body))).map { case ((v, k), b) => make(v, k, b) }.indexed
  }

  private val identStart: Parser[Char] = alpha | Parser.char('_').as('_')
  private val identChar: Parser[Char] = alpha | digit | '_'.as('_')
  private val identifier: Parser[String] = (identStart ~ identChar.rep0).map(_ :: _).map(_.mkString)
  private val typeIdentifier: Parser[String] =
    (identifier ~ ('.' *> identifier).backtrack.rep0).map(_ :: _).map(_.mkString("."))
  private val importPath: Parser[String] = {
    '"' *> Parser.charWhere(ch => ch != '"' && ch != '\n' && ch != '\r').rep0.map(_.mkString) <* '"'
  }

  lazy val expr: Parser[IndexedRec[Expr]] =
    Parser.defer(dataLetP | foldP.backtrack | matchP | typeLetP | tyAbsP | absP | contextP.backtrack | letExprP | ifP | logicalOr)


  lazy val tyAbsP: Parser[IndexedRec[Expr]] = binderP('Λ', expr)(AST.TyAbs.apply)

  lazy val kindP: Parser[Kind] = Parser.defer(kindArrowP)

  private lazy val kindArrowP: Parser[Kind] = arrowChain(kindAtomP, kindP)(Kind.Arrow.apply)

  private lazy val kindAtomP: Parser[Kind] = {
    val star = '*'.as(Kind.Star)
    star | Parser.defer(kindArrowP).parens
  }

  lazy val absP: Parser[IndexedRec[Expr]] = {
    val name = 'λ' -*> identifier
    ((name ~ (spaced(':') *> typeP)) ~ (spaced('.') *> Parser.defer(expr))).map {
      case ((n, t), b) => AST.Abs(Variable(n), t, b)
    }.indexed
  }

  private def functionType(params: Seq[(String, IndexedRec[Type])], returnType: IndexedRec[Type]): IndexedRec[Type] =
    params.map(_._2).foldRight(returnType)((from, to) => at(from.extract, AST.Arrow(from, to)))

  private def functionValue(params: Seq[(String, IndexedRec[Type])], value: IndexedRec[Expr]): IndexedRec[Expr] =
    params.foldRight(value) { case ((name, types), body) => at(types.extract, AST.Abs(Variable(name), types, body)) }

  private def polymorphicType(params: Seq[(TypeVariable, Kind)], bodyType: IndexedRec[Type]): IndexedRec[Type] =
    params.foldRight(bodyType) { case ((v, k), body) => at(body.extract, AST.ForAll(v, k, body)) }

  private def polymorphicValue(params: Seq[(TypeVariable, Kind)], value: IndexedRec[Expr]): IndexedRec[Expr] =
    params.foldRight(value) { case ((v, k), body) => at(body.extract, AST.TyAbs(v, k, body)) }

  private lazy val typeParamP: Parser[(TypeVariable, Kind)] = {
    val kindAnn = (spaced(':') *> kindP).?
    (identifier ~ kindAnn).brackets.map {
      case (n, k) => (TypeVariable(n), k.getOrElse(Kind.Star))
    }
  }

  private lazy val functionParamsP: Parser0[Seq[(String, IndexedRec[Type])]] = {
    val namedParam = identifier ~ (spaced(':') *> typeP)
    val unitParam = (Parser.index.with1 <* unitParens).map(offset => ("_", at(offset, AST.Primitive("unit"))))
    val param = unitParam.backtrack | namedParam.parens
    param.rep0
  }

  private lazy val letExprP: Parser[IndexedRec[Expr]] = {
    Parser.defer((binding ~ inBody).map {
      case ((true, v, t, value), body) => AST.LetRec(v, t, value, body)
      case ((false, v, t, value), body) => AST.Let(v, t, value, body)
    }).indexed
  }

  private lazy val constraintP: Parser[Constraint[IndexedRec]] =
    (identifier.map(TypeVariable.apply) ~ typeP.brackets.rep0).map(Constraint.apply)

  private lazy val whereClauseP: Parser0[Seq[Constraint[IndexedRec]]] =
    (spaced("where") *> constraintP.repSep(spaced(',').backtrack)).backtrack.?.map(_.fold(List.empty)(_.toList))

  private lazy val topLetP: Parser[IndexedRec[Decl]] = {
    val typeParams = sp *> typeParamP.rep0
    val params = sp *> functionParamsP
    (letHead ~ typeParams ~ params ~ whereClauseP ~ typeAnnP ~ valueP).map {
      case ((((((recursive, name), typeParams), params), constraints), returnType), value) =>
        val fullType = polymorphicType(typeParams, functionType(params, returnType))
        val fullValue = polymorphicValue(typeParams, functionValue(params, value))
        if (constraints.isEmpty) {
          if (recursive) AST.TopLetRec(Variable(name), fullType, fullValue)
          else AST.TopLet(Variable(name), fullType, fullValue)
        } else AST.TopLetWhere(Variable(name), typeParams, constraints, fullType, fullValue, recursive)
    }.indexed
  }

  private val letHead: Parser[(Boolean, String)] = {
    val head = "let" *> sp1 *> ("rec" <* sp1).as(true).?.map(_.getOrElse(false))
    head ~ identifier
  }

  private lazy val inBody: Parser0[IndexedRec[Expr]] = spaced("in") *> Parser.defer(expr)

  private lazy val typeAnnP: Parser0[IndexedRec[Type]] = spaced(':') *> typeP
  private lazy val valueP: Parser0[IndexedRec[Expr]] = spaced('=') *> expr

  private lazy val binding: Parser[(Boolean, Variable, IndexedRec[Type], IndexedRec[Expr])] = {
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

  private lazy val topImportP: Parser[IndexedRec[Decl]] =
    ("import" -+> importPath).map(path => AST.TopImport[IndexedRec, Decl](path)).indexed

  private lazy val typeBinding: Parser[(TypeVariable, Seq[(TypeVariable, Kind)], IndexedRec[Type])] = {
    val name = "type" -+> identifier
    val params = typeParamP.rep0
    (name ~ params ~ (spaced('=') *> typeP)).map { case ((name, params), alias) =>
      (TypeVariable(name), params, alias)
    }
  }

  lazy val typeLetP: Parser[IndexedRec[Expr]] = (typeBinding ~ inBody).map { case ((v, params, alias), body) =>
    AST.TypeLet(v, params, alias, body)
  }.indexed

  private lazy val topTypeP: Parser[IndexedRec[Decl]] = typeBinding.map { case (v, params, alias) =>
    AST.TopType(v, params, alias)
  }.indexed

  private lazy val dataConstructorP: Parser[DataConstructor[IndexedRec]] = {
    (identifier.map(Variable.apply) ~ typeP.parens.rep0).map(DataConstructor.apply)
  }

  private lazy val dataBinding: Parser[(Boolean, TypeVariable, Seq[(TypeVariable, Kind)], Seq[DataConstructor[IndexedRec]])] = {
    val recursive = "data" -+> ("rec".as(true) <* gap).?.map(_.getOrElse(false))
    val constructors = spaced('=') *> '{' -*> dataConstructorP.repSep(spaced(',').backtrack) <*- '}'
    (recursive ~ identifier ~ typeParamP.rep0 ~ constructors).map { case (((recursive, name), params), constructors) =>
      (recursive, TypeVariable(name), params, constructors.toList)
    }
  }

  lazy val dataLetP: Parser[IndexedRec[Expr]] = (dataBinding ~ inBody).map { case ((recursive, v, params, constructors), body) =>
    AST.DataLet(v, params, constructors, body, recursive)
  }.indexed

  private lazy val topDataP: Parser[IndexedRec[Decl]] = dataBinding.map { case (recursive, v, params, constructors) =>
    AST.TopData(v, params, constructors, recursive)
  }.indexed

  lazy val traitP: Parser[IndexedRec[Decl]] = {
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
      case (((f, t), s), m) => AST.TopTrait(f, t, s, m.toList)
    }.indexed
  }

  lazy val implP: Parser[IndexedRec[Decl]] = {
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
      case ((((implParams, name), targets), context), methods) => AST.TopImpl(name, implParams, targets, context, methods.toList)
    }.indexed
  }

  private lazy val topDeclP: Parser[IndexedRec[Decl]] =
    Parser.defer(topImportP.backtrack | topDataP.backtrack | topTypeP.backtrack | traitP.backtrack | implP.backtrack | topLetP)

  lazy val typeP: Parser[IndexedRec[Type]] = Parser.defer(forAllP | typeLambdaP | arrowTypeP)

  lazy val typeLambdaP: Parser[IndexedRec[Type]] = binderP('λ', typeP)(AST.TypeAbs.apply)

  lazy val forAllP: Parser[IndexedRec[Type]] = binderP('∀', typeP)(AST.ForAll.apply)

  lazy val arrowTypeP: Parser[IndexedRec[Type]] =
    arrowChain(typeAppP, typeP)((from, to) => at(from.extract, AST.Arrow(from, to)))

  private lazy val typeAppP: Parser[IndexedRec[Type]] = {
    val typeArg = Parser.defer(typeP).brackets
    (typeAtomP ~ typeArg.rep0).map { case (init, args) =>
      args.foldLeft(init)((acc, arg) => at(acc.extract, AST.TypeApp(acc, arg)))
    }
  }

  private lazy val typeAtomP: Parser[IndexedRec[Type]] = {
    val unit = unitParens.as(AST.Primitive[IndexedRec, Type]("unit")).indexed.backtrack
    unit | namedTypeP | Parser.defer(typeP).parens
  }

  private lazy val namedTypeP: Parser[IndexedRec[Type]] =
    typeIdentifier.map { name =>
      if (BuiltinTypes.isPrimitive(name)) AST.Primitive[IndexedRec, Type](name)
      else AST.TypeVar[IndexedRec, Type](TypeVariable(name))
    }.indexed

  private lazy val typeVarP: Parser[IndexedRec[Type]] =
    identifier.map(n => AST.TypeVar[IndexedRec, Type](TypeVariable(n))).indexed

  lazy val ifP: Parser[IndexedRec[Expr]] = {
    val cond = "if" -+> expr
    val trueBranch = sp.with1 *> ("then" -+> expr)
    val elseBranch = sp.with1 *> ("else" -+> expr)
    (cond ~ trueBranch ~ elseBranch).map { case ((cond, tr), el) =>
      AST.If(cond, tr, el)
    }.indexed
  }

  private lazy val matchCaseP: Parser[MatchCase[IndexedRec]] = {
    val name = identifier.map(Variable.apply)
    val binder = identifier.map(Variable.apply).parens
    val body = spaced("=>") *> Parser.defer(expr)
    (name ~ binder.rep0 ~ body).map { case ((name, binders), body) =>
      MatchCase(name, binders, body)
    }
  }

  lazy val matchP: Parser[IndexedRec[Expr]] = {
    val scrutinee = "match" -+> expr
    val cases = sp.with1 *> ('{' -*> matchCaseP.repSep(gap.backtrack) <*- '}')
    (scrutinee ~ cases).map { case (scrutinee, cases) =>
      AST.Match(scrutinee, cases.toList)
    }.indexed
  }

  lazy val contextP: Parser[IndexedRec[Expr]] = {
    val monad = "context" -*> Parser.defer(typeP).brackets
    val monadicBinding = (identifier.map(Variable.apply) ~ typeAnnP ~ valueP <* spaced(';')).backtrack.map {
      case ((name, types), value) => ContextBinding[IndexedRec](name, types, value, monadic = true)
    }
    val pureLet = (binding <* spaced(';')).backtrack.map { case (recursive, name, types, value) =>
      ContextBinding[IndexedRec](name, types, value, monadic = false, recursive)
    }
    val bindings = (pureLet | monadicBinding).rep0
    val body = '{' -*> (bindings.with1 ~ Parser.defer(expr)) <*- '}'
    (monad ~ (sp.with1 *> body)).map { case (m, (bs, result)) => AST.Context(m, bs, result) }.indexed
  }

  lazy val foldP: Parser[IndexedRec[Expr]] = {
    val scrutinee = "fold" -+> expr
    val resultType = sp.with1 *> ("as" -+> typeP)
    val cases = sp.with1 *> ("with" -+> matchCaseP.repSep(gap))
    (scrutinee ~ resultType ~ cases).map { case ((scrutinee, resultType), cases) =>
      AST.Fold(scrutinee, resultType, cases.toList)
    }.indexed
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
    (identifier *> identifier.parens.void.rep0 *> spaced("=>")).void

  private val bitOrOp: Parser[BinOps] =
    (Parser.not(caseStartLookahead.backtrack).with1 *> '|' <* Parser.not(Parser.char('|'))).backtrack.as(BinOps.Or)

  private val xorOp: Parser[BinOps] = '^'.as(BinOps.Xor)

  private val exprKeywords: Set[String] = Set(
    "in", "then", "else", "with", "as", "where", "context",
    "let", "rec", "if", "match", "fold", "data", "type",
    "import", "trait", "impl", "def", "true", "false",
    "foreign", "intrinsic"
  )

  private val infixIdentOp: Parser[(IndexedRec[Expr], IndexedRec[Expr]) => IndexedRec[Expr]] =
    (Parser.not(caseStartLookahead.backtrack).with1 *> (Parser.index.with1 ~ identifier.filter(n => !exprKeywords(n)))).map { case (offset, n) => (l, r) =>
      at(l.extract, AST.App(at(l.extract, AST.App(at(offset, AST.Var(Variable(n))), l)), r))
    }

  lazy val logicalOr: Parser[IndexedRec[Expr]]      = binaryLevel("||".as(BinOps.ShortOr), logicalAnd)
  lazy val logicalAnd: Parser[IndexedRec[Expr]]     = binaryLevel("&&".as(BinOps.ShortAnd), bitwiseOr)
  lazy val bitwiseOr: Parser[IndexedRec[Expr]]      = binaryLevel(bitOrOp, bitwiseXor)
  lazy val bitwiseXor: Parser[IndexedRec[Expr]]     = binaryLevel(xorOp, bitwiseAnd)
  lazy val bitwiseAnd: Parser[IndexedRec[Expr]]     = binaryLevel(bitAndOp, equitive)
  lazy val equitive: Parser[IndexedRec[Expr]]       = binaryLevel(eqOp, additive)
  lazy val additive: Parser[IndexedRec[Expr]]       = binaryLevel(addOp, multiplicative)
  lazy val multiplicative: Parser[IndexedRec[Expr]] = binaryLevel(mulOp, infixApp)
  lazy val infixApp: Parser[IndexedRec[Expr]]       = opLevel(infixIdentOp, unaryP, atomicStep = true)

  lazy val unaryP: Parser[IndexedRec[Expr]] = {
    val neg = ('-' -*> Parser.defer(unaryP)).map(b => AST.UnaryOp(UnaryOps.Neg, b)).indexed
    val not = ('!' -*> Parser.defer(unaryP)).map(b => AST.UnaryOp(UnaryOps.Not, b)).indexed
    neg | not | Parser.defer(appP)
  }

  lazy val appP: Parser[IndexedRec[Expr]] = {
    type Postfix = Either[IndexedRec[Expr], IndexedRec[Type]]
    val unitArg: Parser[Postfix] = (Parser.index.with1 <* unitParens).map(offset => Left(at(offset, AST.UnitLit()))).backtrack
    val exprArg: Parser[Postfix] = Parser.defer(expr).parens.map(Left.apply)
    val typeArg: Parser[Postfix] = Parser.defer(typeP).brackets.map(Right.apply)
    (atom ~ (unitArg | exprArg | typeArg).rep0).map { case (f, args) =>
      args.foldLeft(f) {
        case (acc, Left(a)) => at(acc.extract, AST.App(acc, a))
        case (acc, Right(t)) => at(acc.extract, AST.TyApp(acc, t))
      }
    }
  }

  lazy val atom: Parser[IndexedRec[Expr]] =
    Parser.defer(blockP | unitP.backtrack | numP | charP | stringP | interpStringP | boolP | foreignP | intrinsicP | varP | Parser.defer(expr).parens)

  lazy val blockP: Parser[IndexedRec[Expr]] = {
    val discarded = (Parser.defer(expr) <* spaced(';')).backtrack.rep0
    val result = Parser.defer(expr).?
    ('{' -*> discarded ~ result <*- '}').map { case (d, r) => AST.Block(d, r) }.indexed
  }

  val foreignP: Parser[IndexedRec[Expr]] = {
    ("foreign" *> typeP.brackets ~ (sp1.with1 *> identifier.map(Variable.apply))).map {
      case (t, v) => AST.Foreign(v, t)
    }.indexed
  }

  val intrinsicP: Parser[IndexedRec[Expr]] = {
    val name = identifier.brackets
    val op = name.map(n => StandardLibrary.intrinsicOp(n).getOrElse(sys.error(s"Unknown intrinsic $n")))
    val arg = Parser.defer(expr).parens
    ("intrinsic" *> op ~ arg.rep).map { case (op, args) =>
      AST.Intrinsic(op, args.toList)
    }.indexed
  }

  val varP: Parser[IndexedRec[Expr]] = identifier.map(n => AST.Var[IndexedRec, Expr](Variable(n))).indexed

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

  val numP: Parser[IndexedRec[Expr]] = {
    val digits = Numbers.digits
    val exponent = Parser.charIn("eE") ~ Parser.charIn("+-").? ~ digits
    val floatValue = (digits ~ '.' ~ digits ~ exponent.?).string
    val floatLit = (floatValue ~ floatSuffixP.?).map { case (value, suffix) => AST.Num[IndexedRec, Expr](value, suffix.getOrElse("f64")) }
    val intLit = (digits ~ intSuffixP.?).map { case (value, suffix) => AST.Num[IndexedRec, Expr](value, suffix.getOrElse("i32")) }
    (floatLit.backtrack | intLit).indexed
  }

  val charP: Parser[IndexedRec[Expr]] = ('\'' *> alpha.map(AST.Char[IndexedRec, Expr]) <* '\'').indexed

  private val commonStringEscape = Parser.char('\\').as('\\') |
    Parser.char('n').as('\n') |
    Parser.char('r').as('\r') |
    Parser.char('t').as('\t') |
    Parser.char('0').as('\u0000')

  val stringP: Parser[IndexedRec[Expr]] = {
    val escaped = Parser.char('\\') *> (
      Parser.char('"').as('"') | commonStringEscape
    )
    val plain = Parser.charWhere(ch => ch != '"' && ch != '\\' && ch != '\n' && ch != '\r')
    ('"' *> (escaped | plain).rep0 <* '"').map(chars => AST.StringLit[IndexedRec, Expr](chars.mkString)).indexed
  }

  val interpStringP: Parser[IndexedRec[Expr]] = {
    val escaped = Parser.char('\\') *> (
      Parser.char('`').as('`') |
      Parser.char('{').as('{') |
      Parser.char('}').as('}') | commonStringEscape
    )
    val plain = Parser.charWhere(ch => ch != '`' && ch != '{' && ch != '\\' && ch != '\n' && ch != '\r')
    val literalPart: Parser[Either[(Int, String), IndexedRec[Expr]]] =
      (Parser.index.with1 ~ (escaped | plain).rep).map { case (offset, chars) => Left((offset, chars.toList.mkString)) }
    val exprPart: Parser[Either[(Int, String), IndexedRec[Expr]]] =
      ('{' -*> Parser.defer(expr) <*- '}').map(Right.apply)
    (Parser.index.with1 ~ ('`' *> (literalPart | exprPart).rep0 <* '`')).map { case (offset, parts) =>
      if (parts.forall(_.isLeft)) at(offset, AST.StringLit(parts.collect { case Left((_, s)) => s }.mkString))
      else at(offset, AST.StrInterp(parts.map {
        case Left((o, s)) => at(o, AST.StringLit(s))
        case Right(e) => e
      }))
    }
  }

  val boolP: Parser[IndexedRec[Expr]] =
    ("true".as(AST.Bool[IndexedRec, Expr](true)) | "false".as(AST.Bool[IndexedRec, Expr](false))).indexed

  val unitP: Parser[IndexedRec[Expr]] = unitParens.as(AST.UnitLit[IndexedRec, Expr]()).indexed

  val programParser: Parser0[IndexedRec[AST.Program.type]] = {
    val sep = sp1.with1 *> Parser.charIn("\n;").rep.void <* sp
    sp *> topDeclP.repSep(sep).map(decls => programI(decls.toList)) <* sp
  }
}