package com.yuuki14202028

import cats.parse.{Numbers, Parser, Parser0}
import cats.parse.Rfc5234.{alpha, digit}
import scala.language.implicitConversions

object ParserAST {

  given Conversion[String, Parser[Unit]] = Parser.string
  given Conversion[Char, Parser[Unit]] = Parser.char

  // コメント: 行末までの `// …` と、入れ子にできる `/* … */`。空白と同じ扱い
  private val lineComment: Parser[Unit] = Parser.string("//") *> Parser.charWhere(ch => ch != '\n' && ch != '\r').rep0.void
  private val blockComment: Parser[Unit] = Parser.recursive[Unit] { nested =>
    val inside = nested | (Parser.not(Parser.string("*/")).with1 *> Parser.anyChar).void
    Parser.string("/*") *> inside.rep0 *> Parser.string("*/")
  }
  private val comment: Parser[Unit] = lineComment | blockComment

  private val sp: Parser0[Unit] = (Parser.charIn(" \t\n\r").void | comment).rep0.void
  // 行の中の空白（宣言の区切りの改行は含まない。行末のコメントは含む）
  private val sp1: Parser0[Unit] = (Parser.charIn(" \t").void | comment).rep0.void
  private val gap: Parser[Unit] = (Parser.charIn(" \t\n\r").void | comment).rep.void

  private def spaced(sep: Parser[Unit]): Parser[Unit] = sp.with1 *> sep <* sp

  extension [A](p: Parser[A]) {
    private infix def -*>[B](right: Parser0[B]): Parser[B] = p *> sp *> right
    // キーワードの後には空白が要る。空白がなければ後戻りする（letter や database を let / data と読まない）
    private infix def -+>[B](right: Parser0[B]): Parser[B] = (p *> gap).backtrack *> right
    private infix def <*-[B](close: Parser0[B]): Parser[A] = p <* sp <* close
    private def parens: Parser[A] = '(' -*> p <*- ')'
    private def brackets: Parser[A] = '[' -*> p <*- ']'
  }

  private def at[I](span: Span, node: AST[IndexedRec, I]): IndexedRec[I] = HCofree(span, node)

  // パーサが読んだ範囲
  extension [A](p: Parser[A]) {
    private def spanned: Parser[(Span, A)] =
      (Parser.index.with1 ~ p ~ Parser.index).map { case ((start, value), end) => (Span(start, end), value) }
  }

  extension [I](p: Parser[AST[IndexedRec, I]]) {
    private def indexed: Parser[IndexedRec[I]] = p.spanned.map { case (span, node) => at(span, node) }
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

  private def arrowChain[A](atom: Parser[A], whole: => Parser[A])(make: (A, A) => A): Parser[A] = {
    val arrowTail = sp.with1.soft *> '→' -*> Parser.defer(whole)
    (atom ~ arrowTail.?).map {
      case (from, Some(to)) => make(from, to)
      case (a, None) => a
    }
  }

  // 束縛子の頭部: ∀a. / ∀(f: * → *). （sigil は ∀・λ・Λ）
  private def binderHead(sigil: Char): Parser[(TypeVariable, Kind)] = {
    val plain = identifier.map(n => (TypeVariable(n), Kind.Star))
    val annotated = (identifier.map(TypeVariable.apply) ~ (spaced(':') *> Parser.defer(kindP))).parens
    (Parser.char(sigil) -*> (annotated.backtrack | plain)) <* spaced('.')
  }

  private def binderP[I](sigil: Char, body: => Parser[IndexedRec[I]])(make: (TypeVariable, Kind, IndexedRec[I]) => AST[IndexedRec, I]): Parser[IndexedRec[I]] =
    (binderHead(sigil) ~ Parser.defer(body)).map { case ((v, k), b) => make(v, k, b) }.indexed

  private val identStart: Parser[Char] = alpha | Parser.char('_').as('_')
  private val identChar: Parser[Char] = alpha | digit | '_'.as('_')
  private val identifier: Parser[String] = (identStart ~ identChar.rep0).map(_ :: _).map(_.mkString)
  // module のメンバーは M.x と修飾して書ける（型・trait・値・コンストラクタに共通）
  private val qualifiedIdentifier: Parser[String] =
    (identifier ~ ('.' *> identifier).backtrack.rep0).map(_ :: _).map(_.mkString("."))
  private val typeIdentifier: Parser[String] = qualifiedIdentifier
  private val importPath: Parser[String] = {
    '"' *> Parser.charWhere(ch => ch != '"' && ch != '\n' && ch != '\r').rep0.map(_.mkString) <* '"'
  }

  lazy val expr: Parser[IndexedRec[Expr]] =
    Parser.defer(recExprP | usingP | instanceP | dataLetP | foldP | matchP | typeLetP | tyAbsP | absP | contextP.backtrack | letExprP | ifP | operatorsP)


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
    params.map(_._2).foldRight(returnType)((from, to) => at(from.extract.to(to.extract), AST.Arrow(from, to)))

  private def functionValue(params: Seq[(String, IndexedRec[Type])], value: IndexedRec[Expr]): IndexedRec[Expr] =
    params.foldRight(value) { case ((name, types), body) => at(types.extract.to(body.extract), AST.Abs(Variable(name), types, body)) }

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
    val unitParam = unitParens.spanned.map { case (span, _) => ("_", at(span, AST.Primitive("unit"))) }
    val param = unitParam.backtrack | namedParam.parens
    param.rep0
  }

  // 束縛に書けるパターン: 引数のある構成子か _（引数のない名前は、これまでどおり変数の束縛）
  private def destructuring(pattern: Pattern[IndexedRec]): Boolean = pattern match {
    case Pattern.Located(_, inner) => destructuring(inner)
    case Pattern.Constructor(_, args, _) => args.nonEmpty
    case Pattern.Wildcard() => true
    case _ => false
  }

  private lazy val bindingPatternP: Parser[Pattern[IndexedRec]] = Parser.defer(patternP).filter(destructuring)

  // let パターン: T = e
  private lazy val patternBinding: Parser[((Pattern[IndexedRec], IndexedRec[Type]), IndexedRec[Expr])] =
    (("let" -+> bindingPatternP) ~ typeAnnP).backtrack ~ valueP

  private lazy val letExprP: Parser[IndexedRec[Expr]] = {
    val destructured = (patternBinding ~ inBody).map { case (((pattern, types), value), body) => AST.LetPattern(pattern, types, value, body) }
    val plain = Parser.defer(binding ~ inBody).map { case ((v, t, value), body) => AST.Let(v, t, value, body) }
    (destructured | plain).indexed
  }

  // rec の後には、宣言 1 つか { } で囲んだ宣言の組を書く
  private val recKeyword: Parser[Unit] = ("rec" <* Parser.not(identChar)).backtrack <* sp

  private def recGroup[A](member: Parser[A]): Parser[List[A]] = {
    val sep = (sp1.with1.soft *> Parser.charIn("\n;").rep.void) <* sp
    ('{' -*> member.repSep(sep) <*- '}').map(_.toList) | member.map(List(_))
  }

  private lazy val recBindings: Parser[List[RecBinding[IndexedRec]]] =
    recKeyword *> recGroup(binding).map(_.map { case (v, t, value) => RecBinding[IndexedRec](v, t, value) })

  // 式の中の rec: rec let … in e / rec { let …  let … } in e / rec data … in e / rec { data …  data … } in e
  private lazy val recExprP: Parser[IndexedRec[Expr]] = {
    val lets = (recBindings ~ inBody).map { case (bindings, body) => AST.LetRec(bindings, body) }
    val datas = ((recKeyword *> recGroup(dataBinding)) ~ inBody).map { case (datas, body) =>
      AST.DataLetRec(datas.map { case (v, params, constructors) => DataBinding[IndexedRec](v, params, constructors) }, body)
    }
    (datas.backtrack | lets).indexed
  }

  private lazy val simpleConstraintP: Parser[Constraint[IndexedRec]] =
    (qualifiedIdentifier.map(TypeVariable.apply) ~ typeP.brackets.rep0).map { case (name, args) => Constraint[IndexedRec](name, args) }

  // C[τ̄]、または量化された制約 ∀a. P₁, …, Pₖ ⇒ C[τ̄]（前提は省略できる。量化された前提は括弧で囲む）
  private lazy val constraintP: Parser[Constraint[IndexedRec]] = {
    val premise = simpleConstraintP | Parser.defer(constraintP).parens
    val premises = (premise.repSep(spaced(',').backtrack) <* spaced('⇒')).backtrack.?
    val quantified = (binderHead('∀').rep ~ premises ~ simpleConstraintP).map { case ((params, premises), head) =>
      Constraint[IndexedRec](head.name, head.arg, params.toList, premises.fold(List.empty)(_.toList))
    }
    (quantified | simpleConstraintP).spanned.map { case (span, c) => c.copy(span = Some(span)) }
  }

  private lazy val withClauseP: Parser0[Seq[Constraint[IndexedRec]]] =
    (spaced("with") *> constraintP.repSep(spaced(',').backtrack)).backtrack.?.map(_.fold(List.empty)(_.toList))

  // with C₁, …, Cₖ は、型では [C₁] → … → [Cₖ] → τ、値では λ[C₁]. … λ[Cₖ]. e（型パラメータの直下に置く）
  private def constrainedType(constraints: Seq[Constraint[IndexedRec]], bodyType: IndexedRec[Type]): IndexedRec[Type] =
    constraints.foldRight(bodyType)((c, to) => at(c.span.getOrElse(to.extract), AST.InstArrow(c, to)))

  private def constrainedValue(constraints: Seq[Constraint[IndexedRec]], value: IndexedRec[Expr]): IndexedRec[Expr] =
    constraints.foldRight(value)((c, body) => at(body.extract, AST.InstAbs(c, body)))

  private lazy val topLetP: Parser[IndexedRec[Decl]] = binding.map {
    case (v, t, value) => AST.TopLet(v, t, value)
  }.indexed

  private val letHead: Parser[String] = "let" -+> identifier

  private lazy val inBody: Parser0[IndexedRec[Expr]] = spaced("in") *> Parser.defer(expr)

  private lazy val typeAnnP: Parser0[IndexedRec[Type]] = spaced(':') *> typeP
  private lazy val valueP: Parser0[IndexedRec[Expr]] = spaced('=') *> expr

  private lazy val binding: Parser[(Variable, IndexedRec[Type], IndexedRec[Expr])] = {
    val typeParams = sp *> typeParamP.rep0
    val params = sp *> functionParamsP
    (letHead ~ typeParams ~ params ~ typeAnnP ~ withClauseP ~ valueP).map {
      case (((((name, typeParams), params), returnType), constraints), value) => (
        Variable(name),
        polymorphicType(typeParams, constrainedType(constraints, functionType(params, returnType))),
        polymorphicValue(typeParams, constrainedValue(constraints, functionValue(params, value)))
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
    // フィールドには名前を付けられる: MkStudent(name: String)(math: i32)
    val field = (((identifier <* spaced(':')).backtrack.map(Option(_)) ~ typeP) | typeP.map(t => (Option.empty[String], t))).parens
    (identifier.map(Variable.apply) ~ field.rep0).map { case (name, fields) =>
      DataConstructor[IndexedRec](name, fields.map(_._2), if (fields.exists(_._1.nonEmpty)) fields.map(_._1) else Nil)
    }
  }

  private lazy val dataBinding: Parser[(TypeVariable, Seq[(TypeVariable, Kind)], Seq[DataConstructor[IndexedRec]])] = {
    val constructors = spaced('=') *> '{' -*> dataConstructorP.repSep(spaced(',').backtrack) <*- '}'
    (("data" -+> identifier) ~ typeParamP.rep0 ~ constructors).map { case ((name, params), constructors) =>
      (TypeVariable(name), params, constructors.toList)
    }
  }

  lazy val dataLetP: Parser[IndexedRec[Expr]] = (dataBinding ~ inBody).map { case ((v, params, constructors), body) =>
    AST.DataLet(v, params, constructors, body)
  }.indexed

  private lazy val topDataP: Parser[IndexedRec[Decl]] = dataBinding.map { case (v, params, constructors) =>
    AST.TopData(v, params, constructors)
  }.indexed

  lazy val traitP: Parser[IndexedRec[Decl]] = {
    val head = "trait" -+> identifier.map(TypeVariable.apply)
    val typeParams = sp *> typeParamP.rep0
    val params = sp *> functionParamsP
    val supers = withClauseP
    // `= 本体` を書いたメソッドはデフォルトを持つ
    val default = ((sp.with1.soft *> Parser.char('=')) *> sp *> Parser.defer(expr)).?
    val method = ((("def" -+> identifier.map(Variable.apply)) ~ typeParams ~ params ~ typeAnnP ~ withClauseP ~ default).map {
      case (((((name, typeParams), params), returnType), constraints), default) =>
        val sig = polymorphicType(typeParams, constrainedType(constraints, functionType(params, returnType)))
        MethodSig(name, sig, default.map(value => polymorphicValue(typeParams, constrainedValue(constraints, functionValue(params, value)))))
    } <* sp).rep
    val body = sp.with1 *> ('{' -*> method <*- '}')
    (head ~ typeParams ~ supers ~ body).map {
      case (((f, t), s), m) => AST.TopTrait(f, t, s, m.toList)
    }.indexed
  }

  // impl の本体: { def m = e … }
  private lazy val implMethodsP: Parser[List[MethodImpl[IndexedRec]]] = {
    val typeParams = sp *> typeParamP.rep0
    val params = sp *> functionParamsP
    val returnAnn = typeAnnP.backtrack.?
    val method = ((("def" -+> identifier.map(Variable.apply)) ~ typeParams ~ params ~ returnAnn ~ withClauseP ~ valueP).map {
      case (((((name, typeParams), params), returnType), constraints), value) =>
        val body = polymorphicValue(typeParams, constrainedValue(constraints, functionValue(params, value)))
        val sig = returnType.map(rt => polymorphicType(typeParams, constrainedType(constraints, functionType(params, rt))))
        MethodImpl(name, sig, body)
    } <* sp).rep
    ('{' -*> method <*- '}').map(_.toList)
  }

  lazy val implP: Parser[IndexedRec[Decl]] = {
    val implParams = (sp.with1 *> typeParamP.rep.map(_.toList)).backtrack | gap.as(List.empty[(TypeVariable, Kind)])
    val head = ("impl" *> implParams) ~ (sp.with1 *> qualifiedIdentifier.map(TypeVariable.apply))
    val targets = sp *> typeP.brackets.rep.map(_.toList)
    (head ~ targets ~ withClauseP ~ (sp.with1 *> implMethodsP)).map {
      case ((((implParams, name), targets), context), methods) => AST.TopImpl(name, implParams, targets, context, methods, Seq.empty)
    }.indexed
  }

  // 式としての impl: 辞書の値を作る（型パラメータと文脈は書かない。外側のものが使える）
  lazy val instanceP: Parser[IndexedRec[Expr]] =
    (("impl" -+> qualifiedIdentifier.map(TypeVariable.apply)) ~ (sp *> Parser.defer(typeP).brackets.rep.map(_.toList)) ~ (sp.with1 *> Parser.defer(implMethodsP))).map {
      case ((name, targets), methods) => AST.Instance(name, targets, methods, Seq.empty)
    }.indexed

  private lazy val topDeriveP: Parser[IndexedRec[Decl]] =
    ("derive" -+> qualifiedIdentifier.map(TypeVariable.apply) ~ qualifiedIdentifier.map(TypeVariable.apply).brackets).map {
      case (traitName, target) => AST.TopDerive[IndexedRec, Decl](traitName, target)
    }.indexed

  // 宣言の先頭のキーワード。ここを過ぎたら後戻りしないので、中の構文エラーはその位置で報告される
  private def keyword(word: String): Parser[Unit] = (Parser.string(word) *> gap).backtrack

  private lazy val declsP: Parser0[List[IndexedRec[Decl]]] = {
    val sep = (sp1.with1.soft *> Parser.charIn("\n;").rep.void) <* sp
    topDeclP.repSep0(sep)
  }

  private lazy val moduleP: Parser[IndexedRec[Decl]] =
    ((keyword("module") *> identifier) ~ (sp.with1 *> ('{' -*> declsP <*- '}'))).map {
      case (name, decls) => AST.TopModule[IndexedRec, Decl](name, decls)
    }.indexed

  // infixl 6 + = add / infixr 5 ++ = append / infix 4 == = eq / prefix - = neg
  private lazy val operatorP: Parser[IndexedRec[Decl]] = {
    val level = Parser.charIn('0' to '9').map(_.asDigit) <* gap
    val assoc = keyword("infixl").as(Assoc.Left) | keyword("infixr").as(Assoc.Right) | keyword("infix").as(Assoc.Non)
    val binary = ((assoc ~ level).map { case (a, l) => Option(Fixity(a, l)) } ~ operatorSymbol)
    val prefix = keyword("prefix") *> operatorSymbol.map(symbol => (Option.empty[Fixity], symbol))
    ((binary | prefix) ~ (spaced('=') *> qualifiedIdentifier)).map { case ((fixity, symbol), function) =>
      AST.TopOperator[IndexedRec, Decl](symbol, fixity, Variable(function))
    }.indexed
  }

  // interpolate = show / literal [] = Nil, Cons
  private lazy val syntaxP: Parser[IndexedRec[Decl]] = {
    val interpolate = (keyword("interpolate") *> spaced('=') *> qualifiedIdentifier).map(f => AST.TopSyntax[IndexedRec, Decl](SyntaxHook.Interpolate, Seq(Variable(f))))
    val literal = ((keyword("literal") *> Parser.char('[') *> sp *> Parser.char(']')).backtrack *> spaced('=') *>
      ((qualifiedIdentifier <* spaced(',')) ~ qualifiedIdentifier)).map { case (nil, cons) =>
        AST.TopSyntax[IndexedRec, Decl](SyntaxHook.ListLiteral, Seq(Variable(nil), Variable(cons)))
      }
    (interpolate.backtrack | literal).indexed
  }

  private lazy val openP: Parser[IndexedRec[Decl]] =
    (keyword("open") *> identifier.repSep('.')).map(path => AST.TopOpen[IndexedRec, Decl](path.toList)).indexed

  // rec let / rec data / rec impl と、同じ種類の宣言を囲んだ rec { … }
  private lazy val topRecP: Parser[IndexedRec[Decl]] =
    (recKeyword *> recGroup(topDataP.backtrack | implP.backtrack | topDeriveP.backtrack | topLetP)).map(decls => AST.TopRec[IndexedRec, Decl](decls)).indexed

  private lazy val recDataP: Parser[IndexedRec[Decl]] =
    (recKeyword *> recGroup(topDataP)).map(decls => AST.TopRec[IndexedRec, Decl](decls)).indexed

  private lazy val opaqueP: Parser[IndexedRec[Decl]] =
    (keyword("opaque") *> (recDataP | topDataP.backtrack | topTypeP)).map(decl => AST.TopModified[IndexedRec, Decl](Modifier.Opaque, decl)).indexed

  private lazy val privateP: Parser[IndexedRec[Decl]] = {
    val decl = Parser.defer(moduleP | operatorP | syntaxP.backtrack | opaqueP | topRecP | topDataP.backtrack | topTypeP.backtrack | traitP.backtrack | topLetP)
    (keyword("private") *> decl).map(decl => AST.TopModified[IndexedRec, Decl](Modifier.Private, decl)).indexed
  }

  private lazy val topDeclP: Parser[IndexedRec[Decl]] =
    Parser.defer(moduleP | openP | operatorP | syntaxP.backtrack | privateP | opaqueP | topRecP |
      topImportP.backtrack | topDataP.backtrack | topTypeP.backtrack | traitP.backtrack | implP.backtrack | topDeriveP.backtrack | topLetP)

  lazy val typeP: Parser[IndexedRec[Type]] = Parser.defer(forAllP | typeLambdaP | arrowTypeP)

  lazy val typeLambdaP: Parser[IndexedRec[Type]] = binderP('λ', typeP)(AST.TypeAbs.apply)

  lazy val forAllP: Parser[IndexedRec[Type]] = binderP('∀', typeP)(AST.ForAll.apply)

  lazy val arrowTypeP: Parser[IndexedRec[Type]] =
    arrowChain(typeAppP, typeP)((from, to) => at(from.extract.to(to.extract), AST.Arrow(from, to)))

  private lazy val typeAppP: Parser[IndexedRec[Type]] = {
    val typeArg = Parser.defer(typeP).brackets.spanned
    (typeAtomP ~ typeArg.rep0).map { case (init, args) =>
      args.foldLeft(init) { case (acc, (span, arg)) => at(acc.extract.to(span), AST.TypeApp(acc, arg)) }
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
    val name = qualifiedIdentifier.map(Variable.apply)
    val binder = identifier.map(Variable.apply).parens
    val body = spaced("=>") *> Parser.defer(expr)
    (name ~ binder.rep0 ~ body).map { case ((name, binders), body) =>
      MatchCase(name, binders, body)
    }
  }

  // パターン: _ / 名前 / C(p)(q) / リテラル / p | q
  private lazy val patternP: Parser[Pattern[IndexedRec]] =
    simplePatternP.repSep(spaced('|').backtrack).map { alternatives =>
      if (alternatives.tail.isEmpty) alternatives.head else Pattern.Or[IndexedRec](alternatives.toList)
    }

  private lazy val simplePatternP: Parser[Pattern[IndexedRec]] = {
    val number: Parser[Pattern[IndexedRec]] = (Parser.char('-').?.with1 ~ numP.map(_.project)).collect {
      case (sign, AST.Num(value, typeName)) => Pattern.Literal[IndexedRec](LiteralValue.Num(sign.fold(value)(_ => s"-$value"), typeName), None)
    }
    val string: Parser[Pattern[IndexedRec]] = stringP.map(_.project).collect {
      case AST.StringLit(value) => Pattern.Literal[IndexedRec](LiteralValue.Str(value), None)
    }
    val char: Parser[Pattern[IndexedRec]] = Parser.defer(charLiteral).map(c => Pattern.Literal[IndexedRec](LiteralValue.Char(c), None))
    val bool: Parser[Pattern[IndexedRec]] =
      (("true".as(true) | "false".as(false)) <* Parser.not(identChar)).backtrack.map(b => Pattern.Literal[IndexedRec](LiteralValue.Bool(b), None))
    val named: Parser[Pattern[IndexedRec]] = (qualifiedIdentifier ~ Parser.defer(patternP).parens.rep0).map {
      case ("_", Nil) => Pattern.Wildcard[IndexedRec]()
      case (name, args) => Pattern.Constructor[IndexedRec](Variable(name), args, Nil)
    }
    // 部分パターンごとに位置を記録する
    (number | string | char | bool | named).spanned.map { case (span, pattern) => Pattern.Located[IndexedRec](span, pattern) }
  }

  // `if 条件`: 分岐の条件
  private lazy val guardP: Parser0[Option[IndexedRec[Expr]]] =
    (sp.with1.soft *> (Parser.string("if") *> gap).backtrack *> Parser.defer(expr)).?

  lazy val matchP: Parser[IndexedRec[Expr]] = {
    val scrutinee = "match" -+> expr
    val patternCase = (patternP.spanned ~ guardP ~ (spaced("=>") *> Parser.defer(expr))).map { case (((span, pattern), guard), body) =>
      PatternCase[IndexedRec](pattern, guard, body, Some(span))
    }
    val cases = sp.with1 *> ('{' -*> patternCase.repSep(gap.backtrack) <*- '}')
    (scrutinee ~ cases).map { case (scrutinee, cases) =>
      AST.PatternMatch(scrutinee, cases.toList)
    }.indexed
  }

  lazy val contextP: Parser[IndexedRec[Expr]] = {
    val monad = "context" -*> Parser.defer(typeP).brackets
    val monadicBinding: Parser[ContextStep[IndexedRec]] = (identifier.map(Variable.apply) ~ typeAnnP ~ valueP <* spaced(';')).backtrack.map {
      case ((name, types), value) => ContextStep.Bind[IndexedRec](name, types, value)
    }
    val pureLet: Parser[ContextStep[IndexedRec]] = (binding <* spaced(';')).backtrack.map { case (name, types, value) =>
      ContextStep.Let[IndexedRec](name, types, value)
    }
    val recLets: Parser[ContextStep[IndexedRec]] = (recBindings <* spaced(';')).backtrack.map(ContextStep.Rec[IndexedRec](_))
    val patternLet: Parser[ContextStep[IndexedRec]] = (patternBinding <* spaced(';')).backtrack.map { case ((pattern, types), value) =>
      ContextStep.LetPattern[IndexedRec](pattern, types, value)
    }
    val patternBind: Parser[ContextStep[IndexedRec]] = (bindingPatternP ~ typeAnnP ~ valueP <* spaced(';')).backtrack.map {
      case ((pattern, types), value) => ContextStep.BindPattern[IndexedRec](pattern, types, value)
    }
    val bindings = (recLets | patternLet | pureLet | patternBind | monadicBinding).rep0
    val body = '{' -*> (bindings.with1 ~ Parser.defer(expr)) <*- '}'
    (monad ~ (sp.with1 *> body)).map { case (m, (bs, result)) => AST.Context(m, bs, result, None) }.indexed
  }

  lazy val foldP: Parser[IndexedRec[Expr]] = {
    val scrutinee = "fold" -+> expr
    val resultType = sp.with1 *> ("as" -+> typeP)
    val cases = sp.with1 *> ('{' -*> matchCaseP.repSep(gap.backtrack) <*- '}')
    (scrutinee ~ resultType ~ cases).map { case ((scrutinee, resultType), cases) =>
      AST.Fold(scrutinee, resultType, cases.toList)
    }.indexed
  }

  // 演算子の記号: 記号文字の並びを最長一致で 1 つの演算子と読む（コメントの始まりと、構文に使う記号は除く）
  private val reservedSymbols: Set[String] = Set("=", "=>", "->", ":")
  private val symbolChars: String = "+-*/%<>=!&|^~?$@:"
  private val operatorSymbol: Parser[String] =
    (Parser.not(Parser.string("//") | Parser.string("/*")).with1 *> Parser.charIn(symbolChars)).rep.string
      .filter(symbol => !reservedSymbols(symbol)).backtrack

  private lazy val caseStartLookahead: Parser[Unit] =
    Parser.defer(patternP *> guardP *> spaced("=>")).void

  private val exprKeywords: Set[String] = Set(
    "in", "then", "else", "with", "as", "context",
    "let", "rec", "if", "match", "fold", "data", "type",
    "import", "trait", "impl", "def", "derive", "true", "false",
    "foreign", "intrinsic", "module", "open", "private", "opaque", "summon", "using", "infixl", "infixr", "infix", "interpolate", "literal"
  )

  private val infixIdentOp: Parser[(IndexedRec[Expr], IndexedRec[Expr]) => IndexedRec[Expr]] =
    (Parser.not(caseStartLookahead.backtrack).with1 *> identifier.filter(n => !exprKeywords(n)).spanned).map { case (span, n) => (l, r) =>
      at(l.extract.to(r.extract), AST.App(at(l.extract.to(span), AST.App(at(span, AST.Var(Variable(n))), l)), r))
    }

  // a ⊕ b ⊗ c: 優先順位は宣言で決まるので、ここでは並びのまま読む（型検査が木に組む）。
  // 次の分岐の先頭の負のリテラル（-1 => …）は演算子と読まない
  lazy val operatorsP: Parser[IndexedRec[Expr]] = {
    val operator = (Parser.not(caseStartLookahead.backtrack).with1 *> operatorSymbol.spanned).backtrack
    val step = (operator ~ (sp.with1 *> Parser.defer(infixApp))).map { case ((span, symbol), operand) => OpStep[IndexedRec](symbol, span, operand) }
    (Parser.defer(infixApp) ~ (sp.with1.soft *> step).rep0).map {
      case (first, Nil) => first
      case (first, rest) => at(first.extract.to(rest.last.operand.extract), AST.OpSeq(first, rest))
    }
  }

  lazy val infixApp: Parser[IndexedRec[Expr]] = opLevel(infixIdentOp, unaryP, atomicStep = true)

  // 前置の演算子は、二項のどの演算子よりも強く結び付く
  lazy val unaryP: Parser[IndexedRec[Expr]] = {
    val prefixed = (operatorSymbol ~ (sp.with1 *> Parser.defer(unaryP))).map { case (symbol, body) => AST.UnaryOp(symbol, body, None) }.indexed
    prefixed | Parser.defer(appP)
  }

  lazy val appP: Parser[IndexedRec[Expr]] = {
    enum Postfix {
      case Value(argument: IndexedRec[Expr])
      case Types(argument: IndexedRec[Type])
      case Dictionary(argument: IndexedRec[Expr]) // f(with d): 制約を解決せず、辞書を直接渡す
      case Member(name: String, typeArgs: Seq[IndexedRec[Type]]) // e.f[T]: e の型の module にある関数 f
      case Named(arguments: Seq[NamedArg[IndexedRec]])           // C(x = e, …): 名前付きの構築
    }
    // 式の続きの .f は、行を変えて書ける
    val member: Parser[Postfix] =
      (((sp.with1 *> Parser.char('.')).backtrack *> identifier) ~ Parser.defer(typeP).brackets.rep0).map { case (name, typeArgs) => Postfix.Member(name, typeArgs) }
    val unitArg: Parser[Postfix] = unitParens.spanned.map { case (span, _) => Postfix.Value(at(span, AST.UnitLit())) }.backtrack
    val dictArg: Parser[Postfix] =
      ((Parser.char('(') *> sp *> Parser.string("with") *> gap).backtrack *> Parser.defer(expr) <*- ')').map(Postfix.Dictionary.apply)
    val exprArg: Parser[Postfix] = Parser.defer(expr).parens.map(Postfix.Value.apply)
    val typeArg: Parser[Postfix] = Parser.defer(typeP).brackets.map(Postfix.Types.apply)
    // name = の後の = は、== などの演算子の一部ではない
    val namedHead: Parser[(Span, String)] = identifier.spanned <* sp <* Parser.char('=') <* Parser.not(Parser.charIn(symbolChars))
    def namedValue(head: Parser[(Span, String)]): Parser[NamedArg[IndexedRec]] =
      (head ~ (sp *> Parser.defer(expr))).map { case ((span, name), value) => NamedArg(name, span, value) }
    val namedArgs: Parser[Postfix] =
      (namedValue((Parser.char('(') *> sp *> namedHead).backtrack) ~ namedValue((spaced(',').backtrack *> namedHead)).rep0 <*- ')')
        .map { case (first, rest) => Postfix.Named(first +: rest) }
    (atom ~ (unitArg | dictArg | namedArgs | exprArg | typeArg | member).spanned.rep0).map { case (f, args) =>
      args.foldLeft(f) {
        case (acc, (span, Postfix.Value(a))) => at(acc.extract.to(span), AST.App(acc, a))
        case (acc, (span, Postfix.Types(t))) => at(acc.extract.to(span), AST.TyApp(acc, t))
        // .f[T] や a.b[T] の直後の (with d) は、その関数の制約を満たす辞書（x.f(with d)）
        case (acc, (span, Postfix.Dictionary(d))) => acc.project match {
          case AST.Select(receiver, name, typeArgs, dictionaries, None) => at(acc.extract.to(span), AST.Select(receiver, name, typeArgs, dictionaries :+ d, None))
          case AST.Path(name, typeArgs, dictionaries) => at(acc.extract.to(span), AST.Path(name, typeArgs, dictionaries :+ d))
          case _ => at(acc.extract.to(span), AST.InstApp(acc, Evidence.Explicit(d)))
        }
        case (acc, (span, Postfix.Member(name, typeArgs))) => at(acc.extract.to(span), AST.Select(acc, name, typeArgs, Seq.empty, None))
        case (acc, (span, Postfix.Named(arguments))) => at(acc.extract.to(span), AST.NamedApp(acc, arguments, Seq.empty))
      }
    }
  }

  lazy val atom: Parser[IndexedRec[Expr]] =
    Parser.defer(blockP | listP | unitP.backtrack | numP | charP | stringP | interpStringP | boolP | foreignP | intrinsicP | summonP | pathP | varP | Parser.defer(expr).parens)

  // [x, y, z]（空の [] も読み、型検査がエラーにする）
  lazy val listP: Parser[IndexedRec[Expr]] =
    ('[' -*> Parser.defer(expr).repSep0(spaced(',').backtrack) <*- ']').map(elements => AST.ListLit[IndexedRec, Expr](elements, None, None)).indexed

  // using d in e: e の中では、辞書 d をインスタンスとして解決に使う
  lazy val usingP: Parser[IndexedRec[Expr]] =
    (("using" -+> Parser.defer(expr)) ~ inBody).map { case (dictionary, body) => AST.Using(dictionary, body) }.indexed

  // summon[C[τ̄]]: 解決されたインスタンスの辞書
  lazy val summonP: Parser[IndexedRec[Expr]] =
    ((Parser.string("summon") *> Parser.char('[')).backtrack *> sp *> constraintP <*- ']').map(c => AST.Summon[IndexedRec, Expr](c, None)).indexed

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

  // キーワードは変数として読まない（`1 +` の後の `let` を式の続きと読んで、離れた位置でエラーにしないため）
  lazy val varP: Parser[IndexedRec[Expr]] =
    identifier.filter(n => !exprKeywords(n)).backtrack.map(n => AST.Var[IndexedRec, Expr](Variable(n))).indexed

  // . を含む名前 a.b.c と、その直後の型引数。module の修飾か値へのドット記法かは、型検査が決める
  lazy val pathP: Parser[IndexedRec[Expr]] = {
    val dotted = (identifier.filter(n => !exprKeywords(n)) ~ ('.' *> identifier).backtrack.rep).backtrack.map { case (head, rest) => (head :: rest.toList).mkString(".") }
    (dotted ~ Parser.defer(typeP).brackets.rep0).map { case (name, typeArgs) => AST.Path[IndexedRec, Expr](Variable(name), typeArgs, Seq.empty) }.indexed
  }

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

  private val commonStringEscape = Parser.char('\\').as('\\') |
    Parser.char('n').as('\n') |
    Parser.char('r').as('\r') |
    Parser.char('t').as('\t') |
    Parser.char('0').as('\u0000')

  // 文字リテラル: 'a' / '0' / ' ' / '\n' / '\''
  private val charLiteral: Parser[Char] = {
    val escaped = Parser.char('\\') *> (Parser.char('\'').as('\'') | commonStringEscape)
    val plain = Parser.charWhere(ch => ch != '\'' && ch != '\\' && ch != '\n' && ch != '\r')
    '\'' *> (escaped | plain) <* '\''
  }

  lazy val charP: Parser[IndexedRec[Expr]] = charLiteral.map(AST.Char[IndexedRec, Expr]).indexed

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
    val literalPart: Parser[Either[String, IndexedRec[Expr]]] =
      (escaped | plain).rep.map(chars => Left(chars.toList.mkString))
    val exprPart: Parser[Either[String, IndexedRec[Expr]]] =
      ('{' -*> Parser.defer(expr) <*- '}').map(Right.apply)
    ('`' *> (literalPart | exprPart).rep0 <* '`').spanned.map { case (span, parts) =>
      if (parts.forall(_.isLeft)) at(span, AST.StringLit(parts.collect { case Left(s) => s }.mkString))
      else at(span, AST.StrInterp(parts.map {
        case Left(s) => InterpPart.Text[IndexedRec](s)
        case Right(e) => InterpPart.Embed[IndexedRec](e, None)
      }))
    }
  }

  val boolP: Parser[IndexedRec[Expr]] =
    ("true".as(AST.Bool[IndexedRec, Expr](true)) | "false".as(AST.Bool[IndexedRec, Expr](false))).indexed

  val unitP: Parser[IndexedRec[Expr]] = unitParens.as(AST.UnitLit[IndexedRec, Expr]()).indexed

  val programParser: Parser0[IndexedRec[AST.Program.type]] = {
    sp *> declsP.map(programI) <* sp
  }
}