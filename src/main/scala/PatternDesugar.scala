package com.yuuki14202028

/** match のパターンの脱糖。入れ子のパターン・リテラル・変数・_ を、
 * 構成子 1 段の分岐（AST.Match）と if / let へ展開する。
 *
 * 分岐は決定木にまとめる: 先頭の行で最初に検査が必要な位置を選び、その位置の構成子（またはリテラル）で
 * 行を振り分けることを繰り返す。型検査は同じ決定木で網羅性と到達不能な分岐を調べる（check）。
 */
object PatternDesugar {

  private def invariant(msg: String): Nothing = sys.error(s"Compiler invariant violation: $msg")

  // 検査の対象になる値（それを束縛した変数と型）
  private type Occurrence = (Variable, TypeRec[Type])

  private final case class Row(patterns: List[Pattern[TypeRec]], bindings: Seq[(Variable, Occurrence)], index: Int)

  private enum Tree {
    case Fail
    case Leaf(row: Int, bindings: Seq[(Variable, Occurrence)])
    case Switch(on: Occurrence, branches: Seq[(ConstructorShape[TypeRec], Seq[Occurrence], Tree)])
    // default が None なのは、リテラルだけですべての値を尽くしている場合（bool の true / false）
    case Test(on: Occurrence, literals: Seq[(LiteralValue, Option[TypeRec[Expr]], Tree)], default: Option[Tree])
  }

  private class Compiler {
    private var counter = 0

    def fresh(prefix: String): Variable = {
      counter += 1
      Variable(s"$$$prefix$counter")
    }

    private def irrefutable(pattern: Pattern[TypeRec]): Boolean = pattern match {
      case Pattern.Wildcard() | Pattern.Bind(_) => true
      case _ => false
    }

    private def bound(pattern: Pattern[TypeRec], on: Occurrence): Seq[(Variable, Occurrence)] = pattern match {
      case Pattern.Bind(name) => Seq(name -> on)
      case _ => Seq.empty
    }

    // 位置 column を、その位置の値を分解した部分パターン replacement に置き換える
    private def refine(row: Row, column: Int, on: Occurrence, replacement: List[Pattern[TypeRec]]): Row =
      row.copy(patterns = row.patterns.patch(column, replacement, 1), bindings = row.bindings ++ bound(row.patterns(column), on))

    def compile(occurrences: List[Occurrence], rows: List[Row]): Tree = rows match {
      case Nil => Tree.Fail
      case first :: _ => first.patterns.indexWhere(p => !irrefutable(p)) match {
        case -1 => Tree.Leaf(first.index, first.bindings ++ first.patterns.zip(occurrences).flatMap { case (p, on) => bound(p, on) })
        case column =>
          val on = occurrences(column)
          first.patterns(column) match {
            case Pattern.Constructor(_, _, siblings) =>
              Tree.Switch(on, siblings.map { shape =>
                val fields = shape.fields.map(fieldType => fresh("p") -> fieldType).toList
                val matching = rows.flatMap { row =>
                  row.patterns(column) match {
                    case Pattern.Constructor(name, args, _) => Option.when(name == shape.name)(refine(row, column, on, args.toList))
                    case Pattern.Literal(_, _) => invariant("a literal pattern cannot be matched against a data type")
                    case _ => Some(refine(row, column, on, fields.map(_ => Pattern.Wildcard[TypeRec]())))
                  }
                }
                (shape, fields, compile(occurrences.patch(column, fields, 1), matching))
              })
            case Pattern.Literal(_, _) =>
              val literals = rows.map(_.patterns(column)).collect { case Pattern.Literal(value, equals) => (value, equals) }.distinctBy(_._1)
              val rest = occurrences.patch(column, Nil, 1)
              def rowsFor(value: Option[LiteralValue]): List[Row] = rows.flatMap { row =>
                row.patterns(column) match {
                  case Pattern.Literal(v, _) => Option.when(value.contains(v))(refine(row, column, on, Nil))
                  case Pattern.Constructor(_, _, _) => invariant("a constructor pattern cannot be matched against a literal type")
                  case _ => Some(refine(row, column, on, Nil))
                }
              }
              val complete = Set(true, false).forall(b => literals.exists(_._1 == LiteralValue.Bool(b)))
              Tree.Test(
                on,
                literals.map { case (value, equals) => (value, equals, compile(rest, rowsFor(Some(value)))) },
                Option.when(!complete)(compile(rest, rowsFor(None)))
              )
            case _ => invariant("the selected pattern must be refutable")
          }
      }
    }
  }

  private def leaves(tree: Tree): Seq[Int] = tree match {
    case Tree.Fail => Seq.empty
    case Tree.Leaf(row, _) => Seq(row)
    case Tree.Switch(_, branches) => branches.flatMap { case (_, _, sub) => leaves(sub) }
    case Tree.Test(_, literals, default) => literals.flatMap { case (_, _, sub) => leaves(sub) } ++ default.toSeq.flatMap(leaves)
  }

  // どの分岐にも当たらない値の例（値を束縛した変数 → その値の形）。網羅されていれば None
  private def unmatched(tree: Tree): Option[Map[Variable, String]] = tree match {
    case Tree.Fail => Some(Map.empty)
    case Tree.Leaf(_, _) => None
    case Tree.Switch(on, branches) =>
      branches.iterator.flatMap { case (shape, fields, sub) =>
        unmatched(sub).map { shapes =>
          val args = fields.map { case (field, _) => s"(${shapes.getOrElse(field, "_")})" }.mkString
          shapes -- fields.map(_._1) + (on._1 -> s"${shape.name.name}$args")
        }
      }.nextOption()
    case Tree.Test(on, literals, default) =>
      // リテラルのどれでもない値。bool なら残りの値がわかる
      val other = literals.collectFirst { case (LiteralValue.Bool(b), _, _) => (!b).toString }.getOrElse("_")
      default.flatMap(unmatched).map(_ + (on._1 -> other))
        .orElse(literals.iterator.flatMap { case (value, _, sub) => unmatched(sub).map(_ + (on._1 -> value.shown)) }.nextOption())
  }

  private def treeOf(compiler: Compiler, scrutinee: Occurrence, patterns: Seq[Pattern[TypeRec]]): Tree =
    compiler.compile(List(scrutinee), patterns.zipWithIndex.map { case (p, index) => Row(List(p), Seq.empty, index) }.toList)

  // 型検査から呼ばれる: すべての値がどれかの分岐に当たり、どの分岐にも当たる値があること
  def check(scrutineeType: TypeRec[Type], patterns: Seq[Pattern[TypeRec]]): EitherS[Unit] = {
    val root = Variable("$scrutinee")
    val tree = treeOf(new Compiler, root -> scrutineeType, patterns)
    val reached = leaves(tree).toSet
    unmatched(tree) match {
      case Some(shapes) => Left(CompileError.NonExhaustivePatterns(shapes.getOrElse(root, "_")))
      case None => patterns.indices.find(!reached(_)).map(index => CompileError.UnreachableCase(index + 1)).toLeft(())
    }
  }

  def desugar(program: TypeRec[AST.Program.type]): TypeRec[AST.Program.type] = {
    val compiler = new Compiler
    val alg: Algebra[TypedAST, TypeRec] = [x] => he => (he.ann, he.ast) match {
      case (ExprAnn(resultType), AST.PatternMatch(scrutinee, cases)) => lower(compiler, resultType, scrutinee, cases)
      case (ann, node) => HCofree(ann, node)
    }
    program.cata(alg)
  }

  private def lower(compiler: Compiler, resultType: TypeRec[Type], scrutinee: TypeRec[Expr], cases: Seq[PatternCase[TypeRec]]): TypeRec[Expr] = {
    val root = compiler.fresh("m") -> typeOf(scrutinee)
    val tree = treeOf(compiler, root, cases.map(_.pattern))
    val uses = leaves(tree).groupBy(identity).view.mapValues(_.length).toMap

    // 複数の葉から使われる本体は、束縛される変数を引数に取る関数として一度だけ置く
    val shared: Map[Int, Variable] = cases.indices.filter(uses.getOrElse(_, 0) > 1).map(_ -> compiler.fresh("case")).toMap
    def binders(index: Int, bindings: Seq[(Variable, Occurrence)]): Seq[(Variable, Occurrence)] =
      patternVariables(cases(index).pattern).map(name => name -> bindings.collectFirst { case (n, on) if n == name => on }
        .getOrElse(invariant(s"pattern variable ${name.name} is not bound")))
    def ref(on: Occurrence): TypeRec[Expr] = varrType(on._1, on._2)
    def unitParam: Variable = Variable("$unit")
    def sharedType(index: Int, bound: Seq[(Variable, Occurrence)]): TypeRec[Type] =
      if (bound.isEmpty) arrowT(unitTypeT, resultType) else bound.map(_._2._2).foldRight(resultType)(arrowT)

    def generate(tree: Tree): TypeRec[Expr] = tree match {
      case Tree.Fail => invariant("a non-exhaustive match must be rejected by the type checker")
      case Tree.Leaf(index, bindings) =>
        val bound = binders(index, bindings)
        shared.get(index) match {
          case Some(function) =>
            val functionRef = varrType(function, sharedType(index, bound))
            if (bound.isEmpty) mkApp(functionRef, unitLitT(unitTypeT)) else bound.map { case (_, on) => ref(on) }.foldLeft(functionRef)(mkApp)
          case None =>
            bound.foldRight(cases(index).body) { case ((name, on), body) => letT(name, resultType, on._2, ref(on), body) }
        }
      case Tree.Switch(on, branches) =>
        matchExprT(resultType, ref(on), branches.map { case (shape, fields, sub) => MatchCase[TypeRec](shape.name, fields.map(_._1), generate(sub)) })
      case Tree.Test(on, literals, default) =>
        def branch(value: Boolean): TypeRec[Expr] =
          literals.collectFirst { case (LiteralValue.Bool(`value`), _, sub) => generate(sub) }
            .orElse(default.map(generate))
            .getOrElse(invariant("a bool test must cover both values"))
        if (literals.exists(_._1.isInstanceOf[LiteralValue.Bool])) ifT(resultType, ref(on), branch(true), branch(false))
        else literals.foldRight(default.map(generate).getOrElse(invariant("a literal test must have a default"))) {
          case ((value, equals, sub), otherwise) =>
            val literal = value match {
              case LiteralValue.Num(text, typeName) => numT(text, typeName, on._2)
              case LiteralValue.Char(c) => charT(c, on._2)
              case LiteralValue.Bool(_) => invariant("bool literals are tested with if")
            }
            val equality = equals.getOrElse(invariant("the equality of a literal pattern must be resolved by the type checker"))
            ifT(resultType, mkApp(mkApp(equality, ref(on)), literal), generate(sub), otherwise)
        }
    }

    val matched = letT(root._1, resultType, root._2, scrutinee, generate(tree))
    // 共有する本体の関数は、スクルーティニーの評価より外側に置く（本体はパターンの変数だけを引数に取る）
    val boundOf: Map[Int, Seq[(Variable, Occurrence)]] = {
      def collect(tree: Tree): Seq[(Int, Seq[(Variable, Occurrence)])] = tree match {
        case Tree.Fail => Seq.empty
        case Tree.Leaf(index, bindings) => Seq(index -> binders(index, bindings))
        case Tree.Switch(_, branches) => branches.flatMap { case (_, _, sub) => collect(sub) }
        case Tree.Test(_, literals, default) => literals.flatMap { case (_, _, sub) => collect(sub) } ++ default.toSeq.flatMap(collect)
      }
      collect(tree).toMap
    }
    shared.toSeq.sortBy(_._1).foldRight(matched) { case ((index, function), body) =>
      val bound = boundOf(index)
      val value =
        if (bound.isEmpty) mkAbs(unitParam, unitTypeT, cases(index).body)
        else bound.foldRight(cases(index).body) { case ((name, on), acc) => mkAbs(name, on._2, acc) }
      letT(function, resultType, sharedType(index, bound), value, body)
    }
  }
}
