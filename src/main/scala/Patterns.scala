package com.yuuki14202028

/** 型検査を済ませたパターン。Any はどの値にも当たる（変数なら binding に束縛する） */
enum CheckedPattern {
  case Any(binding: Option[Symbol])
  case Constructor(definition: ConstructorDef, arguments: List[CheckedPattern])
  case Bool(value: Boolean)
  // test は「値を受け取って、このリテラルと等しいかを返す関数」
  case Constant(value: Literal, test: Typed)
  case Or(alternatives: List[CheckedPattern])
}

/** match の分岐の並びを決定木にする。
 *
 * 行（Row）は「まだ調べていない値それぞれに対するパターン」の並び。先頭の行で最初に「調べる必要のあるパターン」がある列を選び、
 * その値で行を振り分けることを繰り返す。網羅性と到達不能な分岐も、できた木から調べる。
 */
object Patterns {

  /** guarded: 条件付きの分岐（条件が偽なら後続の行へ進む） */
  final case class Row(patterns: List[CheckedPattern], bindings: List[(Symbol, Occurrence)], caseIndex: Int, guarded: Boolean)

  /** 分岐ごとのパターンから木を作る。p | q は、同じ分岐を指す行に分ける */
  def compile(env: Env, root: Occurrence, cases: List[(CheckedPattern, Boolean)]): Decision =
    compile(env, List(root), cases.zipWithIndex.flatMap { case ((pattern, guarded), index) =>
      alternatives(pattern).map(alternative => Row(List(alternative), Nil, index, guarded))
    })

  /** p | q を含まないパターンの並びに展開する */
  private def alternatives(pattern: CheckedPattern): List[CheckedPattern] = pattern match {
    case CheckedPattern.Or(choices) => choices.flatMap(alternatives)
    case CheckedPattern.Constructor(definition, arguments) =>
      arguments.foldRight(List(List.empty[CheckedPattern])) { (argument, rest) =>
        for (first <- alternatives(argument); others <- rest) yield first :: others
      }.map(CheckedPattern.Constructor(definition, _))
    case other => List(other)
  }

  private def compile(env: Env, occurrences: List[Occurrence], rows: List[Row]): Decision = rows match {
    case Nil => Decision.Fail
    case first :: later =>
      first.patterns.indexWhere(!_.isInstanceOf[CheckedPattern.Any]) match {
        case -1 =>
          val bindings = first.patterns.zip(occurrences).flatMap { case (pattern, on) => bound(pattern, on) }
          Decision.Leaf(first.caseIndex, first.bindings ++ bindings, Option.when(first.guarded)(compile(env, occurrences, later)))
        case column =>
          val on = occurrences(column)
          /** 列 column を replacement に置き換えた行。keep が None を返した行は捨てる */
          def refined(keep: CheckedPattern => Option[List[CheckedPattern]]): List[Row] = rows.flatMap { row =>
            keep(row.patterns(column)).map { replacement =>
              row.copy(patterns = row.patterns.patch(column, replacement, 1), bindings = row.bindings ++ bound(row.patterns(column), on))
            }
          }
          first.patterns(column) match {
            case CheckedPattern.Constructor(_, _) =>
              val (data, typeArguments) = Type.dataApplication(on.tpe)
                .getOrElse(sys.error(s"Compiler invariant violation: ${Type.show(on.tpe)} is matched by a constructor"))
              Decision.Switch(on, env.dataTypes(data).constructors.map { constructor =>
                val fields = constructor.fieldTypes(typeArguments).map(Occurrence(new Symbol("field"), _))
                // この構成子の値に当たりうる行だけを残し、選んだ列をフィールドのパターンに置き換える
                val matching = refined {
                  case CheckedPattern.Constructor(found, arguments) => Option.when(found.symbol eq constructor.symbol)(arguments)
                  case CheckedPattern.Any(_) => Some(fields.map(_ => CheckedPattern.Any(None)))
                  case _ => None
                }
                Decision.Branch(constructor, fields, compile(env, occurrences.patch(column, fields, 1), matching))
              })

            case CheckedPattern.Bool(_) =>
              def when(value: Boolean): Decision = compile(env, occurrences.patch(column, Nil, 1), refined {
                case CheckedPattern.Bool(found) => Option.when(found == value)(Nil)
                case CheckedPattern.Any(_) => Some(Nil)
                case _ => None
              })
              Decision.IfBool(on, when(true), when(false))

            case CheckedPattern.Constant(value, test) =>
              val whenEqual = compile(env, occurrences.patch(column, Nil, 1), refined {
                case CheckedPattern.Constant(found, _) => Option.when(found == value)(Nil)
                case CheckedPattern.Any(_) => Some(Nil)
                case _ => None
              })
              // 等しくなかったときは、このリテラルの行だけを除いて、同じ値を調べ続ける
              val otherwise = compile(env, occurrences, rows.filter(_.patterns(column) match {
                case CheckedPattern.Constant(found, _) => found != value
                case _ => true
              }))
              Decision.Equal(on, value, test, whenEqual, otherwise)

            case CheckedPattern.Any(_) | CheckedPattern.Or(_) => sys.error("Compiler invariant violation: unexpected pattern in a decision tree")
          }
      }
  }

  private def bound(pattern: CheckedPattern, on: Occurrence): List[(Symbol, Occurrence)] = pattern match {
    case CheckedPattern.Any(Some(symbol)) => List(symbol -> on)
    case _ => Nil
  }

  /** 木の葉に現れる分岐の番号と、その分岐に着く葉の数 */
  def reached(decision: Decision): Map[Int, Int] = {
    def merge(left: Map[Int, Int], right: Map[Int, Int]): Map[Int, Int] =
      right.foldLeft(left) { case (acc, (index, count)) => acc.updated(index, acc.getOrElse(index, 0) + count) }
    decision match {
      case Decision.Fail => Map.empty
      case Decision.Leaf(caseIndex, _, otherwise) => merge(Map(caseIndex -> 1), otherwise.fold(Map.empty)(reached))
      case Decision.Switch(_, branches) => branches.map(branch => reached(branch.next)).foldLeft(Map.empty[Int, Int])(merge)
      case Decision.IfBool(_, whenTrue, whenFalse) => merge(reached(whenTrue), reached(whenFalse))
      case Decision.Equal(_, _, _, whenEqual, otherwise) => merge(reached(whenEqual), reached(otherwise))
    }
  }

  /** どの分岐にも当たらない値の例。網羅されていれば None */
  def unmatched(decision: Decision, root: Occurrence): Option[String] =
    unmatchedShapes(decision).map(_.getOrElse(root.symbol, "_"))

  /** 当たらない値について、調べた値（を束縛した変数）ごとの形 */
  private def unmatchedShapes(decision: Decision): Option[Map[Symbol, String]] = decision match {
    case Decision.Fail => Some(Map.empty)
    case Decision.Leaf(_, _, otherwise) => otherwise.flatMap(unmatchedShapes)
    case Decision.Switch(on, branches) =>
      branches.iterator.flatMap { branch =>
        unmatchedShapes(branch.next).map { shapes =>
          val arguments = branch.fields.map(field => s"(${shapes.getOrElse(field.symbol, "_")})").mkString
          shapes -- branch.fields.map(_.symbol) + (on.symbol -> s"${shortName(branch.constructor.symbol)}$arguments")
        }
      }.nextOption()
    case Decision.IfBool(on, whenTrue, whenFalse) =>
      unmatchedShapes(whenTrue).map(_ + (on.symbol -> "true")).orElse(unmatchedShapes(whenFalse).map(_ + (on.symbol -> "false")))
    case Decision.Equal(_, _, _, whenEqual, otherwise) => unmatchedShapes(otherwise).orElse(unmatchedShapes(whenEqual))
  }

  private def shortName(symbol: Symbol): String = symbol.name.substring(symbol.name.lastIndexOf('.') + 1)
}
