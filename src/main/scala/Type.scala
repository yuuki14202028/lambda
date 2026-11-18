package com.yuuki14202028

/** 束縛子に書かれていた名前。表示にだけ使い、型の比較には関わらない */
final class BinderName(val name: String) {
  override def equals(other: Any): Boolean = other.isInstanceOf[BinderName]
  override def hashCode: Int = 0
  override def toString: String = name
}

/** 検査に使う型。
 *
 * 型の中の束縛変数（∀ / λ が束縛するもの）は、名前ではなく「内側から数えて何番目の束縛子か」（Bound）で表す。
 * 項の Λ や宣言の型引数が束縛する変数は Var（Symbol）で表す。
 * こうすると束縛変数の名前の違いが表現に現れないので、型が等しいかは == で比べられる。
 * 型は常に正規形（型レベルの λ の適用は、作るときに計算してしまう）で持つ。
 *
 * 制約は辞書の型で表す: Show[a] は「a についての Show の辞書」の型で、量化された制約 ∀a. Show[a] ⇒ Show[f[a]] は
 * 辞書を作る関数の型 ∀a. [Show[a]] → Show[f[a]]。Given(c, t) は「c の辞書を、解決して渡してもらう関数」の型 [c] → t。
 */
enum Type {
  case Var(symbol: Symbol)
  case Bound(index: Int)
  case Data(symbol: Symbol)
  case Primitive(name: String)
  case Arrow(from: Type, to: Type)
  case Given(constraint: Type, to: Type)
  case Forall(name: BinderName, kind: Kind, body: Type)
  case Lambda(name: BinderName, kind: Kind, body: Type)
  case Apply(function: Type, argument: Type)
}

object Type {
  val unit: Type = Primitive("unit")
  val bool: Type = Primitive("bool")
  val i32: Type = Primitive("i32")
  val string: Type = Primitive(Primitives.string)

  /** ∀symbol. body（body の中の symbol を束縛変数にする） */
  def forall(symbol: Symbol, kind: Kind, body: Type): Type = Forall(BinderName(symbol.name), kind, close(symbol, body))

  def lambda(symbol: Symbol, kind: Kind, body: Type): Type = Lambda(BinderName(symbol.name), kind, close(symbol, body))

  /** 型の適用。function が λ なら計算する */
  def apply(function: Type, argument: Type): Type = function match {
    case Lambda(_, _, body) => open(body, argument)
    case _ => Apply(function, argument)
  }

  private def close(symbol: Symbol, tpe: Type, depth: Int = 0): Type = tpe match {
    case Var(found) if found eq symbol => Bound(depth)
    case Arrow(from, to) => Arrow(close(symbol, from, depth), close(symbol, to, depth))
    case Given(constraint, to) => Given(close(symbol, constraint, depth), close(symbol, to, depth))
    case Forall(name, kind, body) => Forall(name, kind, close(symbol, body, depth + 1))
    case Lambda(name, kind, body) => Lambda(name, kind, close(symbol, body, depth + 1))
    case Apply(function, argument) => Apply(close(symbol, function, depth), close(symbol, argument, depth))
    case Var(_) | Bound(_) | Data(_) | Primitive(_) => tpe
  }

  /** 束縛子を 1 つ外し、その束縛変数を argument に置き換える（∀a. body の body に型を渡す） */
  def open(body: Type, argument: Type, depth: Int = 0): Type = body match {
    case Bound(index) =>
      if (index == depth) shift(argument, depth)
      else if (index > depth) Bound(index - 1)
      else body
    case Arrow(from, to) => Arrow(open(from, argument, depth), open(to, argument, depth))
    case Given(constraint, to) => Given(open(constraint, argument, depth), open(to, argument, depth))
    case Forall(name, kind, inner) => Forall(name, kind, open(inner, argument, depth + 1))
    case Lambda(name, kind, inner) => Lambda(name, kind, open(inner, argument, depth + 1))
    case Apply(function, inner) => Type.apply(open(function, argument, depth), open(inner, argument, depth))
    case Var(_) | Data(_) | Primitive(_) => body
  }

  /** 束縛子の内側へ持ち込む型の、外を指す番号をずらす */
  private def shift(tpe: Type, by: Int, depth: Int = 0): Type = tpe match {
    case Bound(index) => if (index >= depth) Bound(index + by) else tpe
    case Arrow(from, to) => Arrow(shift(from, by, depth), shift(to, by, depth))
    case Given(constraint, to) => Given(shift(constraint, by, depth), shift(to, by, depth))
    case Forall(name, kind, body) => Forall(name, kind, shift(body, by, depth + 1))
    case Lambda(name, kind, body) => Lambda(name, kind, shift(body, by, depth + 1))
    case Apply(function, argument) => Apply(shift(function, by, depth), shift(argument, by, depth))
    case Var(_) | Data(_) | Primitive(_) => tpe
  }

  /** Var を型に置き換える。置き換える型は束縛子の外を指さないものに限る */
  def substitute(tpe: Type, replacements: Map[Symbol, Type]): Type = tpe match {
    case Var(symbol) => replacements.getOrElse(symbol, tpe)
    case Arrow(from, to) => Arrow(substitute(from, replacements), substitute(to, replacements))
    case Given(constraint, to) => Given(substitute(constraint, replacements), substitute(to, replacements))
    case Forall(name, kind, body) => Forall(name, kind, substitute(body, replacements))
    case Lambda(name, kind, body) => Lambda(name, kind, substitute(body, replacements))
    case Apply(function, argument) => Type.apply(substitute(function, replacements), substitute(argument, replacements))
    case Bound(_) | Data(_) | Primitive(_) => tpe
  }

  /** 束縛子の外を指す Bound を含まないか */
  def isClosed(tpe: Type, depth: Int = 0): Boolean = tpe match {
    case Bound(index) => index < depth
    case Arrow(from, to) => isClosed(from, depth) && isClosed(to, depth)
    case Given(constraint, to) => isClosed(constraint, depth) && isClosed(to, depth)
    case Forall(_, _, body) => isClosed(body, depth + 1)
    case Lambda(_, _, body) => isClosed(body, depth + 1)
    case Apply(function, argument) => isClosed(function, depth) && isClosed(argument, depth)
    case Var(_) | Data(_) | Primitive(_) => true
  }

  /** D[T1][T2] の形なら、データ型 D と型引数 */
  def dataApplication(tpe: Type): Option[(Symbol, List[Type])] = tpe match {
    case Data(symbol) => Some((symbol, Nil))
    case Apply(function, argument) => dataApplication(function).map { case (symbol, arguments) => (symbol, arguments :+ argument) }
    case _ => None
  }

  /** tpe の中に現れる Var */
  def variables(tpe: Type): Set[Symbol] = tpe match {
    case Var(symbol) => Set(symbol)
    case Arrow(from, to) => variables(from) ++ variables(to)
    case Given(constraint, to) => variables(constraint) ++ variables(to)
    case Forall(_, _, body) => variables(body)
    case Lambda(_, _, body) => variables(body)
    case Apply(function, argument) => variables(function) ++ variables(argument)
    case Bound(_) | Data(_) | Primitive(_) => Set.empty
  }

  /** 先頭の ∀ を、新しい型変数で開く */
  def openForalls(tpe: Type): (List[(Symbol, Kind)], Type) = tpe match {
    case Forall(name, kind, body) =>
      val parameter = new Symbol(name.name)
      val (rest, inner) = openForalls(open(body, Var(parameter)))
      ((parameter, kind) :: rest, inner)
    case _ => (Nil, tpe)
  }

  /** [C] → [D] → T を (List(C, D), T) に分ける */
  def givens(tpe: Type): (List[Type], Type) = tpe match {
    case Given(constraint, to) =>
      val (constraints, result) = givens(to)
      (constraint :: constraints, result)
    case _ => (Nil, tpe)
  }

  /** A → B → C を (List(A, B), C) に分ける */
  def arrows(tpe: Type): (List[Type], Type) = tpe match {
    case Arrow(from, to) =>
      val (parameters, result) = arrows(to)
      (from :: parameters, result)
    case _ => (Nil, tpe)
  }

  def show(tpe: Type): String = tpe match {
    case Var(symbol) => symbol.name
    case Bound(index) => s"#$index"
    case Data(symbol) => symbol.name
    case Primitive(name) => name
    case Arrow(from, to) =>
      val left = from match {
        case Arrow(_, _) | Given(_, _) | Forall(_, _, _) | Lambda(_, _, _) => s"(${show(from)})"
        case _ => show(from)
      }
      s"$left → ${show(to)}"
    case Given(constraint, to) => s"[${show(constraint)}] → ${show(to)}"
    case Forall(name, kind, body) => s"∀${showBinder(name, kind)}. ${show(open(body, Var(new Symbol(name.name))))}"
    case Lambda(name, kind, body) => s"λ${showBinder(name, kind)}. ${show(open(body, Var(new Symbol(name.name))))}"
    case Apply(function, argument) =>
      val head = function match {
        case Arrow(_, _) | Given(_, _) | Forall(_, _, _) | Lambda(_, _, _) => s"(${show(function)})"
        case _ => show(function)
      }
      s"$head[${show(argument)}]"
  }

  private def showBinder(name: BinderName, kind: Kind): String =
    if (kind == Kind.Star) name.name else s"(${name.name}: ${kind.show})"
}
