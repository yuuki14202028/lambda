package com.yuuki14202028

/** ラムダ式をそのまま評価するインタプリタ。言語の意味の基準になる（バックエンドの出力と突き合わせる相手）。
 * 呼び出しは Scala の再帰でたどるので、深い再帰のために大きなスタックを持つスレッドで動かす。
 */
object Interpreter {

  enum Value {
    case Integer(value: Long)
    case Floating(value: Double)
    case Bool(value: Boolean)
    case Character(value: Char)
    case Text(value: String)
    case Unit
    case Closure(parameter: Symbol, body: Lambda, scope: Scope)
    // 引数を remaining 個受け取り終えたら run を呼ぶ（C の関数）
    case Foreign(name: String, remaining: Int, received: List[Value])
  }

  /** 変数の値の置き場。rec の値は、置き場を先に作ってから入れる */
  final class Cell(var value: Option[Value])

  type Scope = Map[Symbol, Cell]

  final case class Result(exitCode: Int, output: String)

  def run(program: Lambda.Program): Result = {
    var result = Option.empty[Either[Throwable, Result]]
    val thread = new Thread(null, () => result = Some(try Right(new Interpreter().run(program)) catch { case error: Throwable => Left(error) }), "interpreter", 1L << 30)
    thread.start()
    thread.join()
    result.get.fold(error => throw error, identity)
  }
}

final class Interpreter {
  import Interpreter.*

  private val output = new StringBuilder

  private def invariant(message: String): Nothing = sys.error(s"Compiler invariant violation: $message")

  def run(program: Lambda.Program): Result = {
    val scope = program.declarations.foldLeft(Map.empty: Scope) {
      case (scope, Lambda.Decl.Let(symbol, value)) => scope + (symbol -> new Cell(Some(evaluate(value, scope))))
      case (scope, Lambda.Decl.Rec(bindings)) => recursive(bindings, scope)
    }
    apply(evaluate(Lambda.Variable(program.main), scope), Value.Unit) match {
      case Value.Integer(exitCode) => Result(exitCode.toInt, output.toString)
      case other => invariant(s"main returned $other")
    }
  }

  private def recursive(bindings: List[(Symbol, Lambda)], scope: Scope): Scope = {
    val cells = bindings.map { case (symbol, _) => symbol -> new Cell(None) }
    val inner = scope ++ cells
    bindings.zip(cells).foreach { case ((_, value), (_, cell)) => cell.value = Some(evaluate(value, inner)) }
    inner
  }

  private def evaluate(term: Lambda, scope: Scope): Value = term match {
    case Lambda.Variable(symbol) =>
      scope.getOrElse(symbol, invariant(s"$symbol is not in scope")).value.getOrElse(invariant(s"$symbol is read before it is initialized"))
    case Lambda.Constant(value) => value match {
      case Literal.Int(text, _) => Value.Integer(BigInt(text).toLong)
      case Literal.Float(text, _) => Value.Floating(text.toDouble)
      case Literal.Bool(value) => Value.Bool(value)
      case Literal.Char(value) => Value.Character(value)
      case Literal.Str(value) => Value.Text(value)
      case Literal.Unit => Value.Unit
    }
    case Lambda.Abstraction(parameter, body) => Value.Closure(parameter, body, scope)
    case Lambda.Apply(function, argument) => apply(evaluate(function, scope), evaluate(argument, scope))
    case Lambda.Let(symbol, value, body) => evaluate(body, scope + (symbol -> new Cell(Some(evaluate(value, scope)))))
    case Lambda.LetRec(bindings, body) => evaluate(body, recursive(bindings, scope))
    case Lambda.If(condition, thenBranch, elseBranch) => evaluate(condition, scope) match {
      case Value.Bool(true) => evaluate(thenBranch, scope)
      case Value.Bool(false) => evaluate(elseBranch, scope)
      case other => invariant(s"the condition of an if is $other")
    }
    case Lambda.Primitive(intrinsic, arguments) => primitive(intrinsic, arguments.map(evaluate(_, scope)))
    case Lambda.Foreign(name, parameters, _) => Value.Foreign(name, parameters.length, Nil)
  }

  private def apply(function: Value, argument: Value): Value = function match {
    case Value.Closure(parameter, body, scope) => evaluate(body, scope + (parameter -> new Cell(Some(argument))))
    case Value.Foreign(name, 1, received) => foreign(name, received :+ argument)
    case Value.Foreign(name, remaining, received) => Value.Foreign(name, remaining - 1, received :+ argument)
    case other => invariant(s"$other is applied")
  }

  // ---- 組み込み演算 ----

  /** 整数を、型の幅と符号に合わせて丸める */
  private def wrap(value: Long, typeName: String): Long = typeName match {
    case "i8" => value.toByte
    case "i16" => value.toShort
    case "i32" => value.toInt
    case "u8" => value & 0xffL
    case "u16" => value & 0xffffL
    case "u32" => value & 0xffffffffL
    case _ => value
  }

  private def isUnsigned64(typeName: String): Boolean = typeName == "u64" || typeName == "usize"

  private def primitive(intrinsic: Intrinsic, arguments: List[Value]): Value = {
    val typeName = intrinsic.typeName
    (intrinsic.op, arguments) match {
      case (PrimOp.Eq, List(left, right)) => Value.Bool(left == right)
      case (PrimOp.Neq, List(left, right)) => Value.Bool(left != right)
      case (PrimOp.Not, List(Value.Bool(value))) => Value.Bool(!value)
      case (PrimOp.And, List(Value.Bool(left), Value.Bool(right))) => Value.Bool(left & right)
      case (PrimOp.Or, List(Value.Bool(left), Value.Bool(right))) => Value.Bool(left | right)
      case (PrimOp.Xor, List(Value.Bool(left), Value.Bool(right))) => Value.Bool(left ^ right)

      case (PrimOp.Neg, List(Value.Integer(value))) => Value.Integer(wrap(-value, typeName))
      case (op, List(Value.Integer(left), Value.Integer(right))) =>
        val order = if (isUnsigned64(typeName)) java.lang.Long.compareUnsigned(left, right) else left.compare(right)
        op match {
          case PrimOp.Add => Value.Integer(wrap(left + right, typeName))
          case PrimOp.Sub => Value.Integer(wrap(left - right, typeName))
          case PrimOp.Mul => Value.Integer(wrap(left * right, typeName))
          case PrimOp.Div => Value.Integer(wrap(if (isUnsigned64(typeName)) java.lang.Long.divideUnsigned(left, right) else left / right, typeName))
          case PrimOp.Mod => Value.Integer(wrap(if (isUnsigned64(typeName)) java.lang.Long.remainderUnsigned(left, right) else left % right, typeName))
          case PrimOp.And => Value.Integer(left & right)
          case PrimOp.Or => Value.Integer(left | right)
          case PrimOp.Xor => Value.Integer(left ^ right)
          case PrimOp.Lt => Value.Bool(order < 0)
          case PrimOp.Leq => Value.Bool(order <= 0)
          case PrimOp.Gt => Value.Bool(order > 0)
          case PrimOp.Geq => Value.Bool(order >= 0)
          case _ => invariant(s"intrinsic ${intrinsic.name} is applied to integers")
        }

      case (PrimOp.Neg, List(Value.Floating(value))) => Value.Floating(-value)
      case (op, List(Value.Floating(left), Value.Floating(right))) =>
        def rounded(value: Double): Value = Value.Floating(if (typeName == "f32") value.toFloat.toDouble else value)
        op match {
          case PrimOp.Add => rounded(left + right)
          case PrimOp.Sub => rounded(left - right)
          case PrimOp.Mul => rounded(left * right)
          case PrimOp.Div => rounded(left / right)
          case PrimOp.Lt => Value.Bool(left < right)
          case PrimOp.Leq => Value.Bool(left <= right)
          case PrimOp.Gt => Value.Bool(left > right)
          case PrimOp.Geq => Value.Bool(left >= right)
          case _ => invariant(s"intrinsic ${intrinsic.name} is applied to floating-point numbers")
        }

      case _ => invariant(s"intrinsic ${intrinsic.name} is applied to $arguments")
    }
  }

  // ---- C の関数（runtime/ffi.c と同じ名前・同じ意味） ----

  private def foreign(name: String, arguments: List[Value]): Value = (name, arguments) match {
    case ("print_string_inline", List(Value.Text(text))) => output ++= text; Value.Integer(0)
    case ("print_int", List(Value.Integer(value))) => output ++= s"$value\n"; Value.Integer(value)
    case ("print_int_inline", List(Value.Integer(value))) => output ++= value.toString; Value.Integer(value)
    case ("put_char", List(Value.Integer(value))) => output += value.toChar; Value.Integer(value)
    case ("read_line", List(Value.Unit)) => Value.Text(Option(scala.io.StdIn.readLine()).fold("")(_ + "\n"))
    case ("add_ints", List(Value.Integer(left), Value.Integer(right))) => Value.Integer((left + right).toInt)
    case ("int_to_string" | "long_to_string", List(Value.Integer(value))) => Value.Text(value.toString)
    case ("double_to_string", List(Value.Floating(value))) => Value.Text(value.toString)
    case ("string_to_int", List(Value.Text(text))) =>
      Value.Integer(text.trim.takeWhile(c => c.isDigit || c == '-').toIntOption.getOrElse(0))
    case ("string_length", List(Value.Text(text))) => Value.Integer(text.length)
    case ("string_equal", List(Value.Text(left), Value.Text(right))) => Value.Bool(left == right)
    case ("string_compare", List(Value.Text(left), Value.Text(right))) => Value.Integer(left.compareTo(right).sign)
    case ("concat", List(Value.Text(left), Value.Text(right))) => Value.Text(left + right)
    case ("substring", List(Value.Text(text), Value.Integer(start), Value.Integer(length))) =>
      val from = start.toInt.max(0).min(text.length)
      Value.Text(text.substring(from, (from + length.toInt.max(0)).min(text.length)))
    case ("char_at", List(Value.Text(text), Value.Integer(index))) => Value.Character(text.charAt(index.toInt))
    case ("char_to_string", List(Value.Character(value))) => Value.Text(value.toString)
    case ("char_code", List(Value.Character(value))) => Value.Integer(value.toLong)
    case ("char_from_code", List(Value.Integer(code))) => Value.Character(code.toChar)
    case _ => sys.error(s"The interpreter does not know the foreign function $name for arguments $arguments")
  }
}
