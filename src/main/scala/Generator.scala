package com.yuuki14202028

import scala.collection.mutable.ListBuffer

/** Lambda を ARM64 (macOS) のアセンブリに落とす。
 *
 * 値はすべて 64 ビットの 1 語（整数は型の幅に丸めて符号・ゼロ拡張した形、浮動小数点数はビット列のまま、
 * 文字列は char*、関数はクロージャへのポインタ）。
 *
 * クロージャ: ヒープ上の [コードのアドレス, 捕まえた変数 0, 捕まえた変数 1, …]。
 * 呼び出し:   x0 = クロージャ、x1 = 引数。結果は x0。
 * フレーム:   [x29] = 呼び出し元の x29 / x30、[x29 + 16] = 自分のクロージャ、[x29 + 24] = 引数、その上に let の変数。
 *            計算の途中の値は、sp に積んで下ろす。
 *
 * rec の変数だけは、値を入れるセル（ヒープの 1 語）へのポインタを置き、クロージャはそのセルを捕まえる。
 * トップレベルの変数は大域の領域に置くので、捕まえない。
 * 関数本体の末尾位置の呼び出しは、フレームを畳んでからジャンプする。
 * メモリは malloc するだけで解放しない。
 */
object Generator {

  def generate(program: Lambda.Program): String = new Generator().program(program)

  private type Code = Seq[String]

  private enum Place {
    case Slot(index: Int)
    case Captured(index: Int)
  }

  /** cell: 置き場にあるのは値ではなく、値を入れるセルへのポインタ（rec の変数） */
  private final case class Stored(place: Place, cell: Boolean)

  /** 生成中の関数から見える局所変数。slots は使っているスロットの数、locals は let のために確保した数 */
  private final case class Frame(variables: Map[Symbol, Stored], slots: Int, locals: Int) {
    def bind(symbol: Symbol, cell: Boolean): Frame =
      copy(variables = variables + (symbol -> Stored(Place.Slot(slots), cell)), slots = slots + 1)

    def size: Int = (locals + 5) / 2 * 16
  }

  private val closureSlot = 0
  private val argumentSlot = 1

  private def slot(index: Int): String = s"[x29, #${16 + 8 * index}]"

  private val stackSize = 1L << 30
}

private final class Generator {
  import Generator.*
  import Lambda.*

  private val functions = ListBuffer.empty[String]
  private val strings = ListBuffer.empty[(String, String)]
  private var globals = Map.empty[Symbol, String]
  private var counter = 0

  private def invariant(message: String): Nothing = sys.error(s"Compiler invariant violation: $message")

  private def fresh(prefix: String): String = {
    counter += 1
    s"${prefix}_$counter"
  }

  def program(program: Program): String = {
    val locals = program.declarations.map {
      case Decl.Let(_, value) => countLocals(value)
      case Decl.Rec(bindings) => bindings.map(binding => countLocals(binding._2)).sum
    }.maxOption.getOrElse(0)

    function("lambda_start", Frame(Map.empty, 2, locals)) { frame =>
      val declarations = program.declarations.flatMap {
        case Decl.Let(symbol, value) =>
          val code = expression(value, frame, tail = false)
          code ++ defineGlobal(symbol)
        case Decl.Rec(bindings) =>
          val define = bindings.map(binding => defineGlobal(binding._1))
          bindings.zip(define).flatMap { case ((_, value), store) => expression(value, frame, tail = false) ++ store }
      }
      declarations ++ expression(Apply(Variable(program.main), Constant(Literal.Unit)), frame, tail = true)
    }

    val lines = entry ++ functions ++
      Vector(".section __TEXT,__cstring,cstring_literals") ++ strings.flatMap((label, text) => Vector(s"$label:", s".asciz \"$text\"")) ++
      globals.values.toVector.sorted.map(label => s".zerofill __DATA,__bss,$label,8,3")
    lines.map(line => if (line.endsWith(":") || line.startsWith(".")) line else s"    $line").mkString("", "\n", "\n")
  }

  /** 深い再帰のために大きなスタックを確保し、sp を移してからプログラムを動かす */
  private def entry: Code =
    Vector(".section __TEXT,__text,regular,pure_instructions", ".globl _main", ".p2align 2", "_main:",
      "stp x29, x30, [sp, #-16]!", "stp x19, x20, [sp, #-16]!", "mov x19, sp") ++
      // mmap(0, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0)
      Vector("mov x0, #0") ++ constant("x1", stackSize) ++ Vector("mov w2, #3", "mov w3, #0x1002", "mov w4, #-1", "mov x5, #0", "bl _mmap") ++
      constant("x9", stackSize) ++
      Vector("add x0, x0, x9", "mov sp, x0", "bl lambda_start", "mov sp, x19", "ldp x19, x20, [sp], #16", "ldp x29, x30, [sp], #16", "ret")

  private def defineGlobal(symbol: Symbol): Code = {
    val label = fresh("global")
    globals += symbol -> label
    Vector(s"adrp x9, $label@PAGE", s"str x0, [x9, $label@PAGEOFF]")
  }

  // ---- 関数とフレーム ----

  private def function(label: String, frame: Frame)(body: Frame => Code): Unit =
    functions ++= Vector(".p2align 2", s"$label:") ++ constant("x16", frame.size) ++
      Vector("sub sp, sp, x16", "stp x29, x30, [sp]", "mov x29, sp", s"str x0, ${slot(closureSlot)}", s"str x1, ${slot(argumentSlot)}") ++
      body(frame)

  private def leave(frame: Frame): Code = Vector("ldp x29, x30, [sp]") ++ constant("x16", frame.size) ++ Vector("add sp, sp, x16")

  private def returnValue(frame: Frame): Code = leave(frame) :+ "ret"

  /** parameter を受け取る関数を作り、captured を捕まえたクロージャを x0 に置く */
  private def closure(parameter: Symbol, captured: List[Symbol], locals: Int, frame: Frame)(body: Frame => Code): Code = {
    val label = fresh("function")
    val variables = captured.zipWithIndex.map((symbol, index) => symbol -> Stored(Place.Captured(index), frame.variables(symbol).cell))
    function(label, Frame(variables.toMap + (parameter -> Stored(Place.Slot(argumentSlot), cell = false)), 2, locals))(body)

    Vector(s"mov x0, #${8 * (1 + captured.length)}", "bl _malloc", s"adrp x9, $label@PAGE", s"add x9, x9, $label@PAGEOFF", "str x9, [x0]") ++
      captured.zipWithIndex.flatMap((symbol, index) => storage("x9", symbol, frame) :+ s"str x9, [x0, #${8 * (index + 1)}]")
  }

  /** 変数の置き場にある語（rec の変数ならセルへのポインタ）を読む */
  private def storage(register: String, symbol: Symbol, frame: Frame): Code = frame.variables.get(symbol) match {
    case Some(Stored(Place.Slot(index), _)) => Vector(s"ldr $register, ${slot(index)}")
    case Some(Stored(Place.Captured(index), _)) => Vector(s"ldr $register, ${slot(closureSlot)}", s"ldr $register, [$register, #${8 * (index + 1)}]")
    case None =>
      val label = globals.getOrElse(symbol, invariant(s"$symbol is not in scope"))
      Vector(s"adrp $register, $label@PAGE", s"ldr $register, [$register, $label@PAGEOFF]")
  }

  private def variable(register: String, symbol: Symbol, frame: Frame): Code =
    storage(register, symbol, frame) ++ (if (frame.variables.get(symbol).exists(_.cell)) Vector(s"ldr $register, [$register]") else Vector.empty)

  private def free(term: Lambda): List[Symbol] = term match {
    case Variable(symbol) => List(symbol)
    case Constant(_) | Foreign(_, _, _) => Nil
    case Abstraction(parameter, body) => free(body).filterNot(_ == parameter)
    case Apply(function, argument) => free(function) ++ free(argument)
    case Let(symbol, value, body) => free(value) ++ free(body).filterNot(_ == symbol)
    case LetRec(bindings, body) => (bindings.flatMap(binding => free(binding._2)) ++ free(body)).filterNot(bindings.map(_._1).contains)
    case If(condition, thenBranch, elseBranch) => free(condition) ++ free(thenBranch) ++ free(elseBranch)
    case Primitive(_, arguments) => arguments.flatMap(free)
  }

  /** この関数のフレームに置く let の変数の数（中の関数の分は数えない） */
  private def countLocals(term: Lambda): Int = term match {
    case Variable(_) | Constant(_) | Foreign(_, _, _) | Abstraction(_, _) => 0
    case Apply(function, argument) => countLocals(function) + countLocals(argument)
    case Let(_, value, body) => 1 + countLocals(value) + countLocals(body)
    case LetRec(bindings, body) => bindings.map(binding => 1 + countLocals(binding._2)).sum + countLocals(body)
    case If(condition, thenBranch, elseBranch) => countLocals(condition) + countLocals(thenBranch) + countLocals(elseBranch)
    case Primitive(_, arguments) => arguments.map(countLocals).sum
  }

  // ---- 式 ----

  private val push: Code = Vector("str x0, [sp, #-16]!")
  private def pop(register: String): Code = Vector(s"ldr $register, [sp], #16")

  private def constant(register: String, value: Long): Code = {
    val parts = (0 until 4).map(index => (value >>> (16 * index)) & 0xffff)
    Vector(s"movz $register, #${parts(0)}") ++ (1 until 4).collect { case index if parts(index) != 0 => s"movk $register, #${parts(index)}, lsl #${16 * index}" }
  }

  /** 式の値を x0 に置く。tail なら、その値を関数の結果として返す所まで生成する */
  private def expression(term: Lambda, frame: Frame, tail: Boolean): Code = term match {
    case Apply(function, argument) =>
      val call = if (tail) leave(frame) ++ Vector("ldr x9, [x0]", "br x9") else Vector("ldr x9, [x0]", "blr x9")
      expression(function, frame, tail = false) ++ push ++ expression(argument, frame, tail = false) ++ Vector("mov x1, x0") ++ pop("x0") ++ call

    case Let(symbol, value, body) =>
      val inner = frame.bind(symbol, cell = false)
      expression(value, frame, tail = false) ++ Vector(s"str x0, ${slot(frame.slots)}") ++ expression(body, inner, tail)

    case LetRec(bindings, body) =>
      val inner = bindings.foldLeft(frame)((frame, binding) => frame.bind(binding._1, cell = true))
      val cells = bindings.indices.map(index => slot(frame.slots + index))
      cells.flatMap(cell => Vector("mov x0, #8", "bl _malloc", s"str x0, $cell")) ++
        bindings.zip(cells).flatMap { case ((_, value), cell) => expression(value, inner, tail = false) ++ Vector(s"ldr x9, $cell", "str x0, [x9]") } ++
        expression(body, inner, tail)

    case If(condition, thenBranch, elseBranch) =>
      val otherwise = fresh("Lelse")
      val end = fresh("Lend")
      expression(condition, frame, tail = false) ++ Vector(s"cbz x0, $otherwise") ++
        expression(thenBranch, frame, tail) ++ (if (tail) Vector.empty else Vector(s"b $end")) ++
        Vector(s"$otherwise:") ++ expression(elseBranch, frame, tail) ++ Vector(s"$end:")

    case _ => value(term, frame) ++ (if (tail) returnValue(frame) else Vector.empty)
  }

  /** 呼び出しも分岐もしない式 */
  private def value(term: Lambda, frame: Frame): Code = term match {
    case Variable(symbol) => variable("x0", symbol, frame)

    case Constant(literal) => literal match {
      case Literal.Int(text, _) => constant("x0", BigInt(text).toLong)
      case Literal.Float(text, "f32") => constant("x0", java.lang.Float.floatToRawIntBits(text.toFloat) & 0xffffffffL)
      case Literal.Float(text, _) => constant("x0", java.lang.Double.doubleToRawLongBits(text.toDouble))
      case Literal.Bool(value) => constant("x0", if (value) 1 else 0)
      case Literal.Char(value) => constant("x0", value.toLong)
      case Literal.Unit => constant("x0", 0)
      case Literal.Str(value) =>
        val label = fresh("Lstring")
        strings += label -> escape(value)
        Vector(s"adrp x0, $label@PAGE", s"add x0, x0, $label@PAGEOFF")
    }

    case Abstraction(parameter, body) =>
      val captured = free(term).distinct.filter(frame.variables.contains)
      closure(parameter, captured, countLocals(body), frame)(expression(body, _, tail = true))

    case Primitive(intrinsic, List(operand)) => expression(operand, frame, tail = false) ++ unary(intrinsic)
    case Primitive(intrinsic, List(left, right)) =>
      expression(left, frame, tail = false) ++ push ++ expression(right, frame, tail = false) ++ Vector("mov x1, x0") ++ pop("x0") ++ binary(intrinsic)

    case Foreign(name, parameters, result) =>
      if (parameters.isEmpty) invariant(s"the foreign function $name takes no arguments")
      foreign(name, parameters, result, Nil, frame)

    case _ => invariant(s"$term is not a simple expression")
  }

  private def escape(text: String): String =
    text.getBytes(java.nio.charset.StandardCharsets.UTF_8).map { byte =>
      if (byte >= 0x20 && byte < 0x7f && byte != '"' && byte != '\\') byte.toChar.toString else f"\\${byte & 0xff}%03o"
    }.mkString

  // ---- 組み込み演算（左が x0、右が x1） ----

  /** x0 を、型の幅と符号に合わせて丸める */
  private def wrap(typeName: String): Code = typeName match {
    case "i8" => Vector("sxtb x0, w0")
    case "i16" => Vector("sxth x0, w0")
    case "i32" => Vector("sxtw x0, w0")
    case "u8" => Vector("and x0, x0, #0xff")
    case "u16" => Vector("and x0, x0, #0xffff")
    case "u32" | "char" => Vector("mov w0, w0")
    case _ => Vector.empty
  }

  /** 浮動小数点数のレジスタと、同じ幅の整数のレジスタの接頭辞 */
  private def floatRegisters(typeName: String): (String, String) = if (typeName == "f32") ("s", "w") else ("d", "x")

  private def unary(intrinsic: Intrinsic): Code = intrinsic.op match {
    case PrimOp.Not => Vector("eor x0, x0, #1")
    case PrimOp.Neg if Primitives.floats(intrinsic.typeName) =>
      val (float, integer) = floatRegisters(intrinsic.typeName)
      Vector(s"fmov ${float}0, ${integer}0", s"fneg ${float}0, ${float}0", s"fmov ${integer}0, ${float}0")
    case PrimOp.Neg => Vector("neg x0, x0") ++ wrap(intrinsic.typeName)
    case _ => invariant(s"intrinsic ${intrinsic.name} takes one argument")
  }

  private def binary(intrinsic: Intrinsic): Code =
    if (Primitives.floats(intrinsic.typeName)) floating(intrinsic) else integer(intrinsic)

  private def integer(intrinsic: Intrinsic): Code = {
    val typeName = intrinsic.typeName
    // 64 ビットより狭い型の値は拡張してあるので、符号つきの演算で正しい結果になる
    val unsigned = typeName == "u64" || typeName == "usize"
    val divide = if (unsigned) "udiv" else "sdiv"
    def compare(signedCondition: String, unsignedCondition: String): Code =
      Vector("cmp x0, x1", s"cset x0, ${if (unsigned) unsignedCondition else signedCondition}")

    intrinsic.op match {
      case PrimOp.Add => Vector("add x0, x0, x1") ++ wrap(typeName)
      case PrimOp.Sub => Vector("sub x0, x0, x1") ++ wrap(typeName)
      case PrimOp.Mul => Vector("mul x0, x0, x1") ++ wrap(typeName)
      case PrimOp.Div => Vector(s"$divide x0, x0, x1") ++ wrap(typeName)
      case PrimOp.Mod => Vector(s"$divide x9, x0, x1", "msub x0, x9, x1, x0") ++ wrap(typeName)
      case PrimOp.And => Vector("and x0, x0, x1")
      case PrimOp.Or => Vector("orr x0, x0, x1")
      case PrimOp.Xor => Vector("eor x0, x0, x1")
      case PrimOp.Eq => compare("eq", "eq")
      case PrimOp.Neq => compare("ne", "ne")
      case PrimOp.Lt => compare("lt", "lo")
      case PrimOp.Leq => compare("le", "ls")
      case PrimOp.Gt => compare("gt", "hi")
      case PrimOp.Geq => compare("ge", "hs")
      case PrimOp.Neg | PrimOp.Not => invariant(s"intrinsic ${intrinsic.name} takes two arguments")
    }
  }

  private def floating(intrinsic: Intrinsic): Code = {
    val (float, integer) = floatRegisters(intrinsic.typeName)
    val operands = Vector(s"fmov ${float}0, ${integer}0", s"fmov ${float}1, ${integer}1")
    def arithmetic(instruction: String): Code = operands ++ Vector(s"$instruction ${float}0, ${float}0, ${float}1", s"fmov ${integer}0, ${float}0")
    // 条件は、どちらかが NaN のときに偽になるものを選ぶ（ne だけは真）
    def compare(condition: String): Code = operands ++ Vector(s"fcmp ${float}0, ${float}1", s"cset x0, $condition")

    intrinsic.op match {
      case PrimOp.Add => arithmetic("fadd")
      case PrimOp.Sub => arithmetic("fsub")
      case PrimOp.Mul => arithmetic("fmul")
      case PrimOp.Div => arithmetic("fdiv")
      case PrimOp.Eq => compare("eq")
      case PrimOp.Neq => compare("ne")
      case PrimOp.Lt => compare("mi")
      case PrimOp.Leq => compare("ls")
      case PrimOp.Gt => compare("gt")
      case PrimOp.Geq => compare("ge")
      case _ => invariant(s"intrinsic ${intrinsic.name} is applied to floating-point numbers")
    }
  }

  // ---- C の関数 ----

  /** 引数を 1 つ受け取るたびに、それまでの引数を捕まえたクロージャを返す。最後の引数で C の関数を呼ぶ */
  private def foreign(name: String, parameters: List[String], result: String, received: List[Symbol], frame: Frame): Code = {
    val parameter = new Symbol(s"argument${received.length}")
    val arguments = received :+ parameter
    closure(parameter, received, locals = 0, frame) { inner =>
      val body = if (arguments.length == parameters.length) foreignCall(name, parameters.zip(arguments), result, inner) else foreign(name, parameters, result, arguments, inner)
      body ++ returnValue(inner)
    }
  }

  private def foreignCall(name: String, arguments: List[(String, Symbol)], result: String, frame: Frame): Code = {
    // unit の引数は C に渡さない
    val (floats, integers) = arguments.filterNot(_._1 == "unit").partition((typeName, _) => Primitives.floats(typeName))
    if (floats.length > 8 || integers.length > 8) invariant(s"the foreign function $name takes too many arguments")

    val pass = integers.zipWithIndex.flatMap { case ((_, symbol), index) => variable(s"x$index", symbol, frame) } ++
      floats.zipWithIndex.flatMap { case ((typeName, symbol), index) =>
        val (float, integer) = floatRegisters(typeName)
        variable("x9", symbol, frame) :+ s"fmov $float$index, ${integer}9"
      }
    val receive = result match {
      case "f32" => Vector("fmov w0, s0")
      case "f64" => Vector("fmov x0, d0")
      case "bool" => Vector("cmp w0, #0", "cset x0, ne")
      case "unit" => Vector("mov x0, #0")
      case _ => wrap(result)
    }
    pass ++ Vector(s"bl _$name") ++ receive
  }
}
