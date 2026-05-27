package com.yuuki14202028

import cats.data.{ReaderT, State}
import cats.syntax.all.*

import scala.annotation.tailrec

object Generator {
  sealed trait Binding {
    def name: String
  }
  case class HeapBinding(name: String) extends Binding
  case class StackBinding(name: String, offset: Int) extends Binding
  case class Env(values: List[Binding], directFunctions: Map[String, Label]) {
    def withHeap(name: String): Env = copy(values = HeapBinding(name) :: values)
    def withStack(name: String, offset: Int): Env = copy(values = StackBinding(name, offset) :: values)
    def withDirectFunction(name: String, label: Label): Env = copy(directFunctions = directFunctions.updated(name, label))
  }

  object Env {
    val empty: Env = Env(Nil, Map.empty)
  }

  case class Label(name: String)
  case class Reg(name: String)

  sealed trait Operand {
    def render: String
  }
  case class Register(reg: Reg) extends Operand {
    def render: String = reg.name
  }
  case class Immediate(value: Long) extends Operand {
    def render: String = s"#$value"
  }
  case class LabelOperand(label: Label) extends Operand {
    def render: String = label.name
  }
  case class Page(label: Label) extends Operand {
    def render: String = s"${label.name}@PAGE"
  }
  case class PageOff(label: Label) extends Operand {
    def render: String = s"${label.name}@PAGEOFF"
  }
  case class Shift(kind: String, amount: Int) extends Operand {
    def render: String = s"$kind #$amount"
  }
  case class Condition(name: String) extends Operand {
    def render: String = name
  }
  case class Memory(base: Reg, offset: Option[Int] = None, writeBack: Boolean = false, postIndex: Boolean = false) extends Operand {
    def render: String = {
      val off = offset.map(o => s", #$o").getOrElse("")
      if (postIndex) s"[${base.name}], #${offset.getOrElse(0)}"
      else s"[${base.name}$off]${if (writeBack) "!" else ""}"
    }
  }

  sealed trait AsmLine {
    def render: String
  }
  case object Blank extends AsmLine {
    def render: String = ""
  }
  case class Directive(name: String, args: Vector[String] = Vector.empty) extends AsmLine {
    def render: String = if (args.isEmpty) s".$name" else s".$name ${args.mkString(", ")}"
  }
  case class LabelDef(label: Label) extends AsmLine {
    def render: String = s"${label.name}:"
  }
  case class Instruction(mnemonic: String, operands: Vector[Operand] = Vector.empty) extends AsmLine {
    def render: String = if (operands.isEmpty) s"    $mnemonic" else s"    $mnemonic ${operands.map(_.render).mkString(", ")}"
  }

  case class Code(lines: Vector[AsmLine]) {
    def ++(that: Code): Code = Code(lines ++ that.lines)
  }

  object Code {
    val empty: Code = Code(Vector.empty)
    def apply(lines: AsmLine*): Code = Code(lines.toVector)
    def concat(values: Iterable[Code]): Code = Code(values.toVector.flatMap(_.lines))
  }

  case class StringDef(label: Label, value: String)
  case class FunctionDef(label: Label, body: Code)
  case class AssemblyProgram(externs: Set[String], strings: Vector[StringDef], main: FunctionDef, functions: Vector[FunctionDef])

  private val x0 = Reg("x0")
  private val x1 = Reg("x1")
  private val x9 = Reg("x9")
  private val x10 = Reg("x10")
  private val x16 = Reg("x16")
  private val x29 = Reg("x29")
  private val x30 = Reg("x30")
  private val sp = Reg("sp")
  private val w0 = Reg("w0")
  private val w1 = Reg("w1")
  private val w10 = Reg("w10")
  private val s0 = Reg("s0")
  private val s1 = Reg("s1")
  private val d0 = Reg("d0")
  private val d1 = Reg("d1")

  private def r(reg: Reg): Operand = Register(reg)
  private def imm(value: Long): Operand = Immediate(value)
  private def mem(base: Reg): Operand = Memory(base)
  private def mem(base: Reg, offset: Int): Operand = Memory(base, Some(offset))
  private def pre(base: Reg, offset: Int): Operand = Memory(base, Some(offset), writeBack = true)
  private def post(base: Reg, offset: Int): Operand = Memory(base, Some(offset), postIndex = true)
  private def ins(mnemonic: String, operands: Operand*): AsmLine = Instruction(mnemonic, operands.toVector)
  private def code(lines: AsmLine*): Code = Code(lines*)

  case class GenState(funcs: Vector[FunctionDef], counter: Int, externs: Set[String], strings: Vector[StringDef])
  type GenS[A] = State[GenState, A]
  type Gen[A] = ReaderT[GenS, Env, A]
  private type GenCode[I] = Gen[Code]
  private type Child[I] = (TypeRec[I], GenCode[I])

  extension [I](self: Child[I]) {
    private def original: TypeRec[I] = self._1
    private def code: Gen[Code] = self._2
  }

  private def fresh(prefix: String): GenS[Label] = {
    State { st =>
      val c = st.counter + 1
      (st.copy(counter = c), Label(s"_${prefix}_$c"))
    }
  }

  private def label(): GenS[String] = {
    State { st =>
      val c = st.counter + 1
      (st.copy(counter = c), s"L$c")
    }
  }

  private def addFunc(fn: FunctionDef): GenS[Unit] =
    State.modify(st => st.copy(funcs = st.funcs :+ fn))

  private def addExtern(symbol: String): GenS[Unit] =
    State.modify(st => st.copy(externs = st.externs + symbol))

  private def addString(value: String): GenS[Label] = {
    State { st =>
      val c = st.counter + 1
      val label = Label(s"Lstr$c")
      (st.copy(counter = c, strings = st.strings :+ StringDef(label, value)), label)
    }
  }

  private def liftS[A](sa: GenS[A]): Gen[A] = ReaderT.liftF(sa)
  private def pure[A](a: A): Gen[A] = ReaderT.pure[GenS, Env, A](a)
  private val ask: Gen[Env] = ReaderT.ask[GenS, Env]

  private def sequenceExprCode(values: Seq[Gen[Code]]): Gen[List[Code]] =
    values.foldRight(pure[List[Code]](Nil)) { (value, acc) =>
      value.flatMap(code => acc.map(code :: _))
    }

  private def loadImm32(n: Int): Code = {
    if (n >= 0 && n <= 0xFFFF) code(ins("mov", r(w0), imm(n)))
    else {
      val low  = n & 0xFFFF
      val high = (n >>> 16) & 0xFFFF
      val first = code(ins("movz", r(w0), imm(low)))
      if (high != 0) first ++ code(ins("movk", r(w0), imm(high), Shift("lsl", 16))) else first
    }
  }

  private def loadImm64(n: Long): Code = {
    val parts = (0 until 4).map(i => (n >>> (i * 16)) & 0xFFFFL)
    val firstIdx = parts.indexWhere(_ != 0)
    if (firstIdx < 0) code(ins("mov", r(x0), imm(0)))
    else {
      val first = code(ins("movz", r(x0), imm(parts(firstIdx)), Shift("lsl", firstIdx * 16)))
      val rest = parts.zipWithIndex.collect {
        case (part, idx) if idx != firstIdx && part != 0 => code(ins("movk", r(x0), imm(part), Shift("lsl", idx * 16)))
      }
      first ++ Code.concat(rest)
    }
  }

  private def loadNum(value: String, typeName: String): Code = typeName match {
    case "i64" | "u64" | "isize" | "usize" => loadImm64(java.lang.Long.parseUnsignedLong(value))
    case "f32" => loadImm32(java.lang.Float.floatToRawIntBits(value.toFloat))
    case "f64" => loadImm64(java.lang.Double.doubleToRawLongBits(value.toDouble))
    case _ => loadImm32(java.lang.Integer.parseUnsignedInt(value))
  }

  private def varLookup(name: String, env: Env): Code = {
    val found = env.values.foldLeft(Option.empty[(Binding, Int)]) {
      case (some @ Some(_), _) => some
      case (None, binding @ HeapBinding(`name`)) => Some(binding -> env.values.takeWhile(_ ne binding).count(_.isInstanceOf[HeapBinding]))
      case (None, binding @ StackBinding(`name`, _)) => Some(binding -> 0)
      case (None, _) => None
    }
    found match {
      case Some((StackBinding(_, offset), _)) =>
        code(ins("ldr", r(x0), mem(x29, offset)))
      case Some((HeapBinding(_), idx)) =>
        code(ins("ldr", r(x0), mem(x29, -16))) ++
          Code.concat(List.fill(idx)(code(ins("ldr", r(x0), mem(x0, 8))))) ++
          code(ins("ldr", r(x0), mem(x0)))
      case None =>
        env.directFunctions.get(name) match {
          case Some(label) => closureAllocNoEnv(label)
          case None => sys.error(s"Unbound variable: $name")
        }
    }
  }

  @tailrec
  private def primitiveName(t: TypeRec[Type]): Option[String] = t.project match {
    case AST.Primitive(name) => Some(name)
    case AST.TypeApp(function, _) => primitiveName(function)
    case _ => None
  }

  private def is64BitScalar(name: String): Boolean =
    Set("i64", "u64", "isize", "usize", BuiltinTypes.CString, BuiltinTypes.VoidPtr, BuiltinTypes.Ptr).contains(name)

  private def isUnsigned(name: String): Boolean =
    Set("u8", "u16", "u32", "u64", "usize").contains(name)

  private def scalarReg(prefix: String, index: Int): Reg = Reg(s"$prefix$index")

  private def binInstr(op: BinOps, operandType: TypeRec[Type]): Code = primitiveName(operandType) match {
    case Some("f32") =>
      op match {
        case BinOps.Add => code(ins("fmov", r(s1), r(w1)), ins("fmov", r(s0), r(w0)), ins("fadd", r(s0), r(s1), r(s0)), ins("fmov", r(w0), r(s0)))
        case BinOps.Sub => code(ins("fmov", r(s1), r(w1)), ins("fmov", r(s0), r(w0)), ins("fsub", r(s0), r(s1), r(s0)), ins("fmov", r(w0), r(s0)))
        case BinOps.Mul => code(ins("fmov", r(s1), r(w1)), ins("fmov", r(s0), r(w0)), ins("fmul", r(s0), r(s1), r(s0)), ins("fmov", r(w0), r(s0)))
        case BinOps.Div => code(ins("fmov", r(s1), r(w1)), ins("fmov", r(s0), r(w0)), ins("fdiv", r(s0), r(s1), r(s0)), ins("fmov", r(w0), r(s0)))
        case BinOps.Mod => sys.error("Modulo must be implemented by the standard library")
        case BinOps.Eq  => code(ins("fmov", r(s1), r(w1)), ins("fmov", r(s0), r(w0)), ins("fcmp", r(s1), r(s0)), ins("cset", r(w0), Condition("eq")))
        case BinOps.Neq => code(ins("fmov", r(s1), r(w1)), ins("fmov", r(s0), r(w0)), ins("fcmp", r(s1), r(s0)), ins("cset", r(w0), Condition("ne")))
        case BinOps.Lt  => code(ins("fmov", r(s1), r(w1)), ins("fmov", r(s0), r(w0)), ins("fcmp", r(s1), r(s0)), ins("cset", r(w0), Condition("lt")))
        case BinOps.Leq => code(ins("fmov", r(s1), r(w1)), ins("fmov", r(s0), r(w0)), ins("fcmp", r(s1), r(s0)), ins("cset", r(w0), Condition("le")))
        case BinOps.Gt  => code(ins("fmov", r(s1), r(w1)), ins("fmov", r(s0), r(w0)), ins("fcmp", r(s1), r(s0)), ins("cset", r(w0), Condition("gt")))
        case BinOps.Geq => code(ins("fmov", r(s1), r(w1)), ins("fmov", r(s0), r(w0)), ins("fcmp", r(s1), r(s0)), ins("cset", r(w0), Condition("ge")))
        case BinOps.And => code(ins("and", r(w0), r(w1), r(w0)))
        case BinOps.Or  => code(ins("orr", r(w0), r(w1), r(w0)))
        case BinOps.Xor => code(ins("eor", r(w0), r(w1), r(w0)))
        case BinOps.ShortAnd | BinOps.ShortOr => sys.error(s"Short-circuit operator $op must be lowered by operator resolution")
      }
    case Some("f64") =>
      op match {
        case BinOps.Add => code(ins("fmov", r(d1), r(x1)), ins("fmov", r(d0), r(x0)), ins("fadd", r(d0), r(d1), r(d0)), ins("fmov", r(x0), r(d0)))
        case BinOps.Sub => code(ins("fmov", r(d1), r(x1)), ins("fmov", r(d0), r(x0)), ins("fsub", r(d0), r(d1), r(d0)), ins("fmov", r(x0), r(d0)))
        case BinOps.Mul => code(ins("fmov", r(d1), r(x1)), ins("fmov", r(d0), r(x0)), ins("fmul", r(d0), r(d1), r(d0)), ins("fmov", r(x0), r(d0)))
        case BinOps.Div => code(ins("fmov", r(d1), r(x1)), ins("fmov", r(d0), r(x0)), ins("fdiv", r(d0), r(d1), r(d0)), ins("fmov", r(x0), r(d0)))
        case BinOps.Mod => sys.error("Modulo must be implemented by the standard library")
        case BinOps.Eq  => code(ins("fmov", r(d1), r(x1)), ins("fmov", r(d0), r(x0)), ins("fcmp", r(d1), r(d0)), ins("cset", r(w0), Condition("eq")))
        case BinOps.Neq => code(ins("fmov", r(d1), r(x1)), ins("fmov", r(d0), r(x0)), ins("fcmp", r(d1), r(d0)), ins("cset", r(w0), Condition("ne")))
        case BinOps.Lt  => code(ins("fmov", r(d1), r(x1)), ins("fmov", r(d0), r(x0)), ins("fcmp", r(d1), r(d0)), ins("cset", r(w0), Condition("lt")))
        case BinOps.Leq => code(ins("fmov", r(d1), r(x1)), ins("fmov", r(d0), r(x0)), ins("fcmp", r(d1), r(d0)), ins("cset", r(w0), Condition("le")))
        case BinOps.Gt  => code(ins("fmov", r(d1), r(x1)), ins("fmov", r(d0), r(x0)), ins("fcmp", r(d1), r(d0)), ins("cset", r(w0), Condition("gt")))
        case BinOps.Geq => code(ins("fmov", r(d1), r(x1)), ins("fmov", r(d0), r(x0)), ins("fcmp", r(d1), r(d0)), ins("cset", r(w0), Condition("ge")))
        case BinOps.And => code(ins("and", r(x0), r(x1), r(x0)))
        case BinOps.Or  => code(ins("orr", r(x0), r(x1), r(x0)))
        case BinOps.Xor => code(ins("eor", r(x0), r(x1), r(x0)))
        case BinOps.ShortAnd | BinOps.ShortOr => sys.error(s"Short-circuit operator $op must be lowered by operator resolution")
      }
    case Some(name) =>
      val p = if (is64BitScalar(name)) "x" else "w"
      val r0 = scalarReg(p, 0)
      val r1 = scalarReg(p, 1)
      val signedCond = Map(BinOps.Lt -> "lt", BinOps.Leq -> "le", BinOps.Gt -> "gt", BinOps.Geq -> "ge")
      val unsignedCond = Map(BinOps.Lt -> "lo", BinOps.Leq -> "ls", BinOps.Gt -> "hi", BinOps.Geq -> "hs")
      op match {
        case BinOps.Add => code(ins("add", r(r0), r(r1), r(r0)))
        case BinOps.Sub => code(ins("sub", r(r0), r(r1), r(r0)))
        case BinOps.Mul => code(ins("mul", r(r0), r(r1), r(r0)))
        case BinOps.Div => code(ins(if (isUnsigned(name)) "udiv" else "sdiv", r(r0), r(r1), r(r0)))
        case BinOps.Mod => sys.error("Modulo must be implemented by the standard library")
        case BinOps.Eq  => code(ins("cmp", r(r1), r(r0)), ins("cset", r(w0), Condition("eq")))
        case BinOps.Neq => code(ins("cmp", r(r1), r(r0)), ins("cset", r(w0), Condition("ne")))
        case BinOps.And => code(ins("and", r(r0), r(r1), r(r0)))
        case BinOps.Or  => code(ins("orr", r(r0), r(r1), r(r0)))
        case BinOps.Xor => code(ins("eor", r(r0), r(r1), r(r0)))
        case BinOps.ShortAnd | BinOps.ShortOr => sys.error(s"Short-circuit operator $op must be lowered by operator resolution")
        case cmp        => code(ins("cmp", r(r1), r(r0)), ins("cset", r(w0), Condition(if (isUnsigned(name)) unsignedCond(cmp) else signedCond(cmp))))
      }
    case None => sys.error(s"Unsupported binary operand type: ${operandType.show}")
  }

  private def asmString(value: String): String =
    value.flatMap {
      case '"' => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case '\u0000' => "\\0"
      case ch => ch.toString
    }

  private def function(label: Label, body: Code): FunctionDef =
    FunctionDef(label, code(Blank, Directive("p2align", Vector("2")), LabelDef(label)) ++ body)

  private def liftedFn(label: Label, body: Code): FunctionDef =
    function(label,
      code(
        ins("stp", r(x29), r(x30), pre(sp, -16)),
        ins("mov", r(x29), r(sp)),
        ins("sub", r(sp), r(sp), imm(16)),
        ins("stp", r(x0), r(x1), pre(sp, -16)),
        ins("mov", r(x0), imm(16)),
        ins("bl", LabelOperand(Label("_malloc"))),
        ins("mov", r(x9), r(x0)),
        ins("ldp", r(x0), r(x1), post(sp, 16)),
        ins("str", r(x1), mem(x9)),
        ins("ldr", r(x10), mem(x0, 8)),
        ins("str", r(x10), mem(x9, 8)),
        ins("str", r(x9), mem(x29, -16))
      ) ++ body ++ epilogue
    )

  private case class ForeignSig(args: Vector[TypeRec[Type]], returnType: TypeRec[Type])
  private sealed trait ForeignArgSource
  private case class SavedEnvArg(envIndex: Int) extends ForeignArgSource
  private case object SavedCurrentArg extends ForeignArgSource

  private def destructForeignSig(t: TypeRec[Type]): Option[ForeignSig] = {
    @tailrec
    def loop(current: TypeRec[Type], args: Vector[TypeRec[Type]]): ForeignSig = destructArrow(current) match {
      case Some((from, to)) => loop(to, args :+ from)
      case None => ForeignSig(args, current)
    }
    val sig = loop(t, Vector.empty)
    Option.when(sig.args.nonEmpty)(sig)
  }

  private def foreignArgLocations(args: Vector[TypeRec[Type]]): Vector[(TypeRec[Type], Reg)] = {
    val (_, _, locations) = args.foldLeft((0, 0, Vector.empty[(TypeRec[Type], Reg)])) {
      case ((intCount, floatCount, acc), t) =>
        primitiveName(t) match {
          case Some("f32") =>
            if (floatCount >= 8) sys.error("Foreign functions with more than 8 floating-point arguments are not supported")
            (intCount, floatCount + 1, acc :+ (t -> scalarReg("s", floatCount)))
          case Some("f64") =>
            if (floatCount >= 8) sys.error("Foreign functions with more than 8 floating-point arguments are not supported")
            (intCount, floatCount + 1, acc :+ (t -> scalarReg("d", floatCount)))
          case _ =>
            if (intCount >= 8) sys.error("Foreign functions with more than 8 integer/pointer arguments are not supported")
            (intCount + 1, floatCount, acc :+ (t -> scalarReg("x", intCount)))
        }
    }
    locations
  }

  private def loadSavedForeignArg(source: ForeignArgSource): Code = source match {
    case SavedCurrentArg =>
      code(ins("ldr", r(x10), mem(x29, -32)))
    case SavedEnvArg(envIndex) =>
      val hops = code(ins("ldr", r(x9), mem(x29, -16)), ins("ldr", r(x9), mem(x9, 8))) ++
        Code.concat(List.fill(envIndex)(code(ins("ldr", r(x9), mem(x9, 8)))))
      hops ++ code(ins("ldr", r(x10), mem(x9)))
  }

  private def moveForeignArg(source: ForeignArgSource, t: TypeRec[Type], target: Reg): Code =
    loadSavedForeignArg(source) ++ (primitiveName(t) match {
      case Some("f32") => code(ins("fmov", r(target), r(w10)))
      case Some("f64") => code(ins("fmov", r(target), r(x10)))
      case _ => code(ins("mov", r(target), r(x10)))
    })

  private def foreignReturnMove(t: TypeRec[Type]): Code = primitiveName(t) match {
    case Some("f32") => code(ins("fmov", r(w0), r(s0)))
    case Some("f64") => code(ins("fmov", r(x0), r(d0)))
    case _ => Code.empty
  }

  private def foreignCaptureWrapper(label: Label, nextLabel: Label): FunctionDef =
    function(label,
      code(
        ins("stp", r(x29), r(x30), pre(sp, -16)),
        ins("mov", r(x29), r(sp)),
        ins("sub", r(sp), r(sp), imm(16)),
        ins("stp", r(x0), r(x1), pre(sp, -16)),
        ins("mov", r(x0), imm(16)),
        ins("bl", LabelOperand(Label("_malloc"))),
        ins("mov", r(x9), r(x0)),
        ins("ldp", r(x0), r(x1), post(sp, 16)),
        ins("str", r(x1), mem(x9)),
        ins("ldr", r(x10), mem(x0, 8)),
        ins("str", r(x10), mem(x9, 8)),
        ins("str", r(x9), mem(x29, -16))
      ) ++ closureAlloc(nextLabel) ++ epilogue
    )

  private def foreignCallWrapper(label: Label, symbol: String, args: Vector[TypeRec[Type]], returnType: TypeRec[Type]): FunctionDef = {
    val previousCount = args.length - 1
    val sources = args.indices.map { argIndex =>
      if (argIndex == previousCount) SavedCurrentArg else SavedEnvArg(previousCount - 1 - argIndex)
    }.toVector
    val moves = foreignArgLocations(args).zip(sources).map { case ((t, target), source) => moveForeignArg(source, t, target) }
    function(label,
      code(
        ins("stp", r(x29), r(x30), pre(sp, -16)),
        ins("mov", r(x29), r(sp)),
        ins("str", r(x0), pre(sp, -16)),
        ins("str", r(x1), pre(sp, -16))
      ) ++ Code.concat(moves) ++ code(
        ins("bl", LabelOperand(Label(s"_$symbol")))
      ) ++ foreignReturnMove(returnType) ++ code(
        ins("mov", r(sp), r(x29)),
        ins("ldp", r(x29), r(x30), post(sp, 16)),
        ins("ret")
      )
    )
  }

  private def foreignWrappers(labels: Vector[Label], symbol: String, sig: ForeignSig): Vector[FunctionDef] = {
    val captureWrappers = labels.dropRight(1).zip(labels.tail).map { case (label, nextLabel) => foreignCaptureWrapper(label, nextLabel) }
    captureWrappers :+ foreignCallWrapper(labels.last, symbol, sig.args, sig.returnType)
  }

  private def closureAlloc(label: Label): Code = code(
    ins("mov", r(x0), imm(16)),
    ins("bl", LabelOperand(Label("_malloc"))),
    ins("adrp", r(x9), Page(label)),
    ins("add", r(x9), r(x9), PageOff(label)),
    ins("str", r(x9), mem(x0)),
    ins("ldr", r(x9), mem(x29, -16)),
    ins("str", r(x9), mem(x0, 8))
  )

  private def closureAllocNoEnv(label: Label): Code = code(
    ins("mov", r(x0), imm(16)),
    ins("bl", LabelOperand(Label("_malloc"))),
    ins("adrp", r(x9), Page(label)),
    ins("add", r(x9), r(x9), PageOff(label)),
    ins("str", r(x9), mem(x0)),
    ins("mov", r(x9), imm(0)),
    ins("str", r(x9), mem(x0, 8))
  )

  private val epilogue: Code = code(
    ins("mov", r(sp), r(x29)),
    ins("ldp", r(x29), r(x30), post(sp, 16)),
    ins("ret")
  )

  private def directFn(label: Label, param: String, body: Code): FunctionDef =
    function(label,
      code(
        ins("stp", r(x29), r(x30), pre(sp, -16)),
        ins("mov", r(x29), r(sp)),
        ins("sub", r(sp), r(sp), imm(16)),
        ins("str", r(x1), mem(x29, -16))
      ) ++ body ++ epilogue
    )

  @tailrec
  private def isFunctionType(t: TypeRec[Type]): Boolean = t.project match {
    case AST.Arrow(_, _) => true
    case AST.ForAll(_, _, body) => isFunctionType(body)
    case _ => false
  }

  private val appSeq: Code = code(
    ins("mov", r(x1), r(x0)),
    ins("ldr", r(x0), post(sp, 16)),
    ins("ldr", r(x16), mem(x0)),
    ins("blr", r(x16))
  )

  private def mainFunction(body: Code): FunctionDef =
    FunctionDef(Label("_main"),
      code(
        Directive("text"),
        Directive("globl", Vector("_main")),
        Directive("p2align", Vector("2")),
        LabelDef(Label("_main")),
        ins("stp", r(x29), r(x30), pre(sp, -16)),
        ins("mov", r(x29), r(sp)),
        ins("sub", r(sp), r(sp), imm(16)),
        ins("mov", r(x0), imm(0)),
        ins("str", r(x0), mem(x29, -16))
      ) ++ body ++ epilogue
    )

  private val genAlg: HCofreeParaAlgebra[AST, TypeAnn, GenCode] = [x] => (ann, node) => node match {
    case AST.Num(v, t) => pure(loadNum(v, t))
    case AST.Char(v) => pure(loadImm32(v.toInt))
    case AST.StringLit(v) => for {
      lbl <- liftS(addString(v))
    } yield code(
      ins("adrp", r(x0), Page(lbl)),
      ins("add", r(x0), r(x0), PageOff(lbl))
    )
    case AST.Bool(v) => pure(loadImm32(if (v) 1 else 0))
    case AST.UnitLit() => pure(loadImm32(0))

    case AST.Block(discarded, result) => for {
      discardedCode <- sequenceExprCode(discarded.map(_.code))
      resultCode <- result match {
        case Some(expr) => expr.code
        case None => pure(loadImm32(0))
      }
    } yield Code.concat(discardedCode) ++ resultCode

    case AST.UnaryOp(UnaryOps.Neg, body) =>
      body.code.map(_ ++ (primitiveName(typeOf(body.original)) match {
        case Some("f32") => code(ins("fmov", r(s0), r(w0)), ins("fneg", r(s0), r(s0)), ins("fmov", r(w0), r(s0)))
        case Some("f64") => code(ins("fmov", r(d0), r(x0)), ins("fneg", r(d0), r(d0)), ins("fmov", r(x0), r(d0)))
        case Some(name) if is64BitScalar(name) => code(ins("neg", r(x0), r(x0)))
        case _ => code(ins("neg", r(w0), r(w0)))
      }))
    case AST.UnaryOp(UnaryOps.Not, body) =>
      body.code.map(_ ++ code(
        ins("cmp", r(w0), imm(0)),
        ins("cset", r(w0), Condition("eq"))
      ))

    case AST.BinOp(op, l, rExpr) => for {
      lc <- l.code
      rc <- rExpr.code
    } yield lc ++ code(ins("str", r(x0), pre(sp, -16))) ++ rc ++ code(ins("ldr", r(x1), post(sp, 16))) ++ binInstr(op, typeOf(l.original))

    case AST.Intrinsic(IntrinsicOps.BinOp(op, _), Seq(l, rExpr)) => for {
      lc <- l.code
      rc <- rExpr.code
    } yield lc ++ code(ins("str", r(x0), pre(sp, -16))) ++ rc ++ code(ins("ldr", r(x1), post(sp, 16))) ++ binInstr(op, typeOf(l.original))

    case AST.Intrinsic(IntrinsicOps.UnaryOp(UnaryOps.Neg, _), Seq(body)) =>
      body.code.map(_ ++ (primitiveName(typeOf(body.original)) match {
        case Some("f32") => code(ins("fmov", r(s0), r(w0)), ins("fneg", r(s0), r(s0)), ins("fmov", r(w0), r(s0)))
        case Some("f64") => code(ins("fmov", r(d0), r(x0)), ins("fneg", r(d0), r(d0)), ins("fmov", r(x0), r(d0)))
        case Some(name) if is64BitScalar(name) => code(ins("neg", r(x0), r(x0)))
        case _ => code(ins("neg", r(w0), r(w0)))
      }))

    case AST.Intrinsic(IntrinsicOps.UnaryOp(UnaryOps.Not, _), Seq(body)) =>
      body.code.map(_ ++ code(
        ins("cmp", r(w0), imm(0)),
        ins("cset", r(w0), Condition("eq"))
      ))

    case AST.Intrinsic(op, args) =>
      pure(sys.error(s"Unsupported intrinsic $op with ${args.length} arguments"))

    case AST.Var(Variable(name)) =>
      ask.map(env => varLookup(name, env))

    case AST.Foreign(Variable(name), _) => for {
      sig = ann match {
        case ExprAnn(t) => destructForeignSig(t).getOrElse(sys.error(s"Foreign must have function type: ${t.show}"))
        case _ => sys.error("Foreign node must have expression annotation")
      }
      labels <- liftS(sig.args.indices.toList.traverse(_ => fresh(s"foreign_$name")))
      _ <- liftS(addExtern(name))
      _ <- liftS(State.modify[GenState](st => st.copy(funcs = st.funcs ++ foreignWrappers(labels.toVector, name, sig))))
    } yield closureAllocNoEnv(labels.head)

    case AST.Abs(Variable(param), _, body) => for {
      lbl <- liftS(fresh("lambda"))
      bc  <- body.code.local((env: Env) => env.withHeap(param))
      _   <- liftS(addFunc(liftedFn(lbl, bc)))
    } yield closureAlloc(lbl)

    case AST.TyAbs(_, _, body) => body.code

    case AST.Let(Variable(param), _, value, body) => for {
      vc <- value.code
      bc <- body.code.local((env: Env) => env.withHeap(param))
    } yield vc ++ bindValueToHeap ++ bc ++ restoreHeap

    case AST.LetRec(Variable(param), _, value, body) => for {
      vc <- value.code.local((env: Env) => env.withHeap(param))
      bc <- body.code.local((env: Env) => env.withHeap(param))
    } yield allocateRecCell ++ vc ++ code(
      ins("ldr", r(x9), mem(x29, -16)),
      ins("str", r(x0), mem(x9))
    ) ++ bc ++ restoreHeap

    case AST.TypeLet(_, _, _, body) => body.code
    case AST.DataLet(_, _, _, _, _) => pure(sys.error("DataLet must be Church encoded before code generation"))
    case AST.Match(_, _) => pure(sys.error("Match must be Church encoded before code generation"))
    case AST.Fold(_, _, _) => pure(sys.error("Fold must be Church encoded before code generation"))

    case AST.App(f, a) =>
      @tailrec
      def directName(expr: TypeRec[Expr]): Option[String] = expr.project match {
        case AST.Var(Variable(name)) => Some(name)
        case AST.TyApp(function, _) => directName(function)
        case _ => None
      }
      ask.flatMap { env =>
        directName(f.original).flatMap(env.directFunctions.get) match {
          case Some(label) =>
            a.code.map(ac => ac ++ code(ins("mov", r(x1), r(x0)), ins("bl", LabelOperand(label))))
          case None => for {
            fc <- f.code
            ac <- a.code
          } yield fc ++ code(ins("str", r(x0), pre(sp, -16))) ++ ac ++ appSeq
        }
      }

    case AST.TyApp(f, _) => f.code

    case AST.If(c, t, e) => for {
      l <- liftS(label())
      cond <- c.code
      trB <- t.code
      elB <- e.code
    } yield cond ++ code(
      ins("cbnz", r(w0), LabelOperand(Label(s".$l" + "true"))),
      ins("b", LabelOperand(Label(s".$l" + "false"))),
      LabelDef(Label(s".$l" + "true"))
    ) ++ trB ++ code(
      ins("b", LabelOperand(Label(s".$l" + "end"))),
      LabelDef(Label(s".$l" + "false"))
    ) ++ elB ++ code(
      LabelDef(Label(s".$l" + "end"))
    )

    case AST.Program(_) => pure(Code.empty)

    case AST.TopLet(_, _, _) => pure(Code.empty)
    case AST.TopLetRec(_, _, _) => pure(Code.empty)
    case AST.TopImport(_) => pure(Code.empty)
    case AST.TopType(_, _, _) => pure(Code.empty)
    case AST.TopData(_, _, _, _) => pure(Code.empty)

    case AST.Primitive(_) => pure(Code.empty)
    case AST.TypeVar(_) => pure(Code.empty)
    case AST.Arrow(_, _)  => pure(Code.empty)
    case AST.ForAll(_, _, _) => pure(Code.empty)
    case AST.TypeApp(_, _) => pure(Code.empty)
    case AST.TypeAbs(_, _, _) => pure(Code.empty)
  }

  private val bindValueToHeap: Code = code(
    ins("ldr", r(x9), mem(x29, -16)),
    ins("str", r(x9), pre(sp, -16)),
    ins("str", r(x0), pre(sp, -16)),
    ins("mov", r(x0), imm(16)),
    ins("bl", LabelOperand(Label("_malloc"))),
    ins("ldr", r(x1), post(sp, 16)),
    ins("str", r(x1), mem(x0)),
    ins("ldr", r(x9), post(sp, 16)),
    ins("str", r(x9), mem(x0, 8)),
    ins("str", r(x0), mem(x29, -16))
  )

  private val restoreHeap: Code = code(
    ins("mov", r(x9), r(x0)),
    ins("ldr", r(x0), mem(x29, -16)),
    ins("ldr", r(x0), mem(x0, 8)),
    ins("str", r(x0), mem(x29, -16)),
    ins("mov", r(x0), r(x9))
  )

  private val allocateRecCell: Code = code(
    ins("ldr", r(x9), mem(x29, -16)),
    ins("str", r(x9), pre(sp, -16)),
    ins("mov", r(x0), imm(16)),
    ins("bl", LabelOperand(Label("_malloc"))),
    ins("ldr", r(x9), post(sp, 16)),
    ins("str", r(x9), mem(x0, 8)),
    ins("mov", r(x9), imm(0)),
    ins("str", r(x9), mem(x0)),
    ins("str", r(x0), mem(x29, -16))
  )

  @tailrec
  private def unwrapTypeAbs(expr: TypeRec[Expr]): TypeRec[Expr] = expr.project match {
    case AST.TyAbs(_, _, body) => unwrapTypeAbs(body)
    case _ => expr
  }

  private def directFunctionValue(name: String, value: TypeRec[Expr]): Option[(String, TypeRec[Expr])] = unwrapTypeAbs(value).project match {
    case AST.Abs(Variable(param), _, body) if freeVars(value).subsetOf(Set(Variable(name))) && !isFunctionType(typeOf(body)) =>
      Some(param -> body)
    case _ => None
  }

  private def genDecl(decl: TypeRec[Decl], env: Env): GenS[(Code, Env)] = decl.project match {
    case AST.TopLet(Variable(param), _, value) =>
      value.paraAnn(genAlg).run(env).map(code => (code ++ bindValueToHeap, env.withHeap(param)))

    case AST.TopLetRec(Variable(param), _, value) =>
      directFunctionValue(param, value) match {
        case Some((arg, body)) => for {
          label <- fresh(s"direct_$param")
          valueEnv = env.withDirectFunction(param, label)
          bodyCode <- body.paraAnn(genAlg).run(valueEnv.withStack(arg, -16))
          _ <- addFunc(directFn(label, arg, bodyCode))
        } yield {
          (Code.empty, valueEnv)
        }
        case None =>
          val valueEnv = env.withHeap(param)
          value.paraAnn(genAlg).run(valueEnv).map { vc =>
            val recCode = allocateRecCell ++ vc ++ code(
              ins("ldr", r(x9), mem(x29, -16)),
              ins("str", r(x0), mem(x9))
            )
            (recCode, valueEnv)
          }
      }

    case AST.TopImport(_) => State.pure((Code.empty, env))
    case AST.TopType(_, _, _) => State.pure((Code.empty, env))
    case AST.TopData(_, _, _, _) => State.pure((Code.empty, env))
  }

  private def topLevelName(decl: TypeRec[Decl]): Option[Variable] = decl.project match {
    case AST.TopLet(variable, _, _) => Some(variable)
    case AST.TopLetRec(variable, _, _) => Some(variable)
    case AST.TopImport(_) | AST.TopType(_, _, _) | AST.TopData(_, _, _, _) => None
  }

  private def topLevelFreeVars(decl: TypeRec[Decl]): Set[Variable] = decl.project match {
    case AST.TopLet(_, _, value) => freeVars(value)
    case AST.TopLetRec(variable, _, value) => freeVars(value) - variable
    case AST.TopImport(_) | AST.TopType(_, _, _) | AST.TopData(_, _, _, _) => Set.empty
  }

  private def genProgram(decls: Seq[TypeRec[Decl]]): Gen[AssemblyProgram] = ReaderT { initialEnv =>
    decls.foldLeft(State.pure[GenState, (Code, Env)]((Code.empty, initialEnv))) { (acc, decl) =>
      acc.flatMap { case (code, env) =>
        genDecl(decl, env).map { case (declCode, nextEnv) => (code ++ declCode, nextEnv) }
      }
    }.flatMap { case (declCode, env) =>
      val main = appT(intTypeT, varrType(Variable("main"), arrowT(unitTypeT, intTypeT)), unitLitT(unitTypeT))
      main.paraAnn(genAlg).run(env).map { mainCode =>
        AssemblyProgram(Set.empty, Vector.empty, mainFunction(declCode ++ mainCode), Vector.empty)
      }
    }
  }

  private def render(program: AssemblyProgram): String = {
    val externs = program.externs.toList.sorted.map(sym => Directive("extern", Vector(s"_$sym")).render)
    val strings =
      if (program.strings.isEmpty) Nil
      else Directive("section", Vector("__TEXT,__cstring,cstring_literals")).render ::
        program.strings.toList.flatMap { case StringDef(label, value) =>
          List(LabelDef(label).render, Directive("asciz", Vector(s""""${asmString(value)}"""")).render)
        }
    val body = program.main.body.lines.map(_.render) ++ program.functions.flatMap(_.body.lines.map(_.render))
    (externs ++ strings ++ body).mkString("\n") + "\n"
  }

  def generateProgram(prog: TypeRec[AST.Program.type]): AssemblyProgram = {
    val gen = prog.project match {
      case AST.Program(decls) => genProgram(decls)
    }
    val (finalSt, program) = gen.run(Env.empty).run(GenState(Vector.empty, 0, Set.empty, Vector.empty)).value
    program.copy(externs = finalSt.externs, strings = finalSt.strings, functions = finalSt.funcs)
  }

  def generate(prog: TypeRec[AST.Program.type]): String = render(generateProgram(prog))
}
