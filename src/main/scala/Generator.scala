package com.yuuki14202028

import cats.data.{ReaderT, State}
import cats.syntax.all.*

object Generator {
  type Env = List[String]

  case class GenState(funcs: Vector[List[String]], counter: Int, externs: Set[String], strings: Vector[(String, String)])
  type GenS[A] = State[GenState, A]
  type GenR[A] = ReaderT[GenS, Env, A]
  type Gen[I]  = GenR[List[String]]
  private type Child[I] = (TypeRec[I], Gen[I])

  extension [I](self: Child[I]) {
    private def original: TypeRec[I] = self._1
    private def code: Gen[I] = self._2
  }

  private def fresh(prefix: String): GenS[String] = {
    State { st =>
      val c = st.counter + 1
      (st.copy(counter = c), s"_${prefix}_$c")
    }
  }

  private def label(): GenS[String] = {
    State { st =>
      val c = st.counter + 1
      (st.copy(counter = c), s"L$c")
    }
  }

  private def addFunc(fn: List[String]): GenS[Unit] = {
    State.modify(st => st.copy(funcs = st.funcs :+ fn))
  }

  private def addExtern(symbol: String): GenS[Unit] = {
    State.modify(st => st.copy(externs = st.externs + symbol))
  }

  private def addString(value: String): GenS[String] = {
    State { st =>
      val c = st.counter + 1
      val label = s"Lstr$c"
      (st.copy(counter = c, strings = st.strings :+ (label -> value)), label)
    }
  }

  private def liftS[A](sa: GenS[A]): GenR[A] = ReaderT.liftF(sa)
  private def pure[A](a: A): GenR[A] = ReaderT.pure[GenS, Env, A](a)
  private val ask: GenR[Env] = ReaderT.ask[GenS, Env]

  private def sequenceExprCode(values: List[Gen[Expr]]): GenR[List[List[String]]] =
    values.foldRight(pure[List[List[String]]](Nil)) { (value, acc) =>
      value.flatMap(code => acc.map(code :: _))
    }

  private def loadImm32(n: Int): List[String] = {
    if (n >= 0 && n <= 0xFFFF) List(s"    mov w0, #$n")
    else {
      val low  = n & 0xFFFF
      val high = (n >>> 16) & 0xFFFF
      val first = s"    movz w0, #$low"
      if (high != 0) List(first, s"    movk w0, #$high, lsl #16") else List(first)
    }
  }

  private def loadImm64(n: Long): List[String] = {
    val parts = (0 until 4).map(i => (n >>> (i * 16)) & 0xFFFFL)
    val firstIdx = parts.indexWhere(_ != 0)
    if (firstIdx < 0) List("    mov x0, #0")
    else {
      val first = s"    movz x0, #${parts(firstIdx)}, lsl #${firstIdx * 16}"
      first :: parts.zipWithIndex.toList.collect {
        case (part, idx) if idx != firstIdx && part != 0 => s"    movk x0, #$part, lsl #${idx * 16}"
      }
    }
  }

  private def loadNum(value: String, typeName: String): List[String] = typeName match {
    case "i64" | "u64" | "isize" | "usize" => loadImm64(java.lang.Long.parseUnsignedLong(value))
    case "f32" => loadImm32(java.lang.Float.floatToRawIntBits(value.toFloat))
    case "f64" => loadImm64(java.lang.Double.doubleToRawLongBits(value.toDouble))
    case _ => loadImm32(java.lang.Integer.parseUnsignedInt(value))
  }

  private def varLookup(name: String, env: Env): List[String] = {
    val idx = env.indexOf(name)
    if (idx < 0) sys.error(s"Unbound variable: $name")
    "    ldr x0, [x29, #-16]" ::
      List.fill(idx)("    ldr x0, [x0, #8]") :::
      List("    ldr x0, [x0]")
  }

  private def primitiveName(t: TypeRec[Type]): Option[String] = t.projectT match {
    case AST.Primitive(name) => Some(name)
    case AST.TypeApp(function, _) => primitiveName(function)
    case _ => None
  }

  private def is64BitScalar(name: String): Boolean =
    Set("i64", "u64", "isize", "usize", BuiltinTypes.CString, BuiltinTypes.VoidPtr, BuiltinTypes.Ptr).contains(name)

  private def isUnsigned(name: String): Boolean =
    Set("u8", "u16", "u32", "u64", "usize").contains(name)

  private def binInstr(op: BinOps, operandType: TypeRec[Type]): List[String] = primitiveName(operandType) match {
    case Some("f32") =>
      val opInstr = op match {
        case BinOps.Add => List("    fmov s1, w1", "    fmov s0, w0", "    fadd s0, s1, s0", "    fmov w0, s0")
        case BinOps.Sub => List("    fmov s1, w1", "    fmov s0, w0", "    fsub s0, s1, s0", "    fmov w0, s0")
        case BinOps.Mul => List("    fmov s1, w1", "    fmov s0, w0", "    fmul s0, s1, s0", "    fmov w0, s0")
        case BinOps.Div => List("    fmov s1, w1", "    fmov s0, w0", "    fdiv s0, s1, s0", "    fmov w0, s0")
        case BinOps.Eq  => List("    fmov s1, w1", "    fmov s0, w0", "    fcmp s1, s0", "    cset w0, eq")
        case BinOps.Neq => List("    fmov s1, w1", "    fmov s0, w0", "    fcmp s1, s0", "    cset w0, ne")
        case BinOps.Lt  => List("    fmov s1, w1", "    fmov s0, w0", "    fcmp s1, s0", "    cset w0, lt")
        case BinOps.Leq => List("    fmov s1, w1", "    fmov s0, w0", "    fcmp s1, s0", "    cset w0, le")
        case BinOps.Gt  => List("    fmov s1, w1", "    fmov s0, w0", "    fcmp s1, s0", "    cset w0, gt")
        case BinOps.Geq => List("    fmov s1, w1", "    fmov s0, w0", "    fcmp s1, s0", "    cset w0, ge")
      }
      opInstr
    case Some("f64") =>
      op match {
        case BinOps.Add => List("    fmov d1, x1", "    fmov d0, x0", "    fadd d0, d1, d0", "    fmov x0, d0")
        case BinOps.Sub => List("    fmov d1, x1", "    fmov d0, x0", "    fsub d0, d1, d0", "    fmov x0, d0")
        case BinOps.Mul => List("    fmov d1, x1", "    fmov d0, x0", "    fmul d0, d1, d0", "    fmov x0, d0")
        case BinOps.Div => List("    fmov d1, x1", "    fmov d0, x0", "    fdiv d0, d1, d0", "    fmov x0, d0")
        case BinOps.Eq  => List("    fmov d1, x1", "    fmov d0, x0", "    fcmp d1, d0", "    cset w0, eq")
        case BinOps.Neq => List("    fmov d1, x1", "    fmov d0, x0", "    fcmp d1, d0", "    cset w0, ne")
        case BinOps.Lt  => List("    fmov d1, x1", "    fmov d0, x0", "    fcmp d1, d0", "    cset w0, lt")
        case BinOps.Leq => List("    fmov d1, x1", "    fmov d0, x0", "    fcmp d1, d0", "    cset w0, le")
        case BinOps.Gt  => List("    fmov d1, x1", "    fmov d0, x0", "    fcmp d1, d0", "    cset w0, gt")
        case BinOps.Geq => List("    fmov d1, x1", "    fmov d0, x0", "    fcmp d1, d0", "    cset w0, ge")
      }
    case Some(name) =>
      val r = if (is64BitScalar(name)) "x" else "w"
      val signedCond = Map(BinOps.Lt -> "lt", BinOps.Leq -> "le", BinOps.Gt -> "gt", BinOps.Geq -> "ge")
      val unsignedCond = Map(BinOps.Lt -> "lo", BinOps.Leq -> "ls", BinOps.Gt -> "hi", BinOps.Geq -> "hs")
      op match {
        case BinOps.Add => List(s"    add  ${r}0, ${r}1, ${r}0")
        case BinOps.Sub => List(s"    sub  ${r}0, ${r}1, ${r}0")
        case BinOps.Mul => List(s"    mul  ${r}0, ${r}1, ${r}0")
        case BinOps.Div => List(s"    ${if (isUnsigned(name)) "udiv" else "sdiv"} ${r}0, ${r}1, ${r}0")
        case BinOps.Eq  => List(s"    cmp  ${r}1, ${r}0", "    cset w0, eq")
        case BinOps.Neq => List(s"    cmp  ${r}1, ${r}0", "    cset w0, ne")
        case cmp        => List(s"    cmp  ${r}1, ${r}0", s"    cset w0, ${if (isUnsigned(name)) unsignedCond(cmp) else signedCond(cmp)}")
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

  private def liftedFn(label: String, body: List[String]): List[String] = {
    List(
      "", ".p2align 2", s"$label:",
      "    stp x29, x30, [sp, #-16]!",
      "    mov x29, sp",
      "    sub sp, sp, #16",
      "    stp x0, x1, [sp, #-16]!",
      "    mov x0, #16",
      "    bl _malloc",
      "    mov x9, x0",
      "    ldp x0, x1, [sp], #16",
      "    str x1, [x9]",
      "    ldr x10, [x0, #8]",
      "    str x10, [x9, #8]",
      "    str x9, [x29, #-16]"
    ) ++ body ++ List(
      "    mov sp, x29",
      "    ldp x29, x30, [sp], #16",
      "    ret"
    )
  }

  private def foreignArgMove(t: TypeRec[Type]): List[String] = primitiveName(t) match {
    case Some("f32") => List("    fmov s0, w1")
    case Some("f64") => List("    fmov d0, x1")
    case _ => List("    mov x0, x1")
  }

  private def foreignReturnMove(t: TypeRec[Type]): List[String] = primitiveName(t) match {
    case Some("f32") => List("    fmov w0, s0")
    case Some("f64") => List("    fmov x0, d0")
    case _ => Nil
  }

  private def foreignWrapper(label: String, symbol: String, argType: TypeRec[Type], returnType: TypeRec[Type]): List[String] =
    List(
      "", ".p2align 2", s"$label:",
      "    stp x29, x30, [sp, #-16]!",
      "    mov x29, sp"
    ) ++ foreignArgMove(argType) ++ List(
      s"    bl _$symbol"
    ) ++ foreignReturnMove(returnType) ++ List(
      "    ldp x29, x30, [sp], #16",
      "    ret"
    )

  private def closureAlloc(label: String): List[String] = List(
    "    mov x0, #16",
    "    bl _malloc",
    s"    adrp x9, $label@PAGE",
    s"    add x9, x9, $label@PAGEOFF",
    "    str x9, [x0]",
    "    ldr x9, [x29, #-16]",
    "    str x9, [x0, #8]"
  )

  private val appSeq: List[String] = List(
    "    mov x1, x0",
    "    ldr x0, [sp], #16",
    "    ldr x16, [x0]",
    "    blr x16"
  )

  private def mainWrapper(body: List[String]): List[String] = List(
    ".text", ".globl _main", ".p2align 2", "_main:",
    "    stp x29, x30, [sp, #-16]!",
    "    mov x29, sp",
    "    sub sp, sp, #16",
    "    mov x0, #0",
    "    str x0, [x29, #-16]"
  ) ++ body ++ List(
    "    mov sp, x29",
    "    ldp x29, x30, [sp], #16",
    "    ret"
  )

  private val genAlg: HCofreeParaAlgebra[AST, TypeAnn, Gen] = [x] => (ann, node) => node match {
    case AST.Num(v, t) => pure(loadNum(v, t))
    case AST.Char(v) => pure(loadImm32(v.toInt))
    case AST.StringLit(v) => for {
      lbl <- liftS(addString(v))
    } yield List(
      s"    adrp x0, $lbl@PAGE",
      s"    add x0, x0, $lbl@PAGEOFF"
    )
    case AST.Bool(v) => pure(loadImm32(if (v) 1 else 0))
    case AST.UnitLit() => pure(loadImm32(0))

    case AST.Block(discarded, result) => for {
      discardedCode <- sequenceExprCode(discarded.toList.map(_.code))
      resultCode <- result match {
        case Some(expr) => expr.code
        case None => pure(loadImm32(0))
      }
    } yield discardedCode.flatten ++ resultCode

    case AST.UnaryOp(UnaryOps.Neg, body) => {
      body.code.map(_ ++ (primitiveName(typeOf(body.original)) match {
        case Some("f32") => List("    fmov s0, w0", "    fneg s0, s0", "    fmov w0, s0")
        case Some("f64") => List("    fmov d0, x0", "    fneg d0, d0", "    fmov x0, d0")
        case Some(name) if is64BitScalar(name) => List("    neg x0, x0")
        case _ => List("    neg w0, w0")
      }))
    }
    case AST.UnaryOp(UnaryOps.Not, body) => {
      body.code.map(_ ++ List(
        "    cmp w0, #0",
        "    cset w0, eq"
      ))
    }

    case AST.BinOp(op, l, r) => for {
      lc <- l.code
      str = List("    str x0, [sp, #-16]!")
      rc <- r.code
      ldr = "    ldr x1, [sp], #16" :: binInstr(op, typeOf(l.original))
    } yield lc ++ str ++ rc ++ ldr

    case AST.Var(Variable(name)) =>
      ask.map(env => varLookup(name, env))

    case AST.Foreign(Variable(name), _) => for {
      lbl <- liftS(fresh(s"foreign_$name"))
      (argType, returnType) = ann match {
        case ExprAnn(t) => destructArrow(t).getOrElse(sys.error(s"Foreign must have function type: ${t.show}"))
        case _ => sys.error("Foreign node must have expression annotation")
      }
      _   <- liftS(addExtern(name))
      _   <- liftS(addFunc(foreignWrapper(lbl, name, argType, returnType)))
    } yield List(
      "    mov x0, #16",
      "    bl _malloc",
      s"    adrp x9, $lbl@PAGE",
      s"    add x9, x9, $lbl@PAGEOFF",
      "    str x9, [x0]",
      "    mov x9, #0",
      "    str x9, [x0, #8]"
    )

    case AST.Abs(Variable(param), types, body) => for {
      lbl <- liftS(fresh("lambda"))
      bc  <- body.code.local((env: Env) => param :: env)
      _   <- liftS(addFunc(liftedFn(lbl, bc)))
    } yield closureAlloc(lbl)

    case AST.TyAbs(_, body) => body.code

    case AST.Let(Variable(param), types, value, body) => for {
      vc <- value.code
      bc <- body.code.local((env: Env) => param :: env)
    } yield vc ++ List(
      "    ldr x9, [x29, #-16]",
      "    str x9, [sp, #-16]!",
      "    str x0, [sp, #-16]!",
      "    mov x0, #16",
      "    bl _malloc",
      "    ldr x1, [sp], #16",
      "    str x1, [x0]",
      "    ldr x9, [sp], #16",
      "    str x9, [x0, #8]",
      "    str x0, [x29, #-16]"
    ) ++ bc ++ List(
      "    mov x9, x0",
      "    ldr x0, [x29, #-16]",
      "    ldr x0, [x0, #8]",
      "    str x0, [x29, #-16]",
      "    mov x0, x9"
    )

    case AST.LetRec(Variable(param), types, value, body) => for {
      vc <- value.code.local((env: Env) => param :: env)
      bc <- body.code.local((env: Env) => param :: env)
    } yield List(
      "    ldr x9, [x29, #-16]",
      "    str x9, [sp, #-16]!",
      "    mov x0, #16",
      "    bl _malloc",
      "    ldr x9, [sp], #16",
      "    str x9, [x0, #8]",
      "    mov x9, #0",
      "    str x9, [x0]",
      "    str x0, [x29, #-16]"
    ) ++ vc ++ List(
      "    ldr x9, [x29, #-16]",
      "    str x0, [x9]"
    ) ++ bc ++ List(
      "    mov x9, x0",
      "    ldr x0, [x29, #-16]",
      "    ldr x0, [x0, #8]",
      "    str x0, [x29, #-16]",
      "    mov x0, x9"
    )

    case AST.TypeLet(_, _, _, body) => body.code
    case AST.DataLet(_, _, _, _) => pure(sys.error("DataLet must be Church encoded before code generation"))
    case AST.Match(_, _) => pure(sys.error("Match must be Church encoded before code generation"))

    case AST.App(f, a) => for {
      fc <- f.code
      str = List("    str x0, [sp, #-16]!")
      ac <- a.code
    } yield fc ++ str ++ ac ++ appSeq

    case AST.TyApp(f, _) => f.code

    case AST.If(c, t, e) => for {
      l <- liftS(label())
      cond <- c.code
      ct = List(
        s"    cbnz w0, .${l}true",
        s"    b .${l}false",
        s".${l}true:"
      )
      trB <- t.code
      te = List(
        s"    b .${l}end",
        s".${l}false:"
      )
      elB <- e.code
      end = List(
        s".${l}end:"
      )
    } yield cond ++ ct ++ trB ++ te ++ elB ++ end

    case AST.Program(_) => {
      pure(Nil)
    }

    case AST.TopLet(_, _, _) => pure(Nil)
    case AST.TopLetRec(_, _, _) => pure(Nil)
    case AST.TopType(_, _, _) => pure(Nil)
    case AST.TopData(_, _, _) => pure(Nil)

    case AST.Primitive(_) => pure(Nil)
    case AST.TypeVar(_) => pure(Nil)
    case AST.Arrow(_, _)  => pure(Nil)
    case AST.ForAll(_, _) => pure(Nil)
    case AST.TypeApp(_, _) => pure(Nil)
  }

  private def genExpr(expr: TypeRec[Expr]): Gen[Expr] = expr.paraAnn(genAlg)

  private def bindTop(valueCode: List[String]): List[String] =
    valueCode ++ List(
      "    ldr x9, [x29, #-16]",
      "    str x9, [sp, #-16]!",
      "    str x0, [sp, #-16]!",
      "    mov x0, #16",
      "    bl _malloc",
      "    ldr x1, [sp], #16",
      "    str x1, [x0]",
      "    ldr x9, [sp], #16",
      "    str x9, [x0, #8]",
      "    str x0, [x29, #-16]"
    )

  private def genDecl(decl: TypeRec[Decl], env: Env): GenS[(List[String], Env)] = decl.tail match {
    case AST.TopLet(Variable(param), _, value) =>
      genExpr(value).run(env).map(code => (bindTop(code), param :: env))

    case AST.TopLetRec(Variable(param), _, value) =>
      val valueEnv = param :: env
      genExpr(value).run(valueEnv).map { vc =>
        val code = List(
          "    ldr x9, [x29, #-16]",
          "    str x9, [sp, #-16]!",
          "    mov x0, #16",
          "    bl _malloc",
          "    ldr x9, [sp], #16",
          "    str x9, [x0, #8]",
          "    mov x9, #0",
          "    str x9, [x0]",
          "    str x0, [x29, #-16]"
        ) ++ vc ++ List(
          "    ldr x9, [x29, #-16]",
          "    str x0, [x9]"
        )
        (code, valueEnv)
      }

    case AST.TopType(_, _, _) => State.pure((Nil, env))
    case AST.TopData(_, _, _) => State.pure((Nil, env))
  }

  private def genProgram(decls: Seq[TypeRec[Decl]]): GenR[List[String]] = ReaderT { initialEnv =>
    decls.foldLeft(State.pure[GenState, (List[String], Env)]((Nil, initialEnv))) { (acc, decl) =>
      acc.flatMap { case (code, env) =>
        genDecl(decl, env).map { case (declCode, nextEnv) => (code ++ declCode, nextEnv) }
      }
    }.flatMap { case (declCode, env) =>
      val main = appT(intTypeT, varrType(Variable("main"), arrowT(unitTypeT, intTypeT)), unitLitT(unitTypeT))
      genExpr(main).run(env).map(mainCode => mainWrapper(declCode ++ mainCode))
    }
  }

  def generate(prog: TypeRec[AST.Program.type]): String = {
    val gen = prog.tail match {
      case AST.Program(decls) => genProgram(decls)
    }
    val (finalSt, mainCode) = gen.run(Nil).run(GenState(Vector.empty, 0, Set.empty, Vector.empty)).value
    val externs = finalSt.externs.toList.sorted.map(sym => s".extern _$sym")
    val strings = if (finalSt.strings.isEmpty) Nil
      else ".section __TEXT,__cstring,cstring_literals" :: finalSt.strings.toList.flatMap {
        case (label, value) => List(s"$label:", s"""    .asciz "${asmString(value)}"""")
      }
    (externs ++ strings ++ mainCode ++ finalSt.funcs.toList.flatten).mkString("\n") + "\n"
  }
}
