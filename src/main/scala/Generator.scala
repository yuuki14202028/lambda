package com.yuuki14202028

import cats.data.{ReaderT, State}
import cats.syntax.all.*

object Generator {
  type Env = List[String]

  case class GenState(funcs: Vector[List[String]], counter: Int, externs: Set[String])
  type GenS[A] = State[GenState, A]
  type GenR[A] = ReaderT[GenS, Env, A]
  type Gen[I]  = GenR[List[String]]

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

  private def liftS[A](sa: GenS[A]): GenR[A] = ReaderT.liftF(sa)
  private def pure[A](a: A): GenR[A] = ReaderT.pure[GenS, Env, A](a)
  private val ask: GenR[Env] = ReaderT.ask[GenS, Env]

  private def loadImm(n: Int): List[String] = {
    if (n >= 0 && n <= 0xFFFF) List(s"    mov w0, #$n")
    else {
      val low  = n & 0xFFFF
      val high = (n >>> 16) & 0xFFFF
      val first = s"    movz w0, #$low"
      if (high != 0) List(first, s"    movk w0, #$high, lsl #16") else List(first)
    }
  }

  private def varLookup(name: String, env: Env): List[String] = {
    val idx = env.indexOf(name)
    if (idx < 0) sys.error(s"Unbound variable: $name")
    "    ldr x0, [x29, #-16]" ::
      List.fill(idx)("    ldr x0, [x0, #8]") :::
      List("    ldr x0, [x0]")
  }

  private def binInstr(op: BinOps): String = op match {
    case BinOps.Add => "    add  w0, w1, w0"
    case BinOps.Sub => "    sub  w0, w1, w0"
    case BinOps.Mul => "    mul  w0, w1, w0"
    case BinOps.Div => "    sdiv w0, w1, w0"
    case BinOps.Eq  => "    cmp  w1, w0\n    cset w0, eq"
    case BinOps.Neq => "    cmp  w1, w0\n    cset w0, ne"
    case BinOps.Lt  => "    cmp  w1, w0\n    cset w0, lt"
    case BinOps.Leq => "    cmp  w1, w0\n    cset w0, le"
    case BinOps.Gt  => "    cmp  w1, w0\n    cset w0, gt"
    case BinOps.Geq => "    cmp  w1, w0\n    cset w0, ge"
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

  private def foreignWrapper(label: String, symbol: String): List[String] = List(
    "", ".p2align 2", s"$label:",
    "    stp x29, x30, [sp, #-16]!",
    "    mov x29, sp",
    "    mov w0, w1",
    s"    bl _$symbol",
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

  private val genAlg: Algebra[AST, Gen] = [x] => (node: AST[Gen, x]) => node match {
    case AST.Num(v) => pure(loadImm(v))
    case AST.Char(v) => pure(loadImm(v.toInt))

    case AST.UnaryOp(UnaryOps.Neg, body) => {
      body.map(_ ++ List("    neg w0, w0"))
    }

    case AST.BinOp(op, l, r) => for {
      lc <- l
      str = List("    str x0, [sp, #-16]!")
      rc <- r
      ldr = List("    ldr x1, [sp], #16", binInstr(op))
    } yield lc ++ str ++ rc ++ ldr

    case AST.Var(Variable(name)) =>
      ask.map(env => varLookup(name, env))

    case AST.Foreign(Variable(name)) => for {
      lbl <- liftS(fresh(s"foreign_$name"))
      _   <- liftS(addExtern(name))
      _   <- liftS(addFunc(foreignWrapper(lbl, name)))
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
      bc  <- body.local((env: Env) => param :: env)
      _   <- liftS(addFunc(liftedFn(lbl, bc)))
    } yield closureAlloc(lbl)

    case AST.Let(Variable(param), types, value, body) => for {
      vc <- value
      bc <- body.local((env: Env) => param :: env)
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

    case AST.App(f, a) => for {
      fc <- f
      str = List("    str x0, [sp, #-16]!")
      ac <- a
    } yield fc ++ str ++ ac ++ appSeq

    case AST.If(c, t, e) => for {
      l <- liftS(label())
      cond <- c
      ct = List(
        s"    cbnz w0, .${l}true",
        s"    b .${l}false",
        s".${l}true:"
      )
      trB <- t
      te = List(
        s"    b .${l}end",
        s".${l}false:"
      )
      elB <- e
      end = List(
        s".${l}end:"
      )
    } yield cond ++ ct ++ trB ++ te ++ elB ++ end

    case AST.Program(seq) => {
      seq.toList.sequence.map(parts => mainWrapper(parts.flatten))
    }

    case AST.Primitive(_) => pure(Nil)
    case AST.Arrow(_, _)  => pure(Nil)
  }

  def generate(prog: Rec[AST.Program.type]): String = {
    val gen = prog.cata(genAlg)
    val (finalSt, mainCode) = gen.run(Nil).run(GenState(Vector.empty, 0, Set.empty)).value
    val externs = finalSt.externs.toList.sorted.map(sym => s".extern _$sym")
    (externs ++ mainCode ++ finalSt.funcs.toList.flatten).mkString("\n") + "\n"
  }
}
