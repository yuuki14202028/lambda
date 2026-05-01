package com.yuuki14202028

import AST.Program

sealed trait TypeAnn[I]

case object ProgramAnn extends TypeAnn[Program.type]
case class ExprAnn(t: Rec[Type]) extends TypeAnn[Expr]
case object TypeAnn extends TypeAnn[Type]
