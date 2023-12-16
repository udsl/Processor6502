package com.udsl.processor6502.assembler

trait SyntaxErrorListener:
  def doNotify(syn: SyntaxErrorRecord): Unit

