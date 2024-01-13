package com.udsl.processor6502.assembler

trait ErrorListener:
  def doNotify(err: ErrorRecord): Unit

