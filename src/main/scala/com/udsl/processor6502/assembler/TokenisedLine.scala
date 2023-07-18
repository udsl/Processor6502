package com.udsl.processor6502.assembler

trait TokenisedLine(val _sourceText: String, val _lineNumber: Int):
  def sourceText: String = _sourceText
  def lineNumber: Int = _lineNumber

