package com.udsl.processor6502.assembler

class SourceLine(val text: String, val lineNum: Int){
  override def toString: String = s"lineNum: $lineNum -> '$text'"
}

object SourceLine:
  def apply(text: String, lineNum: Int) =
    new SourceLine(text, lineNum)

  def apply() =
    new SourceLine("NONE", -99)