package com.udsl.processor6502.assembler

trait TokenisedLine():
  def sourceText: String = ???
  def lineNumber: Int = ???

class TokenisedLineBaseData(val source: String, val lineNum: Int) :
  def sourceText: String = source
  def lineNumber: Int = lineNum

object TokenisedLineBaseData:
  def apply(source: String, lineNum: Int) =
    new TokenisedLineBaseData(source, lineNum)