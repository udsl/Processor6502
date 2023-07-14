package com.udsl.processor6502.assembler.version1

import com.udsl.processor6502.assembler.version1.ParserV1
import com.udsl.processor6502.assembler.AssemblerToken

import scala.collection.mutable.ListBuffer

class TokenisedLine(val sourceText: String, val lineNumber: Int):
  val tokens = new ListBuffer[AssemblerToken]()

  def +(other: AssemblerToken): tokens.type = {
    tokens += other
  }

  def hasSyntaxError: Boolean =
    ParserV1.sytaxErrorList.exists(s => s.lineNumber == lineNumber)

  override def toString: String =
    var str: String = s"Line number: $lineNumber has ${tokens.length} tokens,  Source: '$sourceText', Tokens: \n"
    for t <- tokens do
      str += s"\t$t - ${t.predictedAddressingModes}\n"
    str

object TokenisedLine:
  def apply(line: UntokenisedLine) : TokenisedLine =
    new TokenisedLine(line.sourceText, line.lineNumber)

  def apply(line: String) : TokenisedLine =
    new TokenisedLine(line, -1)


class UntokenisedLine( val lineNumber: Int, val sourceText: String):
  override def toString: String =
    s"lineNumber: $lineNumber, source: '$sourceText'"



