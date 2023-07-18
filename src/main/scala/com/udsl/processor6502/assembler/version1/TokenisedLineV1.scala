package com.udsl.processor6502.assembler.version1

import com.udsl.processor6502.assembler.TokenisedLine
import com.udsl.processor6502.assembler.version1.ParserV1

import scala.collection.mutable.ListBuffer

class TokenisedLineV1(sourceText: String, lineNumber: Int) extends TokenisedLine(sourceText, lineNumber):
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


object TokenisedLineV1:
  def apply(line: UntokenisedLine) : TokenisedLineV1 =
    new TokenisedLineV1(line.sourceText, line.lineNumber)

  def apply(line: String) : TokenisedLineV1 =
    new TokenisedLineV1(line, -1)


class UntokenisedLine( val lineNumber: Int, val sourceText: String):
  override def toString: String =
    s"lineNumber: $lineNumber, source: '$sourceText'"



