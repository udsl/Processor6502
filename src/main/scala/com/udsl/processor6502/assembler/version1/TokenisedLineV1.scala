package com.udsl.processor6502.assembler.version1

import com.udsl.processor6502.assembler.{TokenisedLine, TokenisedLineBaseData}
import com.udsl.processor6502.assembler.version1.ParserV1

import scala.collection.mutable.ListBuffer

class TokenisedLineV1(baseData: TokenisedLineBaseData) extends TokenisedLine():
  override def sourceText: String = baseData.source
  override def lineNumber : Int = baseData.lineNum

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
    new TokenisedLineV1(TokenisedLineBaseData(line.sourceText, line.lineNumber))

  def apply(line: String) : TokenisedLineV1 =
    new TokenisedLineV1(TokenisedLineBaseData(line, -1))


class UntokenisedLine( val lineNumber: Int, val sourceText: String):
  override def toString: String =
    s"lineNumber: $lineNumber, source: '$sourceText'"



