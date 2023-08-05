package com.udsl.processor6502.assembler.version1

import com.udsl.processor6502.assembler.{AssemblyData, SourceLine, TokenisedLine}

import scala.collection.mutable.ListBuffer

class TokenisedLineV1(override val source: SourceLine) extends TokenisedLine(source):

  val tokens = new ListBuffer[AssemblerToken]()

  def +(other: AssemblerToken): tokens.type = {
    tokens += other
  }

  def hasSyntaxError: Boolean =
    AssemblyData.sytaxErrorList.exists(s => s.lineNumber == source.lineNum)

  override def toString: String =
    var str: String = s"Line number: ${source.lineNum} has ${tokens.length} tokens,  Source: '${source.text}', Tokens: \n"
    for t <- tokens do
      str += s"\t$t - ${t.predictedAddressingModes}\n"
    str


object TokenisedLineV1:
  def apply(line: SourceLine) : TokenisedLineV1 =
    new TokenisedLineV1(line)




