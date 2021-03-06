package com.udsl.processor6502.assembler

import com.udsl.processor6502.assembler.SyntaxErrorToken
import com.udsl.processor6502.assembler.AssemblerToken
import scala.collection.mutable.ListBuffer

class TokenisedLine(val sourceLine: UntokenisedLine):
  val tokens = new ListBuffer[AssemblerToken]()

  def +(other: AssemblerToken) = {
    tokens += other
  }

  def hasSyntaxError: Boolean =
    tokens.exists(_ match {
      case SyntaxErrorToken(_, _) => true;
      case _ => false
    })

  
  override def toString =
    var str: String = s"Line number: ${sourceLine.lineNumber} has ${tokens.length} tokens,  Source: '${sourceLine.source}', Tokens: \n"
    for t <- tokens do
      str += s"\t${t} - ${t.predictedAddressingModes}\n"
    str

object TokenisedLine:
  def apply(line: UntokenisedLine) : TokenisedLine =
    new TokenisedLine(line)

  def apply(line: String) : TokenisedLine =
    new TokenisedLine(UntokenisedLine( -1, line))


class UntokenisedLine( val lineNumber: Int, val source: String):
  override def toString(): String =
    s"lineNumber: ${lineNumber}, source: '${source}'"



