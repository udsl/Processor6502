package com.udsl.processor6502.assembler

import com.udsl.processor6502.assembler.AssemblerTokenType.SyntaxErrorToken

import scala.collection.mutable.ListBuffer

class TokenisedLine(val sourceLine: UntokenisedLine):
  val tokens = new ListBuffer[Token]()

  def +(other: Token) = {
    tokens += other
  }

  def hasSyntaxError: Boolean =
    tokens.exists(y => y.typeOfToken == SyntaxErrorToken)
  
  override def toString =
    var str: String = s"Line number: ${sourceLine.lineNumber} has ${tokens.length} tokens,  Source: '${sourceLine.source}', Tokens: \n"
    for t <- tokens do
      str += s"\t${t.typeOfToken.toString} - ${t.tokenVal.pridictedMode}\n"
    str

object TokenisedLine:
  def apply(line: UntokenisedLine) : TokenisedLine =
    new TokenisedLine(line)


class UntokenisedLine( val lineNumber: Int, val source: String):
  override def toString(): String =
    s"lineNumber: ${lineNumber}, source: '${source}'"



