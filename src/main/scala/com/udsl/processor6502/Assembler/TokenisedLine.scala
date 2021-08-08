package com.udsl.processor6502.Assembler

import scala.collection.mutable.ListBuffer

class TokenisedLine(val sourceLine: UntokenisedLine) {
  var tokens = new ListBuffer[Token]()

  def +(other: Token) = {
    tokens += other
  }

  override def toString = {
    var str: String = s"Line number: ${sourceLine.lineNumber}\n Source: '${sourceLine.source}', Token count: ${tokens.length}\nTokens: "
    for( t <- tokens) {
      str += s"${t.typeOfToken.toString} "
    }
    str += "\n"
    str
  }

}

object TokenisedLine{
  def apply(line: UntokenisedLine) : TokenisedLine = {
    new TokenisedLine(line)
  }
}

class UntokenisedLine( val lineNumber: Int, val source: String){
  override def toString: String = {
    s"lineNumber: ${lineNumber}, source: '${source}'"
  }
}

