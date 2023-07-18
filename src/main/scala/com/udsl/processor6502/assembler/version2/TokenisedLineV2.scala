package com.udsl.processor6502.assembler.version2
import com.udsl.processor6502.assembler.TokenisedLine

class TokenisedLineV2(sourceText: String, lineNumber: Int) extends TokenisedLine(sourceText, lineNumber):
  var tokens: Seq[TokenV2] = List[TokenV2]()

  def add(token: TokenV2): Unit =
    tokens = tokens :+ token

  def +(other: TokenV2): Seq[TokenV2] = {
    tokens :+ other
  }

object TokenisedLineV2:
  def apply(sourceLine: String, lineNum: Int): TokenisedLineV2 =
    new TokenisedLineV2(sourceLine, lineNum)
    



