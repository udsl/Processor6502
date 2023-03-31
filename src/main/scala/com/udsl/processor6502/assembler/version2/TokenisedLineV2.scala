package com.udsl.processor6502.assembler.version2

class TokenisedLineV2(val sourceLine: String, val lineNumber: Int):
  var tokens: Seq[TokenV2] = List[TokenV2]()

  def add(token: TokenV2): Unit =
    tokens = tokens :+ token
object TokenisedLineV2:
  def apply(sourceLine: String, lineNum: Int): TokenisedLineV2 =
    new TokenisedLineV2(sourceLine, lineNum)
    



