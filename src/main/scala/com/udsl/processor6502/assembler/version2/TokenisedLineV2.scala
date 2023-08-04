package com.udsl.processor6502.assembler.version2

import com.udsl.processor6502.assembler.{SourceLine, TokenisedLine}

class TokenisedLineV2(val source: SourceLine) extends TokenisedLine(source):
  var tokens: Seq[TokenV2] = List[TokenV2]()

  def add(token: TokenV2): Unit =
    tokens = tokens :+ token

  def +(other: TokenV2): Seq[TokenV2] = {
    tokens :+ other
  }

object TokenisedLineV2:
  def apply(source: SourceLine): TokenisedLineV2 =
    new TokenisedLineV2(source)

    



