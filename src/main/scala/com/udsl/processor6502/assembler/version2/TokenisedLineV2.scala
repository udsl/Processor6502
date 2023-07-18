package com.udsl.processor6502.assembler.version2
import com.udsl.processor6502.assembler.{TokenisedLine, TokenisedLineBaseData}

class TokenisedLineV2(baseData: TokenisedLineBaseData) extends TokenisedLine():
  override def sourceText: String = baseData.source
  override def lineNumber: Int = baseData.lineNum


  var tokens: Seq[TokenV2] = List[TokenV2]()

  def add(token: TokenV2): Unit =
    tokens = tokens :+ token

  def +(other: TokenV2): Seq[TokenV2] = {
    tokens :+ other
  }

object TokenisedLineV2:
  def apply(sourceText: String, lineNum: Int): TokenisedLineV2 =
    new TokenisedLineV2(TokenisedLineBaseData(sourceText, lineNum))

    



