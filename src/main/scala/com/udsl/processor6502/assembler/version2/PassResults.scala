package com.udsl.processor6502.assembler.version2

import com.udsl.processor6502.assembler.version2.{TokenV2, TokenisedLineV2}

class FirstPassResult(val tokenisedLine : TokenisedLineV2) :
  def lineNumber: Int = tokenisedLine.source.lineNum
  def tokens: Seq[TokenV2] = tokenisedLine.tokens


object FirstPassResult :
  def apply(source: TokenisedLineV2): FirstPassResult =
    new FirstPassResult(source)


class SecondPassResult(val firstPassResult : FirstPassResult) :
  def lineNumber: Int = firstPassResult.lineNumber


object SecondPassResult :
  def apply(source: FirstPassResult): SecondPassResult =
    new SecondPassResult(source)    