package com.udsl.processor6502.assemblier2

class FirstPassResult(val tokenisedLine : TokenisedLine) :
  def lineNumber: Int = tokenisedLine.lineNumber
  def tokens: Seq[Token] = tokenisedLine.tokens


object FirstPassResult :
  def apply(source: TokenisedLine): FirstPassResult =
    new FirstPassResult(source)


class SecondPassResult(val firstPassResult : FirstPassResult) :
  def lineNumber: Int = firstPassResult.lineNumber


object SecondPassResult :
  def apply(source: FirstPassResult): SecondPassResult =
    new SecondPassResult(source)    