package com.udsl.processor6502.assembler

class Token( val typeOfToken: AssemblerTokenType.Value, var tokenValue: String ) {
  def intValue : Int = {
    tokenValue.toInt
  }
}

object Token {
  def apply(typeOfToken: AssemblerTokenType.Value) : Token = {
    new Token(typeOfToken, "")
  }
}

object AssemblerTokenType extends Enumeration {
  val BlankLineToken = Value("BlankLineToken")
  val CommentLineToken = Value("CommentLineToken")
  val NoneCommentLine = Value("NoneCommentLine")
  val LabelToken = Value("LabelToken")
}

case class AssemblerTokenType()
