package com.udsl.processor6502.assembler

class Token( val typeOfToken: AssemblerTokenType.Value, var tokenValue: String ) {
  def intValue : Int =
    try
      return tokenValue.toInt
    catch
      case _ => return -1;


  def message : String =
    tokenValue

  override def toString =
    s"TokenType: ${typeOfToken}, Value: '${tokenValue} "

}

object Token {
  def apply(typeOfToken: AssemblerTokenType.Value) : Token =
    new Token(typeOfToken, "")

  def apply(typeOfToken: AssemblerTokenType.Value, tokenValue: String) : Token =
    new Token(typeOfToken, tokenValue)
}

object AssemblerTokenType extends Enumeration {
  val BlankLineToken = Value("BlankLineToken")
  val CommentLineToken = Value("CommentLineToken")
  val LineComment = Value("LineComment")
  val NoneCommentLine = Value("NoneCommentLine")
  val LabelToken = Value("LabelToken")
  val CommandToken = Value("CommandToken") 
  val InstructionToken = Value("InstructionToken")
  val SyntaxErrorToken = Value("SyntaxErrorToken")
}

case class AssemblerTokenType()
