package com.udsl.processor6502.assemble2.test

import com.udsl.processor6502.assembler.AssemblyData
import com.udsl.processor6502.assembler2.{BlankLineToken, CommandToken, CommentLineToken, InstructionToken, LabelToken, LineCommentToken, Token2}
import org.scalatest.Assertions.fail

object TestUtils :

  /**
   * verify that the result list only contains the same tokens as the expected list
   * irrespective of order.
   */

  def verifyTokens(expected: List[Token2], result: List[Token2]): Boolean =
    val exp: List[Token2] = expected.sortBy(f => f.name)
    val res: List[Token2] = result.sortBy(f => f.name)
    expected.length == result.length && exp.equals(res)  //expected.sortBy(f => f.name) == result.sortBy(f => f.name)


  def validateTokens(res: Seq[Token2], line: String): Unit =
    res.foreach(t => {
      t.name match {
        case "BlankLineToken" => validateBlankLine(t.asInstanceOf[BlankLineToken])
        case "CommentLineToken" => validateCommentLineToken(t.asInstanceOf[CommentLineToken], line)
        case "LineCommentToken" => validateLineCommentToken(t.asInstanceOf[LineCommentToken], line)
        case "LabelToken" => validateLabelToken(t.asInstanceOf[LabelToken], line)
        case "CommandToken" => validateCommandToken(t.asInstanceOf[CommandToken], line)
        case "InstructionToken" => validateInstructionToken(t.asInstanceOf[InstructionToken], line)
        case "SytaxError" =>
      }
    })
  
  def validateBlankLine(token: BlankLineToken): Unit =
    if !token.tokenText.isBlank then fail("BlankLineToken text not blank.")
  
  def validateCommentLineToken(token: CommentLineToken, line: String): Unit =
    if !line.head.equals(';') then fail("CommentLineToken source text does not start with ';'.")
    if !token.tokenText.isBlank then fail("CommentLineToken text is blank.")
  
  def validateLineCommentToken(token: LineCommentToken, line: String): Unit =
    val semicolonAt = line.indexOf(';')
    if semicolonAt < 0 then fail("LineCommentToken text does not have a semicolon.")
    val lineComment = line.substring(semicolonAt)
    if !token.tokenText.equals(lineComment) then fail("LineCommentToken text is incorrect.")
  
  def validateLabelToken(token: LabelToken, line: String): Unit =
    val colonAt = line.indexOf(':')
    if colonAt < 0 then fail("LabelToken text does not have a colon.")
    val lableText = line.substring(0, colonAt)
    if !token.tokenText.equals(lableText) then fail("LabelToken label text is incorrect.")
    if !AssemblyData.labelIsDefinedV2(lableText) then fail("LabelToken label is not defined.")
  
  def validateCommandToken(token: CommandToken, line: String): Unit =
    if !line.toUpperCase().startsWith(token.command) then fail("CommandToken source does not start with this token's command.")
  
  def validateInstructionToken(token: InstructionToken, line: String): Unit =
    val allFields = line.toUpperCase.split(" ")
    if !allFields.contains(token.instruction.name()) then fail("CommandToken source line does not contain with this token's instruction.")
