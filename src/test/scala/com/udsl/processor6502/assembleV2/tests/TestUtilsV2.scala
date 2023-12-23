package com.udsl.processor6502.assembleV2.tests

import com.udsl.processor6502.assembler.{AssemblyData, LabelFactory}
import com.udsl.processor6502.assembler.version2.{BlankLineTokenV2, CommandTokenV2, CommentLineTokenV2, InstructionTokenV2, LabelTokenV2, LineCommentTokenV2, TokenV2}
import org.scalatest.Assertions.fail

object TestUtilsV2 :

  def verifyTokens(expected: List[TokenV2], result: List[TokenV2]): Boolean = {
    val sortedExpected: List[TokenV2] = expected.sortBy(_.name)
    val sortedResult: List[TokenV2] = result.sortBy(_.name)

    val differences = sortedExpected.diff(sortedResult)

    if (differences.nonEmpty) {
      differences.foreach(token => println(s"Unexpected result '$token' found"))
      return false
    }

    true
  }



  def validateTokens(res: Seq[TokenV2], line: String): Unit =
    res.foreach(t => {
      t.name match {
        case "BlankLineToken" => validateBlankLine(t.asInstanceOf[BlankLineTokenV2])
        case "CommentLineToken" => validateCommentLineToken(t.asInstanceOf[CommentLineTokenV2], line)
        case "LineCommentToken" => validateLineCommentToken(t.asInstanceOf[LineCommentTokenV2], line)
        case "LabelToken" => validateLabelToken(t.asInstanceOf[LabelTokenV2], line)
        case "CommandToken" => validateCommandToken(t.asInstanceOf[CommandTokenV2], line)
        case "InstructionToken" => validateInstructionToken(t.asInstanceOf[InstructionTokenV2], line)
        case "SytaxError" =>
      }
    })
  
  def validateBlankLine(token: BlankLineTokenV2): Unit =
    if !token.tokenText.isBlank then fail("BlankLineToken text not blank.")
  
  def validateCommentLineToken(token: CommentLineTokenV2, line: String): Unit =
    if !line.head.equals(';') then fail("CommentLineToken source text does not start with ';'.")
    if !token.tokenText.isBlank then fail("CommentLineToken text is blank.")
  
  def validateLineCommentToken(token: LineCommentTokenV2, line: String): Unit =
    val semicolonAt = line.indexOf(';')
    if semicolonAt < 0 then fail("LineCommentToken text does not have a semicolon.")
    val lineComment = line.substring(semicolonAt)
    if !token.tokenText.equals(lineComment) then fail("LineCommentToken text is incorrect.")
  
  def validateLabelToken(token: LabelTokenV2, line: String): Unit =
    val colonAt = line.indexOf(':')
    if colonAt < 0 then fail("LabelToken text does not have a colon.")
    val lableText = line.substring(0, colonAt)
    if !token.tokenText.equals(lableText) then fail("LabelToken label text is incorrect.")
    if !LabelFactory.labelIsDefined(lableText) then fail("LabelToken label is not defined.")
  
  def validateCommandToken(token: CommandTokenV2, line: String): Unit =
    if !line.toUpperCase().startsWith(token.command) then fail("CommandToken source does not start with this token's command.")
  
  def validateInstructionToken(token: InstructionTokenV2, line: String): Unit =
    val allFields = line.toUpperCase.split(" ")
    if !allFields.contains(token.tokenText) then fail("CommandToken source line does not contain with this token's instruction.")
