package com.udsl.processor6502.assembler

/**
 *
 * @param text a var so it can be updated as parts are used to create tokens
 * @param lineNum the number of the line in the source file
 */
class SourceLine(var text: String, val lineNum: Int){
  val OriginalText: String = text
  override def toString: String = s"lineNum: $lineNum -> '$OriginalText'"

  def removeComment(): String =
    if text.contains(";") then
      text = text.substring(0, text.indexOf(";")).trim;
    text

  def currentText: String = text
}

object SourceLine:
  def apply(text: String, lineNum: Int) =
    new SourceLine(text, lineNum)

  def apply() =
    new SourceLine("NONE", -99)