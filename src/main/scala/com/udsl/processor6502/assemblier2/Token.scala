package com.udsl.processor6502.assemblier2

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Utilities.{isLabel, numericValue}
import com.udsl.processor6502.cpu.{CpuInstruction, CpuInstructions}
import com.udsl.processor6502.cpu.execution.AddressingMode
import com.udsl.processor6502.assembler.AssemblyData

import scala.collection.mutable.ListBuffer


trait Token (val fields: Array[String] ) :
  val name: String

  def display : String = s"$name -> '${fields.mkString(", ")}'"
  def tokenText: String = ""

  private def canEqual(other: Any) = other.isInstanceOf[Token]

  def fieldsAreEqual(thatFields:Array[String]): Boolean =
    if fields.length != thatFields.length then // not the same length must be false
      false
    else if fields.length == 0 then // no lenth so no fields to check
      true
    else // Check the fields
      fields.forall(p => {
        thatFields.contains(p)
      })

  override def equals(other: Any): Boolean = other match {
    case that: Token =>
      that.canEqual(this) &&
        this.name == that.name &&
        fieldsAreEqual(that.fields)
    case _ => false
  }

case class BlankLineToken(override val fields: Array[String] ) extends Token(fields: Array[String] ):
  override val name: String = "BlankLineToken"
  override def toString: String = display

case class CommentLineToken(override val fields: Array[String] ) extends Token(fields: Array[String] ):
  override val name: String = "CommentLineToken"
  override def toString: String = display

case class LineCommentToken (comment: String, override val fields: Array[String]) extends Token(fields: Array[String] ):
  override val name: String = "LineCommentToken"
  override def toString: String =  s"$name -> '$comment'"
  override def tokenText: String = comment

case class LabelToken (label: String, override val fields: Array[String]) extends Token(fields: Array[String] ):
  override val name: String = "LabelToken"
  override def toString: String = s"$name -> '$label'"
  override def tokenText: String = label

case class CommandToken (command: String, override val fields: Array[String]) extends Token(fields: Array[String] ):
  override val name: String = "CommandToken"
  override def toString: String = s"$name -> '$command'"
  override def tokenText: String = command

case class InstructionToken (instruction: CpuInstruction, override val fields: Array[String]) extends Token(fields: Array[String] ) with StrictLogging:
  override val name: String = "InstructionToken"
  override def toString: String =  s"$name -> '$instruction'"
  override def tokenText: String = instruction.name()

case class SytaxErrorToken (errortext: String, override val fields: Array[String]) extends Token(fields: Array[String] ):
  override val name: String = "SytaxError"
  override def toString =
    s"$name: $errortext"


class TokenisedLine(val sourceLine: String, val lineNumber: Int):
  var tokens: Seq[Token] = List[Token]()

  def add(token: Token): Unit =
    tokens = tokens :+ token
object TokenisedLine:
  def apply(sourceLine: String, lineNum: Int): TokenisedLine =
    new TokenisedLine(sourceLine, lineNum)
    
object Tokeniser :
  def tockenise(text: String, lineNum: Int) : TokenisedLine =
    val tokenisedLine = TokenisedLine.apply(text, lineNum)

    val toTokenise = text.trim
    // Blank line
    if toTokenise == "" then
      tokenisedLine.add(BlankLineToken.apply(Array("")))
    // line comment
    else if toTokenise.head == ';' then
      tokenisedLine.add(CommentLineToken.apply(Array(toTokenise.tail)))
    else
      // Does this line have a comment, comment could have a ; in it so cant split
      val semicolonAt = toTokenise.indexOf(';')

      val beforeComment = if semicolonAt > 0 then
        // The comment is the sub string from semicolonAt otherwise it would have been a comment line above.
        // We acnt use split because the comment itself may contain a ';'
        tokenisedLine.add(LineCommentToken.apply(toTokenise.substring(semicolonAt).trim, Array()))
        toTokenise.substring(0, semicolonAt).trim
      else
        toTokenise

      // is it a command
      val beforeCommentSplit = beforeComment.split("\\s+")
      if !(beforeCommentSplit.head.toUpperCase() match {
        case "ADDR" | "BYT" | "WRD" | "ORIG" | "CLR" | "DEF" =>
          tokenisedLine.add(CommandToken.apply(beforeCommentSplit.head.toUpperCase(),
            if beforeCommentSplit.tail.length > 0 then Array(beforeCommentSplit.tail.mkString(" ")) else Array()))
          true
        case _ => false
      }) then
        // Dose this line have a label ie does the beforeCommentSplit.head end with ':'
        val ins: String = if beforeCommentSplit.head.endsWith(":") then
          val labelText = beforeCommentSplit.head.dropRight(1)
          if isLabel(labelText) then
            tokenisedLine.add(LabelToken.apply(labelText,
              if beforeCommentSplit.tail.length > 0 then Array(beforeCommentSplit.tail.mkString(" ")) else Array()))
            AssemblyData.addLabel(labelText)
          else
            tokenisedLine.add(SytaxErrorToken.apply(s"Bad label test ${beforeCommentSplit.head}", beforeCommentSplit))
          beforeCommentSplit.tail.mkString(" ")
        else
          beforeComment

        // is this line an instruction
        val fields = ins.trim.split("\\s+")
        val instruction: Option[CpuInstruction] = CpuInstructions.getInstruction(fields.head)
        if instruction.isDefined then
          tokenisedLine.add(InstructionToken.apply(instruction.get, fields.tail))
        else
          tokenisedLine.add(SytaxErrorToken.apply("instruction not found.", fields.tail))
    tokenisedLine


