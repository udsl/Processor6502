package com.udsl.processor6502.assembler.version2

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Utilities.{isLabel, numericValue}
import com.udsl.processor6502.assembler.AssemblyData
import com.udsl.processor6502.cpu.execution.AddressingMode
import com.udsl.processor6502.cpu.{CpuInstruction, CpuInstructions}

import scala.collection.mutable.ListBuffer


trait TokenV2 (val fields: Array[String] ) :
  val name: String

  def display : String = s"$name -> '${fields.mkString(", ")}'"
  def tokenText: String = ""

  private def canEqual(other: Any) = other.isInstanceOf[TokenV2]

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
    case that: TokenV2 =>
      that.canEqual(this) &&
        this.name == that.name &&
        fieldsAreEqual(that.fields)
    case _ => false
  }

case class BlankLineTokenV2(override val fields: Array[String] ) extends TokenV2(fields: Array[String] ):
  override val name: String = "BlankLineToken"
  override def toString: String = display

case class CommentLineTokenV2(override val fields: Array[String] ) extends TokenV2(fields: Array[String] ):
  override val name: String = "CommentLineToken"
  override def toString: String = display

case class LineCommentTokenV2 (comment: String, override val fields: Array[String]) extends TokenV2(fields: Array[String] ):
  override val name: String = "LineCommentToken"
  override def toString: String =  s"$name -> '$comment'"
  override def tokenText: String = comment

case class LabelTokenV2 (label: String, override val fields: Array[String]) extends TokenV2(fields: Array[String] ):
  override val name: String = "LabelToken"
  override def toString: String = s"$name -> '$label'"
  override def tokenText: String = label

case class CommandTokenV2 (command: String, override val fields: Array[String]) extends TokenV2(fields: Array[String] ):
  override val name: String = "CommandToken"
  override def toString: String = s"$name -> '$command'"
  override def tokenText: String = command

case class InstructionTokenV2 (mnemonic: String, override val fields: Array[String]) extends TokenV2(fields: Array[String] ) with StrictLogging:
  override val name: String = "InstructionToken"
  override def toString: String =  s"$name -> '$mnemonic'"
  override def tokenText: String = mnemonic

case class SytaxErrorTokenV2 (errortext: String, override val fields: Array[String]) extends TokenV2(fields: Array[String] ):
  override val name: String = "SytaxError"
  override def toString =
    s"$name: $errortext"


