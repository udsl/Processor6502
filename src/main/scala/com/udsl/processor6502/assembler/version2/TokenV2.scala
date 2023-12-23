package com.udsl.processor6502.assembler.version2

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Utilities.{isLabel, numericValue}
import com.udsl.processor6502.assembler.{AssemblyData, LabelFactory}
import com.udsl.processor6502.cpu.execution.AddressingMode
import com.udsl.processor6502.cpu.{CpuInstruction, CpuInstructions}

import scala.collection.mutable.ListBuffer

trait CommandValueType:
  def isValid: Boolean = false
  def isDefined: Boolean = false
  def value: Option[Int] = None

class CommandValueLabel(val label: String) extends CommandValueType:
  override def isDefined: Boolean =
    LabelFactory.labelIsDefined(label)

  override def value: Option[Int] =
    LabelFactory.labelValue(label)

class CommandValueNumeric(val num: Int) extends CommandValueType:
  override def isDefined: Boolean = true
  override def isValid: Boolean = true
  override def value: Option[Int] =
      Some(num)


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

  private var _commandValue: Option[CommandValueType] = None
  def hasValue: Boolean = _commandValue.isDefined
  def value: Int = _commandValue.get.value.get
  def commandValue: CommandValueType = _commandValue.get
  def commandValue_=(newValue: CommandValueType): Unit =
    _commandValue = Some(newValue)

case class InstructionTokenV2 (mnemonic: String, override val fields: Array[String]) extends TokenV2(fields: Array[String] ):
  override val name: String = "InstructionToken"
  override def toString: String =  s"$name -> '$mnemonic'"
  override def tokenText: String = mnemonic

  private var ins: Option[CpuInstruction] = None
  def instruction: CpuInstruction = ins.get

  private var addMode: Option[AddressingMode] = None
  def addressingMode: AddressingMode = addMode.get
  def setAddressingMode(mode: AddressingMode): Unit =
    addMode match
      case Some(m) => throw new Exception(s"Addrfessing mode already set to $m")
      case None => addMode = Some(mode)

  def opcode: Int =
    ins match
      case Some(i) =>
        addMode match
          case Some(a) =>
            i.opcode(a) match
              case Some(code) => code
              case None => throw new Exception(s"Invalid addressing mode '$a' for instruction '$i'")
          case None => throw new Exception("Addressing mode not set")
      case None => throw new Exception("Instruction not set")

object InstructionTokenV2:
  def apply( mnemonic: String, fields: Array[String]) : InstructionTokenV2 =
    val it = new InstructionTokenV2(mnemonic, fields)
    CpuInstructions.getInstruction(mnemonic)  match
      case Some(code) =>
        it.ins = Some(code)
      case None =>
        throw new Exception(s"Invalid mnemonic '$mnemonic'")
    it


case class SytaxErrorTokenV2 (errortext: String, override val fields: Array[String]) extends TokenV2(fields: Array[String] ):
  override val name: String = "SytaxError"
  override def toString =
    s"$name: $errortext"


