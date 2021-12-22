package com.udsl.processor6502.assembler

import com.udsl.processor6502.cpu.execution.AddressingMode

import scala.collection.mutable.ListBuffer

trait AssemblerToken(val mnemonic: String, val fields: Array[String] ):
  val predictedAddressingModes: ListBuffer[AddressingMode] = ListBuffer[AddressingMode]()
  var value: String = ""

  def intValue: Int = value.toInt

  def addPrediction(prediction: AddressingMode): Unit =
    predictedAddressingModes.addOne(prediction)

  def addPredictions(predictions: List[AddressingMode]): Unit =
    predictedAddressingModes.addAll(predictions)



//case class Accumulator(override val mnemonic: String, override val fields: Array[String]) extends AssemblerToken(mnemonic: String, fields: Array[String]):
//  override def toString: String =
//    "accumulator"


case class BlankLineToken(override val mnemonic: String, override val fields: Array[String] ) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "BlankLineToken"


case class CommentLineToken(override val mnemonic: String, override val fields: Array[String] ) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "CommentLineToken"


case class LineComment (override val mnemonic: String, override val fields: Array[String]) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "LineComment"


case class NoneCommentLine (override val mnemonic: String, override val fields: Array[String]) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "NoneCommentLine"


case class LabelToken (override val mnemonic: String, override val fields: Array[String]) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "LabelToken"


case class CommandToken (override val mnemonic: String, override val fields: Array[String]) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "CommandToken"


case class InstructionToken (override val mnemonic: String, override val fields: Array[String]) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "InstructionToken"


case class SyntaxErrorToken (override val mnemonic: String, override val fields: Array[String]) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "SyntaxErrorToken"


case class ExceptionToken (override val mnemonic: String, override val fields: Array[String]) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "ExceptionToken"


case class ClearToken (override val mnemonic: String, override val fields: Array[String]) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "ClearToken"


case class ReferenceToken (override val mnemonic: String, override val fields: Array[String]) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "ReferenceToken"


case class ValueToken (override val mnemonic: String, override val fields: Array[String]) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "ValueToken"


case class OriginToken (override val mnemonic: String, override val fields: Array[String]) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "OriginToken"

case class DefToken (override val mnemonic: String, override val fields: Array[String]) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "DefinitionToken"

case class NoTokenToken (override val mnemonic: String, override val fields: Array[String]) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "NoTokenToken"

