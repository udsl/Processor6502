package com.udsl.processor6502.assembler

import com.udsl.processor6502.cpu.execution.AddressingMode

trait AssemblerToken(mnemonic: String, fields: Array[String] ):
  val predictedAddressingModes: List[AddressingMode] = List[AddressingMode]()
  var value: String = ""

  def intValue: Int = value.toInt

  def addPrediction(prediction: AddressingMode): Unit =
    predictedAddressingModes.appended(prediction)

  def addPredictions(predictions: List[AddressingMode]): Unit =
    for pred <- predictions do
      addPrediction(pred)


case class Accumulator(mnemonic: String, fields: Array[String]) extends AssemblerToken(mnemonic: String, fields: Array[String]):
  override def toString: String =
    "accumulator"


case class BlankLineToken(mnemonic: String, fields: Array[String] ) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "BlankLineToken"


case class CommentLineToken(mnemonic: String, fields: Array[String] ) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "CommentLineToken"


case class LineComment (mnemonic: String, fields: Array[String] ) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "LineComment"


case class NoneCommentLine (mnemonic: String, fields: Array[String] ) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "NoneCommentLine"


case class LabelToken (mnemonic: String, fields: Array[String] ) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "LabelToken"


case class CommandToken (mnemonic: String, fields: Array[String] ) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "CommandToken"


case class InstructionToken (mnemonic: String, fields: Array[String] ) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "InstructionToken"


case class SyntaxErrorToken (mnemonic: String, fields: Array[String] ) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "SyntaxErrorToken"


case class ExceptionToken (mnemonic: String, fields: Array[String] ) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "ExceptionToken"


case class ClearToken (mnemonic: String, fields: Array[String] ) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "ClearToken"


case class ReferenceToken (mnemonic: String, fields: Array[String] ) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "ReferenceToken"


case class ValueToken (mnemonic: String, fields: Array[String] ) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "ValueToken"


case class OriginToken (mnemonic: String, fields: Array[String] ) extends AssemblerToken(mnemonic: String, fields: Array[String] ):
  override def toString: String =
    "OriginToken"


