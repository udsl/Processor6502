package com.udsl.processor6502.assembler.version1

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Utilities.numericValue
import com.udsl.processor6502.assembler.SourceLine
import com.udsl.processor6502.cpu.CpuInstructions
import com.udsl.processor6502.cpu.execution.AddressingMode

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.ListBuffer

@uncheckedVariance
trait AssemblerToken(val mnemonic: String, val fields: Array[String], val sourceLine: SourceLine ):
  val predictedAddressingModes: ListBuffer[AddressingMode] = ListBuffer[AddressingMode]()
  var _value: String = ""

  def value: String = _value
  def value_=(newValue: String): Unit = _value = newValue
  
  def intValue: Option[Int] = numericValue(value)

  /**
   * Abstrct method must be implemented in classes
   * @param prediction the predition to add
   */
  def addPrediction(prediction: AddressingMode): Unit

  /**
   * adds the pridictions one at a time using the addPriduction method defined in each implemntation
   * @param predictions the predictions to add
   */
  def addPredictions(predictions: List[AddressingMode]): Unit =
    for p <- predictions do
      addPrediction(p)

case class BlankLineToken(override val mnemonic: String, override val fields: Array[String], override val sourceLine: SourceLine ) extends AssemblerToken(mnemonic: String, fields: Array[String], sourceLine: SourceLine ):
  override def toString: String =
    "BlankLineToken"
  override def addPrediction(prediction: AddressingMode): Unit = {}


case class CommentLineToken(override val mnemonic: String, override val fields: Array[String], override val sourceLine: SourceLine ) extends AssemblerToken(mnemonic: String, fields: Array[String], sourceLine: SourceLine ):
  override def toString: String =
    "CommentLineToken"
  override def addPrediction(prediction: AddressingMode): Unit = {}

case class LineComment (override val mnemonic: String, override val fields: Array[String], override val sourceLine: SourceLine) extends AssemblerToken(mnemonic: String, fields: Array[String], sourceLine: SourceLine ):
  override def toString: String =
    "LineComment"
  override def addPrediction(prediction: AddressingMode): Unit = {}

case class LabelToken (override val mnemonic: String, override val fields: Array[String], override val sourceLine: SourceLine) extends AssemblerToken(mnemonic: String, fields: Array[String], sourceLine: SourceLine ):
  override def toString: String =
    "LabelToken"
  override def addPrediction(prediction: AddressingMode): Unit = {}

case class CommandToken (override val mnemonic: String, override val fields: Array[String], override val sourceLine: SourceLine) extends AssemblerToken(mnemonic: String, fields: Array[String], sourceLine: SourceLine ):
  override def toString: String =
    "CommandToken"
  override def addPrediction(prediction: AddressingMode): Unit = {}

case class InstructionToken (override val mnemonic: String, override val fields: Array[String], override val sourceLine: SourceLine) extends AssemblerToken(mnemonic: String, fields: Array[String], sourceLine: SourceLine ) with StrictLogging:
  override def toString: String =
    "InstructionToken"
  override def addPrediction(prediction: AddressingMode): Unit = {
    logger.debug(s"Adding AddressingMode prediction $prediction")
    // Is prediction valid for the instruction?
    if CpuInstructions.isAddressingModeValid(mnemonic, prediction) then
      predictedAddressingModes.addOne(prediction)
  }

case class ClearToken (override val mnemonic: String, override val fields: Array[String], override val sourceLine: SourceLine) extends AssemblerToken(mnemonic: String, fields: Array[String], sourceLine: SourceLine ):
  override def toString: String =
    "ClearToken"
  override def addPrediction(prediction: AddressingMode): Unit = {}

case class ReferenceToken (override val mnemonic: String, override val fields: Array[String], override val sourceLine: SourceLine) extends AssemblerToken(mnemonic: String, fields: Array[String], sourceLine: SourceLine ):
  override def toString: String =
    "ReferenceToken"
  override def addPrediction(prediction: AddressingMode): Unit = {}

case class ValueToken (override val mnemonic: String, override val fields: Array[String], override val sourceLine: SourceLine) extends AssemblerToken(mnemonic: String, fields: Array[String], sourceLine: SourceLine ):
  override def toString: String =
    "ValueToken"
  override def addPrediction(prediction: AddressingMode): Unit = {}

case class OriginToken (override val mnemonic: String, override val fields: Array[String], override val sourceLine: SourceLine) extends AssemblerToken(mnemonic: String, fields: Array[String], sourceLine: SourceLine ):
  override def toString: String =
    "OriginToken"
  override def addPrediction(prediction: AddressingMode): Unit = {}

case class DefToken (override val mnemonic: String, override val fields: Array[String], override val sourceLine: SourceLine) extends AssemblerToken(mnemonic: String, fields: Array[String], sourceLine: SourceLine ):
  override def toString: String =
    "DefinitionToken"
  override def addPrediction(prediction: AddressingMode): Unit = {}

case class NoTokenToken (override val mnemonic: String, override val fields: Array[String], override val sourceLine: SourceLine) extends AssemblerToken(mnemonic: String, fields: Array[String], sourceLine: SourceLine ):
  override def toString: String =
    "NoTokenToken"
  override def addPrediction(prediction: AddressingMode): Unit = {}

