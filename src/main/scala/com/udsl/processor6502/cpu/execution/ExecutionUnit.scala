package com.udsl.processor6502.cpu.execution

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Subject
import com.udsl.processor6502.Utilities.constructSourceLine
import com.udsl.processor6502.cpu.{Processor, StatusRegisterFlags}
import com.udsl.processor6502.cpu.Processor.*
import com.udsl.processor6502.disassembler.Disassembler
import com.udsl.processor6502.ui.popups.Executor
import scalafx.event.subscriptions.Subscription

/**
 * Most instructions that explicitly reference memory locations have bit patterns of the form aaabbbcc.
 * The aaa and cc bits determine the opcode, and the bbb bits determine the addressing mode.
 *
 * To make life easier lets break ou the aaa, bbb and cc values.
 *
 * see https://llx.com/Neil/a2/opcodes.html
 */

class ExecutionUnit extends StrictLogging, Subject[ExecutionUnit]:
  var opcode: OpcodeValue = NULL(NotApplicable)
  var operand: (Int, Int) = (0, 0)

  val pcSubscription: Subscription = Processor.pc._addr.onChange {
    (_, oldValue, newValue) => {
      logger.info(s"PC subscription fired - ${oldValue}, ${newValue}")
      opcode = getInstruction(newValue.##)
      operand = getInstructionOperand(newValue.##)
      notifyObservers()
    }
  }

  def singleStep(): Unit ={
    // Execute the current instruction
    executeIns
    logger.info(s"Next instruction $opcode, operand ${operand}")
  }

  def executeIns: Unit =
    logger.info(s"Executing instruction ${opcode.mnemonic}, operand ${operand}")
    Processor.sr.updateFlag(StatusRegisterFlags.Zero, operand._1 == 0)
    Processor.sr.updateFlag(StatusRegisterFlags.Negative, operand._1 > 127)
    Processor.ix.ebr = operand._1
    val newPc = Processor.pc.inc(2)
    logger.info(s"Updating PC -> $newPc")

  def decodeInstruction(): String =
    opcode match
      case NULL(_) => ""
      case _ =>
        constructSourceLine(opcode.mnemonic, opcode.addressMode, operand)

object ExecutionUnit:
  def apply: ExecutionUnit =
    val eu = new ExecutionUnit()
    eu.opcode = getNextInstruction
    eu.operand = getNextInstructionOperand
    eu
