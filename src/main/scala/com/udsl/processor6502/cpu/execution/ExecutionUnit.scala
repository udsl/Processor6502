package com.udsl.processor6502.cpu.execution

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Subject
import com.udsl.processor6502.Utilities.constructSourceLine
import com.udsl.processor6502.cpu.{Processor, StatusRegisterFlags}
import com.udsl.processor6502.cpu.Processor.*
import com.udsl.processor6502.cpu.StatusRegisterFlags.{Break, Carry, Decimal, Interrupt, Negative, Overflow, Zero}
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
    opcode.mnemonic match {
      case "LDX" => excuteLDX
      case "DEX" => excuteDEX
      case _ => logger.info(s"${opcode.mnemonic} excution not implemented")
    }

  def decodeInstruction(): String =
    opcode match
      case NULL(_) => ""
      case _ =>
        constructSourceLine(opcode.mnemonic, opcode.addressMode, operand)

  def excuteLDX: Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    val value = if effectiveAddr.hasValue then
      Processor.getMemoryByte(effectiveAddr.address)
    else // has no effective address so it must be immediate
      operand._1
    Processor.sr.updateFlag(StatusRegisterFlags.Negative, value > 127)
    Processor.sr.updateFlag(StatusRegisterFlags.Zero, value == 0)
    Processor.ix.ebr = value
    val newPc = Processor.pc.inc(opcode.addressMode.bytes)
    logger.info(s"Updating PC -> $newPc")

  def excuteDEX: Unit =
    val currentIx = Processor.ix.ebr.##
    if currentIx == 0 then
      Processor.ix.ebr = 255
      Processor.sr.setFlag(StatusRegisterFlags.Negative)
      Processor.sr.clearFlag(StatusRegisterFlags.Zero)
      Processor.ix.ebr = 255
    else
      val newIx = currentIx -1
      Processor.sr.updateFlag(StatusRegisterFlags.Negative, newIx > 127)
      Processor.sr.updateFlag(StatusRegisterFlags.Zero, newIx == 0)
      Processor.ix.ebr = newIx
    val newPc = Processor.pc.inc(opcode.addressMode.bytes)
    logger.info(s"Updating PC -> $newPc")



object ExecutionUnit:
  def apply: ExecutionUnit =
    val eu = new ExecutionUnit()
    eu.opcode = getNextInstruction
    eu.operand = getNextInstructionOperand
    eu

  def getEffectiveAddress(opcode: OpcodeValue, operand: (Int, Int)): EffectiveAddress =
    opcode.addressMode match
    case Accumulator | Implied | Immediate =>
      EffectiveAddress(false)
    case ZeroPage | Relative =>
      EffectiveAddress(true, operand._1)
    case ZeroPageX =>
      //TODO verify what happens when $LL + index exceeds 255
      EffectiveAddress(true, operand._1 + Processor.ix.##)
    case ZeroPageY =>
      //TODO verify what happens when $LL + index exceeds 255
      EffectiveAddress(true, operand._1 + Processor.iy.##)
    case IndirectX =>
      val loByte = Processor.getMemoryByte(operand._1)
      val hiByte = Processor.getMemoryByte(operand._1 + 1)
      EffectiveAddress(true, loByte + (hiByte * 256) + Processor.ix.##)
    case IndirectY =>
      val loByte = Processor.getMemoryByte(operand._1)
      val hiByte = Processor.getMemoryByte(operand._1 + 1)
      EffectiveAddress(true, loByte + (hiByte * 256) + Processor.iy.##)
    case Indirect =>
      val indirectAddr = operand._1 + (operand._2 * 256)
      val loByte = Processor.getMemoryByte(indirectAddr)
      val hiByte = Processor.getMemoryByte(indirectAddr + 1)
      EffectiveAddress(true, loByte + (hiByte * 256))
    case Absolute =>
      EffectiveAddress(true, operand._1 + (operand._2 * 256))
    case AbsoluteX =>
      EffectiveAddress(true, operand._1 + (operand._2 * 256) + Processor.ix.##)
    case AbsoluteY =>
      EffectiveAddress(true, operand._1 + (operand._2 * 256) + Processor.iy.##)
    case _ =>
      EffectiveAddress(false)

class EffectiveAddress(val hasValue: Boolean, val address: Int)

object EffectiveAddress:
  def apply(hasValue: Boolean): EffectiveAddress =
    new EffectiveAddress(hasValue, -1)

  def apply(hasValue: Boolean, address: Int): EffectiveAddress =
    new EffectiveAddress(hasValue, address)
