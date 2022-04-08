package com.udsl.processor6502.cpu.execution

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Subject
import com.udsl.processor6502.Utilities.{byteToHexString, constructSourceLine, numToByteString}
import com.udsl.processor6502.cpu.Memory.INTERRUPT_VECTOR
import com.udsl.processor6502.cpu.{Processor, StatusRegisterFlags}
import com.udsl.processor6502.cpu.Processor.*
import com.udsl.processor6502.cpu.StatusRegisterFlags.{Break, Carry, Decimal, Interrupt, Negative, Overflow, Zero}
import com.udsl.processor6502.disassembler.Disassembler
import com.udsl.processor6502.ui.popups.Executor
import scalafx.application.Platform
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
  var runMode: RunMode = RunMode.Stopped

  val pcSubscription: Subscription = Processor.pc._addr.onChange {
    (_, oldValue, newValue) => {
      logger.info(s"PC subscription fired - $oldValue, ${newValue}")
      opcode = getInstruction(newValue.##)
      operand = getInstructionOperand(newValue.##)
      notifyObservers()
    }
  }

  def singleStep(): Unit ={
    runMode = RunMode.SingleStepping
    // Execute the current instruction
    executeIns()
    logger.info(s"Next instruction $opcode, operand $operand")
  }

  def startSlow(): Unit =
    runMode = RunMode.RunningSlow
    Processor.reset
    run()

  def start(): Unit =
    runMode = RunMode.Running
    Processor.reset
    run()

  def run(): Unit =
    // Execute the current instruction
//    while runMode == RunMode.Running || runMode == RunMode.RunningSlow do
//      executeIns
//      if runMode == RunMode.RunningSlow then
//        Thread.sleep(500)
//        Thread.`yield`()
//      logger.info(s"Next instruction $opcode, operand ${operand}")

    val thread = new Thread {
      override def run =
        while runMode == RunMode.Running || runMode == RunMode.RunningSlow do
          executeIns()
          Thread.`yield`()
          if runMode == RunMode.RunningSlow then
            Thread.sleep(50) // slow the loop down a bit
    }
    thread.start




  def executeIns(): Unit =
    logger.info(s"Executing instruction ${opcode.mnemonic}, operand (${byteToHexString(operand._1)}, ${byteToHexString(operand._2)}) at ${Processor.pc.addr}")
    opcode.mnemonic match {
      case "ADC" => executeADC()
      case "LDX" => executeLDX()
      case "STX" => executeSTX()
      case "LDY" => executeLDY()
      case "DEX" => executeDEX()
      case "BNE" => executeBNE()
      case "BRK" => executeBRK()
      case "TXS" => executeTXS()
      case _ => logger.info(s"${opcode.mnemonic} execution not implemented")
    }


  def decodeInstruction(): String =
    opcode match
      case NULL(_) => ""
      case _ =>
        constructSourceLine(opcode.mnemonic, opcode.addressMode, operand)

  def executeTXS(): Unit =
    Processor.sp.ebr = Processor.ix.ebr
    Platform.runLater(new Runnable {
      def run(): Unit = {
        Processor.pc.inc(opcode.addressMode.bytes)
      }
    })

  def executeBRK(): Unit =
    if (runMode == RunMode.Running || runMode == RunMode.RunningSlow) && operand._1 == 0 then
        runMode = RunMode.SingleStepping
        // This is a break to single step, need to step over to the next instruction
        Platform.runLater(new Runnable {
          def run(): Unit = {
            Processor.pc.inc(2)
          }
        })
    else
      Platform.runLater(() => {
        Processor.sr.setFlag(StatusRegisterFlags.Interrupt)
        // now do a jsr to the irq routine ith return address set to byte after break instruction + 1
        val returnAdr = Processor.pc.addr + 2
        Processor.sp.pushByte(((returnAdr / 256) % 256).toShort)
        Processor.sp.pushByte((returnAdr % 255).toShort)
        // get address at INTERRUPT_VECTOR
        Processor.pc.addr = memoryAccess.getMemoryAsAddress(INTERRUPT_VECTOR)
      })


  def executeADC(): Unit =

    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    val value = if effectiveAddr.hasValue then
      memoryAccess.getMemoryByte(effectiveAddr.address)
    else // has no effective address so it must be immediate
      operand._1

    def doRun(): Unit =
      val accVal = Processor.ac.value
      val resVal = accVal + value
      // is accumulator and value +ve and result > 126 then overflow
      val overflow = accVal < 127 && value < 127 && resVal > 126
      Processor.sr.updateFlag(StatusRegisterFlags.Overflow, overflow)
      Processor.sr.updateFlag(StatusRegisterFlags.Negative, resVal % 255 > 127)
      Processor.sr.updateFlag(StatusRegisterFlags.Zero, resVal % 255 == 0)
      Processor.ac.value = resVal % 255
      Processor.pc.inc(opcode.addressMode.bytes)

    if Platform.isFxApplicationThread then
      Platform.runLater(new Runnable {
        def run(): Unit = {
          doRun
        }
      })
    else
      doRun

  def executeLDX(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    val value = if effectiveAddr.hasValue then
      memoryAccess.getMemoryByte(effectiveAddr.address)
    else // has no effective address so it must be immediate
      operand._1
    Platform.runLater(new Runnable {
      def run(): Unit = {
        Processor.sr.updateFlag(StatusRegisterFlags.Negative, value > 127)
        Processor.sr.updateFlag(StatusRegisterFlags.Zero, value == 0)
        Processor.ix.ebr = value
        Processor.pc.inc(opcode.addressMode.bytes)
      }
    })

  def executeSTX(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    memoryAccess.setMemoryByte(effectiveAddr.address, Processor.ix.ebr)
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeLDY(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    val value = if effectiveAddr.hasValue then
      memoryAccess.getMemoryByte(effectiveAddr.address)
    else // has no effective address so it must be immediate
      operand._1
    Platform.runLater(new Runnable {
      def run(): Unit = {
        Processor.sr.updateFlag(StatusRegisterFlags.Negative, value > 127)
        Processor.sr.updateFlag(StatusRegisterFlags.Zero, value == 0)
        Processor.iy.ebr = value
        Processor.pc.inc(opcode.addressMode.bytes)
      }
    })


  def executeDEX(): Unit =
    Platform.runLater(new Runnable {
       def run(): Unit =
         val currentIx = Processor.ix.ebr
         if currentIx == 0 then
            Processor.ix.ebr = 255
            Processor.sr.setFlag(StatusRegisterFlags.Negative)
            Processor.sr.clearFlag(StatusRegisterFlags.Zero)
            Processor.ix.ebr = 255
         else
            val newIx = currentIx - 1
            Processor.sr.updateFlag(StatusRegisterFlags.Negative, newIx > 127)
            Processor.sr.updateFlag(StatusRegisterFlags.Zero, newIx == 0)
            Processor.ix.ebr = newIx
         Processor.pc.inc(opcode.addressMode.bytes)
    })

  def executeBNE(): Unit =
    Platform.runLater(new Runnable {
      def run(): Unit =
        if !Processor.sr.testFlag(StatusRegisterFlags.Zero) then
          val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
          Processor.pc.addr = effectiveAddr.address
        else
          val newPc = Processor.pc.inc(opcode.addressMode.bytes)
    })

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
    case ZeroPage =>
      EffectiveAddress(true, operand._1)
    case Relative =>
      // Relative addressing is used in branch instructions
      // In a real 6502 the PC is incremented on each memory fetch
      // so the PC will be pointing to the instruction following
      // As we dont inc the PC till the instruction is completed we get..
      // convert offset to signed byte value
      val offset = operand._1.toByte
      // then add 2 that the real 6502 would have added on each fetch
      EffectiveAddress(true, Processor.pc.addr + offset + 2)
    case ZeroPageX =>
      //TODO verify what happens when $LL + index exceeds 255
      EffectiveAddress(true, operand._1 + Processor.ix.ebr)
    case ZeroPageY =>
      //TODO verify what happens when $LL + index exceeds 255
      EffectiveAddress(true, operand._1 + Processor.iy.ebr)
    case IndirectX =>
      val loByte = memoryAccess.getMemoryByte(operand._1)
      val hiByte = memoryAccess.getMemoryByte(operand._1 + 1)
      EffectiveAddress(true, loByte + (hiByte * 256) + Processor.ix.ebr)
    case IndirectY =>
      val loByte = memoryAccess.getMemoryByte(operand._1)
      val hiByte = memoryAccess.getMemoryByte(operand._1 + 1)
      EffectiveAddress(true, loByte + (hiByte * 256) + Processor.iy.ebr)
    case Indirect =>
      val indirectAddr = operand._1 + (operand._2 * 256)
      val loByte = memoryAccess.getMemoryByte(indirectAddr)
      val hiByte = memoryAccess.getMemoryByte(indirectAddr + 1)
      EffectiveAddress(true, loByte + (hiByte * 256))
    case Absolute =>
      EffectiveAddress(true, operand._1 + (operand._2 * 256))
    case AbsoluteX =>
      EffectiveAddress(true, operand._1 + (operand._2 * 256) + Processor.ix.ebr)
    case AbsoluteY =>
      EffectiveAddress(true, operand._1 + (operand._2 * 256) + Processor.iy.ebr)
    case _ =>
      EffectiveAddress(false)

class EffectiveAddress(val hasValue: Boolean, val address: Int)

object EffectiveAddress:
  def apply(hasValue: Boolean): EffectiveAddress =
    new EffectiveAddress(hasValue, -1)

  def apply(hasValue: Boolean, address: Int): EffectiveAddress =
    new EffectiveAddress(hasValue, address)

enum RunMode:
  case Stopped
  case SingleStepping
  case Running
  case RunningSlow