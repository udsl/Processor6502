package com.udsl.processor6502.cpu.execution

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Subject
import com.udsl.processor6502.Utilities.{byteToHexString, constructSourceLine, numToByteString}
import com.udsl.processor6502.cpu.Memory.INTERRUPT_VECTOR
import com.udsl.processor6502.cpu.{ByteValue, Processor, StatusFlag}
import com.udsl.processor6502.cpu.Processor.{getNextInstruction, *}
import com.udsl.processor6502.cpu.StatusFlag.{Break, Carry, Decimal, Interrupt, Negative, Overflow, Zero}
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
      logger.debug(s"PC subscription fired - $oldValue, $newValue")
      opcode = getInstruction(newValue.##)
      operand = getInstructionOperand(newValue.##)
      notifyObservers()
    }
  }

  /**
   * Get the current instruction from the PC location and execute it.
   * @return returns the OpcodeValue of the instruction executed
   */
  def singleStep(): OpcodeValue ={
    runMode = RunMode.SingleStepping
    // Always get the next instruction
    loadInstructionAtPc()
    val executing = opcode
    executeIns()
    logger.debug(s"Next instruction $opcode, operand $operand")
    executing
  }

  def loadInstructionAtPc(): Unit =
    opcode = Processor.getNextInstruction
    operand = getNextInstructionOperand

  def startSlow(): Unit =
    runMode = RunMode.RunningSlow
    Processor.reset()
    run()

  def start(): Unit =
    runMode = RunMode.Running
    Processor.reset()
    run()

  def run(): Unit =
    // make sure we have a current instruction
    opcode match {
      case NULL(NotApplicable) => loadInstructionAtPc()
      case _ => ()
    }

    val thread = new Thread {
      override def run(): Unit =
        while runMode == RunMode.Running || runMode == RunMode.RunningSlow do
          executeIns()
          Thread.`yield`()
          if runMode == RunMode.RunningSlow then
            Thread.sleep(50) // slow the loop down a bit
    }
    thread.start()

  //noinspection EmptyParenMethodAccessedAsParameterless
  def executeIns(): Unit =
    def notImplmented() : Unit =
      logger.info(s"${opcode.mnemonic} execution not implemented")

    logger.info(s"Executing instruction ${opcode.mnemonic}, operand (${byteToHexString(operand._1)}, ${byteToHexString(operand._2)}) at ${Processor.pc.addr} Accumulator ${Processor.ac}")
    val execute: Unit =  opcode.mnemonic match {
      case "ADC" => executeADC()
      case "AND" => executeAND()
      case "ASL" => executeASL()
      case "BCC" => executeBCC()
      case "BCS" => executeBCS()
      case "BEQ" => executeBEQ()
      case "BIT" => executeBIT()
      case "BNE" => executeBNE()
      case "BMI" => executeBMI()
      case "BPL" => executeBPL()
      case "BRK" => executeBRK()
      case "BVC" => executeBVC()
      case "BVS" => executeBVS()
      case "CLC" => executeCLC()
      case "CLD" => executeCLD()
      case "CLI" => executeCLI()
      case "CLV" => executeCLV()
      case "CMP" => executeCMP()
      case "CPX" => executeCPX()
      case "CPY" => executeCPY()

      case "DEX" => executeDEX()
      case "LDX" => executeLDX()
      case "LDY" => executeLDY()
      case "STX" => executeSTX()
      case "TXS" => executeTXS()
      case _ => notImplmented()
    }
    if Platform.isFxApplicationThread then
      Platform.runLater(() => {
        execute
      })
    else
      execute

  def decodeInstruction(): String =
    opcode match
      case NULL(_) => ""
      case _ =>
        constructSourceLine(opcode.mnemonic, opcode.addressMode, operand)

  def executeTXS(): Unit =
    Processor.sp.ebr = Processor.ix.ebr
    Processor.pc.inc(opcode.addressMode.bytes)


  def executeADC(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    val value = if effectiveAddr.hasValue then
      memoryAccess.getMemoryByte(effectiveAddr.address)
    else // has no effective address so it must be immediate
      operand._1
    val accVal = Processor.ac.value
    val writeBack = (accVal + value) & 0xFF
    // is accumulator and value +ve and result > 126 then overflow
    val overflow = accVal < 127 && value < 127 && writeBack > 127
    Processor.sr.updateFlag(StatusFlag.Overflow, overflow)
    Processor.sr.updateFlag(StatusFlag.Negative, writeBack > 127)
    Processor.sr.updateFlag(StatusFlag.Zero, writeBack == 0)
    Processor.ac.value = writeBack
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeAND(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    val value = if effectiveAddr.hasValue then
      memoryAccess.getMemoryByte(effectiveAddr.address)
    else // has no effective address so it must be immediate
      operand._1
    val writeBack = Processor.ac.value & value
    Processor.sr.updateFlag(StatusFlag.Zero, writeBack == 0)
    Processor.sr.updateFlag(StatusFlag.Negative, writeBack > 127)
    Processor.ac.value = writeBack
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeASL(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    if effectiveAddr.hasValue then
      val value: Int = memoryAccess.getMemoryByte(effectiveAddr.address)
      Processor.sr.updateFlag(StatusFlag.Carry, (value & 255) >= 128)
      val writeBack = (value << 1) & 0xFE
      Processor.sr.updateFlag(StatusFlag.Zero, writeBack == 0)
      Processor.sr.updateFlag(StatusFlag.Negative, writeBack > 127)
      memoryAccess.setMemoryByte(effectiveAddr.address, writeBack)
    else // must be accumulator if no effective address as no immediate for ASL
      val accVal = Processor.ac.value
      Processor.sr.updateFlag(StatusFlag.Carry, (accVal & 255) >= 128)
      val writeBack =  (accVal << 1) & 0xFE
      Processor.ac.value = writeBack
      Processor.sr.updateFlag(StatusFlag.Zero, writeBack == 0)
      Processor.sr.updateFlag(StatusFlag.Negative, writeBack > 127)
    Processor.pc.inc(opcode.addressMode.bytes)


  def executeLDX(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    val value = if effectiveAddr.hasValue then
      memoryAccess.getMemoryByte(effectiveAddr.address)
    else // has no effective address so it must be immediate
      operand._1
    Processor.sr.updateFlag(StatusFlag.Negative, value > 127)
    Processor.sr.updateFlag(StatusFlag.Zero, value == 0)
    Processor.ix.ebr = value
    Processor.pc.inc(opcode.addressMode.bytes)

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
    Processor.sr.updateFlag(StatusFlag.Negative, value > 127)
    Processor.sr.updateFlag(StatusFlag.Zero, value == 0)
    Processor.iy.ebr = value
    Processor.pc.inc(opcode.addressMode.bytes)



  def executeDEX(): Unit =
   val currentIx = Processor.ix.ebr
   if currentIx == 0 then
      Processor.ix.ebr = 255
      Processor.sr.setFlag(StatusFlag.Negative)
      Processor.sr.clearFlag(StatusFlag.Zero)
      Processor.ix.ebr = 255
   else
      val newIx = currentIx - 1
      Processor.sr.updateFlag(StatusFlag.Negative, newIx > 127)
      Processor.sr.updateFlag(StatusFlag.Zero, newIx == 0)
      Processor.ix.ebr = newIx
   Processor.pc.inc(opcode.addressMode.bytes)
  
  def executeBCC(): Unit =
    if !Processor.sr.testFlag(StatusFlag.Carry) then
      val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
      Processor.pc.addr = effectiveAddr.address
    else
      val newPc = Processor.pc.inc(opcode.addressMode.bytes)

  def executeBCS(): Unit =
    if Processor.sr.testFlag(StatusFlag.Carry) then
      val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
      Processor.pc.addr = effectiveAddr.address
    else
      Processor.pc.inc(opcode.addressMode.bytes)

  def executeBEQ(): Unit =
    if Processor.sr.testFlag(StatusFlag.Zero) then
      val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
      Processor.pc.addr = effectiveAddr.address
    else
      val newPc = Processor.pc.inc(opcode.addressMode.bytes)

  def executeBIT(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    if effectiveAddr.hasValue then
      val value = memoryAccess.getMemoryByte(effectiveAddr.address)
      Processor.sr.updateFlag(StatusFlag.Negative, (value & 0x80) > 0)
      Processor.sr.updateFlag(StatusFlag.Overflow, (value & 0x40) > 0)
      Processor.sr.updateFlag(StatusFlag.Zero, (value & Processor.ac.value) == 0)

  def executeBMI(): Unit =
    if Processor.sr.testFlag(StatusFlag.Negative) then
      val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
      Processor.pc.addr = effectiveAddr.address
    else
      Processor.pc.inc(opcode.addressMode.bytes)

  def executeBNE(): Unit =
    if !Processor.sr.testFlag(StatusFlag.Zero) then
      val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
      Processor.pc.addr = effectiveAddr.address
    else
      Processor.pc.inc(opcode.addressMode.bytes)

  def executeBPL(): Unit =
    if !Processor.sr.testFlag(StatusFlag.Negative) then
      val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
      Processor.pc.addr = effectiveAddr.address
    else
      Processor.pc.inc(opcode.addressMode.bytes)

  def executeBRK(): Unit =
      if (runMode == RunMode.Running || runMode == RunMode.RunningSlow) && operand._1 == 0 then
        runMode = RunMode.SingleStepping
        // This is a break to single step, need to step over to the next instruction
        Processor.pc.inc(2)
      else
        // now do a jsr to the irq routine ith return address set to byte after break instruction + 1
        val returnAdr = Processor.pc.addr + 2
        Processor.sp.pushByte(((returnAdr / 256) & 255).toShort)
        Processor.sp.pushByte((returnAdr & 255).toShort)
        var flagsToPush = Processor.sr.value | Break.mask
        Processor.sp.pushByte(flagsToPush)
        // get address at INTERRUPT_VECTOR
        Processor.pc.addr = memoryAccess.getMemoryAsAddress(INTERRUPT_VECTOR)

  def executeBVC(): Unit =
    if !Processor.sr.testFlag(StatusFlag.Overflow) then
      val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
      Processor.pc.addr = effectiveAddr.address
    else
      Processor.pc.inc(opcode.addressMode.bytes)

  def executeBVS(): Unit =
    if Processor.sr.testFlag(StatusFlag.Overflow) then
      val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
      Processor.pc.addr = effectiveAddr.address
    else
      Processor.pc.inc(opcode.addressMode.bytes)

  def executeCLC(): Unit =
    Processor.sr.clearFlag(StatusFlag.Carry)
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeCLD(): Unit =
    Processor.sr.clearFlag(StatusFlag.Decimal)
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeCLI(): Unit =
    Processor.sr.clearFlag(StatusFlag.Interrupt)
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeCLV(): Unit =
    Processor.sr.clearFlag(StatusFlag.Overflow)
    Processor.pc.inc(opcode.addressMode.bytes)

  /**
   *
   * @param mem value bing compared to
   * @param reg register (acc, X or Y) value
   */
  def doTheCompare(mem: Int, reg: Int): Unit =
    val res = reg - mem
    Processor.sr.updateFlag(StatusFlag.Negative, res < 0)
    Processor.sr.updateFlag(StatusFlag.Zero, res == 0)
    Processor.sr.updateFlag(StatusFlag.Carry, res > 0)

  def executeCMP(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    val value = if effectiveAddr.hasValue then
      memoryAccess.getMemoryByte(effectiveAddr.address)
    else // has no effective address so it must be immediate
      operand._1
    doTheCompare(value, Processor.ac.value)
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeCPX(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    val value = if effectiveAddr.hasValue then
      memoryAccess.getMemoryByte(effectiveAddr.address)
    else // has no effective address so it must be immediate
      operand._1
    doTheCompare(value, Processor.ix.value)
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeCPY(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    val value = if effectiveAddr.hasValue then
      memoryAccess.getMemoryByte(effectiveAddr.address)
    else // has no effective address so it must be immediate
      operand._1
    doTheCompare(value, Processor.iy.value)
    Processor.pc.inc(opcode.addressMode.bytes)


object ExecutionUnit:
  def apply: ExecutionUnit =
    val eu = new ExecutionUnit()
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
      val offset = ByteValue.asSignedValue(operand._1.toByte)
      // then add 2 that the real 6502 would have added on each fetch
      EffectiveAddress(true, Processor.pc.addr + offset + 2)
    case ZeroPageX =>
      //TODO verify what happens when $LL + index exceeds 255
      EffectiveAddress(true, operand._1 + Processor.ix.ebr)
    case ZeroPageY =>
      //TODO verify what happens when $LL + index exceeds 255
      EffectiveAddress(true, operand._1 + Processor.iy.ebr)
    case IndirectX =>
      val zeroPgaeAddr = operand._1 + Processor.ix.ebr
      val loByte = memoryAccess.getMemoryByte(zeroPgaeAddr)
      val hiByte = memoryAccess.getMemoryByte(zeroPgaeAddr + 1)
      EffectiveAddress(true, loByte + (hiByte * 256))
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