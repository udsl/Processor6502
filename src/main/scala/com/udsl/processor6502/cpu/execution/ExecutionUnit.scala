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

class ExecutionUnit(val testing: Boolean = false) extends StrictLogging, Subject[ExecutionUnit]:
  var opcode: OpcodeValue = NULL(NotApplicable)
  var operand: (Int, Int) = (0, 0)
  var runMode: RunMode = RunMode.Stopped

  val pcSubscription: Subscription = Processor.pc._addr.onChange {
    (_, oldValue, newValue) => {
      if !testing then
        logger.debug(s"PC subscription fired - $oldValue, $newValue")
        loadInstructionAtPc()
        notifyObservers()
    }
  }

  /**
   * Get the current instruction from the PC location and execute it.
   * @return returns the OpcodeValue of the instruction executed
   */
  def singleStep(): OpcodeValue ={
    runMode = RunMode.SingleStepping
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
    def notImplemented() : Unit =
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
      case "DEC" => executeDEC()
      case "DEX" => executeDEX()
      case "DEY" => executeDEY()
      case "EOR" => executeEOR()
      case "INC" => executeINC()
      case "INX" => executeINX()
      case "INY" => executeINY()
      case "JMP" => executeJMP()
      case "JSR" => executeJSR()
      case "LDA" => executeLDA()
      case "LDX" => executeLDX()
      case "LDY" => executeLDY()
      case "LSR" => executeLSR()
      case "NOP" => executeNOP()
      case "ORA" => executeORA()
      case "PHA" => executePHA()
      case "PHP" => executePHP()
      case "PLA" => executePLA()
      case "PLP" => executePLP()
      case "ROL" => executeROL()
      case "ROR" => executeROR()
      case "RTI" => executeRTI()
      case "RTS" => executeRTS()
      case "SBC" => executeSBC()

      case "STX" => executeSTX()
      case "TXS" => executeTXS()
      case _ => notImplemented()
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

  def valueFromAddressOrOperand(): Int =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    if effectiveAddr.hasValue then
      memoryAccess.getMemoryByte(effectiveAddr.address)
    else // has no effective address so it must be immediate
      operand._1

  def updateZeroNegativeFlags(from: Int): Unit =
    Processor.sr.updateFlag(StatusFlag.Zero, from == 0)
    Processor.sr.updateFlag(StatusFlag.Negative, (from & 0x80) > 0)

  def updateZeroNegativeCaryFlags(from: Int): Unit =
    updateZeroNegativeFlags(from)
    // for addition the carry happens when from (the result) > 255
    // for subtraction is when result < 0 - Scala ints are signed so we can get real negative numbers
    Processor.sr.updateFlag(StatusFlag.Carry, (from > 0xFF) | (from < 0))

  def is8BitPositive(value: Int): Boolean =
    !is8BitNegative(value)

  def is8BitNegative(value: Int): Boolean =
    (value & 0x80) > 0

  def executeADC(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    val value = if effectiveAddr.hasValue then
      memoryAccess.getMemoryByte(effectiveAddr.address)
    else // has no effective address so it must be immediate
      operand._1
    val accVal = Processor.ac.value
    val carry = if Processor.sr.testFlag(Carry) then 1 else 0
    val writeBack = accVal + value + carry
    /*
      Overflow when either
        1. Two positive numbers are added, and the result is a negative number.
        2. Two negative numbers are added, and the result is a positive number.
      (1) and (2) can be simplified into the following condition:

      Two numbers that have the same sign are added, and the result has a different sign.
    */
    val overflow = (is8BitPositive(accVal) && is8BitPositive(value) && is8BitNegative(writeBack)) |
                   (is8BitNegative(accVal) && is8BitNegative(value) && is8BitPositive(writeBack))
    Processor.sr.updateFlag(StatusFlag.Overflow, overflow)
    updateZeroNegativeCaryFlags(writeBack)

    // As the write back could be greater than 0xFF for example 0xFF (-1) plus 0x01 (1) = 0x100
    // We have an overflow and result is positive (no -0) but not an overflow
    // now 0xFC (-4) add 0xFF (-1) we would expect 0xFB (-5) we get 0x1FB which is a negative result no overflow
    // We have to restrict result to 8 bits and with 0xFF
    Processor.ac.value = writeBack & 0xFF
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeAND(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    val value = if effectiveAddr.hasValue then
      memoryAccess.getMemoryByte(effectiveAddr.address)
    else // has no effective address so it must be immediate
      operand._1
    val writeBack = Processor.ac.value & value
    updateZeroNegativeFlags(writeBack)
    Processor.ac.value = writeBack
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeASL(): Unit =
    def calcWriteBackAndUpdateFlags( value: Int ): Int =
      Processor.sr.updateFlag(StatusFlag.Carry, (value & 0x80) > 0)
      val writeBack = (value << 1) & 0xFE
      updateZeroNegativeFlags(writeBack)
      writeBack

    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    if effectiveAddr.hasValue then
      val value: Int = memoryAccess.getMemoryByte(effectiveAddr.address)
      memoryAccess.setMemoryByte(effectiveAddr.address, calcWriteBackAndUpdateFlags(value))
    else // must be accumulator if no effective address as no immediate for ASL
      Processor.ac.value = calcWriteBackAndUpdateFlags(Processor.ac.value)
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
        Processor.sp.pushAddress(returnAdr)
        val flagsToPush = Processor.sr.value | Break.mask
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

  def executeDEC(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    val value = memoryAccess.getMemoryByte(effectiveAddr.address)
    val res = (value -1) & 255
    updateZeroNegativeFlags(res)
    memoryAccess.setMemoryByte(effectiveAddr.address, res)
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeDEX(): Unit =
    val currentIx = Processor.ix.ebr
    if currentIx == 0 then
      Processor.ix.ebr = 255
      Processor.sr.setFlag(StatusFlag.Negative)
      Processor.sr.clearFlag(StatusFlag.Zero)
      Processor.ix.ebr = 255
    else
      val newIx = (currentIx - 1) & 255
      updateZeroNegativeFlags(newIx)
      Processor.ix.ebr = newIx
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeDEY(): Unit =
    val currentIy = Processor.iy.ebr
    if currentIy == 0 then
      Processor.iy.ebr = 255
      Processor.sr.setFlag(StatusFlag.Negative)
      Processor.sr.clearFlag(StatusFlag.Zero)
    else
      val newIy = (currentIy - 1) & 255
      updateZeroNegativeFlags(newIy)
      Processor.iy.ebr = newIy
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeEOR(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    val value = if effectiveAddr.hasValue then
      memoryAccess.getMemoryByte(effectiveAddr.address)
    else // has no effective address so it must be immediate
      operand._1
    val res =  Processor.ac.value ^ value
    updateZeroNegativeFlags(res)
    Processor.ac.value = res
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeINC(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    val value = memoryAccess.getMemoryByte(effectiveAddr.address)
    val res =  (value + 1) & 0xFF
    updateZeroNegativeFlags(res)
    memoryAccess.setMemoryByte(effectiveAddr.address, res)
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeINX(): Unit =
    val value = Processor.ix.value
    val res =  (value + 1) & 0xFF
    updateZeroNegativeFlags(res)
    Processor.ix.value = res
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeINY(): Unit =
    val value = Processor.iy.value
    val res =  (value + 1) & 0xFF
    updateZeroNegativeFlags(res)
    Processor.iy.value = res
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeJMP(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    val value = effectiveAddr.address
    if effectiveAddr.absolute then
      Processor.pc.addr = value
    else // indirect
      Processor.pc.addr = memoryAccess.getMemoryAsAddress(effectiveAddr.address)

  def executeJSR(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    // return address is not next instruction, this is resolved by the RTS. CPU not this app feature!
    val ret = Processor.pc.addr + 2
    Processor.sp.pushAddress(ret)
    // JSR always absolute
    Processor.pc.addr = effectiveAddr.address

  def executeLDA(): Unit =
    val value = valueFromAddressOrOperand()
    updateZeroNegativeFlags(value)
    Processor.ac.ebr = value
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeLDX(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    val value = if effectiveAddr.hasValue then
      memoryAccess.getMemoryByte(effectiveAddr.address)
    else // has no effective address so it must be immediate
      operand._1
    updateZeroNegativeFlags(value)
    Processor.ix.ebr = value
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeLDY(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    val value = if effectiveAddr.hasValue then
      memoryAccess.getMemoryByte(effectiveAddr.address)
    else // has no effective address so it must be immediate
      operand._1
    Processor.sr.updateFlag(StatusFlag.Negative, (value & 0x80)  > 0)
    Processor.sr.updateFlag(StatusFlag.Zero, value == 0)
    Processor.iy.ebr = value
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeLSR(): Unit =
    def calcWriteBackAndUpdateFlags( value: Int ): Int =
      Processor.sr.updateFlag(StatusFlag.Carry, (value & 0x01) > 0)
      val writeBack = value >>> 1
      Processor.sr.updateFlag(StatusFlag.Zero, writeBack == 0)
      Processor.sr.clearFlag(StatusFlag.Negative)
      writeBack

    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    if effectiveAddr.hasValue then
      val value: Int = memoryAccess.getMemoryByte(effectiveAddr.address)
      memoryAccess.setMemoryByte(effectiveAddr.address, calcWriteBackAndUpdateFlags(value))
    else // must be accumulator if no effective address as no immediate for ASL
      Processor.ac.value = calcWriteBackAndUpdateFlags(Processor.ac.value)
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeNOP(): Unit =
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeORA(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    val value = if effectiveAddr.hasValue then
      memoryAccess.getMemoryByte(effectiveAddr.address)
    else // has no effective address so it must be immediate
      operand._1
    val writeBack = Processor.ac.value | value
    updateZeroNegativeFlags(writeBack)
    Processor.ac.value = writeBack
    Processor.pc.inc(opcode.addressMode.bytes)

  def executePHA(): Unit =
    Processor.sp.pushByte(Processor.ac.value)
    Processor.pc.inc(opcode.addressMode.bytes)

  def executePHP(): Unit =
    Processor.sp.pushByte(Processor.sr.value)
    Processor.pc.inc(opcode.addressMode.bytes)

  def executePLA(): Unit =
    val writeBack = Processor.sp.popByte()
    updateZeroNegativeFlags(writeBack)
    Processor.ac.value = writeBack
    Processor.pc.inc(opcode.addressMode.bytes)

  def executePLP(): Unit =
    val popByte = Processor.sp.popByte() & 0xEF
    Processor.sr.value = popByte
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeROL(): Unit =
    def doRol(value: Int): Int =
      val currentCarryFlag = Processor.sr.testFlag(Carry)
      Processor.sr.updateFlag(Carry, (value & 0x80) > 0)
      val res = if !currentCarryFlag then
        (value << 1) & 0xFE
      else
        ((value << 1) & 0xFE) | 0x01
      updateZeroNegativeFlags(res)
      res

    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    if effectiveAddr.hasValue then
      val writeBack = doRol(memoryAccess.getMemoryByte(effectiveAddr.address))
      memoryAccess.setMemoryByte(effectiveAddr.address, writeBack)
    else // has no effective address so it must be accumulator
      val writeBack = doRol(Processor.ac.value)
      Processor.ac.value = writeBack
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeROR(): Unit =
    def doRor(value: Int): Int =
      val currentCarryFlag = Processor.sr.testFlag(Carry)
      Processor.sr.updateFlag(Carry, (value & 0x01) > 0)
      val res = if !currentCarryFlag then
        (value >> 1) & 0x7F
      else
        ((value >> 1) & 0x7F) | 0x80
      updateZeroNegativeFlags(res)
      res

    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    if effectiveAddr.hasValue then
      val writeBack = doRor(memoryAccess.getMemoryByte(effectiveAddr.address))
      memoryAccess.setMemoryByte(effectiveAddr.address, writeBack)
    else // has no effective address so it must be accumulator
      val writeBack = doRor(Processor.ac.value)
      Processor.ac.value = writeBack
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeRTI(): Unit =
    val retHi = Processor.sp.popByte()
    val retLo = Processor.sp.popByte()
    Processor.pc.addr = (retHi * 256) + retLo
    val popByte = Processor.sp.popByte() & 0xEB // remove unused and interrupt flags
    Processor.sr.value = popByte

  def executeRTS(): Unit =
    val retHi = Processor.sp.popByte()
    val retLo = Processor.sp.popByte()
    Processor.pc.addr = (retHi * 256) + retLo

  def executeSBC(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    val value = if effectiveAddr.hasValue then
       memoryAccess.getMemoryByte(effectiveAddr.address)
    else // has no effective address so it must be immediate
       operand._1
    val accVal = Processor.ac.value
    val borrow = if Processor.sr.testFlag(Carry) then 0 else 1
    val writeBack = accVal - value - borrow
    val overflow = (is8BitPositive(accVal) && is8BitPositive(value) && is8BitNegative(writeBack)) |
        (is8BitNegative(accVal) && is8BitNegative(value) && is8BitPositive(writeBack))
    Processor.sr.updateFlag(StatusFlag.Overflow, overflow)
    updateZeroNegativeCaryFlags(writeBack)

    // As the write back could be greater than 0xFF for example 0xFF (-1) plus 0x01 (1) = 0x100
    // We have an overflow and result is positive (no -0) but not an overflow
    // now 0xFC (-4) add 0xFF (-1) we would expect 0xFB (-5) we get 0x1FB which is a negative result no overflow
    // We have to restrict result to 8 bits and with 0xFF
    Processor.ac.value = writeBack & 0xFF
    Processor.pc.inc(opcode.addressMode.bytes)

  def executeSTX(): Unit =
    val effectiveAddr = ExecutionUnit.getEffectiveAddress(opcode, operand)
    memoryAccess.setMemoryByte(effectiveAddr.address, Processor.ix.ebr)
    Processor.pc.inc(opcode.addressMode.bytes)


  def executeTXS(): Unit =
    Processor.sp.ebr = Processor.ix.ebr
    Processor.pc.inc(opcode.addressMode.bytes)



object ExecutionUnit:
  def apply: ExecutionUnit =
    val eu = new ExecutionUnit()
    eu

  def forTest: ExecutionUnit =
    val eu = new ExecutionUnit(true)
    eu

  def getEffectiveAddress(opcode: OpcodeValue, operand: (Int, Int)): EffectiveAddress =
    def readEffectiveAddress(addr: Int): Int =
      memoryAccess.getMemoryByte(addr) + (memoryAccess.getMemoryByte(addr + 1) * 256)

    opcode.addressMode match
    case Accumulator | Implied | Immediate =>
      EffectiveAddress()
    case ZeroPage =>
      EffectiveAddress(operand._1, true)
    case Relative =>
      // Relative addressing is used in branch instructions
      // In a real 6502 the PC is incremented on each memory fetch
      // so the PC will be pointing to the instruction following
      // As we dont inc the PC till the instruction is completed we get..
      // convert offset to signed byte value
      val offset = ByteValue.asSignedValue(operand._1.toByte)
      // then add 2 that the real 6502 would have added on each fetch
      EffectiveAddress(Processor.pc.addr + offset + 2)
    case ZeroPageX =>
      //TODO verify what happens when $LL + index exceeds 255
      EffectiveAddress(operand._1 + Processor.ix.ebr)
    case ZeroPageY =>
      //TODO verify what happens when $LL + index exceeds 255
      EffectiveAddress(operand._1 + Processor.iy.ebr)
    case IndirectX =>
      val zeroPageAddr = operand._1 + Processor.ix.ebr
      EffectiveAddress(readEffectiveAddress(zeroPageAddr))
    case IndirectY =>
      EffectiveAddress(readEffectiveAddress(operand._1) + Processor.iy.ebr)
    case Indirect =>
      val indirectAddr = operand._1 + (operand._2 * 256)
      EffectiveAddress(readEffectiveAddress(indirectAddr))
    case Absolute =>
      EffectiveAddress(operand._1 + (operand._2 * 256), true)
    case AbsoluteX =>
      EffectiveAddress(operand._1 + (operand._2 * 256) + Processor.ix.ebr)
    case AbsoluteY =>
      EffectiveAddress(operand._1 + (operand._2 * 256) + Processor.iy.ebr)
    case _ =>
      EffectiveAddress()

class EffectiveAddress(val hasValue: Boolean, val address: Int, val absolute: Boolean)

object EffectiveAddress:
  def apply(): EffectiveAddress =
    new EffectiveAddress(false, -1, false)

  def apply(address: Int, absolute: Boolean = false): EffectiveAddress =
    new EffectiveAddress(true, address, absolute)


enum RunMode:
  case Stopped
  case SingleStepping
  case Running
  case RunningSlow