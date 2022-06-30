package com.udsl.processor6502.test

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.AssembleLocation.logger
import com.udsl.processor6502.assembler.{AssembleLocation, InstructionToken}
import com.udsl.processor6502.cpu.Memory.NMI_VECTOR
import com.udsl.processor6502.cpu.Processor.*
import com.udsl.processor6502.cpu.StatusRegister.*
import com.udsl.processor6502.cpu.execution.*
import com.udsl.processor6502.cpu.{Processor, StatusRegisterFlags}
import com.udsl.processor6502.test.ExecutionSpec.{absTestLocation, absTestLocation2, fixedValuesInitialised, logger}
import com.udsl.processor6502.test.InsData.{checkValue, logger}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

trait RegValues(val acc: Int, val ix: Int, val iy:Int, val withCarry:Boolean )

case class ZeroValues() extends RegValues( 0, 0, 0, false)
case class AccValue( override val acc: Int) extends RegValues( acc, 0, 0, false)
case class AccValueWithCarry( override val acc: Int) extends RegValues( acc, 0, 0, true)
case class AccIxValue( override val acc: Int, override val ix: Int) extends RegValues( acc, ix, 0, false)
case class AccIxValueWithCarry( override val acc: Int, override val ix: Int) extends RegValues( acc, ix, 0, true)
case class AccIyValue( override val acc: Int, override val iy: Int) extends RegValues( acc, 0, iy, false)
case class AccIyValueWithCarry( override val acc: Int, override val iy: Int) extends RegValues( acc, 0, iy, true)
case class AccIxIyValue( override val acc: Int, override val ix: Int, override val iy: Int) extends RegValues( acc, ix, iy, false)
case class AccIxIyValueWithCarry( override val acc: Int, override val ix: Int, override val iy: Int) extends RegValues( acc, ix, iy, true)



class InsData( val value: Int, val regValues: RegValues):
  def loByte: Int = value & 255
  def hiByte: Int = (value >> 8) & 255
  def hasHiByte: Boolean = (value > 255)

object InsData extends StrictLogging:

  def apply(value: Int, regValues: RegValues): InsData =
    checkValue(value)
    validate(regValues)
    new InsData(value, regValues)

  def validate(regValues: RegValues): Unit =
    checkAccValue(regValues.acc)
    checkIxValue(regValues.ix)
    checkIyValue(regValues.iy)

  def checkValue(value: Int): Unit =
    if value < 0 || value > 65535 then
      val errorMessage = s"Bad word value $value"
      logger.debug(errorMessage)
      throw new Exception(errorMessage)

  def checkAccValue(acc: Int): Unit =
    if acc < 0 || acc > 255 then
      val errorMessage = s"Bad accumulator value $acc"
      logger.debug(errorMessage)
      throw new Exception(errorMessage)

  def checkIxValue(ix: Int): Unit =
    if ix < 0 || ix > 255 then
      val errorMessage = s"Bad IX value $ix"
      logger.debug(errorMessage)
      throw new Exception(errorMessage)

  def checkIyValue(iy: Int): Unit =
    if iy < 0 || iy > 255 then
      val errorMessage = s"Bad IY value $iy"
      logger.debug(errorMessage)
      throw new Exception(errorMessage)

// InsData( value, acc, ix, iy)
case class InsSourceData(opcode: Int, data: InsData)
case class InsResultData(ac: Int, ix: Int, iy: Int = 0, sr: Int = UNUSED_FLAG_MASK)

trait ResultMemData( val loc:Int, val value: Int, val byte: Boolean)

case class memByteResult(override val loc:Int, override val value: Int ) extends ResultMemData(loc, value, true)
case class memWrdResult(override val loc:Int, override val value: Int ) extends ResultMemData(loc, value, false)
case class memVoidResult() extends ResultMemData(0, 0, false)

class ExecutionSpec extends AnyFlatSpec, should.Matchers, StrictLogging:

  // ADC add with carry
  val dataAdcInstructionTest = List(
    ("ADC 1", InsSourceData(0x69, InsData(10, AccValue(100))), InsResultData(110,0,0), memVoidResult()), // ADC immediate 10
    ("ADC 2", InsSourceData(0x69, InsData(126, AccValue(100))), InsResultData(226,0,0,OVERFLOW_FLAG_MASK | NEGATIVE_FLAG_MASK), memVoidResult()), // ADC immediate 126
    ("ADC 3", InsSourceData(0x65, InsData(101, AccValue(100))), InsResultData(106, 0), memVoidResult()), // ADC zer0 page 101 contains 6
    ("ADC 4", InsSourceData(0x75, InsData(101, AccValue(100))), InsResultData(106, 0), memVoidResult()), //  Token("ADC", 0x75, 2, ZeroPageX), // $LL,X
    ("ADC 5", InsSourceData(0x6D, InsData(101, AccValue(100))), InsResultData(106, 0), memVoidResult()), //  Token("ADC", 0x6D, 3, Absolute),  // $LLLL
    ("ADC 6", InsSourceData(0x7D, InsData(101, AccValue(100))), InsResultData(106, 0), memVoidResult()), //  Token("ADC", 0x7D, 3, AbsoluteX), // $LL,X
    ("ADC 7", InsSourceData(0x79, InsData(101, AccValue(100))), InsResultData(106, 0), memVoidResult()), //  Token("ADC", 0x79, 3, AbsoluteY), // $LL,Y
    // Zeropage 100 set to 0x638 by data initialisation
    ("ADC 8", InsSourceData(0x61, InsData(100, AccValue(100))), InsResultData(101, 0), memVoidResult()), //  Token("ADC", 0x61, 2, IndirectX), // ($LL,X)
    ("ADC 9", InsSourceData(0x61, InsData(100, AccValue(105))), InsResultData(106, 0), memVoidResult()), //  Token("ADC", 0x61, 2, IndirectX), // ($LL,X)
  /*
  Execute instruction with opcode 0x71 at 2000, operand 100 with ac= 99, ix=0, iy=1.
  Zeropage 100 contains address 0x638 which has been initialised with byts 1,2,3,4
  so result will be 99 + 2 = 101.
  */
    ("ADC 10", InsSourceData(0x71, InsData(100, AccIyValue(99, 1))), InsResultData(101, 0, 1, UNUSED_FLAG_MASK), memVoidResult()) //  Token("ADC", 0x71, 2, IndirectY), //"($LL),Y"
  )

  // AND and (with accumulator)
  val dataAndInstructionTest = List(
    ("AND 1", InsSourceData(0x29, InsData(0xF4, AccValue(100))), InsResultData(100, 0, 0), memVoidResult()), // And acc (0x64) immediate with 0xF4 result should be 0x64
    ("AND 2", InsSourceData(0x25, InsData(101, AccValue(100))), InsResultData(4, 0, 0), memVoidResult()), // And acc (0x64) zero page 101 value 6 result should be 4
    ("AND 3", InsSourceData(0x35, InsData(99, AccIxValue(0x66, 2))), InsResultData(6, 2, 0), memVoidResult()), // And acc (0x64) zero page,X (99 + 2 = 101) value 6 result should be 6
    ("AND 4", InsSourceData(0x35, InsData(99, AccIxValue(0x88, 2))), InsResultData(0, 2, 0, ZERO_FLAG_MASK), memVoidResult()), // And acc (0x64) zero page,X (99 + 2 = 101) value 6 result should be 0
    ("AND 5", InsSourceData(0x2D, InsData(absTestLocation, AccIxValue(0xE1, 2))), InsResultData(0x21, 2, 0), memVoidResult()), //  TestData("AND", 0x2D, 3, Absolute),  // $LLLL
    ("AND 6", InsSourceData(0x2D, InsData(absTestLocation, AccIxValue(0xCC, 2))), InsResultData(0, 2, 0, ZERO_FLAG_MASK), memVoidResult()), //  TestData("AND", 0x2D, 3, Absolute),  // $LLLL
    ("AND 7", InsSourceData(0x3D, InsData(absTestLocation, AccIxValue(0xCC, 1))), InsResultData(0xCC, 1, 0, NEGATIVE_FLAG_MASK), memVoidResult()), //  TestData("AND", 0x3D, 3, AbsoluteX), // $LL,X
    ("AND 8.1 AbsoluteY absTestLocation + IY = 2 gives 0x84", InsSourceData(0x39, InsData(absTestLocation, AccIyValue(0xCC, 2))), InsResultData(0x84, 0, 2, NEGATIVE_FLAG_MASK), memVoidResult()), //  TestData("AND", 0x39, 3, AbsoluteY), // $LL,Y
    ("AND 8.2 AbsoluteY absTestLocation + IY = 3 gives 0x00", InsSourceData(0x39, InsData(absTestLocation, AccIyValue(0xCC, 3))), InsResultData(0, 0, 3, ZERO_FLAG_MASK), memVoidResult()),
    ("AND 9.1 IndirectX 100 + IX = 4 gives absTestLocation2 = 0xF0", InsSourceData(0x21, InsData(100, AccIxValue(0x66, 4))), InsResultData(0x60, 4), memVoidResult()), //  TestData("AND", 0x21, 2, IndirectX), // ($LL,X)
    ("AND 10", InsSourceData(0x31, InsData(100, AccIyValue(0x66, 3))), InsResultData(0x04, 0, 3), memVoidResult())//  TestData("AND", 0x31, 2, IndirectY), //"($LL),Y"
  )

  // ASL arithmetic shift left
  val dataAslInstructionTest = List(
    ("ASL 1.0 accumulator", InsSourceData(0x0A, InsData(0xF4, AccValue(0x20))), InsResultData(0x40, 0), memVoidResult()), //  TestData("ASL", 0x0A, 1, Accumulator), // #
    ("ASL 1.1 accumulator", InsSourceData(0x0A, InsData(0xF4, AccValue(0x80))), InsResultData(0x00, 0, 0, CARRY_FLAG_MASK | ZERO_FLAG_MASK), memVoidResult()), //  TestData("ASL", 0x0A, 1, Accumulator), // #
    ("ASL 1.2 accumulator", InsSourceData(0x0A, InsData(0x7F, AccValueWithCarry(0x3F))), InsResultData(0x7E, 0), memVoidResult()), //  TestData("ASL", 0x0A, 1, Accumulator), // #
    ("ASL 2.0 zeroPage ", InsSourceData(0x06, InsData(0x66, ZeroValues())), InsResultData(0, 0, 0), memByteResult(0x66, 0x7E)), //  TestData("ASL", 0x06, 2, ZeroPage),  // $LL
    ("ASL 3.0 zeroPageX 100 -> 0x638, IX = 1 contains 2", InsSourceData(0x16, InsData(0x64, AccIxValue(0,1))), InsResultData(0, 1), memByteResult(0x639, 2)),//  TestData("ASL", 0x16, 2, ZeroPageX), // $LL,X
    ("ASL 3.1 zeroPageX 100 -> 0x638, IX = 3 contains 0x80", InsSourceData(0x16, InsData(0x64, AccIxValue(0,3))), InsResultData(0, 3, 0, CARRY_FLAG_MASK | ZERO_FLAG_MASK), memByteResult(103, 0)), //  TestData("ASL", 0x16, 2, ZeroPageX), // $LL,X
    ("ASL 4.0 absolute absTestLocation2 contains 0xF0", InsSourceData(0x0E, InsData(absTestLocation2, ZeroValues())), InsResultData(0, 0, 0, CARRY_FLAG_MASK | NEGATIVE_FLAG_MASK), memByteResult(absTestLocation2, 0xE0)),//  TestData("ASL", 0x0E, 3, Absolute),  // $LLLL
    ("ASL 5.0 absoluteX absTestLocation2 IX = 1 contains 0x3F", InsSourceData(0x1E, InsData(absTestLocation2, AccIxValue(0,1))), InsResultData(0, 1), memByteResult(absTestLocation2 + 1, 0x7E)),//  TestData("ASL", 0x1E, 3, AbsoluteX), // $LL,X
  )

// BCC branch on carry clear
// BCS branch on carry set
// BEQ branch on equal (zero set)
// BIT bit test
// BMI branch on minus (negative set)
// BNE branch on not equal (zero clear)
// BPL branch on plus (negative clear)
// BRK break / interrupt
// BVC branch on overflow clear
// BVS branch on overflow set
// CLC clear carry
// CLD clear decimal
// CLI clear interrupt disable
// CLV clear overflow
// CMP compare (with accumulator)
// CPX compare with X
// CPY compare with Y
// DEC decrement
// DEX decrement X
// DEY decrement Y
// EOR exclusive or (with accumulator)
// INC increment
// INX increment X
// INY increment Y
// JMP jump
// JSR jump subroutine
// LDA load accumulator
// LDX load X
// LDY load Y
// LSR logical shift right
// NOP no operation
// ORA or with accumulator
// PHA push accumulator
// PHP push processor status (SR)
// PLA pull accumulator
// PLP pull processor status (SR)
// ROL rotate left
// ROR rotate right
// RTI return from interrupt
// RTS return from subroutine
// SBC subtract with carry
// SEC set carry
// SED set decimal
// SEI set interrupt disable
// STA store accumulator
// STX store X
// STY store Y
// TAX transfer accumulator to X
// TAY transfer accumulator to Y
// TSX transfer stack pointer to X
// TXA transfer X to accumulator
// TXS transfer X to stack pointer
// TYA transfer Y to accumulator


  "Given a valid ADC instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataAdcInstructionTest) {
      logger.info(s"\nStarting test: ${title}")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title)
    }
  }

  "Given a valid AND instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataAndInstructionTest) {
      logger.info(s"\nStarting test: ${title}")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title)
    }
  }

  "Given a valid ASL instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataAslInstructionTest) {
      logger.info(s"\nStarting test: ${title}")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title)
    }
  }

object ExecutionSpec extends StrictLogging:

  var fixedValuesInitialised = false
  val testLocation = 2000
  val absTestLocation = 2500
  val absTestLocation2 = 2600

  def checkRes(resData: InsResultData, memRes: ResultMemData, title: String): Unit =
    logger.info(s"Checking results for $title")
    assert(Processor.ac.value == resData.ac, s"AC = ${Processor.ac.value} required ${resData.ac} - ${resData}")
    assert(Processor.ix.value == resData.ix, s"IX = ${Processor.ix.value} required ${resData.ix} - ${resData}")
    assert(Processor.iy.value == resData.iy, s"IY = ${Processor.iy.value} required ${resData.iy} - ${resData}")

    // Ensure we have the UNUSED_FLAG_MASK in the value
    val requiredMask: Int = UNUSED_FLAG_MASK | resData.sr

    assert(Processor.sr.value == requiredMask, s"SR = ${Processor.sr.value} required ${requiredMask} - ${resData}")

    if memRes.byte then
      // checking memory byte
      val checkByte = memoryAccess.getMemoryByte(memRes.loc)
      assert(checkByte == memRes.value,s"Memory byte at = ${memRes.loc} (0x${memRes.loc.toHexString.toUpperCase()}) is $checkByte (0x${checkByte.toHexString.toUpperCase()}) required ${memRes.value} " +
        s"(0x${memRes.value.toHexString.toUpperCase()})")
    else if memRes.loc > 0 then // testing location zer0 makes no sense
      // checking memory wrd
      val checkWrd = memoryAccess.getMemoryWrd(memRes.loc)
      assert(checkWrd == memRes.value,s"Memory word at = ${memRes.loc} (0x${memRes.loc.toHexString.toUpperCase()}) is $checkWrd (0x${checkWrd.toHexString.toUpperCase()}) required ${memRes.value} " +
        s"(0x${memRes.value.toHexString.toUpperCase()})")

  /**
   * Initialise memory and registers for test
   */
  def initFixedValuesForTest() : Unit =
    if !fixedValuesInitialised then
      AssembleLocation.setAssembleLoc(100) // zero page location
      // setting a zero page pointer to address 0x638
      AssembleLocation.setMemoryByte(0x38) // 100 (0x64) = 56
      AssembleLocation.setMemoryByte(6) // 101 (0x65) =6
      AssembleLocation.setMemoryByte(0x3F) // 102 (0x66) = 63
      AssembleLocation.setMemoryByte(0x80) // 103 (0x67) = 128
      AssembleLocation.setMemoryAddress(absTestLocation2) // 103 (ox67) pointer to absTestLocation2

      AssembleLocation.setAssembleLoc(0x638) // set current location to ins location
      for x <- List(1,2,3,4, 0x80) do
        AssembleLocation.setMemoryByte(x)

      AssembleLocation.setAssembleLoc(absTestLocation) // set current location to absTestLocation
      AssembleLocation.setMemoryByte(0x33) // absTestLocation = 0x33
      AssembleLocation.setMemoryByte(0xCC) // (absTestLocation  + 1)= 0xCC
      AssembleLocation.setMemoryByte(0x84) // (absTestLocation  + 2)= 0x84
      AssembleLocation.setMemoryByte(0x00) // (absTestLocation  + 3)= 0x00
      AssembleLocation.setMemoryAddress(absTestLocation2) // (absTestLocation  + 2) pointer to address absTestLocation2

      AssembleLocation.setAssembleLoc(absTestLocation2) // set current location to absTestLocation2
      AssembleLocation.setMemoryByte(0xF0) // absTestLocation2 = 0xF0
      AssembleLocation.setMemoryByte(0x3F) // (absTestLocation2 + 1) = 0xF0


      AssembleLocation.setAssembleLoc(NMI_VECTOR)  // start of vectors in memory
      AssembleLocation.setMemoryAddress(1000) // set NMI vector to address 1000
      AssembleLocation.setMemoryAddress(2000) // set RESET vector to address 2000
      AssembleLocation.setMemoryAddress(3000) // set INTERRUPT vector to address 3000


      AssembleLocation.setAssembleLoc(testLocation) // set current location to ins location
    fixedValuesInitialised = true

  def initValuesForTest(sourceData : InsSourceData) : Unit =
    AssembleLocation.setAssembleLoc(testLocation)
    AssembleLocation.setMemoryByte(sourceData.opcode)
    AssembleLocation.setMemoryByte(sourceData.data.loByte)
    if sourceData.data.hasHiByte then
      AssembleLocation.setMemoryByte(sourceData.data.hiByte)
    initRegisters(sourceData.data)

  def initRegisters(regData: InsData) : Unit =
    Processor.pc.addr = testLocation
    Processor.ac.value = regData.regValues.acc
    Processor.ix.value = regData.regValues.ix
    Processor.iy.value = regData.regValues.iy
    Processor.sr.reset()
    if regData.regValues.withCarry then
      Processor.sr.setFlag(StatusRegisterFlags.Carry)



