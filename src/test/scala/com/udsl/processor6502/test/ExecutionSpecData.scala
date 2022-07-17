package com.udsl.processor6502.test

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.AssembleLocation.logger
import com.udsl.processor6502.assembler.{AssembleLocation, InstructionToken}
import com.udsl.processor6502.cpu.Memory.NMI_VECTOR
import com.udsl.processor6502.cpu.Processor.*
import com.udsl.processor6502.cpu.StatusRegister.*
import com.udsl.processor6502.cpu.StatusFlag.*
import com.udsl.processor6502.cpu.execution.*
import com.udsl.processor6502.cpu.{Processor, StatusRegister}
import com.udsl.processor6502.test.ExecutionSpec.{absTestLocation, absTestLocation2, asHexStr, logger, testLocation}
import com.udsl.processor6502.test.InsData.{checkValue, logger}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

trait RegValues(val acc: Int, val ix: Int, val iy:Int, val withCarry:Boolean = false, val withZero:Boolean = false, val withNegative:Boolean = false, val withOverflow:Boolean = false, val withDecimal:Boolean = false, val withInterupt:Boolean = false  )

case class ZeroValues() extends RegValues( 0, 0, 0)
case class AccValue( override val acc: Int) extends RegValues( acc, 0, 0)
case class AccValueWithCarry( override val acc: Int) extends RegValues( acc, 0, 0, true)
case class AccValueWithZero( override val acc: Int) extends RegValues( acc, 0, 0, false, true)
case class AccValueWithNegative( override val acc: Int) extends RegValues( acc, 0, 0, false, false, true)
case class AccValueWithOverflow( override val acc: Int) extends RegValues( acc, 0, 0, false, false, false, true)
case class AccValueWithDecimal( override val acc: Int) extends RegValues( acc, 0, 0, false, false, false, false, true)
case class AccValueWithInterupt( override val acc: Int) extends RegValues( acc, 0, 0, false, false, false, false, false, true)
case class AccIxValue( override val acc: Int, override val ix: Int) extends RegValues( acc, ix, 0)
case class IxValue( override val ix: Int) extends RegValues( 0, ix, 0)
case class AccIxValueWithCarry( override val acc: Int, override val ix: Int) extends RegValues( acc, ix, 0)
case class AccIyValue( override val acc: Int, override val iy: Int) extends RegValues( acc, 0, iy)
case class AccIyValueWithCarry( override val acc: Int, override val iy: Int) extends RegValues( acc, 0, iy, true)
case class AccIxIyValue( override val acc: Int, override val ix: Int, override val iy: Int) extends RegValues( acc, ix, iy)
case class AccIxIyValueWithCarry( override val acc: Int, override val ix: Int, override val iy: Int) extends RegValues( acc, ix, iy, true)



class InsData( val value: Int, val regValues: RegValues):
  def loByte: Int = value & 255
  def hiByte: Int = (value >> 8) & 255
  def hasHiByte: Boolean = value > 255

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

object Validation extends StrictLogging:
  def asHexStr( v: Int): String =
    s"0x${v.toHexString.toUpperCase()}"

  def basicValidation(): Unit =
    logger.info("not doing any validation")
    assert(Processor.sp.value == 0xFF, s"Stack pointer has changed = ${asHexStr(Processor.sp.value)}")

  def checkPc(shouldBe: Int): Unit =
    if shouldBe > 0 then // include PC check
      val pc = Processor.pc.addr
      assert(pc == shouldBe, s"PC = ${asHexStr(pc)} expected ${asHexStr(shouldBe)}")

  def checkSr(shouldBe: Int): Unit =
    val requiredMask: Int = Unused.mask | shouldBe
    assert(Processor.sr.value == requiredMask, s"SR = (${asHexStr(Processor.sr.value)}) { ${StatusRegister.asFlagsString(Processor.sr.value)} } required (${asHexStr(requiredMask)}) { ${StatusRegister.asFlagsString(shouldBe)} } - ${asHexStr(shouldBe)}")

  def checkAcc(shouldBe: Int): Unit =
    assert(Processor.ac.value == shouldBe, s"AC = ${Processor.ac.value} required ${shouldBe}")

  def checkIx(shouldBe: Int): Unit =
    assert(Processor.ix.value == shouldBe, s"IX = ${Processor.ix.value} required ${shouldBe}")

  def checkIy(shouldBe: Int): Unit =
    assert(Processor.iy.value == shouldBe, s"IY = ${Processor.iy.value} required ${shouldBe}")

  def validateStack4BTK(): Unit =
    // 3 bytes on stack
    assert(Processor.sp.value == 0xFC, s"Stack pointer NOT CORRECT should be 0xFC is ${asHexStr(Processor.sp.value)}")
    // bottom of stack should be retrun address
    val returnAdd = memoryAccess.getMemoryWrd(0x1FE)
    // BRK @2000, then the following byte so return should be to 2002
    assert(returnAdd == 2002, s"Return address on stack incorrect is $returnAdd" )
    val pushedStatus = memoryAccess.getMemoryByte(0x1FD)
    // just the break flag set (32) and the unused (16) = 48
    assert(pushedStatus == 48, s"Status pushed on stack incorrect is $pushedStatus - ${StatusRegister.asFlagsString(pushedStatus)}" )

  def validateStackWithZero4BTK(): Unit =
    // 3 bytes on stack
    assert(Processor.sp.value == 0xFC, s"Stack pointer NOT CORRECT should be 0xFC is ${asHexStr(Processor.sp.value)}")
    // bottom of stack should be retrun address
    val returnAdd = memoryAccess.getMemoryWrd(0x1FE)
    // BRK @2000, then the following byte so return should be to 2002
    assert(returnAdd == 2002, s"Return address on stack incorrect is $returnAdd" )
    val pushedStatus = memoryAccess.getMemoryByte(0x1FD)
    // the break flag set (32), zero (2) and the unused (16) = 50
    assert(pushedStatus == 50, s"Status pushed on stack incorrect is $pushedStatus - ${StatusRegister.asFlagsString(pushedStatus)}" )

trait ResultData( val ac: Int, val ix: Int, val iy: Int, val sr: Int, val pc: Int, val spValidation: () => Unit = Validation.basicValidation)

case class AccResData(override val ac: Int) extends ResultData(ac, 0, 0, 0, 0)
case class AccSrResData(override val ac: Int, override val sr: Int) extends ResultData(ac, 0, 0, sr, 0)
case class AccIySrResData(override val ac: Int, override val iy: Int, override val sr: Int) extends ResultData(ac, 0, iy, sr, 0)
case class IySrResData(override val iy: Int, override val sr: Int) extends ResultData(0, 0, iy, sr, 0)
case class AccIxSrResData(override val ac: Int, override val ix: Int, override val sr: Int) extends ResultData(ac, ix, 0, sr, 0)
case class AccIxResData(override val ac: Int, override val ix: Int) extends ResultData(ac, ix, 0, 0, 0)
case class AccIyResData(override val ac: Int, override val iy: Int) extends ResultData(ac, 0, iy, 0, 0)
case class IxSrResData(override val ix: Int, override val sr: Int) extends ResultData(0, ix, 0, sr, 0)
case class IxResData(override val ix: Int) extends ResultData(0, ix, 0, 0, 0)
case class SrResData(override val sr: Int) extends ResultData(0, 0, 0, sr, 0)
case class AccPcResData(override val ac: Int, override val pc: Int) extends ResultData(ac, 0, 0, 0, pc)
case class AccSrPcResData(override val ac: Int, override val sr: Int, override val pc: Int) extends ResultData(ac, 0, 0, sr, pc)
case class AccPcSpResData(override val ac: Int, override val pc: Int, override val spValidation: () => Unit)  extends ResultData(ac, 0, 0, 0, pc, spValidation)
case class AccSrPcSpResData(override val ac: Int, override val sr: Int, override val pc: Int, override val spValidation: () => Unit)  extends ResultData(ac, 0, 0, sr, pc, spValidation)


trait ResultMemData( val loc:Int, val value: Int, val byte: Boolean)

case class memByteResult(override val loc:Int, override val value: Int ) extends ResultMemData(loc, value, true)
case class memWrdResult(override val loc:Int, override val value: Int ) extends ResultMemData(loc, value, false)
case class memVoidResult() extends ResultMemData(0, 0, false)

object ExecutionSpecData:

  // ADC add with carry
  val dataAdcInstructionTest = List(
    ("ADC 1", InsSourceData(0x69, InsData(10, AccValue(100))), AccResData(110), memVoidResult()), // ADC immediate 10
    ("ADC 2", InsSourceData(0x69, InsData(126, AccValue(100))), AccSrResData(226, Overflow.mask | Negative.mask), memVoidResult()), // ADC immediate 126
    ("ADC 3", InsSourceData(0x65, InsData(101, AccValue(100))), AccResData(106), memVoidResult()), // ADC zer0 page 101 contains 6
    ("ADC 4", InsSourceData(0x75, InsData(101, AccValue(100))), AccResData(106), memVoidResult()), //  Token("ADC", 0x75, 2, ZeroPageX), // $LL,X
    ("ADC 5", InsSourceData(0x6D, InsData(101, AccValue(100))), AccResData(106), memVoidResult()), //  Token("ADC", 0x6D, 3, Absolute),  // $LLLL
    ("ADC 6", InsSourceData(0x7D, InsData(101, AccValue(100))), AccResData(106), memVoidResult()), //  Token("ADC", 0x7D, 3, AbsoluteX), // $LL,X
    ("ADC 7", InsSourceData(0x79, InsData(101, AccValue(100))), AccResData(106), memVoidResult()), //  Token("ADC", 0x79, 3, AbsoluteY), // $LL,Y
    // Zeropage 100 set to 0x638 by data initialisation
    ("ADC 8", InsSourceData(0x61, InsData(100, AccValue(100))), AccResData(101), memVoidResult()), //  Token("ADC", 0x61, 2, IndirectX), // ($LL,X)
    ("ADC 9", InsSourceData(0x61, InsData(100, AccValue(105))), AccResData(106), memVoidResult()), //  Token("ADC", 0x61, 2, IndirectX), // ($LL,X)
  /*
  Execute instruction with opcode 0x71 at 2000, operand 100 with ac= 99, ix=0, iy=1.
  Zeropage 100 contains address 0x638 which has been initialised with byts 1,2,3,4
  so result will be 99 + 2 = 101.
  */
    ("ADC 10", InsSourceData(0x71, InsData(100, AccIyValue(99, 1))), AccIySrResData(101, 1, Unused.mask), memVoidResult()) //  Token("ADC", 0x71, 2, IndirectY), //"($LL),Y"
  )

  // AND and (with accumulator)
  val dataAndInstructionTest = List(
    ("AND 1 acc (0x64) immediate with 0xF4 result should be 0x64", InsSourceData(0x29, InsData(0xF4, AccValue(100))), AccResData(100), memVoidResult()),
    ("AND 2  acc (0x64) zero page 101 value 6 result should be 4", InsSourceData(0x25, InsData(101, AccValue(100))), AccResData(4), memVoidResult()),
    ("AND 3 acc (0x64) zero page,X (99 + 2 = 101) value 6 result should be 6", InsSourceData(0x35, InsData(99, AccIxValue(0x66, 2))), AccIxResData(6, 2), memVoidResult()),
    ("AND 4 acc (0x64) zero page,X (99 + 2 = 101) value 6 result should be 0", InsSourceData(0x35, InsData(99, AccIxValue(0x88, 2))), IxSrResData(2, Zero.mask), memVoidResult()),
    ("AND 5", InsSourceData(0x2D, InsData(absTestLocation, AccIxValue(0xE1, 2))), AccIxResData(0x21, 2), memVoidResult()),
    ("AND 6", InsSourceData(0x2D, InsData(absTestLocation, AccIxValue(0xCC, 2))), IxSrResData(2, Zero.mask), memVoidResult()),
    ("AND 7", InsSourceData(0x3D, InsData(absTestLocation, AccIxValue(0xCC, 1))), AccIxSrResData(0xCC, 1, Negative.mask), memVoidResult()),
    ("AND 8.1 AbsoluteY absTestLocation + IY = 2 gives 0x84", InsSourceData(0x39, InsData(absTestLocation, AccIyValue(0xCC, 2))), AccIySrResData(0x84, 2, Negative.mask), memVoidResult()),
    ("AND 8.2 AbsoluteY absTestLocation + IY = 3 gives 0x00", InsSourceData(0x39, InsData(absTestLocation, AccIyValue(0xCC, 3))), IySrResData(3, Zero.mask), memVoidResult()),
    ("AND 9.1 IndirectX 100 + IX = 7 gives absTestLocation2 = 0xF0", InsSourceData(0x21, InsData(100, AccIxValue(0x66, 7))), AccIxResData(0x60, 7), memVoidResult()),
    ("AND 10", InsSourceData(0x31, InsData(100, AccIyValue(0x66, 3))), AccIyResData(0x04, 3), memVoidResult())
  )

  // ASL arithmetic shift left
  val dataAslInstructionTest = List(
    ("ASL 1.0 accumulator", InsSourceData(0x0A, InsData(0xF4, AccValue(0x20))), AccResData(0x40), memVoidResult()),
    ("ASL 1.1 accumulator", InsSourceData(0x0A, InsData(0xF4, AccValue(0x80))), AccSrResData(0x00, Carry.mask | Zero.mask), memVoidResult()),
    ("ASL 1.2 accumulator", InsSourceData(0x0A, InsData(0x7F, AccValueWithCarry(0x3F))), AccResData(0x7E), memVoidResult()),
    ("ASL 2.0 zeroPage ", InsSourceData(0x06, InsData(0x66, ZeroValues())), AccResData(0), memByteResult(0x66, 0x7E)),
    ("ASL 3.0 zeroPageX 100 -> 0x638, IX = 1 contains 2", InsSourceData(0x16, InsData(0x64, IxValue(1))), IxResData(1), memByteResult(0x639, 2)),
    ("ASL 3.1 zeroPageX 100 -> 0x638, IX = 3 contains 0x80", InsSourceData(0x16, InsData(0x64, IxValue(3))), IxSrResData(3, Carry.mask | Zero.mask), memByteResult(103, 0)),
    ("ASL 4.0 absolute absTestLocation2 contains 0xF0", InsSourceData(0x0E, InsData(absTestLocation2, ZeroValues())), SrResData(Carry.mask | Negative.mask), memByteResult(absTestLocation2, 0xE0)),
    ("ASL 5.0 absoluteX absTestLocation2 IX = 1 contains 0x3F", InsSourceData(0x1E, InsData(absTestLocation2, AccIxValue(0,1))), IxResData(1), memByteResult(absTestLocation2 + 1, 0x7E))
  )

  // BCC branch on carry clear
  val dataBccInstructionTest = List(
    ("BCC 1.0 relative carry clear PC + 6 = 4 branch + 2 fetch", InsSourceData(0x90, InsData(0x4, AccValue(0x20))), AccPcResData(0x20, testLocation + 6), memVoidResult()),
    ("BCC 1.1 relative carry clear -ve offset", InsSourceData(0x90, InsData(0xFC, AccValue(0x20))), AccPcResData(0x20, testLocation -2), memVoidResult()),
    ("BCC 2.0 relative carry set", InsSourceData(0x90, InsData(0x4, AccValueWithCarry(0x20))), AccSrResData(0x20, Carry.mask), memVoidResult())
   )

  // BCS branch on carry set
  val dataBcsInstructionTest = List(
    ("BCS 1.0 relative carry set PC + 6 = 4 branch + 2 fetch", InsSourceData(0xB0, InsData(0x4, AccValueWithCarry(0x20))), AccSrPcResData(0x20, Carry.mask, testLocation + 6), memVoidResult()),
    ("BCS 1.1 relative carry set -ve offset", InsSourceData(0xB0, InsData(0xFC, AccValueWithCarry(0x20))), AccSrPcResData(0x20, Carry.mask, testLocation -2), memVoidResult()),
    ("BCS 2.0 relative carry clear", InsSourceData(0xB0, InsData(0x4, AccValue(0x20))), AccResData(0x20), memVoidResult())
  )

  // BEQ branch on equal (zero set)
  val dataBeqInstructionTest = List(
    ("BEQ 1.0 relative zero set PC + 6 = 4 branch + 2 fetch", InsSourceData(0xF0, InsData(0x4, AccValueWithZero(0x0))), AccSrPcResData(0x0, testLocation + 6, Zero.mask), memVoidResult()),
    ("BEQ 1.1 relative zero set -ve offset", InsSourceData(0xF0, InsData(0xFC, AccValueWithZero(0x0))), AccSrPcResData(0x0, testLocation -2, Zero.mask), memVoidResult()),
    ("BEQ 2.0 relative zero clear", InsSourceData(0xF0, InsData(0x4, AccValue(0x20))), AccPcResData(0x20, testLocation + 2), memVoidResult())
  )

  // BIT bit test
  // bits 7 and 6 of operand are transfered to bit 7 and 6 of SR (N,V) the zero-flag is set to the result of operand AND accumulator.
  val dataBitInstructionTest = List(
    // The AND gives Zero result
    ("BIT 1.0 Zeropage 0x67 = 0x80", InsSourceData(0x24, InsData(0x67, ZeroValues())), AccSrResData(0x0, Zero.mask | Negative.mask), memVoidResult()),
    ("BIT 1.1 Zeropage 0x68 = 0xF0", InsSourceData(0x24, InsData(0x68, ZeroValues())), AccSrResData(0x0, Zero.mask | Negative.mask | Overflow.mask), memVoidResult()),
    ("BIT 1.2 Zeropage 0x69 = 0x40", InsSourceData(0x24, InsData(0x69, ZeroValues())), AccSrResData(0x0, Zero.mask | Overflow.mask), memVoidResult()),
    // The AND gives non Zero result
    ("BIT 2.0 Zeropage 0x67 = 0x80", InsSourceData(0x24, InsData(0x67, AccValue(0xF0))), AccSrResData(0xF0, Negative.mask), memVoidResult()),
    ("BIT 2.1 Zeropage 0x68 = 0xF0", InsSourceData(0x24, InsData(0x68, AccValue(0x30))), AccSrResData(0x30, Negative.mask | Overflow.mask), memVoidResult()),
    ("BIT 2.2 Zeropage 0x69 = 0x40", InsSourceData(0x24, InsData(0x69, AccValue(0xF0))), AccSrResData(0xF0, Overflow.mask), memVoidResult()),
    // for absolute addressing using data starting at location absTestLocation = 2500
    // The AND gives Zero result
    ("BIT 3.0 absTestLocation + 4 (0x9C8) = 0x80", InsSourceData(0x2C, InsData(0x9C8, AccValue(0x30))), AccSrResData(0x30, Zero.mask | Negative.mask), memVoidResult()),
    ("BIT 3.1 absTestLocation + 5 (0x9C9) = 0xF0", InsSourceData(0x2C, InsData(0x9C9, AccValue(0x0F))), AccSrResData(0x0F, Zero.mask | Negative.mask | Overflow.mask), memVoidResult()),
    ("BIT 3.2 absTestLocation + 6 (0x9CA) = 0x40", InsSourceData(0x2C, InsData(0x9CA, AccValue(0xA3))), AccSrResData(0xA3, Zero.mask | Overflow.mask), memVoidResult()),
    // The AND gives non Zero result
    ("BIT 4.0 absTestLocation + 4 (0x9C8) = 0x80", InsSourceData(0x2C, InsData(0x9C8, AccValue(0xF0))), AccSrResData(0xF0, Negative.mask), memVoidResult()),
    ("BIT 4.1 absTestLocation + 5 (0x9C9) = 0xF0", InsSourceData(0x2C, InsData(0x9C9, AccValue(0xC8))), AccSrResData(0xC8, Negative.mask | Overflow.mask), memVoidResult()),
    ("BIT 4.2 absTestLocation + 6 (0x9CA) = 0x40", InsSourceData(0x2C, InsData(0x9CA, AccValue(0x66))), AccSrResData(0x66, Overflow.mask), memVoidResult())
  )

  // BMI branch on minus (negative set)
  val dataBmiInstructionTest = List(
    ("BMI 1.0 relative negative set PC + 6 = 4 branch + 2 fetch", InsSourceData(0x30, InsData(0x4, AccValueWithNegative(0x0))), AccSrPcResData(0x0, Negative.mask, testLocation + 6), memVoidResult()),
    ("BMI 1.1 relative negative set set -ve offset", InsSourceData(0x30, InsData(0xFC, AccValueWithNegative(0x0))), AccSrPcResData(0x0,  Negative.mask, testLocation -2), memVoidResult()),
    ("BMI 2.0 relative negative clear", InsSourceData(0x30, InsData(0x4, AccValue(0x20))), AccPcResData(0x20, testLocation +2), memVoidResult())
  )

  // BNE branch on not equal (zero clear)
  val dataBneInstructionTest = List(
    ("BNE 1.0 relative zero clear PC + 6 = 4 branch + 2 fetch", InsSourceData(0xD0, InsData(0x4, AccValue(0x0))), AccPcResData(0x0, testLocation + 6), memVoidResult()),
    ("BNE 1.1 relative zero clear set -ve offset", InsSourceData(0xD0, InsData(0xFC, AccValue(0x0))), AccPcResData(0x0, testLocation -2), memVoidResult()),
    ("BNE 2.0 relative zero set", InsSourceData(0xD0, InsData(0x4, AccValueWithZero(0x20))), AccSrPcResData(0x20, Zero.mask, testLocation +2), memVoidResult())
  )

  // BPL branch on plus (negative clear)
  val dataBplInstructionTest = List(
    ("BPL 1.0 relative negative clear PC + 6 = 4 branch + 2 fetch", InsSourceData(0x10, InsData(0x4, AccValue(0x0))), AccPcResData(0x0, testLocation + 6), memVoidResult()),
    ("BPL 1.1 relative negative clear set -ve offset", InsSourceData(0x10, InsData(0xFC, AccValue(0x0))), AccPcResData(0x0, testLocation -2), memVoidResult()),
    ("BPL 2.0 relative negative set", InsSourceData(0x10, InsData(0x4, AccValueWithNegative(0x20))), AccSrPcResData(0x20, Negative.mask, testLocation +2), memVoidResult())
  )

  // BRK break / interrupt
  val dataBrkInstructionTest = List(
    // IRQ vector contents 3000 set in test initialisation
    ("BRK 1.0 all flags clear", InsSourceData(0x00, InsData(0x0, AccValue(0x0))), AccPcSpResData(0x0, 3000, Validation.validateStack4BTK), memVoidResult()),
    ("BRK 1.1 with Zero flag set", InsSourceData(0x00, InsData(0x0, AccValueWithZero(0x0))), AccSrPcSpResData(0x0, Zero.mask, 3000, Validation.validateStackWithZero4BTK), memVoidResult())
  )

  // BVC branch on overflow clear
  val dataBvcInstructionTest = List(
    ("BVC 1.0 relative overflow clear PC + 6 = 4 branch + 2 fetch", InsSourceData(0x50, InsData(0x4, AccValue(0x0))), AccPcResData(0x0, testLocation + 6), memVoidResult()),
    ("BVC 1.1 relative overflow clear set -ve offset", InsSourceData(0x50, InsData(0xFC, AccValue(0x0))), AccPcResData(0x0, testLocation -2), memVoidResult()),
    ("BVC 2.0 relative overflow set", InsSourceData(0x50, InsData(0x4, AccValueWithOverflow(0x20))), AccSrPcResData(0x20, Overflow.mask, testLocation +2), memVoidResult())
  )

  // BVS branch on overflow set
  val dataBvsInstructionTest = List(
    ("BVS 1.0 relative overflow set PC + 6 = 4 branch + 2 fetch", InsSourceData(0x70, InsData(0x4, AccValueWithOverflow(0x0))), AccSrPcResData(0x0, Overflow.mask, testLocation + 6), memVoidResult()),
    ("BVS 1.1 relative overflow set -ve offset", InsSourceData(0x70, InsData(0xFC, AccValueWithOverflow(0x0))), AccSrPcResData(0x0, Overflow.mask, testLocation -2), memVoidResult()),
    ("BVS 2.0 relative overflow clear", InsSourceData(0x70, InsData(0x4, AccValue(0x20))), AccPcResData(0x20, testLocation + 2), memVoidResult())
  )

  // CLC clear carry
  val dataClcInstructionTest = List(
    ("CLC 1.0 implied clear carry", InsSourceData(0x18, InsData(0x4, AccValueWithCarry(0x0))), AccResData(0x0), memVoidResult()),
    ("CLC 2.0 NOP carry still set", InsSourceData(0xEA, InsData(0x4, AccValueWithCarry(0x0))), AccSrResData(0x0, Carry.mask), memVoidResult())
  )

  // CLD clear decimal
  val dataCldInstructionTest = List(
    ("CLD 1.0 implied clear decimal", InsSourceData(0xD8, InsData(0x4, AccValueWithDecimal(0x0))), AccResData(0x0), memVoidResult()),
    ("CLD 2.0 NOP decimal still set", InsSourceData(0xEA, InsData(0x4, AccValueWithDecimal(0x0))), AccSrResData(0x0, Decimal.mask), memVoidResult())
  )

  // CLI clear interrupt disable
  val dataCliInstructionTest = List(
    ("CLI 1.0 implied clear interrupt", InsSourceData(0x58, InsData(0x4, AccValueWithInterupt(0x0))), AccResData(0x0), memVoidResult()),
    ("CLI 2.0 NOP interrupt still set", InsSourceData(0xEA, InsData(0x4, AccValueWithInterupt(0x0))), AccSrResData(0x0, Interrupt.mask), memVoidResult())
  )

  // CLV clear overflow
  val dataClvInstructionTest = List(
    ("CLV 1.0 implied clear overflow", InsSourceData(0xB8, InsData(0x4, AccValueWithOverflow(0x0))), AccResData(0x0), memVoidResult()),
    ("CLV 2.0 NOP overflow still set", InsSourceData(0xEA, InsData(0x4, AccValueWithOverflow(0x0))), AccSrResData(0x0, Overflow.mask), memVoidResult())
  )

  // CMP compare (with accumulator)
  val dataCmpInstructionTest = List(
  )

  // CPX compare with X
  val dataCpxInstructionTest = List(
  )

  // CPY compare with Y
  val dataCpyInstructionTest = List(
  )

  // DEC decrement
  val dataDecInstructionTest = List(
  )

  // DEX decrement X
  val dataDexInstructionTest = List(
  )

  // DEY decrement Y
  val dataDeyInstructionTest = List(
  )

  // EOR exclusive or (with accumulator)
  val dataEorInstructionTest = List(
  )

  // INC increment
  val dataIncInstructionTest = List(
  )

  // INX increment X
  val dataInxInstructionTest = List(
  )

  // INY increment Y
  val dataInyInstructionTest = List(
  )

  // JMP jump
  val dataJmpInstructionTest = List(
  )

  // JSR jump subroutine
  val dataJsrInstructionTest = List(
  )

  // LDA load accumulator
  val dataLdaInstructionTest = List(
  )

  // LDX load X
  val dataLdxInstructionTest = List(
  )

  // LDY load Y
  val dataLdyInstructionTest = List(
  )

  // LSR logical shift right
  val dataLsrInstructionTest = List(
  )

  // NOP no operation
  val dataNopInstructionTest = List(
  )

  // ORA or with accumulator
  val dataOraInstructionTest = List(
  )

  // PHA push accumulator
  val dataPhaInstructionTest = List(
  )

  // PHP push processor status (SR)
  val dataPhpInstructionTest = List(
  )

  // PLA pull accumulator
  val dataPlaInstructionTest = List(
  )

  // PLP pull processor status (SR)
  val dataPlpInstructionTest = List(
  )

  // ROL rotate left
  val dataRolInstructionTest = List(
  )

  // ROR rotate right
  val dataRorInstructionTest = List(
  )

  // RTI return from interrupt
  val dataRtiInstructionTest = List(
  )

  // RTS return from subroutine
  val dataRtsInstructionTest = List(
  )

  // SBC subtract with carry
  val dataSbcInstructionTest = List(
  )

  // SEC set carry
  val dataSecInstructionTest = List(
  )

  // SED set decimal
  val dataSedInstructionTest = List(
  )

  // SEI set interrupt disable
  val dataSeiInstructionTest = List(
  )

  // STA store accumulator
  val dataStaInstructionTest = List(
  )

  // STX store X
  val dataStxInstructionTest = List(
  )

  // STY store Y
  val dataStyInstructionTest = List(
  )

  // TAX transfer accumulator to X
  val dataTaxInstructionTest = List(
  )

  // TAY transfer accumulator to Y
  val dataTayInstructionTest = List(
  )

  // TSX transfer stack pointer to X
  val dataTsxInstructionTest = List(
  )

  // TXA transfer X to accumulator
  val dataTxaInstructionTest = List(
  )

  // TXS transfer X to stack pointer
  val dataTxsInstructionTest = List(
  )

  // TYA transfer Y to accumulator
  val dataTyaInstructionTest = List(
  )

