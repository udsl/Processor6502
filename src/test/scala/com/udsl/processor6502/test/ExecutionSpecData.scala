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
import com.udsl.processor6502.test.ExecutionSpec.{absTestLocation, absTestLocation2, asHexStr, logger, testLocation, testLocation2Ptr, zeroPageData}
import com.udsl.processor6502.test.InsData.{checkValue, logger}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

trait RegValues(val acc: Int, val ix: Int, val iy:Int, val withCarry:Boolean = false, val withZero:Boolean = false, val withNegative:Boolean = false, val withOverflow:Boolean = false, val withDecimal:Boolean = false, val withInterrupt:Boolean = false  )

case class ZeroValues() extends RegValues( 0, 0, 0,false, false, false, false, false)
case class SrValue( override val withCarry:Boolean = false, override val withZero:Boolean = false, override val withNegative:Boolean = false, override val withOverflow:Boolean = false, override val withDecimal:Boolean = false) extends RegValues( 0, 0, 0, withCarry, withZero, withNegative, withOverflow, withDecimal)
object SrValue:
  def apply(srValue: Int) : SrValue =
    new SrValue(testValueForFlag(srValue, Carry), testValueForFlag(srValue, Zero), testValueForFlag(srValue, Negative), testValueForFlag(srValue, Overflow), testValueForFlag(srValue, Decimal))

case class AccValue( override val acc: Int) extends RegValues( acc, 0, 0)
case class AccValueWithCarry( override val acc: Int) extends RegValues( acc, 0, 0, true)
case class AccValueWithZero( override val acc: Int) extends RegValues( acc, 0, 0, false, true)
case class AccValueWithNegative( override val acc: Int) extends RegValues( acc, 0, 0, false, false, true)
case class AccValueWithOverflow( override val acc: Int) extends RegValues( acc, 0, 0, false, false, false, true)
case class AccValueWithDecimal( override val acc: Int) extends RegValues( acc, 0, 0, false, false, false, false, true)
case class AccValueWithInterrupt(override val acc: Int) extends RegValues( acc, 0, 0, false, false, false, false, false, true)
case class AccIxValue( override val acc: Int, override val ix: Int) extends RegValues( acc, ix, 0)
case class IxValue( override val ix: Int) extends RegValues( 0, ix, 0)
case class IxValueWithCarry( override val ix: Int) extends RegValues( 0, ix, 0, true)
case class IyValue( override val iy: Int) extends RegValues( 0, 0, iy)
case class IyValueWithCarry( override val iy: Int) extends RegValues( 0, 0, iy, true)
case class IxIyValue( override val ix: Int, override val iy: Int) extends RegValues( 0, ix, iy)
case class AccIxValueWithCarry( override val acc: Int, override val ix: Int) extends RegValues( acc, ix, 0, true)
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
    assert(Processor.ac.value == shouldBe, s"AC = ${Processor.ac.value} {${asHexStr(Processor.ac.value)}} required $shouldBe {${asHexStr(shouldBe)}}")

  def checkIx(shouldBe: Int): Unit =
    assert(Processor.ix.value == shouldBe, s"IX = ${Processor.ix.value} required $shouldBe")

  def checkIy(shouldBe: Int): Unit =
    assert(Processor.iy.value == shouldBe, s"IY = ${Processor.iy.value} required $shouldBe")


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

case class ZeroResData() extends ResultData(0, 0, 0, 0, 0)
case class AccResData(override val ac: Int) extends ResultData(ac, 0, 0, 0, 0)
case class AccSrResData(override val ac: Int, override val sr: Int) extends ResultData(ac, 0, 0, sr, 0)
case class AccIySrResData(override val ac: Int, override val iy: Int, override val sr: Int) extends ResultData(ac, 0, iy, sr, 0)
case class IySrResData(override val iy: Int, override val sr: Int) extends ResultData(0, 0, iy, sr, 0)
case class AccIxSrResData(override val ac: Int, override val ix: Int, override val sr: Int) extends ResultData(ac, ix, 0, sr, 0)
case class AccIxResData(override val ac: Int, override val ix: Int) extends ResultData(ac, ix, 0, 0, 0)
case class AccIyResData(override val ac: Int, override val iy: Int) extends ResultData(ac, 0, iy, 0, 0)
case class IxSrResData(override val ix: Int, override val sr: Int) extends ResultData(0, ix, 0, sr, 0)
case class IxResData(override val ix: Int) extends ResultData(0, ix, 0, 0, 0)
case class IyResData(override val iy: Int) extends ResultData(0, 0, iy, 0, 0)
case class IxIyResData(override val ix: Int, override val iy: Int) extends ResultData(0, ix, iy, 0, 0)
case class SrResData(override val sr: Int) extends ResultData(0, 0, 0, sr, 0)
case class AccPcResData(override val ac: Int, override val pc: Int) extends ResultData(ac, 0, 0, 0, pc)
case class PcResData(override val pc: Int) extends ResultData(0, 0,0, 0, pc)
case class PcSpResData(override val pc: Int, override val spValidation: () => Unit) extends ResultData(0, 0, 0, 0, pc, spValidation)
case class SrPcSpResData(override val sr: Int, override val pc: Int, override val spValidation: () => Unit) extends ResultData(0, 0, 0, sr, pc, spValidation)
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
    ("ADC 1.0 immediate acc = 0x64 add 0x0A", InsSourceData(0x69, InsData(10, AccValue(100))), AccResData(110), memVoidResult()),
    ("ADC 1.1 immediate acc = 0x64 add 126", InsSourceData(0x69, InsData(126, AccValue(100))), AccSrResData(226, Overflow.mask | Negative.mask), memVoidResult()),
    ("ADC 1.2 immediate acc = 0x64 add 0x0A carry set", InsSourceData(0x69, InsData(10, AccValueWithCarry(100))), AccResData(111), memVoidResult()),
    ("ADC 2.0 zeropage 101 -> 0x06", InsSourceData(0x65, InsData(101, AccValue(100))), AccResData(106), memVoidResult()),
    ("ADC 3.0 zeropage,x", InsSourceData(0x75, InsData(100, AccIxValue(100, 1))), AccIxResData(106, 1), memVoidResult()),
    ("ADC 4.0 absolute absTestLocation -> 0x33", InsSourceData(0x6D, InsData(absTestLocation, AccValue(0x64))), AccSrResData(0x97, Negative.mask | Overflow.mask), memVoidResult()),
    ("ADC 5.0 absolute,x absTestLocation + 6 -> 0x40", InsSourceData(0x7D, InsData(absTestLocation, AccIxValue(0x24, 6))), AccIxResData(0x64, 6), memVoidResult()),
    ("ADC 6.0 absolute,y absTestLocation + 6", InsSourceData(0x79, InsData(absTestLocation, AccIyValue(0x64, 6))), AccIySrResData(0xA4, 6, Negative.mask | Overflow.mask), memVoidResult()),
    // Zeropage 100 set to 0x638 by data initialisation
    ("ADC 7.0 (indirect,x) zeroPageData + 7 -> 0xF0", InsSourceData(0x61, InsData(zeroPageData, AccIxValue(0x64, 7))), AccIxSrResData(0x54, 7, Carry.mask), memVoidResult()),
    ("ADC 7.1 (indirect,x) zeroPageData + 7 -> 0xF0", InsSourceData(0x61, InsData(zeroPageData, AccIxValueWithCarry(0x64, 7))), AccIxSrResData(0x55, 7, Carry.mask), memVoidResult()),
    ("ADC 7.2 (indirect,x) zeroPageData + 7 -> 0xF0", InsSourceData(0x61, InsData(zeroPageData, AccIxValue(0x04, 7))), AccIxSrResData(0xF4, 7, Negative.mask), memVoidResult()),
  /*
  Execute instruction with opcode 0x71 at 2000, operand 100 with ac= 99, ix=0, iy=1.
  Zeropage 100 contains address 0x638 which has been initialised with byts 1,2,3,4
  so result will be 99 + 2 = 101.
  */
    ("ADC 8.0 (indirect),y", InsSourceData(0x71, InsData(100, AccIyValue(99, 1))), AccIyResData(101, 1), memVoidResult())
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
    ("ASL 3.0 zeroPageX 100 -> 0x638, IX = 1 contains 2", InsSourceData(0x16, InsData(0x64, IxValue(1))), IxResData(1), memByteResult(0x65, 0x0C)),
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

  def validateStack4BTK(): Unit =
    // 3 bytes on stack
    assert(Processor.sp.value == 0xFC, s"Stack pointer NOT CORRECT should be 0xFC is ${asHexStr(Processor.sp.value)}")
    // bottom of stack should be return address
    val returnAdd = memoryAccess.getMemoryWrd(0x1FE)
    // BRK @2000, then the following byte so return should be to 2002
    assert(returnAdd == 2002, s"Return address on stack incorrect is ${asHexStr(returnAdd)} ($returnAdd)" )
    val pushedStatus = memoryAccess.getMemoryByte(0x1FD)
    // just the break flag set (32) and the unused (16) = 48
    assert(pushedStatus == 48, s"Status pushed on stack incorrect is $pushedStatus - ${StatusRegister.asFlagsString(pushedStatus)}" )

  // BRK break / interrupt
  val dataBrkInstructionTest = List(
    // IRQ vector contents 3000 set in test initialisation
    ("BRK 1.0 all flags clear", InsSourceData(0x00, InsData(0x0, AccValue(0x0))), AccPcSpResData(0x0, 3000, validateStack4BTK), memVoidResult()),
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
    ("CLI 1.0 implied clear interrupt", InsSourceData(0x58, InsData(0x4, AccValueWithInterrupt(0x0))), AccResData(0x0), memVoidResult()),
    ("CLI 2.0 NOP interrupt still set", InsSourceData(0xEA, InsData(0x4, AccValueWithInterrupt(0x0))), AccSrResData(0x0, Interrupt.mask), memVoidResult())
  )

  // CLV clear overflow
  val dataClvInstructionTest = List(
    ("CLV 1.0 implied clear overflow", InsSourceData(0xB8, InsData(0x4, AccValueWithOverflow(0x0))), AccResData(0x0), memVoidResult()),
    ("CLV 2.0 NOP overflow still set", InsSourceData(0xEA, InsData(0x4, AccValueWithOverflow(0x0))), AccSrResData(0x0, Overflow.mask), memVoidResult())
  )

  // CMP compare (with accumulator) only effects Zero, Negative and carry flags
  val dataCmpInstructionTest = List(
    ("CMP 1.0 Acc 0xF0 immediate with 0xF4 result Negative  set", InsSourceData(0xC9, InsData(0xF4, AccValue(0xF0))), AccSrResData(0xF0, Negative.mask), memVoidResult()),
    ("CMP 1.1 Acc 0x80 immediate with 0x40 result overflow set", InsSourceData(0xC9, InsData(0x40, AccValue(0x80))), AccSrResData(0x80, Carry.mask), memVoidResult()),
    ("CMP 1.2 Acc 0x80 immediate with 0x80 result Zero set", InsSourceData(0xC9, InsData(0x80, AccValue(0x80))), AccSrResData(0x80, Zero.mask), memVoidResult()),
    ("CMP 2.0 acc (0x64) zero page 101 value 6 ", InsSourceData(0xC5, InsData(101, AccValue(0x64))), AccSrResData(0x64, Carry.mask), memVoidResult()),
    ("CMP 3.0 acc (0x64) zero page,X (100 + 2 = 102) value 0x3F result should be 6", InsSourceData(0xD5, InsData(100, AccIxValue(0x64, 2))), AccIxSrResData(0x64, 2, Carry.mask), memVoidResult()),
    ("CMP 4.0 acc (0x20) absolute (2500) = 0x33", InsSourceData(0xCD, InsData(absTestLocation, AccValue(0x20))), AccSrResData(0x20, Negative.mask), memVoidResult()),
    ("CMP 5.0 acc (0xCC) absoluteX (2500 + 2) = 0x84", InsSourceData(0xDD, InsData(absTestLocation, AccIxValue(0xCC, 2))), AccIxSrResData(0xCC, 2, Carry.mask), memVoidResult()),
    ("CMP 6.0 acc (0x84) AbsoluteY absTestLocation + IY = 2 gives 0x84", InsSourceData(0xD9, InsData(absTestLocation, AccIyValue(0x84, 2))), AccIySrResData(0x84, 2, Zero.mask), memVoidResult()),
    ("CMP 7.0 acc (0x84) IndirectX 100 + IX = 7 gives absTestLocation2 = 0xF0", InsSourceData(0xC1, InsData(100, AccIxValue(0x66, 7))), AccIxSrResData(0x66, 7, Negative.mask), memVoidResult()),
    ("CMP 8.0 acc (0x66) IndirectY 107 = absTestLocation2 + Y -> 0x66", InsSourceData(0xD1, InsData(107, AccIyValue(0x66, 3))), AccIySrResData(0x66, 3, Zero.mask), memVoidResult())
  )

  // CPX compare with X
  val dataCpxInstructionTest = List(
    ("CPX 1.0 Acc 0xF0 immediate with 0xF4 result Negative  set", InsSourceData(0xE0, InsData(0xF4, AccIxValue(0xF0, 0xF0))), AccIxSrResData(0xF0, 0xF0, Negative.mask), memVoidResult()),
    ("CPX 1.1 Acc 0x80 immediate with 0x40 result overflow set", InsSourceData(0xE0, InsData(0x40, AccIxValue(0x80, 0x80))), AccIxSrResData(0x80, 0x80, Carry.mask), memVoidResult()),
    ("CPX 1.2 Acc 0x80 immediate with 0x80 result Zero set", InsSourceData(0xE0, InsData(0x80, AccIxValue(0x80, 0x80))), AccIxSrResData(0x80, 0x80, Zero.mask), memVoidResult()),
    ("CPX 2.0 acc (0x64) zero page 101 value 6 ", InsSourceData(0xE4, InsData(101, AccIxValue(0x64, 0x64))), AccIxSrResData(0x64, 0x64, Carry.mask), memVoidResult()),
    ("CPX 3.0 acc (0x20) absolute (2500) = 0x33", InsSourceData(0xEC, InsData(absTestLocation, AccIxValue(0x20, 0x20))), AccIxSrResData(0x20, 0x20, Negative.mask), memVoidResult()),
  )

  // CPY compare with Y
  val dataCpyInstructionTest = List(
    ("CPY 1.0 Acc 0xF0 immediate with 0xF4 result Negative  set", InsSourceData(0xC0, InsData(0xF4, AccIyValue(0xF0, 0xF0))), AccIySrResData(0xF0, 0xF0, Negative.mask), memVoidResult()),
    ("CPY 1.1 Acc 0x80 immediate with 0x40 result overflow set", InsSourceData(0xC0, InsData(0x40, AccIyValue(0x80, 0x80))), AccIySrResData(0x80, 0x80, Carry.mask), memVoidResult()),
    ("CPY 1.2 Acc 0x80 immediate with 0x80 result Zero set", InsSourceData(0xC0, InsData(0x80, AccIyValue(0x80, 0x80))), AccIySrResData(0x80, 0x80, Zero.mask), memVoidResult()),
    ("CPY 2.0 acc (0x64) zero page 101 value 6 ", InsSourceData(0xC4, InsData(101, AccIyValue(0x64, 0x64))), AccIySrResData(0x64, 0x64, Carry.mask), memVoidResult()),
    ("CPY 3.0 acc (0x20) absolute (2500) = 0x33", InsSourceData(0xCC, InsData(absTestLocation, AccIyValue(0x20, 0x20))), AccIySrResData(0x20, 0x20, Negative.mask), memVoidResult())
  )


  // DEC decrement
  val dataDecInstructionTest = List(
    ("DEC 1.0 zeropage 0x64 = 0x38", InsSourceData(0xC6, InsData(0x64, ZeroValues())), ZeroResData(),  memByteResult(0x64, 0x37)),
    ("DEC 1.1 zeropage 0x6D = 0x00", InsSourceData(0xC6, InsData(0x6D, ZeroValues())), SrResData(Negative.mask),  memByteResult(0x6D, 0xFF)),
    ("DEC 1.2 zeropage 0x6E = 0x01", InsSourceData(0xC6, InsData(0x6E, ZeroValues())), SrResData(Zero.mask),  memByteResult(0x6E, 0x00)),
    ("DEC 2.0 zeropage,X 0x64 + 4 = 0xF0", InsSourceData(0xD6, InsData(0x64, IxValue(0x04))), IxSrResData(0x04, Negative.mask),  memByteResult(0x68, 0xEF)),
    ("DEC 3.0 Absolute absTestLocation = 0x33", InsSourceData(0xCE, InsData(absTestLocation, ZeroValues())), ZeroResData(),  memByteResult(absTestLocation, 0x32)),
    ("DEC 3.0 Absolute,X  absTestLocation + 4 = 0x80", InsSourceData(0xDE, InsData(absTestLocation, IxValue(0x04))), IxResData(0x04),  memByteResult(absTestLocation + 4, 0x7F))
  )

  // DEX decrement X
  val dataDexInstructionTest = List(
    ("DEX 1.0 implied", InsSourceData(0xCA, InsData(0x64, IxValue(0x20))), IxResData(0x1F), memVoidResult()),
    ("DEX 1.1 implied", InsSourceData(0xCA, InsData(0x64, IxValue(0x00))), IxSrResData(0xFF, Negative.mask), memVoidResult()),
    ("DEX 1.2 implied", InsSourceData(0xCA, InsData(0x64, IxValue(0x01))), IxSrResData(0x00, Zero.mask), memVoidResult()),
  )

  // DEY decrement Y
  val dataDeyInstructionTest = List(
    ("DEY 1.0 implied", InsSourceData(0x88, InsData(0x64, IyValue(0x20))), IyResData(0x1F), memVoidResult()),
    ("DEY 1.1 implied", InsSourceData(0x88, InsData(0x64, IyValue(0x00))), IySrResData(0xFF, Negative.mask), memVoidResult()),
    ("DEY 1.2 implied", InsSourceData(0x88, InsData(0x64, IyValue(0x01))), IySrResData(0x00, Zero.mask), memVoidResult()),
  )

  // EOR exclusive or (with accumulator)
  val dataEorInstructionTest = List(
    ("EOR 1.0 Acc 0xF0 immediate with 0xF4 result Negative  set", InsSourceData(0x49, InsData(0x04, AccValue(0xF0))), AccSrResData(0xF4, Negative.mask), memVoidResult()),
    ("EOR 1.1 Acc 0x80 immediate with 0x40 result zero set", InsSourceData(0x49, InsData(0x40, AccValue(0x40))), AccSrResData(0x00, Zero.mask), memVoidResult()),
    ("EOR 1.2 Acc 0x0F immediate with 0x40 flags unchanged", InsSourceData(0x49, InsData(0x40, AccValue(0x0F))), AccResData(0x4F), memVoidResult()),
    ("EOR 2.0 Acc (0x64) zero page 101 -> 0x06 give 0x62, flags unchanged", InsSourceData(0x45, InsData(101, AccValue(0x64))), AccResData(0x62), memVoidResult()),
    ("EOR 3.0 Acc (0x64) zero page,X (100 + 2 = 102) value 0x3F result should be 6", InsSourceData(0x55, InsData(100, AccIxValue(0x64, 2))), AccIxResData(0x5B, 2), memVoidResult()),
    ("EOR 4.0 Acc (0x20) absolute (2500) = 0x33", InsSourceData(0x4D, InsData(absTestLocation, AccValue(0x20))), AccResData(0x13), memVoidResult()),
    ("EOR 5.0 Acc (0xCC) absoluteX (2500 + 2) = 0x84", InsSourceData(0x5D, InsData(absTestLocation, AccIxValue(0x48, 2))), AccIxSrResData(0xCC, 2, Negative.mask), memVoidResult()),
    ("EOR 6.0 Acc (0x84) AbsoluteY absTestLocation + IY = 2 gives 0x84", InsSourceData(0x59, InsData(absTestLocation, AccIyValue(0x84, 2))), AccIySrResData(0x00, 2, Zero.mask), memVoidResult()),
    ("EOR 7.0 Acc (0x66) IndirectX 100 + IX = 7 gives absTestLocation2 = 0xF0", InsSourceData(0x41, InsData(100, AccIxValue(0x66, 7))), AccIxSrResData(0x96, 7, Negative.mask), memVoidResult()),
    ("EOR 8.0 Acc (0x66) IndirectY 107 = absTestLocation2 + Y -> 0x66", InsSourceData(0x51, InsData(107, AccIyValue(0x66, 3))), AccIySrResData(0x00, 3, Zero.mask), memVoidResult())
  )

  // INC increment
  val dataIncInstructionTest = List(
    ("INC 1.0 zero page 0x65 -> 0x06 give 0x62, flags unchanged", InsSourceData(0xE6, InsData(0x65, ZeroValues())), ZeroResData(), memByteResult(0x65, 0x07)),
    ("INC 2.0 zero page,X (100 + 4 = 102) value 0xF0", InsSourceData(0xF6, InsData(100, IxValue(4))), IxSrResData(4, Negative.mask), memByteResult(0x68, 0xF1)),
    ("INC 3.0 absolute (absTestLocation2 + 4 (0xA2C)) = 0xFF", InsSourceData(0xEE, InsData(0xA2C, ZeroValues())), SrResData(Zero.mask), memByteResult(0xA2C, 0x00)),
    ("INC 4.0 absoluteX (2500 + 3) = 0x00", InsSourceData(0xFE, InsData(absTestLocation, IxValue(3))), IxResData(3), memByteResult(absTestLocation + 3, 0x01)),
  )

  // INX increment X
  val dataInxInstructionTest = List(
    ("INX 1.0 implied, x = 3 flags unchanged", InsSourceData(0xE8, InsData(0x00, IxValue(3))), IxResData(4), memVoidResult()),
    ("INX 1.1 implied, x = 0xF0 negative flag", InsSourceData(0xE8, InsData(0x00, IxValue(0xF0))), IxSrResData(0xF1, Negative.mask), memVoidResult()),
    ("INX 1.2 implied, x = 0xFF Zero flag", InsSourceData(0xE8, InsData(0x00, IxValue(0xFF))), IxSrResData(0x00, Zero.mask), memVoidResult()),
  )

  // INY increment Y
  val dataInyInstructionTest = List(
    ("INY 1.0 implied, x = 3 flags unchanged", InsSourceData(0xC8, InsData(0x00, IyValue(3))), IyResData(4), memVoidResult()),
    ("INY 1.1 implied, x = 0xF0 negative flag", InsSourceData(0xC8, InsData(0x00, IyValue(0xF0))), IySrResData(0xF1, Negative.mask), memVoidResult()),
    ("INY 1.2 implied, x = 0xFF Zero flag", InsSourceData(0xC8, InsData(0x00, IyValue(0xFF))), IySrResData(0x00, Zero.mask), memVoidResult()),
  )

  // JMP jump test that PC is updated does not execute at the destination
  val dataJmpInstructionTest = List(
    ("JMP 1.0 absolute absTestLocation", InsSourceData(0x4C, InsData(absTestLocation, ZeroValues())), PcResData(absTestLocation), memVoidResult()),
    ("JMP 2.0 indirect testLocation2Ptr = 0x9CB -> 0x3FF0", InsSourceData(0x6C, InsData(testLocation2Ptr, ZeroValues())), PcResData(0x3FF0), memVoidResult()),
  )

  def validateJsrStack(): Unit =
    // 2 bytes on stack
    assert(Processor.sp.value == 0xFD, s"Stack pointer NOT CORRECT should be 0xFD is ${asHexStr(Processor.sp.value)}")
    // start of stack should be return address 0x1FF = low byte and 0x1FE = high bye
    // stach is pushed is reverse order do it appears as a word at 0x1FE not an address
    val returnAdd = memoryAccess.getMemoryWrd(0x1FE)
    // @2000, JSR instruction then the 2 byte address so return should be to 2003 but thr RTS does an increment so its only 2002
    assert(returnAdd == 2002, s"Return address on stack incorrect should be 0x7D3 is ${asHexStr(returnAdd)} ($returnAdd)" )

  // JSR jump subroutine. Test that PC is updated and return address pushed to stack. Does not execute at the destination
  val dataJsrInstructionTest = List(
    ("JSR 1.0 absolute absTestLocation", InsSourceData(0x20, InsData(absTestLocation, ZeroValues())), PcSpResData(absTestLocation, validateJsrStack), memVoidResult()),
  )

  // LDA load accumulator
  val dataLdaInstructionTest = List(
    ("LDA 1.0 immediate", InsSourceData(0xA9, InsData(10, AccValue(100))), AccResData(10), memVoidResult()),

    ("LDA 1.1 immediate", InsSourceData(0xA9, InsData(0xF0, AccValue(100))), AccSrResData(0xF0, Negative.mask), memVoidResult()),
    ("LDA 1.2 immediate", InsSourceData(0xA9, InsData(0x00, AccValueWithCarry(100))), AccSrResData(0, Zero.mask | Carry.mask), memVoidResult()),
    ("LDA 2.0 zeropage 101 -> 0x06", InsSourceData(0xA5, InsData(101, AccValue(100))), AccResData(6), memVoidResult()),
    ("LDA 3.0 zeropage,x", InsSourceData(0xB5, InsData(100, AccIxValue(100, 1))), AccIxResData(6, 1), memVoidResult()),
    ("LDA 4.0 absolute absTestLocation -> 0x33", InsSourceData(0xAD, InsData(absTestLocation, AccValue(0x64))), AccResData(0x33), memVoidResult()),
    ("LDA 5.0 absolute,x absTestLocation + 6 -> 0x40", InsSourceData(0xBD, InsData(absTestLocation, AccIxValue(0x24, 6))), AccIxResData(0x40, 6), memVoidResult()),
    ("LDA 6.0 absolute,y absTestLocation + 6", InsSourceData(0xB9, InsData(absTestLocation, AccIyValue(0x64, 6))), AccIyResData(0x40, 6), memVoidResult()),
    ("LDA 7.0 (indirect,x) zeroPageData + 7 -> 0xF0", InsSourceData(0xA1, InsData(zeroPageData, AccIxValue(0x64, 7))), AccIxSrResData(0xF0, 7, Negative.mask), memVoidResult()),
    ("LDA 8.0 (indirect),y", InsSourceData(0xB1, InsData(100, AccIyValue(99, 3))), AccIyResData(0x04, 3), memVoidResult())
  )

  // LDX load X
  val dataLdxInstructionTest = List(
    ("LDX 1.0 immediate", InsSourceData(0xA2, InsData(10, IxValue(100))), IxResData(10), memVoidResult()),
    ("LDX 1.1 immediate", InsSourceData(0xA2, InsData(0xF0, IxValue(100))), IxSrResData(0xF0, Negative.mask), memVoidResult()),
    ("LDX 1.2 immediate", InsSourceData(0xA2, InsData(0x00, IxValueWithCarry(100))), IxSrResData(0, Zero.mask | Carry.mask), memVoidResult()),
    ("LDX 2.0 zeropage 101 -> 0x06", InsSourceData(0xA6, InsData(101, IxValue(100))), IxResData(6), memVoidResult()),
    ("LDA 3.0 zeropage,y", InsSourceData(0xB6, InsData(100, IxIyValue(100, 1))), IxIyResData(6, 1), memVoidResult()),
    ("LDX 4.0 absolute absTestLocation -> 0x33", InsSourceData(0xAE, InsData(absTestLocation, IxValue(0x64))), IxResData(0x33), memVoidResult()),
    ("LDX 54.0 absolute,y absTestLocation + 6", InsSourceData(0xBE, InsData(absTestLocation, IxIyValue(0x64, 6))), IxIyResData(0x40, 6), memVoidResult()),
  )

  // LDY load Y
  val dataLdyInstructionTest = List(
    ("LDY 1.0 immediate", InsSourceData(0xA0, InsData(10, IyValue(100))), IyResData(10), memVoidResult()),
    ("LDY 1.1 immediate", InsSourceData(0xA0, InsData(0xF0, IyValue(100))), IySrResData(0xF0, Negative.mask), memVoidResult()),
    ("LDY 1.2 immediate", InsSourceData(0xA0, InsData(0x00, IyValueWithCarry(100))), IySrResData(0, Zero.mask | Carry.mask), memVoidResult()),
    ("LDY 2.0 zeropage 101 -> 0x06", InsSourceData(0xA4, InsData(101, IyValue(100))), IyResData(6), memVoidResult()),
    ("LDA 3.0 zeropage,x", InsSourceData(0xB4, InsData(100, IxIyValue(1, 99))), IxIyResData(1, 6), memVoidResult()),
    ("LDY 3.0 absolute absTestLocation -> 0x33", InsSourceData(0xAC, InsData(absTestLocation, IyValue(0x64))), IyResData(0x33), memVoidResult()),
    ("LDY 4.0 absolute,x absTestLocation + 6", InsSourceData(0xBC, InsData(absTestLocation, IxIyValue(6, 0x64))), IxIyResData(6, 0x40), memVoidResult()),
  )

  // LSR logical shift right
  val dataLsrInstructionTest = List(
    ("LSR 1.0 accumulator", InsSourceData(0x4A, InsData(0xF4, AccValue(0x20))), AccResData(0x10), memVoidResult()),
    ("LSR 1.1 accumulator", InsSourceData(0x4A, InsData(0xF4, AccValue(0x01))), AccSrResData(0x00, Carry.mask | Zero.mask), memVoidResult()),
    ("LSR 1.2 accumulator", InsSourceData(0x4A, InsData(0x7F, AccValueWithCarry(0x3F))), AccSrResData(0x1F, Carry.mask), memVoidResult()),
    ("LSR 1.3 accumulator", InsSourceData(0x4A, InsData(0x7F, AccValueWithCarry(0x3E))), AccResData(0x1F), memVoidResult()),
    ("LSR 2.0 zeroPage 0x66 -> 0x3F", InsSourceData(0x46, InsData(0x66, ZeroValues())), SrResData(Carry.mask), memByteResult(0x66, 0x1F)),
    ("LSR 3.0 zeroPageX 100 + IX = 9 contains 0", InsSourceData(0x56, InsData(0x64, IxValue(9))), IxSrResData(9, Zero.mask), memByteResult(0x109, 0)),
    ("LSR 4.0 absolute absTestLocation2 contains 0xF0", InsSourceData(0x4E, InsData(absTestLocation2, ZeroValues())), ZeroResData(), memByteResult(absTestLocation2, 0x78)),
    ("LSR 5.0 absoluteX absTestLocation2 IX = 1 contains 0x3F", InsSourceData(0x5E, InsData(absTestLocation2, IxValue(1))), IxSrResData(1, Carry.mask), memByteResult(absTestLocation2 + 1, 0x1F))
  )

  // NOP no operation
  val dataNopInstructionTest = List(
  )

  // ORA or with accumulator
  val dataOraInstructionTest = List(
    ("ORA 1.0 immediate", InsSourceData(0x09, InsData(0x0A, AccValue(0x64))), AccResData(0x6E), memVoidResult()),
    ("ORA 1.1 immediate", InsSourceData(0x09, InsData(0xF0, AccValue(0x64))), AccSrResData(0xF4, Negative.mask), memVoidResult()),
    ("ORA 1.2 immediate", InsSourceData(0x09, InsData(0x00, AccValueWithCarry(0x00))), SrResData(Zero.mask | Carry.mask), memVoidResult()),
    ("ORA 2.0 zeropage 101 -> 0x06", InsSourceData(0x05, InsData(0x65, AccValue(0x64))), AccResData(0x66), memVoidResult()),
    ("ORA 3.0 zeropage,x", InsSourceData(0x15, InsData(0x64, AccIxValue(0x64, 1))), AccIxResData(0x66, 1), memVoidResult()),
    ("ORA 4.0 absolute absTestLocation -> 0x33", InsSourceData(0x0D, InsData(absTestLocation, AccValue(0x64))), AccResData(0x77), memVoidResult()),
    ("ORA 5.0 absolute,x absTestLocation + 6 -> 0x40", InsSourceData(0x1D, InsData(absTestLocation, AccIxValue(0x24, 6))), AccIxResData(0x64, 6), memVoidResult()),
    ("ORA 6.0 absolute,y absTestLocation + 6", InsSourceData(0x19, InsData(absTestLocation, AccIyValue(0x64, 6))), AccIyResData(0x64, 6), memVoidResult()),
    ("ORA 7.0 (indirect,x) zeroPageData + 7 -> 0xF0", InsSourceData(0x01, InsData(zeroPageData, AccIxValue(0x64, 7))), AccIxSrResData(0xF4, 7, Negative.mask), memVoidResult()),
    ("ORA 8.0 (indirect),y", InsSourceData(0x11, InsData(0x64, AccIyValue(0x63, 3))), AccIyResData(0x67, 3), memVoidResult())
  )

  def validateStackValue(expected: Int, stackValue: Int): Unit =
    assert(expected == stackValue, s"Incorrect value on stack expected ${asHexStr(expected)} was ${asHexStr(stackValue)}")

  def assertOnlyOneByteOnStack(): Unit =
    assert(Processor.sp.value == 0xFE, s"Stack pointer NOT CORRECT should be 0xFD is ${asHexStr(Processor.sp.value)}")

  def phaValidation(): Unit =
    assertOnlyOneByteOnStack()
    val stackValue = memoryAccess.getMemoryByte(0x1FF)
    validateStackValue(0x64, stackValue)

  // PHA push accumulator
  val dataPhaInstructionTest = List(
    ("PHA 1.0 implied", InsSourceData(0x48, InsData(0x00, AccValue(0x64))), AccPcSpResData(0x64, testLocation + 1, phaValidation), memVoidResult()),
  )

  def phpValidation(expectedValue: Int): Unit =
    assertOnlyOneByteOnStack()
    val stackValue = memoryAccess.getMemoryByte(0x1FF)
    validateStackValue(expectedValue, stackValue)

  def phpValidation1(): Unit =
    phpValidation(Carry.mask | Zero.mask | Unused.mask)

  def phpValidation2(): Unit =
    phpValidation(Overflow.mask | Negative.mask | Unused.mask)

// PHP push processor status (SR)
  val dataPhpInstructionTest = List(
    ("PHP 1.0 implied", InsSourceData(0x08, InsData(0x00, SrValue(Carry.mask | Zero.mask))), SrPcSpResData(Carry.mask | Zero.mask, testLocation + 1, phpValidation1), memVoidResult()),
    ("PHP 1.0 implied", InsSourceData(0x08, InsData(0x00, SrValue(Overflow.mask | Negative.mask))), SrPcSpResData(Overflow.mask | Negative.mask, testLocation + 1, phpValidation2), memVoidResult()),
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

