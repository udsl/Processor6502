package com.udsl.processor6502.test

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.AssembleLocation.logger
import com.udsl.processor6502.assembler.{AssembleLocation, InstructionToken}
import com.udsl.processor6502.cpu.Memory.NMI_VECTOR
import com.udsl.processor6502.cpu.Processor.*
import com.udsl.processor6502.cpu.StatusRegister.*
import com.udsl.processor6502.cpu.execution.*
import com.udsl.processor6502.cpu.{Processor, StatusRegisterFlags}
import com.udsl.processor6502.test.ExecutionSpec.{absTestLocation, absTestLocation2, fixedValuesInitialised, logger, testLocation}
import com.udsl.processor6502.test.InsData.{checkValue, logger}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

trait RegValues(val acc: Int, val ix: Int, val iy:Int, val withCarry:Boolean )

case class ZeroValues() extends RegValues( 0, 0, 0, false)
case class AccValue( override val acc: Int) extends RegValues( acc, 0, 0, false)
case class AccValueWithCarry( override val acc: Int) extends RegValues( acc, 0, 0, true)
case class AccIxValue( override val acc: Int, override val ix: Int) extends RegValues( acc, ix, 0, false)
case class IxValue( override val ix: Int) extends RegValues( 0, ix, 0, false)
case class AccIxValueWithCarry( override val acc: Int, override val ix: Int) extends RegValues( acc, ix, 0, true)
case class AccIyValue( override val acc: Int, override val iy: Int) extends RegValues( acc, 0, iy, false)
case class AccIyValueWithCarry( override val acc: Int, override val iy: Int) extends RegValues( acc, 0, iy, true)
case class AccIxIyValue( override val acc: Int, override val ix: Int, override val iy: Int) extends RegValues( acc, ix, iy, false)
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

trait ResultData( val ac: Int, val ix: Int, val iy: Int, val sr: Int, val pc: Int)

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
case class AccSrPcResData(override val ac: Int, override val pc: Int, override val sr: Int) extends ResultData(ac, 0, 0, sr, pc)


trait ResultMemData( val loc:Int, val value: Int, val byte: Boolean)

case class memByteResult(override val loc:Int, override val value: Int ) extends ResultMemData(loc, value, true)
case class memWrdResult(override val loc:Int, override val value: Int ) extends ResultMemData(loc, value, false)
case class memVoidResult() extends ResultMemData(0, 0, false)

object ExecutionSpecData:

  // ADC add with carry
  val dataAdcInstructionTest = List(
    ("ADC 1", InsSourceData(0x69, InsData(10, AccValue(100))), AccResData(110), memVoidResult()), // ADC immediate 10
    ("ADC 2", InsSourceData(0x69, InsData(126, AccValue(100))), AccSrResData(226,OVERFLOW_FLAG_MASK | NEGATIVE_FLAG_MASK), memVoidResult()), // ADC immediate 126
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
    ("ADC 10", InsSourceData(0x71, InsData(100, AccIyValue(99, 1))), AccIySrResData(101, 1, UNUSED_FLAG_MASK), memVoidResult()) //  Token("ADC", 0x71, 2, IndirectY), //"($LL),Y"
  )

  // AND and (with accumulator)
  val dataAndInstructionTest = List(
    ("AND 1", InsSourceData(0x29, InsData(0xF4, AccValue(100))), AccResData(100), memVoidResult()), // And acc (0x64) immediate with 0xF4 result should be 0x64
    ("AND 2", InsSourceData(0x25, InsData(101, AccValue(100))), AccResData(4), memVoidResult()), // And acc (0x64) zero page 101 value 6 result should be 4
    ("AND 3", InsSourceData(0x35, InsData(99, AccIxValue(0x66, 2))), AccIxResData(6, 2), memVoidResult()), // And acc (0x64) zero page,X (99 + 2 = 101) value 6 result should be 6
    ("AND 4", InsSourceData(0x35, InsData(99, AccIxValue(0x88, 2))), IxSrResData(2, ZERO_FLAG_MASK), memVoidResult()), // And acc (0x64) zero page,X (99 + 2 = 101) value 6 result should be 0
    ("AND 5", InsSourceData(0x2D, InsData(absTestLocation, AccIxValue(0xE1, 2))), AccIxResData(0x21, 2), memVoidResult()), //  TestData("AND", 0x2D, 3, Absolute),  // $LLLL
    ("AND 6", InsSourceData(0x2D, InsData(absTestLocation, AccIxValue(0xCC, 2))), IxSrResData(2, ZERO_FLAG_MASK), memVoidResult()), //  TestData("AND", 0x2D, 3, Absolute),  // $LLLL
    ("AND 7", InsSourceData(0x3D, InsData(absTestLocation, AccIxValue(0xCC, 1))), AccIxSrResData(0xCC, 1, NEGATIVE_FLAG_MASK), memVoidResult()), //  TestData("AND", 0x3D, 3, AbsoluteX), // $LL,X
    ("AND 8.1 AbsoluteY absTestLocation + IY = 2 gives 0x84", InsSourceData(0x39, InsData(absTestLocation, AccIyValue(0xCC, 2))), AccIySrResData(0x84, 2, NEGATIVE_FLAG_MASK), memVoidResult()), //  TestData("AND", 0x39, 3, AbsoluteY), // $LL,Y
    ("AND 8.2 AbsoluteY absTestLocation + IY = 3 gives 0x00", InsSourceData(0x39, InsData(absTestLocation, AccIyValue(0xCC, 3))), IySrResData(3, ZERO_FLAG_MASK), memVoidResult()),
    ("AND 9.1 IndirectX 100 + IX = 4 gives absTestLocation2 = 0xF0", InsSourceData(0x21, InsData(100, AccIxValue(0x66, 4))), AccIxResData(0x60, 4), memVoidResult()), //  TestData("AND", 0x21, 2, IndirectX), // ($LL,X)
    ("AND 10", InsSourceData(0x31, InsData(100, AccIyValue(0x66, 3))), AccIyResData(0x04, 3), memVoidResult())
  )

  // ASL arithmetic shift left
  val dataAslInstructionTest = List(
    ("ASL 1.0 accumulator", InsSourceData(0x0A, InsData(0xF4, AccValue(0x20))), AccResData(0x40), memVoidResult()),
    ("ASL 1.1 accumulator", InsSourceData(0x0A, InsData(0xF4, AccValue(0x80))), AccSrResData(0x00, CARRY_FLAG_MASK | ZERO_FLAG_MASK), memVoidResult()),
    ("ASL 1.2 accumulator", InsSourceData(0x0A, InsData(0x7F, AccValueWithCarry(0x3F))), AccResData(0x7E), memVoidResult()),
    ("ASL 2.0 zeroPage ", InsSourceData(0x06, InsData(0x66, ZeroValues())), AccResData(0), memByteResult(0x66, 0x7E)),
    ("ASL 3.0 zeroPageX 100 -> 0x638, IX = 1 contains 2", InsSourceData(0x16, InsData(0x64, IxValue(1))), IxResData(1), memByteResult(0x639, 2)),
    ("ASL 3.1 zeroPageX 100 -> 0x638, IX = 3 contains 0x80", InsSourceData(0x16, InsData(0x64, IxValue(3))), IxSrResData(3, CARRY_FLAG_MASK | ZERO_FLAG_MASK), memByteResult(103, 0)),
    ("ASL 4.0 absolute absTestLocation2 contains 0xF0", InsSourceData(0x0E, InsData(absTestLocation2, ZeroValues())), SrResData(CARRY_FLAG_MASK | NEGATIVE_FLAG_MASK), memByteResult(absTestLocation2, 0xE0)),
    ("ASL 5.0 absoluteX absTestLocation2 IX = 1 contains 0x3F", InsSourceData(0x1E, InsData(absTestLocation2, AccIxValue(0,1))), IxResData(1), memByteResult(absTestLocation2 + 1, 0x7E))
  )

  // BCC branch on carry clear
  val dataBccInstructionTest = List(
    ("BCC 1.0 relative carry clear PC + 6 = 4 branch + 2 fetch", InsSourceData(0x90, InsData(0x4, AccValue(0x20))), AccPcResData(0x20, testLocation + 6), memVoidResult()),
    ("BCC 1.1 relative carry clear -ve offset", InsSourceData(0x90, InsData(0xFC, AccValue(0x20))), AccPcResData(0x20, testLocation -2), memVoidResult()),
    ("BCC 2.0 relative carry set", InsSourceData(0x90, InsData(0x4, AccValueWithCarry(0x20))), AccSrResData(0x20, CARRY_FLAG_MASK), memVoidResult())
   )

  // BCS branch on carry set
  val dataBcsInstructionTest = List(
    ("BCS 1.0 relative carry set PC + 6 = 4 branch + 2 fetch", InsSourceData(0xB0, InsData(0x4, AccValueWithCarry(0x20))), AccSrPcResData(0x20, testLocation + 6, CARRY_FLAG_MASK), memVoidResult()),
    ("BCS 1.1 relative carry set -ve offset", InsSourceData(0xB0, InsData(0xFC, AccValueWithCarry(0x20))), AccSrPcResData(0x20, testLocation -2, CARRY_FLAG_MASK), memVoidResult()),
    ("BCS 2.0 relative carry clear", InsSourceData(0xB0, InsData(0x4, AccValue(0x20))), AccResData(0x20), memVoidResult())
  )

  // BEQ branch on equal (zero set)
  val dataBeqInstructionTest = List(
  )

  // BIT bit test
  val dataBitInstructionTest = List(
  )

  // BMI branch on minus (negative set)
  val dataBmiInstructionTest = List(
  )

  // BNE branch on not equal (zero clear)
  val dataBneInstructionTest = List(
  )

  // BPL branch on plus (negative clear)
  val dataBplInstructionTest = List(
  )

  // BRK break / interrupt
  val dataBrkInstructionTest = List(
  )

  // BVC branch on overflow clear
  val dataBvcInstructionTest = List(
  )

  // BVS branch on overflow set
  val dataBvsInstructionTest = List(
  )

  // CLC clear carry
  val dataClcInstructionTest = List(
  )

  // CLD clear decimal
  val dataCldInstructionTest = List(
  )

  // CLI clear interrupt disable
  val dataCliInstructionTest = List(
  )

  // CLV clear overflow
  val dataClvInstructionTest = List(
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

