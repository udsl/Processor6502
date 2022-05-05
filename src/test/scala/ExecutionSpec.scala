import ExecutionSpec.{absTestLocation, fixedValuesInitialised}
import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.AssembleLocation.logger
import com.udsl.processor6502.assembler.{AssembleLocation, InstructionToken}
import com.udsl.processor6502.cpu.Memory.NMI_VECTOR
import com.udsl.processor6502.cpu.execution.{Absolute, AbsoluteX, AbsoluteY, Immediate, IndirectX, IndirectY, ZeroPage, ZeroPageX}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import com.udsl.processor6502.cpu.Processor.*
import com.udsl.processor6502.cpu.Processor
import com.udsl.processor6502.cpu.execution.ExecutionUnit
import com.udsl.processor6502.cpu.StatusRegister.{NEGATIVE_FLAG_MASK, OVERFLOW_FLAG_MASK, UNUSED_FLAG_MASK, ZERO_FLAG_MASK}

class InsData( val value: Int, val acc: Int = 0x64, val ix: Int = 0, val iy:Int = 0):
  def loByte: Int = value % 256
  def hiByte: Int = (value / 256) % 256
  def hasHiByte: Boolean = (value > 255)

object InsData extends StrictLogging:
  def apply(value: Int): InsData =
    checkValue(value)
    new InsData(value)

  def apply(value: Int, acc: Int): InsData =
    checkValue(value)
    checkAccValue(acc)
    new InsData(value, acc)

  def apply(value: Int, acc: Int, ix: Int): InsData =
    checkValue(value)
    checkAccValue(acc)
    checkIxValue(ix)
    new InsData(value, acc, ix)

  def apply(value: Int, acc: Int, ix: Int, iy: Int): InsData =
    checkValue(value)
    checkAccValue(acc)
    checkIxValue(ix)
    new InsData(value, acc, ix, iy)

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


case class InsSourceData(opcode: Int, data: InsData)
case class InsResultData(ac: Int, ix: Int, iy: Int, sr: Int)

val dataAdcInstructionTest = List(
  (InsSourceData(0x69, InsData(10)), InsResultData(110,0,0,UNUSED_FLAG_MASK)), // ADC immediate 10
  (InsSourceData(0x69, InsData(126)), InsResultData(226,0,0, UNUSED_FLAG_MASK | OVERFLOW_FLAG_MASK | NEGATIVE_FLAG_MASK)), // ADC immediate 126
  (InsSourceData(0x65, InsData(101)),  InsResultData(106, 0, 0, UNUSED_FLAG_MASK)) // ADC zer0 page 101 contains 6
//  Token("ADC", 0x75, 2, ZeroPageX), // $LL,X
//  Token("ADC", 0x6D, 3, Absolute),  // $LLLL
//  Token("ADC", 0x7D, 3, AbsoluteX), // $LL,X
//  Token("ADC", 0x79, 3, AbsoluteY), // $LL,Y
//  Token("ADC", 0x61, 2, IndirectX), // ($LL,X)
//  Token("ADC", 0x71, 2, IndirectY), //"($LL),Y"
)

// InsData( address, acc, ix, iy)
val dataAndInstructionTest = List(
  (InsSourceData(0x29, InsData(0xF4)), InsResultData(100, 0, 0, UNUSED_FLAG_MASK)), // And acc (0x64) immediate with 0xF4 result should be 0x64
  (InsSourceData(0x25, InsData(101)), InsResultData(4, 0, 0, UNUSED_FLAG_MASK)), // And acc (0x64) zero page 101 value 6 result should be 4
  (InsSourceData(0x35, InsData(99, 0x66, 2)), InsResultData(6, 2, 0, UNUSED_FLAG_MASK)), // And acc (0x64) zero page,X (99 + 2 = 101) value 6 result should be 6
  (InsSourceData(0x35, InsData(99, 0x88, 2)), InsResultData(0, 2, 0, UNUSED_FLAG_MASK | ZERO_FLAG_MASK)), // And acc (0x64) zero page,X (99 + 2 = 101) value 6 result should be 0
  (InsSourceData(0x2D, InsData(absTestLocation, 0xE1, 2)), InsResultData(0x21, 2, 0, UNUSED_FLAG_MASK)), //  TestData("AND", 0x2D, 3, Absolute),  // $LLLL
  (InsSourceData(0x2D, InsData(absTestLocation, 0xCC, 2)), InsResultData(0, 2, 0, UNUSED_FLAG_MASK | ZERO_FLAG_MASK)), //  TestData("AND", 0x2D, 3, Absolute),  // $LLLL
  (InsSourceData(0x3D, InsData(absTestLocation, 0xCC, 1)), InsResultData(0xCC, 1, 0, UNUSED_FLAG_MASK | NEGATIVE_FLAG_MASK)), //  TestData("AND", 0x3D, 3, AbsoluteX), // $LL,X
  (InsSourceData(0x39, InsData(absTestLocation, 0xCC, 0, 2)), InsResultData(0, 0, 2, UNUSED_FLAG_MASK | ZERO_FLAG_MASK)), //  TestData("AND", 0x39, 3, AbsoluteY), // $LL,Y
  (InsSourceData(0x21, InsData(100, 0x66, 2, 2)), InsResultData(0x60, 2, 2, UNUSED_FLAG_MASK)), //  TestData("AND", 0x21, 2, IndirectX), // ($LL,X)
  (InsSourceData(0x31, InsData(100, 0x66, 2, 3)), InsResultData(0x04, 2, 3, UNUSED_FLAG_MASK))//  TestData("AND", 0x31, 2, IndirectY), //"($LL),Y"
)

class ExecutionSpec extends AnyFlatSpec, should.Matchers, StrictLogging:

  "Given a valid ADC instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((insData, resData) <- dataAdcInstructionTest) {
      ExecutionSpec.initValuesForTest(insData)
      executionUnit.singleStep()
      ExecutionSpec.checkRes(resData)
    }
  }

  "Given a valid AND instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((insData, resData) <- dataAndInstructionTest) {
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Current location ${AssembleLocation.currentLocation} PC ${Processor.pc.addr}")
      executionUnit.singleStep()
      ExecutionSpec.checkRes(resData)
    }
  }

object ExecutionSpec extends StrictLogging:
  var fixedValuesInitialised = false
  val testLocation = 2000
  val absTestLocation = 2500
  val absTestLocation2 = 2600

  def checkRes(resData: InsResultData): Unit =
    assert(Processor.ac.value == resData.ac, s"AC ${Processor.ac.value} required ${resData.ac} - ${resData}")
    assert(Processor.ix.value == resData.ix, s"IX ${Processor.ix.value} required ${resData.ix} - ${resData}")
    assert(Processor.iy.value == resData.iy, s"IY ${Processor.iy.value} required ${resData.iy} - ${resData}")
    assert(Processor.sr.value == resData.sr, s"SR ${Processor.sr.value} required ${resData.sr} - ${resData}")

  /**
   * Initialise memory and registers for test
   */
  def initFixedValuesForTest() : Unit =
    if !fixedValuesInitialised then
      AssembleLocation.setAssembleLoc(100) // zero page location
      AssembleLocation.setMemoryByte(56) // set the byte to 56
      AssembleLocation.setMemoryByte(6) // and the following byte to 6
      AssembleLocation.setMemoryAddress(absTestLocation2)

      AssembleLocation.setAssembleLoc(0x638) // set current location to ins location
      for x <- List(1,2,3,4) do
        AssembleLocation.setMemoryByte(x)

      AssembleLocation.setAssembleLoc(absTestLocation) // set current location to ins location
      AssembleLocation.setMemoryByte(0x33)
      AssembleLocation.setMemoryByte(0xCC)

      AssembleLocation.setAssembleLoc(absTestLocation2) // set current location to ins location
      AssembleLocation.setMemoryByte(0xF0)


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
    Processor.ac.value = regData.acc
    Processor.ix.value = regData.ix
    Processor.iy.value = regData.iy
    Processor.sr.reset()


