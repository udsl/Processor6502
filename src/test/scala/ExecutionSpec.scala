import com.udsl.processor6502.assembler.{AssembleLocation, InstructionToken}
import com.udsl.processor6502.cpu.Memory.NMI_VECTOR
import com.udsl.processor6502.cpu.execution.{Absolute, AbsoluteX, AbsoluteY, Immediate, IndirectX, IndirectY, ZeroPage, ZeroPageX}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import com.udsl.processor6502.cpu.Processor.*
import com.udsl.processor6502.cpu.Processor
import com.udsl.processor6502.cpu.execution.ExecutionUnit
import com.udsl.processor6502.cpu.StatusRegister.{UNUSED_FLAG_MASK, OVERFLOW_FLAG_MASK, NEGATIVE_FLAG_MASK}


case class InsData(opcode: Int, loByte: Int, hiByte: Int)
case class InsResultData(ac: Int, ix: Int, iy: Int, sr: Int)

val dataAdcInstructionTest = List(
//  (InsData(0x69, 10, 0), InsResultData(110,0,0,UNUSED_FLAG_MASK)), // ADC immediate 10
  (InsData(0x69, 126, 0), InsResultData(226,0,0, UNUSED_FLAG_MASK | OVERFLOW_FLAG_MASK | NEGATIVE_FLAG_MASK)) // ADC immediate 126
//  Token("ADC", 0x65, 2, ZeroPage),  // $LL
//  Token("ADC", 0x75, 2, ZeroPageX), // $LL,X
//  Token("ADC", 0x6D, 3, Absolute),  // $LLLL
//  Token("ADC", 0x7D, 3, AbsoluteX), // $LL,X
//  Token("ADC", 0x79, 3, AbsoluteY), // $LL,Y
//  Token("ADC", 0x61, 2, IndirectX), // ($LL,X)
//  Token("ADC", 0x71, 2, IndirectY), //"($LL),Y"
)

class ExecutionSpec extends AnyFlatSpec, should.Matchers:

  "Given a valid ADC instruction token" should "should assemble to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    for ((insData, resData) <- dataAdcInstructionTest) {
      ExecutionSpec.initValuesForTest(insData)
      executionUnit.singleStep()
      ExecutionSpec.checkRes(resData)
    }
  }

object ExecutionSpec:

  def checkRes(resData: InsResultData): Unit =
    assert(Processor.ac.value == resData.ac, s"AC ${Processor.ac.value} required ${resData.ac}")
    assert(Processor.ix.value == resData.ix, s"SR ${Processor.ix.value} required ${resData.ix}")
    assert(Processor.iy.value == resData.iy, s"SR ${Processor.iy.value} required ${resData.iy}")
    assert(Processor.sr.value == resData.sr, s"SR ${Processor.sr.value} required ${resData.sr}")

  /**
   * Initialise memory and registers for test
   */
  def initValuesForTest(data : InsData) : Unit =
    AssembleLocation.setAssembleLoc(100) // zero page location
    AssembleLocation.setMemoryByte(56) // set the byte to 56
    AssembleLocation.setAssembleLoc(NMI_VECTOR)  // start of vectors in memory
    AssembleLocation.setMemoryAddress(1000) // set NMI vector to address 1000
    AssembleLocation.setMemoryAddress(2000) // set RESET vector to address 2000
    AssembleLocation.setMemoryAddress(3000) // set INTERRUPT vector to address 3000
    AssembleLocation.setAssembleLoc(2000) // set current location to ins location
    AssembleLocation.setMemoryByte(data.opcode)
    AssembleLocation.setMemoryByte(data.loByte)
    AssembleLocation.setMemoryByte(data.hiByte)
    initRegisters()

  def initRegisters() : Unit =
    Processor.reset
    Processor.ac.value = 100
    Processor.ix.value = 0
    Processor.iy.value = 0
    Processor.sr.reset()


