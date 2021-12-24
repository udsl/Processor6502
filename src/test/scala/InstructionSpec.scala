
import com.udsl.processor6502.assembler
import com.udsl.processor6502.cpu.{CpuInstruction, CpuInstructions}
import com.udsl.processor6502.cpu.execution.{Absolute, AbsoluteX, AbsoluteY, Accumulator, AddressingMode, Immediate, Implied, Indirect, IndirectX, IndirectY, Relative, ZeroPage, ZeroPageX}
import org.scalatest.*
import flatspec.*
import matchers.*


class TestData(val code: String, val opcode: Int, val insLength: Int, val addMode: AddressingMode){

}

object TestData{
  def apply(code: String, opcode: Int, insLength: Int, addMode: AddressingMode): TestData = {
    new TestData(code, opcode, insLength, addMode)
  }
}

val dataValidInstructionTest: List[TestData] = List(
  /*
  ADC - Add Memory to Accumulator with Carry
  immediate	    ADC #oper	    69	2
  zeropage	    ADC oper	    65	2
  zeropage,X	  ADC oper,X	  75	2
  absolute	    ADC oper	    6D	3
  absolute,X	  ADC oper,X	  7D	3
  absolute,Y	  ADC oper,Y	  79	3
  (indirect,X)	ADC (oper,X)	61	2
  (indirect),Y	ADC (oper),Y	71	2
*/
  TestData("ADC", 0x69, 2, Immediate), // #
  TestData("ADC", 0x65, 2, ZeroPage),  // $LL
  TestData("ADC", 0x75, 2, ZeroPageX), // $LL,X
  TestData("ADC", 0x6D, 3, Absolute),  // $LLLL
  TestData("ADC", 0x7D, 3, AbsoluteX), // $LL,X
  TestData("ADC", 0x79, 3, AbsoluteY), // $LL,Y
  TestData("ADC", 0x61, 2, IndirectX), // ($LL,X)
  TestData("ADC", 0x71, 2, IndirectY), //"($LL),Y"

  /*
 AND - Add Memory to Accumulator with Carry
 immediate	  AND #oper	    29	2
 zeropage	    AND oper	    25	2
 zeropage,X	  AND oper,X	  35	2
 absolute	    AND oper	    2D	3
 absolute,X	  AND oper,X	  3D	3
 absolute,Y	  AND oper,Y	  39	3
 (indirect,X)	AND (oper,X)	21	2
 (indirect),Y	AND (oper),Y	31	2
*/
  TestData("AND", 0x29, 2, Immediate), // #
  TestData("AND", 0x25, 2, ZeroPage),  // $LL
  TestData("AND", 0x35, 2, ZeroPageX), // $LL,X
  TestData("AND", 0x2D, 3, Absolute),  // $LLLL
  TestData("AND", 0x3D, 3, AbsoluteX), // $LL,X
  TestData("AND", 0x39, 3, AbsoluteY), // $LL,Y
  TestData("AND", 0x21, 2, IndirectX), // ($LL,X)
  TestData("AND", 0x31, 2, IndirectY), //"($LL),Y"

  /*
 ASL - Add Memory to Accumulator with Carry
 accumulator  ASL A   	    0A	1
 zeropage	    ASL oper	    06	2
 zeropage,X	  ASL oper,X	  16	2
 absolute	    ASL oper	    0E	3
 absolute,X	  ASL oper,X	  1E	3
*/
  TestData("ASL", 0x0A, 1, Accumulator), // #
  TestData("ASL", 0x06, 2, ZeroPage),  // $LL
  TestData("ASL", 0x16, 2, ZeroPageX), // $LL,X
  TestData("ASL", 0x0E, 3, Absolute),  // $LLLL
  TestData("ASL", 0x1E, 3, AbsoluteX), // $LL,X

  TestData("BCC", 0x90, 2, Relative), // $LL
  TestData("BCS", 0xB0, 2, Relative), // $LL
  TestData("BEQ", 0xF0, 2, Relative), // $LL

  TestData("BIT", 0x24, 2, ZeroPage), // $LL
  TestData("BIT", 0x2C, 3, Absolute), // $LLHH

  TestData("BMI", 0x30, 2, Relative), // $LL
  TestData("BNE", 0xD0, 2, Relative), // $LL

  TestData("BRK", 0x00, 1, Implied), // $LL

  TestData("BVC", 0x50, 2, Relative), // $LL
  TestData("BVS", 0x70, 2, Relative), // $LL


  TestData("CLC", 0x18, 1, Implied),
  TestData("CLD", 0xD8, 1, Implied),
  TestData("CLI", 0x58, 1, Implied),
  TestData("CLV", 0xB8, 1, Implied),

  /**
   * CMP Compare Memory with Accumulator
   *
   * immediate	CMP #oper	C9	2
   * zeropage	CMP oper	C5	2
   * zeropage,X	CMP oper,X	D5	2
   * absolute	CMP oper	CD	3
   * absolute,X	CMP oper,X	DD	3
   * absolute,Y	CMP oper,Y	D9	3
   * (indirect,X)	CMP (oper,X)	C1	2
   * (indirect),Y	CMP (oper),Y	D1	2
   */
  TestData("CMP", 0xC9, 2, Immediate), // #
  TestData("CMP", 0xC5, 2, ZeroPage),  // $LL
  TestData("CMP", 0xD5, 2, ZeroPageX), // $LL,X
  TestData("CMP", 0xCD, 3, Absolute),  // $LLHH
  TestData("CMP", 0xDD, 3, AbsoluteX), // $LL,X
  TestData("CMP", 0xD9, 3, AbsoluteY), // $LL,X
  TestData("CMP", 0xC1, 2, IndirectX), // $LL,X
  TestData("CMP", 0xD1, 2, IndirectY), // $LL,X

  /**
   * CPX Compare Memory and Index X
   *
   * immediate CPX #oper E0	2
   * zeropage	 CPX oper	 E4	2
   * absolute	 CPX oper	 EC	3
   */
  TestData("CPX", 0xE0, 2, Immediate), // #
  TestData("CPX", 0xE4, 2, ZeroPage),  // $LL
  TestData("CPX", 0xEC, 3, Absolute),  // $LLHH


  /**
   * CPY Compare Memory and Index Y
   *
   * immediate CPY #oper C0	2
   * zeropage	 CPY oper	 C4	2
   * absolute  CPY oper	 CC	3
   */
  TestData("CPY", 0xC0, 2, Immediate), // #
  TestData("CPY", 0xC4, 2, ZeroPage),  // $LL
  TestData("CPY", 0xCC, 3, Absolute),  // $LLHH

  /**
   * DEC Compare Memory and Index Y
   *
   * zeropage   DEC #oper C6	2
   * zeropage,X DEC oper	D6	2
   * absolute   DEC oper	CE	3
   * absolute,X DEC oper	DE	3
   */
  TestData("DEC", 0xC6, 2, ZeroPage),
  TestData("DEC", 0xD6, 2, ZeroPageX),
  TestData("DEC", 0xCE, 3, Absolute),
  TestData("DEC", 0xDE, 3, AbsoluteX),

  TestData("DEX", 0xCA, 1, Implied),

  TestData("DEY", 0x88, 1, Implied),

  /**
   * EOR Compare Memory with Accumulator
   *
   * immediate	EOR #oper	49	2
   * zeropage	EOR oper	45	2
   * zeropage,X	EOR oper,X	55	2
   * absolute	EOR oper	4D	3
   * absolute,X	EOR oper,X	5D	3
   * absolute,Y	EOR oper,Y	59	3
   * (indirect,X)	EOR (oper,X)	41	2
   * (indirect),Y	EOR (oper),Y	51	2
   */
  TestData("EOR", 0x49, 2, Immediate), // #
  TestData("EOR", 0x45, 2, ZeroPage),  // $LL
  TestData("EOR", 0x55, 2, ZeroPageX), // $LL,X
  TestData("EOR", 0x4D, 3, Absolute),  // $LLHH
  TestData("EOR", 0x5D, 3, AbsoluteX), // $LL,X
  TestData("EOR", 0x59, 3, AbsoluteY), // $LL,X
  TestData("EOR", 0x41, 2, IndirectX), // $LL,X
  TestData("EOR", 0x51, 2, IndirectY), // $LL,X

  /**
   * INC Compare Memory and Index Y
   *
   * zeropage   INC #oper E6	2
   * zeropage,X INC oper	F6	2
   * absolute   INC oper	EE	3
   * absolute,X INC oper	FE	3
   */
  TestData("INC", 0xE6, 2, ZeroPage),
  TestData("INC", 0xF6, 2, ZeroPageX),
  TestData("INC", 0xEE, 3, Absolute),
  TestData("INC", 0xFE, 3, AbsoluteX),

  TestData("INX", 0xE8, 1, Implied),

  TestData("INY", 0xC8, 1, Implied),

  /**
   * JMP Jump to New Location
   *
   * absolute	JMP oper	4C	3
   * indirect	JMP (oper)	6C	3
   */
  TestData("JMP", 0x4C, 3, Absolute),
  TestData("JMP", 0x6C, 3, Indirect),

  TestData("JSR", 0x20, 3, Absolute),

  TestData("LDX", 0xA6, 2, ZeroPage), //"$LL"
)

class InstructionSpec extends AnyFlatSpec, should.Matchers {
  "An instruction" should "should have the correct opcode and addressing mode" in {
    for (data <- dataValidInstructionTest) {
      assert(CpuInstructions.isValidInstruction(data.code), s"Instruction not found ${data.code}")
      val (opcode: Int, bytes:Int)  = CpuInstructions.getInstructionOpcodeBytes(data.code, data.addMode)
      assert(1 to 3 contains bytes, s"Failed to get valid bytes for ${data.code} with ${data.addMode} addressing")
      assert(opcode >= 0,s"Failed to get valid opcode for ${data.code}")
      assert(opcode == data.opcode, s"Invalid opcode for ${data.code} - ${data.addMode}! expected 0x${data.opcode.toHexString.toUpperCase()} got 0x${opcode.toHexString.toUpperCase()}.")
      assert(bytes == data.insLength, s"Invalid instruction length for ${data.code} - ${data.addMode}! expected ${data.insLength} got ${bytes}.")
    }
  }

}


