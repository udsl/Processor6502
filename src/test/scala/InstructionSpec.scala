
import com.udsl.processor6502.cpu.{CpuInstruction, CpuInstructions}
import com.udsl.processor6502.cpu.execution.{Absolute, AbsoluteX, AbsoluteY, Accumulator, AddressingMode, DecodedInstruction, Immediate, IndirectX, IndirectY, ZeroPage, ZeroPageX}
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
 
  TestData("INC", 0xFE, 3, AbsoluteX), //"$LL,X"
  TestData("LDX", 0xA6, 2, ZeroPage), //"$LL"
)

class InstructionSpec extends AnyFlatSpec with should.Matchers {
  "An instruction" should "should have the correct opcode and addressing mode" in {
    for (data <- dataValidInstructionTest) {
      assert(CpuInstructions.isValidInstruction(data.code))
      val instruction = CpuInstructions.getInstruction(data.code)

      instruction match {
        case Some(value) =>
          val bytes = value.bytes(data.addMode)
          bytes match
            case Some(value) =>
              assert(value == data.insLength)
            case None => fail(s"Failed to get valid bytes for ${data.code} with ${data.addMode} addressing")
          value.opcode(data.addMode) match
            case Some(value) =>
              assert(value == data.opcode, s"Invalid opcode 0x${data.opcode.toHexString} expected 0x${value.toHexString}")
            case None => fail(s"Failed to get valid opcode for ${data.code}")

        case None => fail(s"Invalid addressing mode for ${data.code}")
      }
    }
  }

}


