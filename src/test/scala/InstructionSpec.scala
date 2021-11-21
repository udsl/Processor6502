import com.udsl.processor6502.cpu.execution.DecodedInstruction

import org.scalatest._
import flatspec._
import matchers._


class TestData(val code: Int, val opcode: String, val insLength: Int, val addMode: String){

}

object TestData{
  def apply(code: Int, opcode: String, insLength: Int, addMode: String): TestData = {
    new TestData(code, opcode, insLength, addMode)
  }
}
//
//val dataFirTest: List[TestData] = List(
//  TestData(0x69, "ADC", 2, "#"),
//  TestData(0x65, "ADC", 2, "$LL"),
//  TestData(0x75, "ADC", 2, "$LL,X"),
//  TestData(0x6D, "ADC", 3, "absolute"),
//  TestData(0x7D, "ADC", 3, "$LL,X"),
//  TestData(0x79, "ADC", 3, "$LL,Y"),
//  TestData(0x61, "ADC", 2, "($LL,X)"),
//  TestData(0x71, "ADC", 2, "($LL),Y"),
//  TestData(0xFE, "INC", 3, "$LL,X"),
//  TestData(0xA6, "LDX", 2, "$LL")
//)
//
//class InstructionSpec extends AnyFlatSpec with should.Matchers {
//  "An instruction" should "should have the correct opcode and addressing mode" in {
//    for (data <- dataFirTest) {
//      val ins = Instruction(data.code)
//      assert(ins.value === data.code)
//      assert(ins.opcode.toString == data.opcode)
//      assert(ins.addMode.value.bytes == data.insLength)
//      assert(ins.insLen == data.insLength)
//      assert(ins.addMode.toString === data.addMode)
//    }
//  }
//
//}
//

