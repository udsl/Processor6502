import com.udsl.processor6502.cpu.execution.Instruction
import org.scalatest.flatspec.AnyFlatSpec

class InstructionTest extends AnyFlatSpec  {
  "An instruction" should "should have the correct opcode and addressing mode" in {
    val ins = Instruction(105)
    assert(ins.value === 60)
  }
}
