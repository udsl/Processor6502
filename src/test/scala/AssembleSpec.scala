import com.udsl.processor6502.assembler.Assemble6502SecondPass.assembleInstructionToken
import com.udsl.processor6502.assembler.{AssembleLocation, AssemblerToken, InstructionToken, TokenisedLine}
import com.udsl.processor6502.cpu.execution.{Absolute, AbsoluteX, AbsoluteY, AddressingMode, Immediate, IndirectX, IndirectY, ZeroPage, ZeroPageX, ZeroPageY}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should


class Token( override val mnemonic: String, override val fields: Array[String] ) extends InstructionToken(mnemonic: String, fields: Array[String] )

object Token:
  def apply(mnemonic: String, fields: Array[String], predictions: List[AddressingMode] ): Token =
    val ins = new Token(mnemonic, fields)
    ins.addPredictions(predictions)
    ins

val dataValidTokensTest: List[(InstructionToken, Int, Int, Int)] = List(
  (Token("LDA", Array[String]("#$20"), List(Immediate)), 0xA9, 32, -1),
  (Token("LDA", Array[String]("$10"), List(ZeroPage)), 0xA5, 16, -1),
  (Token("LDA", Array[String]("10,X"), List(ZeroPageX)), 0xB5, 10, -1),
  (Token("LDA", Array[String]("$1234"), List(Absolute)), 0xAD, 0x34, 0x12),
  (Token("LDA", Array[String]("$3456,X"), List(AbsoluteX)), 0xBD, 0x56, 0x34),
  (Token("LDA", Array[String]("$3456,Y"), List(AbsoluteY)), 0xB9, 0x56, 0x34),
  (Token("LDA", Array[String]("($34,X)"), List(IndirectX)), 0xA1, 0x34, -1),
  (Token("LDA", Array[String]("($56),Y"), List(IndirectY)), 0xB1, 0x56, -1)
)

class AssembleSpec extends AnyFlatSpec, should.Matchers {
  "Given a valid instruction token" should "should assemble to the correct opcode and value" in {
    for ((token, opcode, loByte, hiByte) <- dataValidTokensTest) {
      val srtLoc = AssembleLocation.currentLocation
      assembleInstructionToken(token, TokenisedLine(s"testing $token - $opcode"))
      val memValue = AssembleLocation.getMemoryByte(srtLoc)
      assert(memValue == opcode, s"Expected opcode: 0x${opcode.toHexString.toUpperCase} was 0x${memValue.toHexString.toUpperCase}")
      val lowByte = AssembleLocation.getMemoryByte(srtLoc+1)
      assert(lowByte == loByte, s"Expected loByte: 0x${loByte.toHexString.toUpperCase} was 0x${lowByte.toHexString.toUpperCase}")
      if hiByte > -1 then
        val highByte = AssembleLocation.getMemoryByte(srtLoc+2)
        assert(highByte == hiByte, s"Expected highByte: 0x${hiByte.toHexString.toUpperCase} was 0x${highByte.toHexString.toUpperCase}")
    }
  }
}
