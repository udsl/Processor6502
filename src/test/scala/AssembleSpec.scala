import com.udsl.processor6502.assembler.Assemble6502SecondPass.{assembleCommandToken, assembleInstructionToken}
import com.udsl.processor6502.assembler.{AssembleLocation, AssemblerToken, CommandToken, InstructionToken, TokenisedLine}
import com.udsl.processor6502.cpu.execution.{Absolute, AbsoluteX, AbsoluteY, AddressingMode, Immediate, IndirectX, IndirectY, ZeroPage, ZeroPageX, ZeroPageY}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should


class Token( override val mnemonic: String, override val fields: Array[String] ) extends InstructionToken(mnemonic: String, fields: Array[String] )

object Token:
  def apply(mnemonic: String, fields: Array[String], predictions: List[AddressingMode] ): Token =
    val ins = new Token(mnemonic, fields)
    ins.addPredictions(predictions)
    ins

class AddrBytWrdToken(override val mnemonic: String, override val fields: Array[String]) extends CommandToken( mnemonic: String, fields: Array[String])

object AddrBytWrdToken:
  def apply(mnemonic: String, fields: Array[String]): AddrBytWrdToken =
    val cmd = new AddrBytWrdToken(mnemonic, fields)
    cmd

/**
 * List of tubles (token, opcode, loByte, hiByte) test data
 * where token is mnemonic, fields, predictions - produced by tokeniser
 *    mnemonic: the instruction or assemblier command text
 *    fields: the space seperated 'parameters' to the instruction - that is the none comment part of an assembly source line
 *    predictions: the list of possible addressing modes
 * opcode is the HEX value the token should compile to
 * loByte & hiByte the optional bytes that follow the instruction in memory - depends on addressing mode. -1 = byte not valid.
 */

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

val dataAddrBytWrdTokensTest: List[(AddrBytWrdToken, Int, Int)] = List(
  (AddrBytWrdToken("ADDR", Array[String]("$FECA")), 2, 0xFECA),
  (AddrBytWrdToken("BYT", Array[String]("102")), 1, 102),
  (AddrBytWrdToken("WRD", Array[String]("$FECA")), 2, 0xCAFE) // A word not an address so hi byte first
)

/*
Testing the DefToken is not straight forwards because the lable is defined during pass 1 and retrieved during pass 2 and
does not update any memory, the value could be used do do that else where!
*/

class AssembleSpec extends AnyFlatSpec, should.Matchers {
  
  "Given a valid instruction token" should "should assemble to the correct opcode and value" in {
    for ((token, opcode, loByte, hiByte) <- dataValidTokensTest) {
      // get the current assembly location
      val srtLoc = AssembleLocation.currentLocation
      var written = 0
      // assemble the token into memory
      assembleInstructionToken(token, TokenisedLine(s"testing $token - $opcode"))
      if opcode > -1 then
        // get the values written to memory and verify correct
        val memValue = AssembleLocation.getMemoryByte(srtLoc)
        assert(memValue == opcode, s"Expected opcode: 0x${opcode.toHexString.toUpperCase} was 0x${memValue.toHexString.toUpperCase}")
        written += 1
      if loByte > -1 then
        val lowByte = AssembleLocation.getMemoryByte(srtLoc+1)
        assert(lowByte == loByte, s"Expected loByte: 0x${loByte.toHexString.toUpperCase} was 0x${lowByte.toHexString.toUpperCase}")
        written += 1
      if hiByte > -1 then
        val highByte = AssembleLocation.getMemoryByte(srtLoc+2)
        assert(highByte == hiByte, s"Expected highByte: 0x${hiByte.toHexString.toUpperCase} was 0x${highByte.toHexString.toUpperCase}")
        written += 1
      assert(AssembleLocation.currentLocation - srtLoc == written, s"Expected $written written but was ${AssembleLocation.currentLocation - srtLoc}")
    }
  }

  "Given a valid command token" should "should assemble" in {
    AssembleLocation.setAssembleLoc(1024)
    for ((token, bytes, value) <- dataAddrBytWrdTokensTest) {
      val srtLoc = AssembleLocation.currentLocation
      var written = 0
      assembleCommandToken(token)
      written = AssembleLocation.currentLocation - srtLoc
      assert(written == bytes)
      val lowByte = AssembleLocation.getMemoryByte(srtLoc)
      val highByte = AssembleLocation.getMemoryByte(srtLoc+1)
      if bytes == 1 then
        assert(lowByte == value)
      else
        assert(lowByte + (highByte * 256) == value)
    }
  }

}
