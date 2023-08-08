package com.udsl.processor6502.assemblerV1.tests

import com.udsl.processor6502.assembler.*
import com.udsl.processor6502.assembler.version1.Assemble6502SecondPass.{assembleCommandToken, assembleInstructionToken}
import com.udsl.processor6502.assembler.version1.{CommandToken, InstructionToken, TokenisedLineV1}
import com.udsl.processor6502.cpu.execution.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should


class Token( override val mnemonic: String, override val fields: Array[String], override val sourceLine: SourceLine ) extends InstructionToken(mnemonic: String, fields: Array[String], sourceLine )

object Token:
  def apply(mnemonic: String, fields: Array[String], predictions: List[AddressingMode] ): Token =
    val ins = new Token(mnemonic, fields, SourceLine(s"Testing $mnemonic", 15))
    ins.addPredictions(predictions)
    ins

class AddrBytWrdToken(override val mnemonic: String, override val fields: Array[String], override val sourceLine: SourceLine) extends CommandToken( mnemonic: String, fields: Array[String], sourceLine)

object AddrBytWrdToken:
  def apply(mnemonic: String, fields: Array[String]): AddrBytWrdToken =
    val cmd = new AddrBytWrdToken(mnemonic, fields, SourceLine(s"Testing $mnemonic", 23))
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
// token, opcode, loByte, hiByte
val dataValidTokensTest: List[(InstructionToken, Int, Option[Int], Option[Int])] = List(
  (Token("LDA", Array[String]("#$20"), List(Immediate)), 0xA9, Some(32), None),
  (Token("LDA", Array[String]("$10"), List(ZeroPage)), 0xA5, Some(16), None),
  (Token("LDA", Array[String]("10,X"), List(ZeroPageX)), 0xB5, Some(10), None),
  (Token("LDA", Array[String]("$1234"), List(Absolute)), 0xAD, Some(0x34), Some(0x12)),
  (Token("LDA", Array[String]("$3456,X"), List(AbsoluteX)), 0xBD, Some(0x56), Some(0x34)),
  (Token("LDA", Array[String]("$3456,Y"), List(AbsoluteY)), 0xB9, Some(0x56), Some(0x34)),
  (Token("LDA", Array[String]("($34,X)"), List(IndirectX)), 0xA1, Some(0x34), None),
  (Token("LDA", Array[String]("($56),Y"), List(IndirectY)), 0xB1, Some(0x56), None)
)

// token, opcode, loByte, hiByte, lable, label value
val dataValidLabelsAndTokensTestData: List[(InstructionToken, Int, Option[Int], Option[Int], String, Int)] = List(
  (Token("LDX", Array[String]("#defined"), List(Immediate)), 0xA2, Some(0x20), None, "defined", 0x20),
  (Token("BNE", Array[String]("reference2"), List(Relative)), 0xD0, Some(0xF9), None, "reference2", 1995)
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

class AssembleSpec extends AnyFlatSpec, should.Matchers:
  
  "Given a valid instruction token" should "should assemble to the correct opcode and value" in {
    for ((token, opcode, loByte, hiByte) <- dataValidTokensTest) {
      // get the current assembly location
      val srtLoc = AssembleLocation.currentLocation
      var written = 0
      // assemble the token into memory
      assembleInstructionToken(token, TokenisedLineV1(SourceLine(s"testing $token - $opcode", 72)))
      if opcode > -1 then
        // get the values written to memory and verify correct
        val memValue = AssembleLocation.getMemoryByte(srtLoc)
        assert(memValue == opcode, s"Expected opcode: 0x${opcode.toHexString.toUpperCase} was 0x${memValue.toHexString.toUpperCase}")
        written += 1
      loByte match
        case Some(v) =>
          val lowByte = AssembleLocation.getMemoryByte(srtLoc + 1)
          assert(lowByte == v, s"Expected loByte: 0x${v.toHexString.toUpperCase} was 0x${lowByte.toHexString.toUpperCase}")
          written += 1
          hiByte match
            case Some(v) =>
              val highByte = AssembleLocation.getMemoryByte(srtLoc + 2)
              assert(highByte == v, s"Expected highByte: 0x${v.toHexString.toUpperCase} was 0x${highByte.toHexString.toUpperCase}")
              written += 1
            case None => // no hiByte
        case None => // nether loByte or hiByte
      assert(AssembleLocation.currentLocation - srtLoc == written, s"Expected $written written but was ${AssembleLocation.currentLocation - srtLoc}")
    }
  }

  "Given a defined lable and valid instruction token referencing that lable" should "should assemble to the correct opcode and value" in {
    for ((token, opcode, loByte, hiByte, label, labelValue) <- dataValidLabelsAndTokensTestData) {
      // define the label
      AssemblyData.clear()
      AssemblyData.addLabel(label, labelValue)
      AssembleLocation.setAssembleLoc(2000)
      // get the current assembly location
      val srtLoc = AssembleLocation.currentLocation
      var written = 0
      // assemble the token into memory
      assembleInstructionToken(token, TokenisedLineV1(SourceLine(s"testing $token - $opcode", 104)))
      if opcode > -1 then
        // get the values written to memory and verify correct
        val memValue = AssembleLocation.getMemoryByte(srtLoc)
        assert(memValue == opcode, s"Expected opcode: 0x${opcode.toHexString.toUpperCase} was 0x${memValue.toHexString.toUpperCase}")
        written += 1
      // if we have a loByte we may also have a hiByte can not have a hiByte only
      loByte match
        case Some(v) =>
          val lowByte = AssembleLocation.getMemoryByte(srtLoc + 1)
          assert(lowByte == v, s"Expected loByte: 0x${v.toHexString.toUpperCase} was 0x${lowByte.toHexString.toUpperCase}")
          written += 1
          hiByte match
            case Some(v) =>
              val highByte = AssembleLocation.getMemoryByte(srtLoc + 2)
              assert(highByte == v, s"Expected highByte: 0x${v.toHexString.toUpperCase} was 0x${highByte.toHexString.toUpperCase}")
              written += 1
            case None => // no hiByte
        case None => // nether loByte or hiByte

      assert(AssembleLocation.currentLocation - srtLoc == written, s"Expected $written written but was ${AssembleLocation.currentLocation - srtLoc}")
    }
  }

  "Given a valid command token" should "should assemble" in {
    val startLoc = 1024
    AssembleLocation.setAssembleLoc(startLoc)
    for ((token, bytes, value) <- dataAddrBytWrdTokensTest) {
      val srtLoc = AssembleLocation.currentLocation
      var written = 0
      assembleCommandToken(token)
      written = AssembleLocation.currentLocation - srtLoc
      assert(written == bytes)
      var readLoc: Int = srtLoc
      try {
        val lowByte = AssembleLocation.getMemoryByte(readLoc)
        if bytes == 1 then
          assert(lowByte == value)
        else
          readLoc += 1
          val highByte = AssembleLocation.getMemoryByte(readLoc)
          assert(lowByte + (highByte * 256) == value)
      }
      catch
        case e: Exception => println(s"exception reading byte at $readLoc written by ${token.mnemonic}")
    }
  }

