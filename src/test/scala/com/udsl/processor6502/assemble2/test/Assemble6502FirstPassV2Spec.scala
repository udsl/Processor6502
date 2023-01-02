package com.udsl.processor6502.assemble2.test

import com.udsl.processor6502.assembler.{AssembleLocation, AssemblyData}
import com.udsl.processor6502.assemblier2.{Assemble6502FirstPassV2, InstructionToken, TokenisedLine, Tokeniser}
import com.udsl.processor6502.cpu.execution.{Absolute, AddressingMode, Immediate, ZeroPage}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

class Assemble6502FirstPassV2Spec extends AnyFlatSpec with TableDrivenPropertyChecks with Matchers :

  private def generateTokenData(lineNum: Int, text: String ): TokenisedLine = Tokeniser.tockenise(text, lineNum)

  val testData: TableFor2[TokenisedLine, AddressingMode] = Table(
    ("tokenisedLine", "addressingMode")
    , (generateTokenData(1, "LDA #02"), Immediate)
    , (generateTokenData(2, "STA 2002"), Absolute)
    , (generateTokenData(3, "STA 202"), ZeroPage)
  )

  "Given a TokenisedLine from a valid instruction assemble" should
    "write the correct opcode and update currentLocation correctly" in {
    forAll(testData) {(tokenisedLine, addressingMode) =>
      val cl: Int = AssembleLocation.currentLocation
      AssemblyData.clear()
      Assemble6502FirstPassV2.assemble(tokenisedLine)
      val instructionToken = tokenisedLine.tokens.head.asInstanceOf[InstructionToken]
      val instruction = instructionToken.instruction
      assert(AssembleLocation.getMemoryByte(cl) == instruction.opcode(addressingMode).get)
      val insSize = instruction.bytes(addressingMode).get
      assert(AssembleLocation.currentLocation == cl + insSize)
    }
  }