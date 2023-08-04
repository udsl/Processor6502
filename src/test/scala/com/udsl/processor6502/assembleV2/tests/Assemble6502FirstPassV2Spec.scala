package com.udsl.processor6502.assembleV2.tests

import com.udsl.processor6502.assembler.version2.{Assemble6502FirstPassV2, FirstPassResult, InstructionTokenV2, TokenisedLineV2, TokeniserV2}
import com.udsl.processor6502.assembler.{AssembleLocation, AssemblyData, SourceLine}
import com.udsl.processor6502.cpu.{CpuInstruction, LDA, STA}
import com.udsl.processor6502.cpu.execution.{Absolute, AddressingMode, Immediate, ZeroPage}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2, TableFor3}

class Assemble6502FirstPassV2Spec extends AnyFlatSpec with TableDrivenPropertyChecks with Matchers :

  private def generateTokenData(lineNum: Int, text: String ): TokenisedLineV2 = TokeniserV2.tockenise(SourceLine(text, lineNum))

  val testData: TableFor3[TokenisedLineV2, CpuInstruction, AddressingMode] = Table(
    ("tokenisedLine", "instruction", "addressingMode")
    , (generateTokenData(1, "LDA #02"), LDA(), Immediate)
    , (generateTokenData(2, "STA 2002"), STA(), Absolute)
    , (generateTokenData(3, "STA 202"), STA(), ZeroPage)
  )

  "Given a TokenisedLine from a valid instruction assemble" should
    "write the correct opcode and update the token and currentLocation correctly" in {
    forAll(testData) {(tokenisedLine, ins, addressingMode) =>
      val cl: Int = AssembleLocation.currentLocation
      AssemblyData.clear()
      val res: FirstPassResult = Assemble6502FirstPassV2.apply.assemble(tokenisedLine)
      val instructionToken = res.tokens.head.asInstanceOf[InstructionTokenV2]

      // Verify that the First Pass has updated the token correctly
      val instruction = instructionToken.instruction
      assert(instruction == ins)
      val addMode = instructionToken.addressingMode
      assert(addMode == addressingMode)

      // verify that the opcode written to memory is as set in the token
      assert(AssembleLocation.getMemoryByte(cl) == instructionToken.opcode)
      val insSize = instruction.bytes(addressingMode).get
      assert(AssembleLocation.currentLocation == cl + insSize)
    }
  }