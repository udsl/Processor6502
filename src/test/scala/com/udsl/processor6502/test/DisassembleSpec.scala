package com.udsl.processor6502.test

import com.udsl.processor6502.assembler.Assemble6502SecondPass.assembleInstructionToken
import com.udsl.processor6502.assembler.{AssembleLocation, AssemblerToken, InstructionToken, TokenisedLine}
import com.udsl.processor6502.cpu.execution.*
import com.udsl.processor6502.cpu.execution.Opcode.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should


val testData: List[(Int, OpcodeValue, Boolean)] = List(
  (0xA9, LDA(Immediate), true),
  (0xA5, LDA(ZeroPage), true),
  (0xB5, LDA(ZeroPageX), true),
  (0xAD, LDA(Absolute), true),
  (0xBD, LDA(AbsoluteX), true),
  (0xB9, LDA(AbsoluteY), true),
  (0xA1, LDA(IndirectX), true),
  (0xB1, LDA(IndirectY), true),
  // Fail values
  (0xA9, LDA(ZeroPage), false),
  (0xA9, LDA(ZeroPageX), false),
  (0xA9, LDA(Absolute), false)
)

val undefinedInstructionTestData: List[(Int)] = List(
  (0x02)
)

class DisassembleSpec extends AnyFlatSpec, should.Matchers {
  "Given a valid opcode" should "disassemble to the correct instruction and addressing mode" in {
    for ((opcode, expected, result) <- testData) {
      val disassembled = disassemble(opcode)
      assert(disassembled.equals(expected) == result)
    }
  }

  "Given an invalid opcode" should "disassemble to an undefined instruction without an applicable addressing mode" in {
    for ((opcode) <- undefinedInstructionTestData) {
      val disassembled = disassemble(opcode)
      assert(disassembled.equals(Illegal(NotApplicable)))
    }
  }

}
