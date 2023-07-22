package com.udsl.processor6502.assemblerV1.tests

import com.udsl.processor6502.cpu.execution.{ADC, AND, Absolute, AbsoluteX, AbsoluteY, CMP, EOR, Illegal, Immediate, IndirectX, IndirectY, LDA, ORA, Opcode, SBC, STA, ZeroPage, ZeroPageX}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.runtime.stdLibPatches.Predef.assert


val newTestData: List[(Int, Opcode, Boolean, String)] = List(
  (0x69, ADC(Immediate), true, "ADC 0x69"),
  (0x65, ADC(ZeroPage), true, "ADC 0x65"),
  (0x75, ADC(ZeroPageX), true, "ADC 0x75"),
  (0x6D, ADC(Absolute), true, "ADC 0x6D"),
  (0x7D, ADC(AbsoluteX), true, "ADC 0x7D"),
  (0x79, ADC(AbsoluteY), true, "ADC 0x79"),
  (0x61, ADC(IndirectX), true, "ADC 0x61"),
  (0x71, ADC(IndirectY), true, "ADC 0x71"),

  (0x29, AND(Immediate), true, "AND 0x29"),
  (0x25, AND(ZeroPage), true, "AND 0x25"),
  (0x35, AND(ZeroPageX), true, "AND 0x35"),
  (0x2D, AND(Absolute), true, "AND 0x2D"),
  (0x3D, AND(AbsoluteX), true, "AND 0x3D"),
  (0x39, AND(AbsoluteY), true, "AND 0x39"),
  (0x21, AND(IndirectX), true, "AND 0x21"),
  (0x31, AND(IndirectY), true, "AND 0x31"),

  (0xA9, LDA(Immediate), true, "LDA 0xA9"),
  (0xA5, LDA(ZeroPage), true, "LDA 0xA5"),
  (0xB5, LDA(ZeroPageX), true, "LDA 0xB5"),
  (0xAD, LDA(Absolute), true, "LDA 0xAD"),
  (0xBD, LDA(AbsoluteX), true, "LDA 0xBD"),
  (0xB9, LDA(AbsoluteY), true, "LDA 0xB9"),
  (0xA1, LDA(IndirectX), true, "LDA 0xA1"),
  (0xB1, LDA(IndirectY), true, "LDA 0xB1"),

  (0x49, EOR(Immediate), true, "EOR 0x49"),
  (0x45, EOR(ZeroPage), true, "EOR 0x45"),
  (0x55, EOR(ZeroPageX), true, "EOR 0x55"),
  (0x4D, EOR(Absolute), true, "EOR 0x4D"),
  (0x5D, EOR(AbsoluteX), true, "EOR 0x5D"),
  (0x59, EOR(AbsoluteY), true, "EOR 0x59"),
  (0x41, EOR(IndirectX), true, "EOR 0x41"),
  (0x51, EOR(IndirectY), true, "EOR 0x51"),

  (0x09, ORA(Immediate), true, "ORA 0x09"),
  (0x05, ORA(ZeroPage), true, "ORA 0x05"),
  (0x15, ORA(ZeroPageX), true, "ORA 0x15"),
  (0x0D, ORA(Absolute), true, "ORA 0x0D"),
  (0x1D, ORA(AbsoluteX), true, "ORA 0x1D"),
  (0x19, ORA(AbsoluteY), true, "ORA 0x19"),
  (0x01, ORA(IndirectX), true, "ORA 0x01"),
  (0x11, ORA(IndirectY), true, "ORA 0x11"),

  (0x85, STA(ZeroPage), true, "STA 0x85"),
  (0x95, STA(ZeroPageX), true, "STA 0x95"),
  (0x8D, STA(Absolute), true, "STA 0x8D"),
  (0x9D, STA(AbsoluteX), true, "STA 0x9D"),
  (0x99, STA(AbsoluteY), true, "STA 0x99"),
  (0x81, STA(IndirectX), true, "STA 0x81"),
  (0x91, STA(IndirectY), true, "STA 0x91"),

  (0xC9, CMP(Immediate), true, "CMP 0xC9"),
  (0xC5, CMP(ZeroPage), true, "CMP 0xC5"),
  (0xD5, CMP(ZeroPageX), true, "CMP 0xD5"),
  (0xCD, CMP(Absolute), true, "CMP 0xCD"),
  (0xDD, CMP(AbsoluteX), true, "CMP 0xDD"),
  (0xD9, CMP(AbsoluteY), true, "CMP 0xD9"),
  (0xC1, CMP(IndirectX), true, "CMP 0xC1"),
  (0xD1, CMP(IndirectY), true, "CMP 0xD1"),

  (0xE9, SBC(Immediate), true, "SBC 0xE9"),
  (0xE5, SBC(ZeroPage), true, "SBC 0xE5"),
  (0xF5, SBC(ZeroPageX), true, "SBC 0xF5"),
  (0xED, SBC(Absolute), true, "SBC 0xED"),
  (0xFD, SBC(AbsoluteX), true, "SBC 0xFD"),
  (0xF9, SBC(AbsoluteY), true, "SBC 0xF9"),
  (0xE1, SBC(IndirectX), true, "SBC 0xE1"),
  (0xF1, SBC(IndirectY), true, "SBC 0xF1"),

  // Fail values
  (0xA9, LDA(ZeroPage), false, "ADC 0x69 fail test 1"),
  (0xA9, LDA(ZeroPageX), false, "ADC 0x69 fail test 2"),
  (0xA9, LDA(Absolute), false, "ADC 0x69 fail test 3"),
  // invalid instruction
  (0x02, Illegal, true, "Illegal instruction test!")
)

val undefinedOpcodeTestData: List[Int] = List(
  0x02
)

class OpcodeSpec extends AnyFlatSpec, should.Matchers {
  
  "Given a valid opcode the new InstructionOpcode " should " give the correct instruction and addressing mode" in {
    for ((opcode, expected, result, title) <- newTestData) {
      val disassembled = Opcode(opcode)
      assert(disassembled.equals(expected) == result, s"$title test failed")
    }
  }

  "Given an invalid opcode" should "disassemble to an undefined instruction without an applicable addressing mode" in {
    for (opcode <- undefinedOpcodeTestData) {
      val disassembled = Opcode(opcode)
      assert(disassembled.equals(Illegal))
    }
  }

}
