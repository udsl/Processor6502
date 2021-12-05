package com.udsl.processor6502.cpu

import com.udsl.processor6502.assembler.AddressingMode
import com.udsl.processor6502.assembler.AddressingMode.*
import com.udsl.processor6502.cpu.execution.Immediate

trait CpuInstruction(val code: Map[AddressingMode, Int] ):
  def name(): String = ""

  def opcode(addrMode: AddressingMode): Option[Int] =
    code.get(addrMode)

  def bytes(addrMode: AddressingMode): Option[Int] =
    opcode(addrMode) match
      case Some(_) =>
        addrMode match {
          case AddressingMode.Immediate | AddressingMode.ZeroPage | AddressingMode.ZeroPageX |  AddressingMode.ZeroPageY | AddressingMode.Indirect |
               AddressingMode.ZeroPageIndirectX | AddressingMode.ZeroPageIndirectY | AddressingMode.Relative =>
            Some(2)
          case AddressingMode.AbsoluteIndexedX | AddressingMode.AbsoluteIndexedY | AddressingMode.Absolute =>
            Some(3)
          case AddressingMode.Implied | AddressingMode.Accumulator =>
            Some(1)
          case _ => None
        }
      case None => None





case class ADC() extends CpuInstruction(Map(AddressingMode.Immediate -> 0x69,
  AddressingMode.Immediate -> 0x69,
  AddressingMode.ZeroPage -> 0x65,
  AddressingMode.ZeroPageX -> 0x75,
  AddressingMode.Absolute -> 0x6D,
  AddressingMode.AbsoluteIndexedX -> 0x7D,    // absolute,X
  AddressingMode.AbsoluteIndexedY -> 0x79,    // absolute,Y
  AddressingMode.ZeroPageIndirectX -> 0x61,   // (indirect,X)
  AddressingMode.ZeroPageIndirectY -> 0x71)): // (indirect),Y
  override def name() = "ADC"

  
case class AND() extends CpuInstruction(Map(AddressingMode.Immediate -> 0x29,
  AddressingMode.ZeroPage -> 0x25,
  AddressingMode.ZeroPageX -> 0x35,
  AddressingMode.Absolute -> 0x2D,
  AddressingMode.AbsoluteIndexedX -> 0x3D,    // absolute,X
  AddressingMode.AbsoluteIndexedY -> 0x39,    // absolute,Y
  AddressingMode.ZeroPageIndirectX -> 0x21,   // (indirect,X)
  AddressingMode.ZeroPageIndirectY -> 0x31)): // (indirect),Y
  override def name() = "AND"


case class ASL() extends CpuInstruction(Map(AddressingMode.Accumulator -> 0x0A,
  AddressingMode.ZeroPage -> 0x06,
  AddressingMode.ZeroPageX -> 0x16,
  AddressingMode.Absolute -> 0x0E,
  AddressingMode.AbsoluteIndexedX -> 0x1E    // absolute,X
)):
  override def name() = "ASL"


case class BCC() extends CpuInstruction(Map(AddressingMode.Relative -> 0x90)):
  override def name() = "BCC"


case class BCS() extends CpuInstruction(Map(AddressingMode.Relative -> 0xB0)):
  override def name() = "BCS"


case class BEQ() extends CpuInstruction(Map(AddressingMode.ZeroPage -> 0x24)):
  override def name() = "BEQ"


case class BIT() extends CpuInstruction(Map(AddressingMode.Accumulator -> 0x0A)):
  override def name() = "BIT"


case class BMI() extends CpuInstruction(Map(AddressingMode.Relative -> 0x30)):
  override def name() = "BMI"


case class BNE() extends CpuInstruction(Map(AddressingMode.Relative -> 0xD0)):
  override def name() = "BNE"


case class BPL() extends CpuInstruction(Map(AddressingMode.Relative -> 0x10)):
  override def name() = "BPL"


case class BRK() extends CpuInstruction(Map(AddressingMode.Relative -> 0x00)):
  override def name() = "BRK"


case class BVC() extends CpuInstruction(Map(AddressingMode.Relative -> 0x50)):
  override def name() = "BVC"


case class BVS() extends CpuInstruction(Map(AddressingMode.Relative -> 0x70)):
  override def name() = "BVS"


case class CLC() extends CpuInstruction(Map(AddressingMode.Implied -> 0x18)):
  override def name() = "CLC"


case class CLD() extends CpuInstruction(Map(AddressingMode.Implied -> 0xD8)):
  override def name() = "CLD"


case class CLI() extends CpuInstruction(Map(AddressingMode.Implied -> 0x58)):
  override def name() = "CLI"


case class CLV() extends CpuInstruction(Map(AddressingMode.Implied -> 0xD8)):
  override def name() = "CLV"


case class CMP() extends CpuInstruction(Map(AddressingMode.Immediate -> 0xC9)):
  override def name() = "CMP"


case class CPX() extends CpuInstruction(Map(AddressingMode.Immediate -> 0xE0)):
  override def name() = "CPX"


case class CPY() extends CpuInstruction(Map(AddressingMode.Immediate -> 0xC0)):
  override def name() = "CPY"


case class DEC() extends CpuInstruction(Map(AddressingMode.ZeroPage -> 0xC6)):
  override def name() = "DEC"


case class DEX() extends CpuInstruction(Map(AddressingMode.ZeroPage -> 0xCA)):
  override def name() = "DEX"


case class DEY() extends CpuInstruction(Map(AddressingMode.ZeroPage -> 0x88)):
  override def name() = "DEY"


case class EOR() extends CpuInstruction (Map(AddressingMode.Immediate -> 0x49)):
  override def name() = "EOR"


case class INC() extends CpuInstruction(Map(AddressingMode.ZeroPage -> 0xE6,
  AddressingMode.ZeroPageX -> 0xF6,
  AddressingMode.Absolute -> 0xEE,
  AddressingMode.AbsoluteIndexedX -> 0xFE)):
  override def name() = "INC"


case class INX() extends CpuInstruction(Map(AddressingMode.Immediate -> 0xE8)):
  override def name() = "INX"


case class INY() extends CpuInstruction(Map(AddressingMode.Immediate -> 0xC8)):
  override def name() = "INY"


case class JMP() extends CpuInstruction(Map(AddressingMode.Absolute -> 0x4C, AddressingMode.Indirect -> 0x6C)):
  override def name() = "JMP"


case class JSR() extends CpuInstruction(Map(AddressingMode.Absolute -> 0x20)):
  override def name() = "JSR"


case class LDA() extends CpuInstruction(Map(AddressingMode.Immediate -> 0xA9)):
  override def name() = "LDA"


case class LDX() extends CpuInstruction(Map(AddressingMode.Immediate -> 0xA2,
  AddressingMode.ZeroPage -> 0xA6,
  AddressingMode.ZeroPageY -> 0xB6,
  AddressingMode.Absolute -> 0xAE,
  AddressingMode.AbsoluteIndexedY ->0xBE)):
  override def name() = "LDX"


case class LDY() extends CpuInstruction(Map(AddressingMode.Immediate -> 0xA0)):
  override def name() = "LDY"


case class LSR() extends CpuInstruction(Map(AddressingMode.Accumulator -> 0x4A)):
  override def name() = "LSR"


case class NOP() extends CpuInstruction(Map(AddressingMode.Implied -> 0xEA)):
  override def name() = "NOP"


case class ORA() extends CpuInstruction(Map(AddressingMode.Immediate -> 0x09)):
  override def name() = "ORA"


case class PHA() extends CpuInstruction(Map(AddressingMode.Implied -> 0x48)):
  override def name() = "PHA"


case class PHP() extends CpuInstruction(Map(AddressingMode.Implied -> 0x08)):
  override def name() = "PHP"


case class PLA() extends CpuInstruction(Map(AddressingMode.Implied -> 0x68)):
  override def name() = "PLA"


case class PLP() extends CpuInstruction(Map(AddressingMode.Implied -> 0x28)):
  override def name() = "PLP"


case class ROL() extends CpuInstruction(Map(AddressingMode.Accumulator -> 0x2A)):
  override def name() = "ROL"


case class ROR() extends CpuInstruction(Map(AddressingMode.Accumulator -> 0x6A)):
  override def name() = "ROR"


case class RTI() extends CpuInstruction(Map(AddressingMode.Implied -> 0x40)):
  override def name() = "RTI"


case class RTS() extends CpuInstruction(Map(AddressingMode.Implied -> 0x60)):
  override def name() = "RTS"


case class SBC() extends CpuInstruction(Map(AddressingMode.Immediate -> 0xE9)):
  override def name() = "SBC"

case class SEC() extends CpuInstruction(Map(AddressingMode.Implied -> 0x38)):
  override def name() = "SEC"


case class SED() extends CpuInstruction(Map(AddressingMode.Implied -> 0xF8)):
  override def name() = "SED"


case class SEI() extends CpuInstruction(Map(AddressingMode.Implied -> 0x78)):
  override def name() = "SEI"


case class STA() extends CpuInstruction(Map(AddressingMode.ZeroPage -> 0x85)):
  override def name() = "STA"


case class STX() extends CpuInstruction(Map(AddressingMode.ZeroPage -> 0x86)):
  override def name() = "STX"


case class STY() extends CpuInstruction(Map(AddressingMode.ZeroPage -> 0x84)):
  override def name() = "STY"


case class TAX() extends CpuInstruction(Map(AddressingMode.Implied -> 0xAA)):
  override def name() = "TAX"


case class TAY() extends CpuInstruction(Map(AddressingMode.Implied -> 0xA8)):
  override def name() = "TAY"


case class TSX() extends CpuInstruction(Map(AddressingMode.Implied -> 0xBA)):
  override def name() = "TSX"


case class TXA() extends CpuInstruction(Map(AddressingMode.Implied -> 0x8A)):
  override def name() = "TXA"


case class TXS() extends CpuInstruction(Map(AddressingMode.Implied -> 0x9A)):
  override def name() = "TXS"


case class TYA() extends CpuInstruction(Map(AddressingMode.Implied -> 0x98)):
  override def name() = "TYA"


object CpuInstructions :
  val validInstructions = LazyList(ADC(),AND(),ASL(),BCC(),BCS(),BEQ(),BIT(),BMI(),BNE(),BPL(),BRK(),BVC(),CLC(),CLD(), CLI(),CLV(),CMP(),CPX(),CPY(),DEC(),DEX(),DEY(),EOR(),INC(),INX(),INY(),JMP(),JSR(),LDA(),LDX(),LDY(),LSR(),NOP(),ORA(),PHA(),PHP(),PLA(),PLP(),ROL(),ROR(),RTI(),RTS(),SBC(),SEC(),SED(),SEI(),STA(),STX(),STY(),TAX(),TAY(),TXA(),TXS(),TYA())

  def isValidInstruction(ins: String): Boolean =
    !(ins == null || ins.isEmpty || validInstructions.filter(a => a.name().equals(ins.toUpperCase())).isEmpty)

  def getInstruction(ins: String): Option[CpuInstruction] =
    validInstructions.find(a => a.name().equals(ins.toUpperCase()))


  