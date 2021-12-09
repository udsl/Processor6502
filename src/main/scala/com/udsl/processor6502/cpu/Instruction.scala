package com.udsl.processor6502.cpu

import com.udsl.processor6502.cpu.execution.{Absolute, AbsoluteX, AbsoluteY, AddressingMode, Immediate, Implied, Indirect, IndirectX, IndirectY, Relative, ZeroPage, ZeroPageX, ZeroPageY, Accumulator}

trait CpuInstruction(val code: Map[AddressingMode, Int] ):
  def name(): String = ""

  def opcode(addrMode: AddressingMode): Option[Int] =
    code.get(addrMode)

  def bytes(addrMode: AddressingMode): Option[Int] =
    opcode(addrMode) match
      case Some(_) =>
        addrMode match {
          case Immediate | ZeroPage | ZeroPageX |  ZeroPageY | Indirect |
               IndirectX | IndirectY | Relative =>
            Some(2)
          case AbsoluteX | AbsoluteY | Absolute =>
            Some(3)
          case Implied | Accumulator =>
            Some(1)
          case _ => None
        }
      case None => None

case class ADC() extends CpuInstruction(Map(Immediate -> 0x69,
  Immediate -> 0x69,
  ZeroPage -> 0x65,
  ZeroPageX -> 0x75,
  Absolute -> 0x6D,
  AbsoluteX -> 0x7D,    // absolute,X
  AbsoluteY -> 0x79,    // absolute,Y
  IndirectX -> 0x61,   // (indirect,X)
  IndirectY -> 0x71)): // (indirect),Y
  override def name() = "ADC"

  
case class AND() extends CpuInstruction(Map(Immediate -> 0x29,
  ZeroPage -> 0x25,
  ZeroPageX -> 0x35,
  Absolute -> 0x2D,
  AbsoluteX -> 0x3D,    // absolute,X
  AbsoluteY -> 0x39,    // absolute,Y
  IndirectX -> 0x21,   // (indirect,X)
  IndirectY -> 0x31)): // (indirect),Y
  override def name() = "AND"


case class ASL() extends CpuInstruction(Map(Accumulator -> 0x0A,
  ZeroPage -> 0x06,
  ZeroPageX -> 0x16,
  Absolute -> 0x0E,
  AbsoluteX -> 0x1E    // absolute,X
)):
  override def name() = "ASL"


case class BCC() extends CpuInstruction(Map(Relative -> 0x90)):
  override def name() = "BCC"


case class BCS() extends CpuInstruction(Map(Relative -> 0xB0)):
  override def name() = "BCS"


case class BEQ() extends CpuInstruction(Map(ZeroPage -> 0x24)):
  override def name() = "BEQ"


case class BIT() extends CpuInstruction(Map(Accumulator -> 0x0A)):
  override def name() = "BIT"


case class BMI() extends CpuInstruction(Map(Relative -> 0x30)):
  override def name() = "BMI"


case class BNE() extends CpuInstruction(Map(Relative -> 0xD0)):
  override def name() = "BNE"


case class BPL() extends CpuInstruction(Map(Relative -> 0x10)):
  override def name() = "BPL"


case class BRK() extends CpuInstruction(Map(Relative -> 0x00)):
  override def name() = "BRK"


case class BVC() extends CpuInstruction(Map(Relative -> 0x50)):
  override def name() = "BVC"


case class BVS() extends CpuInstruction(Map(Relative -> 0x70)):
  override def name() = "BVS"


case class CLC() extends CpuInstruction(Map(Implied -> 0x18)):
  override def name() = "CLC"


case class CLD() extends CpuInstruction(Map(Implied -> 0xD8)):
  override def name() = "CLD"


case class CLI() extends CpuInstruction(Map(Implied -> 0x58)):
  override def name() = "CLI"


case class CLV() extends CpuInstruction(Map(Implied -> 0xD8)):
  override def name() = "CLV"


case class CMP() extends CpuInstruction(Map(Immediate -> 0xC9)):
  override def name() = "CMP"


case class CPX() extends CpuInstruction(Map(Immediate -> 0xE0)):
  override def name() = "CPX"


case class CPY() extends CpuInstruction(Map(Immediate -> 0xC0)):
  override def name() = "CPY"


case class DEC() extends CpuInstruction(Map(ZeroPage -> 0xC6)):
  override def name() = "DEC"


case class DEX() extends CpuInstruction(Map(ZeroPage -> 0xCA)):
  override def name() = "DEX"


case class DEY() extends CpuInstruction(Map(ZeroPage -> 0x88)):
  override def name() = "DEY"


case class EOR() extends CpuInstruction (Map(Immediate -> 0x49)):
  override def name() = "EOR"


case class INC() extends CpuInstruction(Map(ZeroPage -> 0xE6,
  ZeroPageX -> 0xF6,
  Absolute -> 0xEE,
  AbsoluteX -> 0xFE)):
  override def name() = "INC"


case class INX() extends CpuInstruction(Map(Immediate -> 0xE8)):
  override def name() = "INX"


case class INY() extends CpuInstruction(Map(Immediate -> 0xC8)):
  override def name() = "INY"


case class JMP() extends CpuInstruction(Map(Absolute -> 0x4C, Indirect -> 0x6C)):
  override def name() = "JMP"


case class JSR() extends CpuInstruction(Map(Absolute -> 0x20)):
  override def name() = "JSR"


case class LDA() extends CpuInstruction(Map(Immediate -> 0xA9)):
  override def name() = "LDA"


case class LDX() extends CpuInstruction(Map(Immediate -> 0xA2,
  ZeroPage -> 0xA6,
  ZeroPageY -> 0xB6,
  Absolute -> 0xAE,
  AbsoluteY ->0xBE)):
  override def name() = "LDX"


case class LDY() extends CpuInstruction(Map(Immediate -> 0xA0,
  ZeroPage -> 0xA4,
  ZeroPageX -> 0xB4,
  Absolute -> 0xAC,
  AbsoluteX ->0xBC)):
  override def name() = "LDY"


case class LSR() extends CpuInstruction(Map(Accumulator -> 0x4A,
  ZeroPage -> 0x46,
  ZeroPageX -> 0x56,
  Absolute -> 0xAE,
  AbsoluteX ->0x5E)):
  override def name() = "LSR"


case class NOP() extends CpuInstruction(Map(Implied -> 0xEA)):
  override def name() = "NOP"


case class ORA() extends CpuInstruction(Map(Immediate -> 0x09,
  ZeroPage -> 0x05,
  ZeroPageX -> 0x15,
  Absolute -> 0x0D,
  AbsoluteX ->0x1D,
  AbsoluteY -> 0x19,
  IndirectX -> 0X01,
  IndirectY-> 0x11)):
  override def name() = "ORA"


case class PHA() extends CpuInstruction(Map(Implied -> 0x48)):
  override def name() = "PHA"


case class PHP() extends CpuInstruction(Map(Implied -> 0x08)):
  override def name() = "PHP"


case class PLA() extends CpuInstruction(Map(Implied -> 0x68)):
  override def name() = "PLA"


case class PLP() extends CpuInstruction(Map(Implied -> 0x28)):
  override def name() = "PLP"


case class ROL() extends CpuInstruction(Map(Accumulator -> 0x2A,
  ZeroPage -> 0x26,
  ZeroPageX -> 0x36,
  Absolute -> 0x2E,
  AbsoluteX ->0x3E)):
  override def name() = "ROL"


case class ROR() extends CpuInstruction(Map(Accumulator -> 0x6A,
  ZeroPage -> 0x66,
  ZeroPageX -> 0x76,
  Absolute -> 0x6E,
  AbsoluteX ->0x7E)):
  override def name() = "ROR"


case class RTI() extends CpuInstruction(Map(Implied -> 0x40)):
  override def name() = "RTI"


case class RTS() extends CpuInstruction(Map(Implied -> 0x60)):
  override def name() = "RTS"


case class SBC() extends CpuInstruction(Map(Immediate -> 0xE9,
  ZeroPage -> 0xE5,
  ZeroPageX -> 0xF5,
  Absolute -> 0xED,
  AbsoluteX ->0xFD,
  AbsoluteY -> 0xF9,
  IndirectX-> 0xE1,
  IndirectY-> 0xF1)):
  override def name() = "SBC"

case class SEC() extends CpuInstruction(Map(Implied -> 0x38)):
  override def name() = "SEC"


case class SED() extends CpuInstruction(Map(Implied -> 0xF8)):
  override def name() = "SED"


case class SEI() extends CpuInstruction(Map(Implied -> 0x78)):
  override def name() = "SEI"


case class STA() extends CpuInstruction(Map(ZeroPage -> 0x85,
  ZeroPageX -> 0x95,
  Absolute -> 0x8D,
  AbsoluteX ->0x9D,
  AbsoluteY -> 0x99,
  IndirectX-> 0x81,
  IndirectY-> 0x91)):
  override def name() = "STA"


case class STX() extends CpuInstruction(Map(ZeroPage -> 0x86,
  ZeroPageX -> 0x96,
  Absolute -> 0x8E)):
  override def name() = "STX"


case class STY() extends CpuInstruction(Map(ZeroPage -> 0x84,
  ZeroPageX -> 0x94,
  Absolute -> 0x8C)):
  override def name() = "STY"


case class TAX() extends CpuInstruction(Map(Implied -> 0xAA)):
  override def name() = "TAX"


case class TAY() extends CpuInstruction(Map(Implied -> 0xA8)):
  override def name() = "TAY"


case class TSX() extends CpuInstruction(Map(Implied -> 0xBA)):
  override def name() = "TSX"


case class TXA() extends CpuInstruction(Map(Implied -> 0x8A)):
  override def name() = "TXA"


case class TXS() extends CpuInstruction(Map(Implied -> 0x9A)):
  override def name() = "TXS"


case class TYA() extends CpuInstruction(Map(Implied -> 0x98)):
  override def name() = "TYA"


object CpuInstructions :
  val validInstructions = LazyList(ADC(),AND(),ASL(),BCC(),BCS(),BEQ(),BIT(),BMI(),BNE(),BPL(),BRK(),BVC(),CLC(),CLD(), CLI(),CLV(),CMP(),CPX(),CPY(),DEC(),DEX(),DEY(),EOR(),INC(),INX(),INY(),JMP(),JSR(),LDA(),LDX(),LDY(),LSR(),NOP(),ORA(),PHA(),PHP(),PLA(),PLP(),ROL(),ROR(),RTI(),RTS(),SBC(),SEC(),SED(),SEI(),STA(),STX(),STY(),TAX(),TAY(),TXA(),TXS(),TYA())

  def isValidInstruction(ins: String): Boolean =
    !(ins == null || ins.isEmpty || validInstructions.filter(a => a.name().equals(ins.toUpperCase())).isEmpty)

  def getInstruction(ins: String): Option[CpuInstruction] =
    validInstructions.find(a => a.name().equals(ins.toUpperCase()))


  