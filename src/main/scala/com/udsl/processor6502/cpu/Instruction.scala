package com.udsl.processor6502.cpu

import com.udsl.processor6502.cpu.execution.{Absolute, AbsoluteX, AbsoluteY, Accumulator, AddressingMode, Immediate, Implied, Indirect, IndirectX, IndirectY, Invalid, Relative, ZeroPage, ZeroPageX, ZeroPageY}
import com.udsl.processor6502.cpu.InsData

class InsData(val opcode: Int, val bytes: Int)

object InsData:
  def apply(opcode: Int, bytes: Int): InsData =
    val data = new InsData(opcode, bytes)
    data
    
trait CpuInstruction(val code: Map[AddressingMode, InsData] ):
  def name(): String = ""

  def getInsDataForAddressingMode(addrMode: AddressingMode): Option[InsData] =
    code.get(addrMode)
    
  def hasAddressingMode(addrMode: AddressingMode): Boolean =
    code.contains(addrMode)
  def opcode(addrMode: AddressingMode): Option[Int] =
    code.get(addrMode) match {
      case Some(v) => Some(v.opcode)
      case _ => None
    }

  def bytes(addrMode: AddressingMode): Option[Int] =
    code.get(addrMode) match {
      case Some(v) => Some(v.bytes)
      case _ => None
    }

case class ADC() extends CpuInstruction(Map(Immediate -> InsData( 0x69, 2),
  ZeroPage -> InsData( 0x65, 2),
  ZeroPageX -> InsData( 0x75, 2),
  Absolute -> InsData( 0x6D, 3),
  AbsoluteX -> InsData( 0x7D, 3),    // absolute,X
  AbsoluteY -> InsData( 0x79, 3),   // absolute,Y
  IndirectX -> InsData( 0x61, 2),  // (indirect,X)
  IndirectY -> InsData( 0x71, 2))): // (indirect),Y
  override def name() = "ADC"

  
case class AND() extends CpuInstruction(Map(Immediate -> InsData( 0x29, 2),
  ZeroPage -> InsData( 0x25, 2),
  ZeroPageX -> InsData( 0x35, 2),
  Absolute -> InsData( 0x2D, 3),
  AbsoluteX -> InsData( 0x3D, 3),   // absolute,X
  AbsoluteY -> InsData( 0x39, 3),   // absolute,Y
  IndirectX -> InsData( 0x21, 2),  // (indirect,X)
  IndirectY -> InsData( 0x31, 2))): // (indirect),Y
  override def name() = "AND"


case class ASL() extends CpuInstruction(Map(Accumulator -> InsData( 0x0A, 1),
  ZeroPage -> InsData( 0x06, 2),
  ZeroPageX -> InsData( 0x16, 2),
  Absolute -> InsData( 0x0E, 3),
  AbsoluteX -> InsData( 0x1E, 3))):  // absolute,X
  override def name() = "ASL"


case class BCC() extends CpuInstruction(Map(Relative -> InsData( 0x90, 2))):
  override def name() = "BCC"


case class BCS() extends CpuInstruction(Map(Relative -> InsData( 0xB0, 2))):
  override def name() = "BCS"


case class BEQ() extends CpuInstruction(Map(Relative -> InsData( 0xF0, 2))):
  override def name() = "BEQ"


case class BIT() extends CpuInstruction(Map(ZeroPage -> InsData( 0x24, 2),
  Absolute -> InsData( 0x2C, 3))):
  override def name() = "BIT"


case class BMI() extends CpuInstruction(Map(Relative -> InsData( 0x30, 2))):
  override def name() = "BMI"


case class BNE() extends CpuInstruction(Map(Relative -> InsData( 0xD0, 2))):
  override def name() = "BNE"


case class BPL() extends CpuInstruction(Map(Relative -> InsData( 0x10, 2))):
  override def name() = "BPL"


case class BRK() extends CpuInstruction(Map(Implied -> InsData( 0x00, 1))):
  override def name() = "BRK"


case class BVC() extends CpuInstruction(Map(Relative -> InsData( 0x50, 2))):
  override def name() = "BVC"


case class BVS() extends CpuInstruction(Map(Relative -> InsData( 0x70, 2))):
  override def name() = "BVS"


case class CLC() extends CpuInstruction(Map(Implied -> InsData( 0x18, 1))):
  override def name() = "CLC"


case class CLD() extends CpuInstruction(Map(Implied -> InsData( 0xD8, 1))):
  override def name() = "CLD"


case class CLI() extends CpuInstruction(Map(Implied -> InsData( 0x58, 1))):
  override def name() = "CLI"


case class CLV() extends CpuInstruction(Map(Implied -> InsData( 0xB8, 1))):
  override def name() = "CLV"


case class CMP() extends CpuInstruction(Map(Immediate -> InsData( 0xC9, 2),
  ZeroPage -> InsData( 0xC5, 2),
  ZeroPageX -> InsData( 0xD5, 2),
  Absolute -> InsData( 0xCD, 3),
  AbsoluteX -> InsData( 0xDD, 3),    // absolute,X
  AbsoluteY -> InsData( 0xD9, 3),   // absolute,Y
  IndirectX -> InsData( 0xC1, 2),  // (indirect,X)
  IndirectY -> InsData( 0xD1, 2))): // (indirect),Y
  override def name() = "CMP"


case class CPX() extends CpuInstruction(Map(Immediate -> InsData( 0xE0, 2),
  ZeroPage -> InsData( 0xE4, 2),
  Absolute -> InsData( 0xEC, 3))):
  override def name() = "CPX"


case class CPY() extends CpuInstruction(Map(Immediate -> InsData( 0xC0, 2),
  ZeroPage -> InsData( 0xC4, 2),
  Absolute -> InsData( 0xCC, 3))):
  override def name() = "CPY"


case class DEC() extends CpuInstruction(Map(ZeroPage -> InsData( 0xC6, 2),
  ZeroPageX -> InsData( 0xD6, 2),
  Absolute -> InsData( 0xCE, 3),
  AbsoluteX -> InsData( 0xDE, 3))):
  override def name() = "DEC"


case class DEX() extends CpuInstruction(Map(Implied -> InsData( 0xCA, 1))):
  override def name() = "DEX"


case class DEY() extends CpuInstruction(Map(Implied -> InsData( 0x88, 1))):
  override def name() = "DEY"


case class EOR() extends CpuInstruction (Map(Immediate -> InsData( 0x49, 2),
  ZeroPage -> InsData( 0x45, 2),
  ZeroPageX -> InsData( 0x55, 2),
  Absolute -> InsData( 0x4D, 3),
  AbsoluteX -> InsData( 0x5D, 3),    // absolute,X
  AbsoluteY -> InsData( 0x59, 3),   // absolute,Y
  IndirectX -> InsData( 0x41, 2),  // (indirect,X)
  IndirectY -> InsData( 0x51, 2))): // (indirect),Y
  override def name() = "EOR"


case class INC() extends CpuInstruction(Map(ZeroPage -> InsData( 0xE6, 2),
  ZeroPageX -> InsData( 0xF6, 2),
  Absolute -> InsData( 0xEE, 3),
  AbsoluteX -> InsData( 0xFE, 3))):
  override def name() = "INC"


case class INX() extends CpuInstruction(Map(Implied -> InsData( 0xE8, 1))):
  override def name() = "INX"


case class INY() extends CpuInstruction(Map(Implied -> InsData( 0xC8, 1))):
  override def name() = "INY"


case class JMP() extends CpuInstruction(Map(Absolute -> InsData( 0x4C, 3),
  Indirect -> InsData( 0x6C,3))):
  override def name() = "JMP"


case class JSR() extends CpuInstruction(Map(Absolute -> InsData( 0x20, 3))):
  override def name() = "JSR"


case class LDA() extends CpuInstruction(Map(Immediate -> InsData( 0xA9, 2),
  ZeroPage -> InsData( 0xA5, 2),
  ZeroPageX -> InsData( 0xB5, 2),
  Absolute -> InsData( 0xAD, 3),
  AbsoluteX -> InsData( 0xBD, 3),    // absolute,X
  AbsoluteY -> InsData( 0xB9, 3),   // absolute,Y
  IndirectX -> InsData( 0xA1, 2),  // (indirect,X)
  IndirectY -> InsData( 0xB1, 2))): // (indirect),Y

  override def name() = "LDA"


case class LDX() extends CpuInstruction(Map(Immediate -> InsData( 0xA2, 2),
  ZeroPage -> InsData( 0xA6, 2),
  ZeroPageY -> InsData( 0xB6, 2),
  Absolute -> InsData( 0xAE, 3),
  AbsoluteY -> InsData( 0xBE, 3))):
  override def name() = "LDX"


case class LDY() extends CpuInstruction(Map(Immediate -> InsData( 0xA0, 2),
  ZeroPage -> InsData( 0xA4, 2),
  ZeroPageX -> InsData( 0xB4, 2),
  Absolute -> InsData( 0xAC, 3),
  AbsoluteX ->InsData( 0xBC, 3))):
  override def name() = "LDY"


case class LSR() extends CpuInstruction(Map(Accumulator -> InsData( 0x4A, 1),
  ZeroPage -> InsData( 0x46, 2),
  ZeroPageX -> InsData( 0x56, 2),
  Absolute -> InsData( 0x4E, 3),
  AbsoluteX -> InsData( 0x5E, 3))):
  override def name() = "LSR"


case class NOP() extends CpuInstruction(Map(Implied -> InsData( 0xEA, 1))):
  override def name() = "NOP"


case class ORA() extends CpuInstruction(Map(Immediate -> InsData( 0x09, 2),
  ZeroPage -> InsData( 0x05, 2),
  ZeroPageX -> InsData( 0x15, 2),
  Absolute -> InsData( 0x0D, 3),
  AbsoluteX -> InsData( 0x1D, 3),
  AbsoluteY -> InsData( 0x19, 3),
  IndirectX -> InsData( 0X01, 2),
  IndirectY->  InsData( 0x11, 2))):
  override def name() = "ORA"


case class PHA() extends CpuInstruction(Map(Implied -> InsData( 0x48, 1))):
  override def name() = "PHA"


case class PHP() extends CpuInstruction(Map(Implied -> InsData( 0x08, 1))):
  override def name() = "PHP"


case class PLA() extends CpuInstruction(Map(Implied -> InsData( 0x68, 1))):
  override def name() = "PLA"


case class PLP() extends CpuInstruction(Map(Implied -> InsData( 0x28, 1))):
  override def name() = "PLP"


case class ROL() extends CpuInstruction(Map(Accumulator -> InsData( 0x2A, 1),
  ZeroPage -> InsData( 0x26, 2),
  ZeroPageX -> InsData( 0x36, 2),
  Absolute -> InsData( 0x2E, 3),
  AbsoluteX -> InsData( 0x3E, 3))):
  override def name() = "ROL"


case class ROR() extends CpuInstruction(Map(Accumulator -> InsData( 0x6A, 1),
  ZeroPage -> InsData( 0x66, 2),
  ZeroPageX -> InsData( 0x76, 2),
  Absolute -> InsData( 0x6E, 3),
  AbsoluteX -> InsData( 0x7E, 3))):
  override def name() = "ROR"


case class RTI() extends CpuInstruction(Map(Implied -> InsData( 0x40, 1))):
  override def name() = "RTI"


case class RTS() extends CpuInstruction(Map(Implied -> InsData( 0x60, 1))):
  override def name() = "RTS"


case class SBC() extends CpuInstruction(Map(Immediate -> InsData( 0xE9, 2),
  ZeroPage -> InsData( 0xE5, 2),
  ZeroPageX -> InsData( 0xF5, 2),
  Absolute -> InsData( 0xED, 3),
  AbsoluteX -> InsData( 0xFD, 3),
  AbsoluteY -> InsData( 0xF9, 3),
  IndirectX-> InsData( 0xE1, 2),
  IndirectY-> InsData( 0xF1, 2))):
  override def name() = "SBC"

case class SEC() extends CpuInstruction(Map(Implied -> InsData( 0x38, 1))):
  override def name() = "SEC"


case class SED() extends CpuInstruction(Map(Implied -> InsData( 0xF8, 1))):
  override def name() = "SED"


case class SEI() extends CpuInstruction(Map(Implied -> InsData( 0x78, 1))):
  override def name() = "SEI"


case class STA() extends CpuInstruction(Map(ZeroPage -> InsData( 0x85, 2),
  ZeroPageX -> InsData( 0x95, 2),
  Absolute -> InsData( 0x8D, 3),
  AbsoluteX -> InsData( 0x9D, 3),
  AbsoluteY -> InsData( 0x99, 3),
  IndirectX-> InsData( 0x81, 2),
  IndirectY-> InsData( 0x91,  2))):
  override def name() = "STA"


case class STX() extends CpuInstruction(Map(ZeroPage -> InsData( 0x86, 2),
  ZeroPageY -> InsData( 0x96, 2),
  Absolute -> InsData( 0x8E, 3))):
  override def name() = "STX"


case class STY() extends CpuInstruction(Map(ZeroPage -> InsData( 0x84, 2),
  ZeroPageX -> InsData( 0x94, 2),
  Absolute -> InsData( 0x8C, 3))):
  override def name() = "STY"


case class TAX() extends CpuInstruction(Map(Implied -> InsData( 0xAA, 1))):
  override def name() = "TAX"


case class TAY() extends CpuInstruction(Map(Implied -> InsData( 0xA8, 1))):
  override def name() = "TAY"


case class TSX() extends CpuInstruction(Map(Implied -> InsData( 0xBA, 1))):
  override def name() = "TSX"


case class TXA() extends CpuInstruction(Map(Implied -> InsData( 0x8A, 1))):
  override def name() = "TXA"


case class TXS() extends CpuInstruction(Map(Implied -> InsData( 0x9A, 1))):
  override def name() = "TXS"


case class TYA() extends CpuInstruction(Map(Implied -> InsData( 0x98, 1))):
  override def name() = "TYA"

case object INVALID extends CpuInstruction(Map()):
  override def name() = "INVALID"

object CpuInstructions :
  val validInstructions: Seq[CpuInstruction] = LazyList(ADC(),AND(),ASL(),BCC(),BCS(),BEQ(),BIT(),BMI(),BNE(),BPL(),BRK(),BVC(),BVS(),CLC(),
    CLD(), CLI(),CLV(),CMP(),CPX(),CPY(),DEC(),DEX(),DEY(),EOR(),INC(),INX(),INY(),JMP(),JSR(),LDA(),LDX(),LDY(),LSR(),
    NOP(),ORA(),PHA(),PHP(),PLA(),PLP(),ROL(),ROR(),RTI(),RTS(),SBC(),SEC(),SED(),SEI(),STA(),STX(),STY(),TAX(),TAY(),
    TSX(),TXA(),TXS(),TYA())

  private def isEmpty(str: String) = str == null || str.trim.isEmpty
  private def isNotEmpty(str: String) = !isEmpty(str)
  def isValidInstruction(ins: String): Boolean =
    isNotEmpty(ins) && validInstructions.exists(a => a.name().equalsIgnoreCase(ins))

  def getInstruction(ins: String): Option[CpuInstruction] =
    validInstructions.find(a => a.name().equals(ins.toUpperCase()))

  def getInstructionOpcodeBytes(ins: String, adrMode: AddressingMode): Option[(Int, Int)] =
    validInstructions.find(a => a.name().equals(ins.toUpperCase())).map(instruction => (instruction.opcode(adrMode).get, instruction.bytes(adrMode).get))

  def isAddressingModeValid(ins: String, adrMode: AddressingMode): Boolean =
    val instruction = validInstructions.find(a => a.name().equals(ins.toUpperCase())).getOrElse(INVALID)
    instruction.code.contains(adrMode)
    