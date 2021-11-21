package com.udsl.processor6502.cpu

trait CpuInstruction():
  def name(): String = ""


case class ORA() extends CpuInstruction:
  override def name() = "ORA"
  
  
case class AND() extends CpuInstruction:
  override def name() = "AND"


case class EOR() extends CpuInstruction :
  override def name() = "EOR"

  
case class ADC() extends CpuInstruction:
  override def name() = "ADC"
  
  
case class STA() extends CpuInstruction:
  override def name() = "STA"

  
case class LDA() extends CpuInstruction:
  override def name() = "LDA"

  
case class CMP() extends CpuInstruction:
  override def name() = "CMP"

  
case class SBC() extends CpuInstruction:
  override def name() = "SBC"

  
case class ASL() extends CpuInstruction:
  override def name() = "ASL"

  
case class ROL() extends CpuInstruction:
  override def name() = "ROL"

  
case class LSR() extends CpuInstruction:
  override def name() = "LSR"

  
case class ROR() extends CpuInstruction:
  override def name() = "ROR"

  
case class STX() extends CpuInstruction:
  override def name() = "STX"

  
case class LDX() extends CpuInstruction:
  override def name() = "LDX"

  
case class DEC() extends CpuInstruction:
  override def name() = "DEC"

  
case class INC() extends CpuInstruction:
  override def name() = "INC"

  
case class BIT() extends CpuInstruction :
  override def name() = "BIT"

  
case class JMP() extends CpuInstruction:
  override def name() = "JMP"
  
  
case class STY() extends CpuInstruction:
  override def name() = "STY"
  
  
case class LDY() extends CpuInstruction:
  override def name() = "LDY"
  
  
case class CPY() extends CpuInstruction:
  override def name() = "CPY"
  
  
case class CPX() extends CpuInstruction:
  override def name() = "CPX"

object CpuInstructions :
  val validInstructions = LazyList(ORA(),AND(),EOR(),ADC(),STA(),LDA(),CMP(),SBC(),ASL(),ROL(),LSR(),ROR(),STX(),LDX(),DEC(),INC(),BIT(),JMP(),STY(),LDY(),CPY(),CPX())

  def isValidInstruction(ins: String): Boolean =
    !(ins == null || ins.isEmpty || validInstructions.filter(a => a.name().equals(ins.toUpperCase())).isEmpty)
  


  