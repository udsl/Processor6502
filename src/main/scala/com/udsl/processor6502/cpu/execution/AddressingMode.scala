package com.udsl.processor6502.cpu.execution

import scala.language.implicitConversions


case class InstructionSize( bytes: Int )

object InstructionSize:
  def apply( bytes: Int ): InstructionSize =
    if bytes > 0 && bytes < 4 then new InstructionSize(bytes)
    else throw new Exception(s"Invalid value for InstructionSize, permitted range 1 - 3")



sealed trait AddressingMode(val size: InstructionSize):
  implicit def InstructionSize2Int(s: InstructionSize): Int = s.bytes


case object Accumulator extends AddressingMode(InstructionSize(1)):
  override def toString: String = "Accumulator"

case object Absolute extends AddressingMode(InstructionSize(3)):
  override def toString: String = "Absolute"

case object AbsoluteX extends AddressingMode(InstructionSize(3)):
  override def toString: String = "Absolute,X"

case object AbsoluteY extends AddressingMode(InstructionSize(3)):
  override def toString: String = "Absolute,Y"

case object Immediate extends AddressingMode(InstructionSize(2)):
  override def toString: String = "Immediate"

case object Implied extends AddressingMode(InstructionSize(1)):
  override def toString: String = "Implied"

case object Indirect extends AddressingMode(InstructionSize(2)):
  override def toString: String = "Indirect"

case object IndirectX extends AddressingMode(InstructionSize(2)):
  override def toString: String = "Indirect,X"

case object IndirectY extends AddressingMode(InstructionSize(2)):
  override def toString: String = "Indirect,Y"

case object Relative extends AddressingMode(InstructionSize(2)):
  override def toString: String = "Relative"

case object ZeroPage extends AddressingMode(InstructionSize(2)):
  override def toString: String = "ZeroPage"

case object ZeroPageX extends AddressingMode(InstructionSize(2)):
  override def toString: String = "ZeroPage,X"

case object ZeroPageY extends AddressingMode(InstructionSize(2)):
  override def toString: String = "ZeroPage,Y"

case object Invalid extends AddressingMode(InstructionSize(1)):
  override def toString: String = "invalid addressing mode"

case object Unknown extends AddressingMode(InstructionSize(1)):
  override def toString: String = "Unknown"

case object NotApplicable extends AddressingMode(InstructionSize(0)):
  override def toString: String = "Not applicable"
