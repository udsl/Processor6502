package com.udsl.processor6502.cpu.execution

import scala.language.implicitConversions


case class InstructionSize( bytes: Int )

object InstructionSize:
  def apply( bytes: Int ): InstructionSize =
    if bytes > 0 && bytes < 4 then new InstructionSize(bytes)
    else throw new Exception(s"Invalid value $bytes for InstructionSize (permitted range 1 - 3)")

  def apply(): InstructionSize =
    new InstructionSize(0)

sealed trait HasErrorText():
  val errorText: String

sealed trait AddressingMode(val mode: String, val size: InstructionSize):
  implicit def InstructionSize2Int(s: InstructionSize): Int = s.bytes
  override def toString: String = mode.toUpperCase()
case object Accumulator extends AddressingMode("Accumulator", InstructionSize(1))

case object Absolute extends AddressingMode("Absolute", InstructionSize(3))

case object AbsoluteX extends AddressingMode("Absolute,X", InstructionSize(3))

case object AbsoluteY extends AddressingMode("Absolute,Y", InstructionSize(3))

case object Immediate extends AddressingMode("Immediate", InstructionSize(2))

case object Implied extends AddressingMode("Implied", InstructionSize(1))

case object Indirect extends AddressingMode("Indirect", InstructionSize(3))

case object IndirectX extends AddressingMode("Indirect,X", InstructionSize(2))

case object IndirectY extends AddressingMode("Indirect,Y", InstructionSize(2))

case object Relative extends AddressingMode("Relative", InstructionSize(2))

case object ZeroPage extends AddressingMode("ZeroPage", InstructionSize(2))

case object ZeroPageX extends AddressingMode("ZeroPage,X", InstructionSize(2))

case object ZeroPageY extends AddressingMode("ZeroPage,Y", InstructionSize(2))

case object Invalid extends AddressingMode("Invalid", InstructionSize())

case object BadOperand extends AddressingMode("BadOperand", InstructionSize())

case object Unknown extends AddressingMode("Unknown", InstructionSize(1))

case class NotApplicable(errorText: String) extends AddressingMode("NotApplicable", InstructionSize(0)) with HasErrorText():
  override def toString: String = s"Not applicable: $errorText"

case class AddressingModeSyntaxError(errorText: String) extends AddressingMode("SyntaxError", InstructionSize(0)) with HasErrorText():
  override def toString: String = s"SyntaxError: $errorText"
