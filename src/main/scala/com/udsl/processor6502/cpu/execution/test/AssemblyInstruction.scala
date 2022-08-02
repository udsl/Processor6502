package com.udsl.processor6502.cpu.execution.test

import scala.language.implicitConversions

class InstructionSize( val bytes: Int )

object InstructionSize:
  def apply( bytes: Int ): InstructionSize =
    if bytes > 0 && bytes < 4 then new InstructionSize(bytes)
    else throw new Exception(s"Invalid value for InstructionSize, permitted range 1 - 3")

implicit def InstructionSizeToInt(insSize: InstructionSize): Int = insSize.bytes
implicit def IntToInstructionSize(bytes: Int): InstructionSize = InstructionSize(bytes)

sealed trait InstructionAddressingMode(val bytes: InstructionSize)

sealed trait InstructionOpcode( val addressingMode: InstructionAddressingMode, val mnemonic: String ):
  override def toString: String = s"$mnemonic - $addressingMode - size ${addressingMode.bytes} bytes"

object NULL extends InstructionOpcode(Unknown, "NULL"):
  override def toString: String = s"$mnemonic - not a real instruction zero bytes"


case class ADC(addressMode: InstructionAddressingMode) extends InstructionOpcode(addressMode, "ADC")



case object Accumulator extends InstructionAddressingMode(1):
  override def toString: String = "Accumulator"


case object Absolute extends InstructionAddressingMode(3):
  override def toString: String = "Absolute"

case object AbsoluteX extends InstructionAddressingMode(3):
  override def toString: String = "Absolute,X"

case object AbsoluteY extends InstructionAddressingMode(3):
  override def toString: String = "Absolute,Y"

case object Immediate extends InstructionAddressingMode(2):
  override def toString: String = "Immediate"

case object Implied extends InstructionAddressingMode(1):
  override def toString: String = "Implied"

case object Indirect extends InstructionAddressingMode(2):
  override def toString: String = "Indirect"

case object IndirectX extends InstructionAddressingMode(2):
  override def toString: String = "Indirect,X"

case object IndirectY extends InstructionAddressingMode(2):
  override def toString: String = "Indirect,Y"

case object Relative extends InstructionAddressingMode(2):
  override def toString: String = "Relative"

case object ZeroPage extends InstructionAddressingMode(2):
  override def toString: String = "ZeroPage"

case object ZeroPageX extends InstructionAddressingMode(2):
  override def toString: String = "ZeroPage,X"

case object ZeroPageY extends InstructionAddressingMode(2):
  override def toString: String = "ZeroPage,Y"

case object Invalid extends InstructionAddressingMode(1):
  override def toString: String = "invalid addressing mode"

case object Unknown extends InstructionAddressingMode(1):
  override def toString: String = "Unknown"

