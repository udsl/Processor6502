package com.udsl.processor6502.cpu.execution

sealed trait AddressingMode(val bytes: Int)

case object Accumulator extends AddressingMode(1):
  override def toString: String = "Accumulator"

case object Absolute extends AddressingMode(3):
  override def toString: String = "Absolute"

case object AbsoluteX extends AddressingMode(3):
  override def toString: String = "Absolute,X"

case object AbsoluteY extends AddressingMode(3):
  override def toString: String = "Absolute,Y"

case object Immediate extends AddressingMode(2):
  override def toString: String = "Immediate"

case object Implied extends AddressingMode(1):
  override def toString: String = "Implied"

case object Indirect extends AddressingMode(2):
  override def toString: String = "Indirect"

case object IndirectX extends AddressingMode(2):
  override def toString: String = "Indirect,X"

case object IndirectY extends AddressingMode(2):
  override def toString: String = "Indirect,Y"

case object Relative extends AddressingMode(2):
  override def toString: String = "Relative"

case object ZeroPage extends AddressingMode(2):
  override def toString: String = "ZeroPage"

case object ZeroPageX extends AddressingMode(2):
  override def toString: String = "ZeroPage,X"

case object ZeroPageY extends AddressingMode(2):
  override def toString: String = "ZeroPage,Y"

case object Invalid extends AddressingMode(1):
  override def toString: String = "invalid addressing mode"

case object Unknown extends AddressingMode(1):
  override def toString: String = "Unknown"

case object NotApplicable extends AddressingMode(0):
  override def toString: String = "Not applicable"
