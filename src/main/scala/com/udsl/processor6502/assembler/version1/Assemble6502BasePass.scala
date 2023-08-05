package com.udsl.processor6502.assembler.version1


import com.udsl.processor6502.Utilities.numericValue
import com.udsl.processor6502.assembler.*

trait Assemble6502BasePass:

  def setMemoryAddress(v: String): Unit =
    AssembleLocation.setMemoryAddress( numericValue(v).get)

  def setMemoryAddress(v: Int, withDisassembly: Boolean = false): Unit =
    AssembleLocation.setMemoryAddress(v, withDisassembly)

  def setMemoryByte(v: String): Unit =
    AssembleLocation.setMemoryByte(if v.charAt(0) == '$' then
      Integer.parseInt(v.substring(1), 16)
    else
      Integer.parseInt(v))

  def setMemoryByte(v: Int): Unit =
    AssembleLocation.setMemoryByte(v)

  def setMemoryByte(v: Int, disassembly: String): Unit =
    AssembleLocation.setMemoryByte(v, disassembly)

class Reference( val name: String):
  def hasValue: Boolean =
    Reference.hasValue(this)

  def value : Option[(Int, Boolean)] =
    Reference.getValue(this)


object Reference:
  def apply(name: String) : Reference =
    new Reference(name)

  def hasValue(instance: Reference): Boolean =
    AssemblyData.labels.contains(instance.name)

  def getValue(instance: Reference) : Option[(Int, Boolean)] =
    AssemblyData.labels.get(instance.name)
