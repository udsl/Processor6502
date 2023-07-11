package com.udsl.processor6502.assembler.version2

import com.udsl.processor6502.Utilities.numericValue
import com.udsl.processor6502.assembler.AssembleLocation

trait Assemble6502PassBaseV2:
  def setMemoryAddress(v: String): Unit =
    setMemoryAddress(numericValue(v).getOrElse( throw new RuntimeException(s"Invalid address $v")))

  def setMemoryAddress(v: Int): Unit =
    AssembleLocation.setMemoryAddress(v)
  
  def setMemoryByte(v: String): Unit =
    AssembleLocation.setMemoryByte(if v.charAt(0) == '$' then
      Integer.parseInt(v.substring(1), 16)
    else
      Integer.parseInt(v))

  def setMemoryByte(v: Int): Unit =
    AssembleLocation.setMemoryByte(v)

  def setMemoryByte(v: Int, disassembly: String): Unit =
    AssembleLocation.setMemoryByte(v, disassembly)

