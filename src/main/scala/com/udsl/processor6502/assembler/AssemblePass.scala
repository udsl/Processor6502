package com.udsl.processor6502.assembler

import com.udsl.processor6502.Utilities.numericValue

trait AssemblePass {
  def setMemoryAddress(v: String): Unit =
    setMemoryAddress(numericValue(v).getOrElse( throw new RuntimeException(s"Invalid address $v")))

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

}
