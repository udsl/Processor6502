package com.udsl.processor6502.disassembler

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Utilities.numToString
import com.udsl.processor6502.cpu.Processor
import com.udsl.processor6502.cpu.Processor.*
import com.udsl.processor6502.cpu.execution.Opcode


object Disassembler extends StrictLogging{
  def fromPC(): Opcode =
    disassemble(pc.addr)

  def disassemble(locatoion: Int): Opcode =
    val ins = getMemoryByte(locatoion)
    val disassembled = Opcode.disassemble(ins)
    logger.info(s"Decoded instruction $disassembled")
    disassembled
}
