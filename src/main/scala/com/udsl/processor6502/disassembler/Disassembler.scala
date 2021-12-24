package com.udsl.processor6502.disassembler

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.NumericFormatType
import com.udsl.processor6502.Utilities.{numToByteString, numToString}
import com.udsl.processor6502.cpu.Processor
import com.udsl.processor6502.cpu.Processor.*
import com.udsl.processor6502.cpu.execution.Opcode


object Disassembler extends StrictLogging{
  def fromPC: Opcode =
    disassemble(getMemoryByte(pc.addr))

  def disassemble(opcodeByte: Int): Opcode =
    val disassembled = Opcode.disassemble(opcodeByte)
    logger.info(s"Disassembled ${numToByteString(opcodeByte, NumericFormatType.HEX )} $disassembled")
    disassembled
}
