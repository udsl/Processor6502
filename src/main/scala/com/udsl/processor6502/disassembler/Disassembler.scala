package com.udsl.processor6502.disassembler

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.NumericFormatType
import com.udsl.processor6502.Utilities.{numToByteString, numToString, numToWordString}
import com.udsl.processor6502.cpu.{Memory, MemoryCell}
import com.udsl.processor6502.cpu.execution.*


object Disassembler extends StrictLogging:
  val memoryAccess = Memory.apply
  var disassemblyLocation: Int = 0

  def disassemble(opcodeByte: Int): Opcode =
    val disassembled = Opcode(opcodeByte)
    logger.info(s"Disassembled ${numToByteString(opcodeByte, NumericFormatType.HEX )} $disassembled")
    disassembled

  def disassemble() : Unit =
    var memCell: MemoryCell = Memory.getMemory(disassemblyLocation)

    while memCell.getValue != 0 do
      val opcode = disassemble(memCell.getValue)
      disassemblyLocation += 1
      val operand: String = opcode.addressingMode match
        case Accumulator => " A"
        case Implied  => " "
        case Immediate => s"#${numToByteString(memoryAccess.getMemoryByte(disassemblyLocation), NumericFormatType.HEX) }"
        case ZeroPage
             | Relative => s"${numToByteString(memoryAccess.getMemoryByte(disassemblyLocation), NumericFormatType.HEX) }"
        case ZeroPageX => s"${numToByteString(memoryAccess.getMemoryByte(disassemblyLocation), NumericFormatType.HEX)}, X"
        case ZeroPageY => s"${numToByteString(memoryAccess.getMemoryByte(disassemblyLocation), NumericFormatType.HEX)}, Y"
        case IndirectX => s"(${numToByteString(memoryAccess.getMemoryByte(disassemblyLocation), NumericFormatType.HEX)}, X)"
        case IndirectY => s"(${numToByteString(memoryAccess.getMemoryByte(disassemblyLocation), NumericFormatType.HEX)}), Y)"
        case Absolute => s"${numToWordString(memoryAccess.getMemoryAsAddress(disassemblyLocation), NumericFormatType.HEX)}"
        case Indirect => s"(${numToWordString(memoryAccess.getMemoryAsAddress(disassemblyLocation), NumericFormatType.HEX)})"
        case AbsoluteX => s"${numToWordString(memoryAccess.getMemoryAsAddress(disassemblyLocation), NumericFormatType.HEX)}, X"
        case AbsoluteY => s"${numToWordString(memoryAccess.getMemoryAsAddress(disassemblyLocation), NumericFormatType.HEX)}, Y"
        case Invalid | Unknown =>  "INVALID"
        case NotApplicable( errorText ) => errorText
        case AddressingModeSyntaxError( errorText )=> errorText

      memCell.getByte.setDisassembly(s"${opcode.mnemonic} $operand")
      disassemblyLocation += opcode.addressingMode.size.bytes -1
      memCell = Memory.getMemory(disassemblyLocation)


