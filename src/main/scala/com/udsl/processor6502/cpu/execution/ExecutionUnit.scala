package com.udsl.processor6502.cpu.execution

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.cpu.Processor
import com.udsl.processor6502.disassembler.Disassembler

/**
 * Most instructions that explicitly reference memory locations have bit patterns of the form aaabbbcc.
 * The aaa and cc bits determine the opcode, and the bbb bits determine the addressing mode.
 *
 * To make life easier lets break ou the aaa, bbb and cc values.
 *
 * see https://llx.com/Neil/a2/opcodes.html
 */

class ExecutionUnit extends StrictLogging:

  def singleStep(): Unit ={
    // get current instruction from PC location
    val i = Processor.getNextInstruction()
    val ins = Disassembler.fromPC()
    logger.info(s"fromPC $ins")

  }

  def decodeInstruction(instructionByte: Int): Unit ={

  }


object ExecutionUnit:
  def apply: ExecutionUnit =
    val eu = new ExecutionUnit()
    eu
