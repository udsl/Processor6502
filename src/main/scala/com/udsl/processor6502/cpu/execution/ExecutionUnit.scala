package com.udsl.processor6502.cpu.execution

import com.udsl.processor6502.cpu.Processor

/**
 * Most instructions that explicitly reference memory locations have bit patterns of the form aaabbbcc.
 * The aaa and cc bits determine the opcode, and the bbb bits determine the addressing mode.
 *
 * To make life easier lets break ou the aaa, bbb and cc values.
 *
 * see https://llx.com/Neil/a2/opcodes.html
 */

class ExecutionUnit {

  def singleStep(): Unit ={
    // get current instruction from PC location
    val i = Processor.getNextInstruction()

//    val aaa = i.value & aaaMask

  }

  def decodeInstruction(instructionByte: Int): Unit ={

  }
}

object ExecutionUnit{
}