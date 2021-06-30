package com.udsl.processor6502.CPU

import com.udsl.processor6502.CPU.EightBitRegister.MAX_VALUE


class StackPointer extends EightBitRegister {
  def pushByte(byt: Int): Unit ={
    println(s"Pushing byte $byt")
    // write byte to memory offset by sp value
    decrement()
  }

  def decrement(): Unit ={
    ebr -= 1
  }
}

object StackPointer {
  def apply(): StackPointer ={
    val s_ = new StackPointer
    s_.ebr = MAX_VALUE
    s_
  }
}