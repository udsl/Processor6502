package com.udsl.processor6502.cpu

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.cpu.EightBitRegister.MAX_VALUE


class StackPointer(name: String) extends EightBitRegister(name: String), StrictLogging {
  def value: Int = _ebr.value

  def value_= (value: Int): Unit =
    ebr = value
  
  def pushByte(byt: Int): Unit ={
    logger.info(s"Pushing byte $byt")
    // write byte to memory offset by sp value
    val addr = 256 + ebr
    StackPointer.memoryAccess.setMemoryByte(addr, byt)
    decrement()
  }

  def popByte(): Int ={
    increment()
    val addr = 255 + ebr
    val byt: Int = StackPointer.memoryAccess.getMemoryByte(addr)
    logger.info(s"Popping byte $byt")
    byt
  }

  def decrement(): Unit ={
    ebr -= 1
  }

  def increment(): Unit ={
    ebr += 1
  }
}


object StackPointer {
  val memoryAccess: Memory = Memory.apply

  def apply(): StackPointer ={
    val s_ = new StackPointer("Stack pointer")
    s_.ebr = MAX_VALUE
    s_
  }
}