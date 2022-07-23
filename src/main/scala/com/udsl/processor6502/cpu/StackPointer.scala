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
    val addr = 0x100 + ebr
    StackPointer.memoryAccess.setMemoryByte(addr, byt)
    decrement()
  }

  def pushAddress(address: Int): Unit ={
    logger.info(s"Pushing address $address")
    pushByte(address & 255) // push the low byte
    pushByte((address >> 8) & 255) // push the hi byte
  }

  def popAddress(address: Int): Int ={
    logger.info(s"Popping address")
    val hiByte = popByte()
    val loByte = popByte()
    (hiByte * 256) + loByte
  }

  def popByte(): Int ={
    increment()
    val addr = 0x100 + ebr
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