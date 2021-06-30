package com.udsl.processor6502.cpu.execution

import com.udsl.processor6502.cpu.execution.Instruction.{aaaMask, bbbMask, ccMask}

class Instruction( private val byt: Int, private val op: Opcode, private val mode: AddressMode){

  def value: Int = { byt }

  // Addressing mode
  def addMode: AddressMode = { mode }

  // Instruction type
  def opcode: Opcode = { op }

  def getAAA: Int = {
    value & aaaMask
  }

  def getBBB: Int = {
    value & bbbMask
  }

  def getCC: Int = {
    value & ccMask
  }
}


object Instruction{
  val aaaMask = Integer.parseInt("11100000", 2)
  val bbbMask = Integer.parseInt("00011100", 2)
  val ccMask = Integer.parseInt("00000011", 2)

  def apply(i: Int): Instruction = {
    val a = aaa(i)
    val b = bbb(i)
    val c = cc(i)
    val ins_ = new Instruction(i, Opcode(c, b, a), AddressMode(c, b))
    ins_
  }

  def cc(i: Int): Int = {
    i & ccMask
  }

  def bbb(i: Int): Int = {
    (i & bbbMask) >> 2
  }

  def aaa(i: Int): Int = {
    (i & aaaMask) >> 5
  }
}
