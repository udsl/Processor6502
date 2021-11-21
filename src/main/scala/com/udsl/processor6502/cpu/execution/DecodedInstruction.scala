package com.udsl.processor6502.cpu.execution:
  
  import com.udsl.processor6502.cpu.execution.DecodedInstruction.{aaaMask, bbbMask, ccMask}
  
  class DecodedInstruction(private val byt: Int, private val op: Opcode, private val mode: AddressMode) {
  
    def value: Int = {
      byt
    }
  
    // Addressing mode
    def addMode: AddressMode = {
      mode
    }
  
    def insLen: Int = {
      mode.value.bytes
    }
  
    // Instruction type
    def opcode: Opcode = {
      op
    }
  
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
  
  
  object DecodedInstruction {
    val aaaMask = Integer.parseInt("11100000", 2)
    val bbbMask = Integer.parseInt("00011100", 2)
    val ccMask = Integer.parseInt("00000011", 2)
    val groupMask = Integer.parseInt("00010000", 2)

    def apply(i: Int): DecodedInstruction = {
      println(s"i ${i.toHexString}")
      val a = aaa(i)
      val b = bbb(i)
      val c = cc(i)
      println(s"i ${i.toBinaryString}, a: ${a.toBinaryString} - $a, b: ${b.toBinaryString} - $b, c: ${c.toBinaryString} - $c")
      val ins_ = new DecodedInstruction(i, Opcode(c, b, a), AddressMode(c, b, a))
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
