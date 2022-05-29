package com.udsl.processor6502.cpu.execution:

  import com.udsl.processor6502.cpu.execution

  sealed trait AddressingMode:
    def bytes: Int
  

  case object Accumulator extends AddressingMode {
    override def toString: String = "A accumulator"

    override def bytes: Int = 1
  }

  case object Absolute extends AddressingMode {
    override def toString: String = "$LLHH absolute"
    override def bytes: Int = 3
  }

  case object AbsoluteX extends AddressingMode {
    override def toString: String = "$LLHH,X AbsoluteX"
    override def bytes: Int = 3
  }

  case object AbsoluteY extends AddressingMode {
    override def toString: String = "$LLHH,Y AbsoluteY"
    override def bytes: Int = 3
  }

  case object Immediate extends AddressingMode {
    override def toString: String = "#$LL Immediate"
    override def bytes: Int = 2
  }

  case object Implied extends AddressingMode {
    override def toString: String = "implied"
    override def bytes: Int = 1
  }

  case object Indirect extends AddressingMode {
    override def toString: String = "($LLHH) Indirect"
    override def bytes: Int = 2
  }

  case object IndirectX extends AddressingMode {
    override def toString: String = "($LL,X) IndirectX"
    override def bytes: Int = 2
  }

  case object IndirectY extends AddressingMode {
    override def toString: String = "($LL),Y IndirectY"
    override def bytes: Int = 2
  }

  case object Relative extends AddressingMode {
    override def toString: String = "Relative"
    override def bytes: Int = 2
  }

  case object ZeroPage extends AddressingMode {
    override def toString: String = "$LL ZeroPage"
    override def bytes: Int = 2
  }

  case object ZeroPageX extends AddressingMode {
    override def toString: String = "$LL,X ZeroPageX"
    override def bytes: Int = 2
  }

  case object ZeroPageY extends AddressingMode {
    override def toString: String = "$LL,Y ZeroPageY"
    override def bytes: Int = 2
  }

  case object Invalid extends AddressingMode {
    override def toString: String = "invalid addressing mode"
    override def bytes: Int = 1
  }

  case object Unknown extends AddressingMode {
    override def toString: String = "NONE"
    override def bytes: Int = 0
  }

  case object NotApplicable extends AddressingMode {
    override def toString: String = "Not applicable"
    override def bytes: Int = 0
  }