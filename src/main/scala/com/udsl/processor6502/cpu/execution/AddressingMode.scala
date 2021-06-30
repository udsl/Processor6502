package com.udsl.processor6502.cpu.execution

class AddressMode( private val addMode: AddressingMode){
  def value: AddressingMode = { addMode }

}

object AddressMode{
  def apply(cc: Int, bbb: Int): AddressMode = {
    val mode: AddressingMode = {
      if (cc == 1) {
        bbb match {
          case 0 => ZeroPageX2
          case 1 => ZeroPage
          case 2 => Immediate
          case 3 => Absolute
          case 4 => ZeroPageY
          case 5 => ZeroPageX
          case 6 => AbsoluteY
          case 7 => AbsoluteX
        }
      }
      else if (cc == 2) {
        bbb match {
          case 0 => Immediate
          case 1 => ZeroPage
          case 2 => ZeroPage
          case 3 => Absolute
          case 4 => Invalid
          case 5 => ZeroPageX
          case 6 => Invalid
          case 7 => AbsoluteX
        }
      }
      else if (cc == 3) {
        Invalid
      }
      else {
        None
      }
    }
    new AddressMode(mode)
  }
}

sealed trait AddressingMode

  case object Invalid extends AddressingMode {
    override def toString: String = "invalid addressing mode"
  }

 case object None extends AddressingMode {
  override def toString: String = "NONE"
 }

case object Immediate extends AddressingMode {
  override def toString: String = "immediate"
}

case object ZeroPage extends AddressingMode {
  override def toString: String = "zero page"
}

case object Accumulator extends AddressingMode {
  override def toString: String = "accumulator"
}

case object Absolute extends AddressingMode {
  override def toString: String = "absolute"
}

case object ZeroPageX extends AddressingMode {
  override def toString: String = "zero page,X"
}

case object ZeroPageX2 extends AddressingMode {
  override def toString: String = "(zero page,X)"
}

case object AbsoluteX extends AddressingMode {
  override def toString: String = "absolute,X"
}

case object ZeroPageY extends AddressingMode {
  override def toString: String = "(zero page),Y"
}

case object AbsoluteY extends AddressingMode {
  override def toString: String = "absolute,Y"
}





