package com.udsl.processor6502.cpu.execution

class Opcode( private val code: OpcodeValue) {
  def v: OpcodeValue = { code }

}

object Opcode{
  def apply(cc: Int, bbb: Int, aaa: Int): Opcode = {
    val code: OpcodeValue = {
      cc match {
        case 0 => zero(aaa)
        case 1 => one(aaa)
        case 2 => two(aaa)
        case _ => illegalOpcode
      }
    }
    new Opcode(code)
  }

  def zero(aaa: Int): OpcodeValue = {
    aaa match {
      case 0 => Illegal
      case 1 => BIT
      case 2 => JMP
      case 3 => JMPabs
      case 4 => STY
      case 5 => LDY
      case 6 => CPY
      case 7 => CPX
      case _ => Illegal
    }
  }

  def one(aaa: Int): OpcodeValue = {
    aaa match {
      case 0 => ORA
      case 1 => AND
      case 2 => EOR
      case 3 => ADC
      case 4 => STA
      case 5 => LDA
      case 6 => CMP
      case 7 => SBC
      case _ => Illegal
    }
  }

  def two(aaa: Int): OpcodeValue = {
    aaa match {
      case 0 => ASL
      case 1 => ROL
      case 2 => LSR
      case 3 => ROR
      case 4 => STX
      case 5 => LDX
      case 6 => DEC
      case 7 => INC
      case _ => Illegal
    }
  }

  def t2(): OpcodeValue = {
    NULL
  }

  def illegalOpcode: OpcodeValue ={
    println( "Found opcode with illegal bits 1 and 2")
    Illegal
  }
}

sealed trait OpcodeValue

case object NULL extends OpcodeValue {
  override def toString: String = "NULL"
}

case object Illegal extends OpcodeValue {
  override def toString: String = "Illegal"
}

case object ORA extends OpcodeValue {
  override def toString: String = "ORA"
}

case object AND extends OpcodeValue {
  override def toString: String = "AND"
}

case object EOR extends OpcodeValue {
  override def toString: String = "EOR"
}

case object ADC extends OpcodeValue {
  override def toString: String = "ADC"
}

case object STA extends OpcodeValue {
  override def toString: String = "STA"
}

case object LDA extends OpcodeValue {
  override def toString: String = "LDA"
}

case object CMP extends OpcodeValue {
  override def toString: String = "CMP"
}

case object SBC extends OpcodeValue {
  override def toString: String = "SBC"
}



case object ASL extends OpcodeValue {
  override def toString: String = "ASL"
}

case object ROL extends OpcodeValue {
  override def toString: String = "ROL"
}

case object LSR extends OpcodeValue {
  override def toString: String = "LSR"
}

case object ROR extends OpcodeValue {
  override def toString: String = "ROR"
}

case object STX extends OpcodeValue {
  override def toString: String = "STX"
}

case object LDX extends OpcodeValue {
  override def toString: String = "LDX"
}

case object DEC extends OpcodeValue {
  override def toString: String = "DEC"
}

case object INC extends OpcodeValue {
  override def toString: String = "INC"
}


case object BIT extends OpcodeValue {
  override def toString: String = "BIT"
}

case object JMP extends OpcodeValue {
  override def toString: String = "JMP"
}

case object JMPabs extends OpcodeValue {
  override def toString: String = "JMP abs"
}

case object STY extends OpcodeValue {
  override def toString: String = "STY"
}

case object LDY extends OpcodeValue {
  override def toString: String = "LDY"
}

case object CPY extends OpcodeValue {
  override def toString: String = "CPY"
}

case object CPX extends OpcodeValue {
  override def toString: String = "CPX"
}

