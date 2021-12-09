package com.udsl.processor6502.cpu.execution:

  import com.udsl.processor6502.cpu.execution

  class AddressMode( private val addMode: AddressingMode){
    def value: AddressingMode = { addMode }
    override def toString: String = value.toString
  }


  object AddressMode{
    def apply(cc: Int, bbb: Int, aaa: Int): AddressMode = {
      val mode: AddressingMode = {
        if (cc == 0) { // 00 implied, relative, absolute, immediate, zero page, zero page X and absolut X
          bbb match {
            case 0 =>  // 000 -> implied, 001 -> absolute, 010- > implied, 011-> implied, 100 -> Invalid, 101 -> Immediate, 110 -> Immediate, 111 -> Immediate
             aaa match {
               case 0 => Implied
               case 1 => Absolute
               case 2 => Implied
               case 3 => Implied
               case i if 5 to 7 contains i => Immediate
               case _ => Invalid
             }
            case 1 =>    // 000 -> Invalid, 001 -> zero page, 010- > Invalid, 011-> implied, 100 ->  zero page, 101 ->  zero page, 110 ->  zero page, 111 ->  zero page
              aaa match {
                case 0 => Invalid
                case 1 => ZeroPage
                case 2 => Invalid
                case 3 => Implied
                case i if 4 until 7 contains i => ZeroPage
                case _ => Invalid
              }
            case 2 => Implied    // 000 -> Implied, 001 -> Implied, 010- > Implied, 011-> implied, 100 ->  Implied, 101 -> Implied, 110 -> Implied, 111 -> Implied
            case 3 =>    // 000 -> Invalid, 001 -> abs, 010- > abs, 011-> indirect, 100 ->  abs, 101 -> Implied, 110 -> abs, 111 -> abs
              aaa match {
                case 0 => Implied
                case i if 2 until 7 contains i => Absolute
                case _ => Invalid
              }
            case 4 => Relative    // 000 -> rel, 001 -> rel, 010- > rel, 011-> rel, 100 ->  rel, 101 -> rel, 110 -> rel, 111 -> rel

            case 5 =>             // 000 -> invalid, 001 -> invalid, 010- > invalid, 011-> , 100 ->  Zero page X, 101 -> Zero page X, 110 -> invalid, 111 -> invalid
              aaa match {
                case 4 => ZeroPageX
                case 5 => ZeroPageX
                case _ => Invalid
              }  // zero page X
            case 6 => Implied     // 000 -> Implied, 001 -> Implied, 010- > Implied, 011-> Implied, 100 ->  Implied, 101 -> Implied, 110 -> Implied, 111 -> Implied
            case 7 =>             // 000 -> Invalid, 001 -> Invalid, 010- > Invalid, 011-> Invalid, 100 ->  Invalid, 101 -> abs X, 110 -> Invalid, 111 -> Invalid
              aaa match{
                case 5 => AbsoluteX
                case _ => Invalid
              }
          }
        }
        else if (cc == 1) {
          bbb match {
            case 0 => IndirectX   // 000 -> ind X, 001 -> ind X, 010- > ind X, 011-> , 100 ->  ind X, 101 -> ind X, 110 -> ind X, 111 -> ind X
            case 1 => ZeroPage    // 000 -> zero page, 001 -> zero page, 010- > zero page, 011-> zero page, 100 ->  zero page, 101 -> zero page, 110 -> zero page, 111 -> zero page
            case 2 =>             // 000 -> Immediate, 001 -> Immediate, 010- Immediate> , 011-> Immediate, 100 ->  Invalid, 101 -> Immediate, 110 -> Immediate, 111 -> Immediate
              aaa match {
                case i if 0 to 3 contains i => Immediate
                case j if 5 to 7 contains j => Immediate
                case _ => Invalid
              }
            case 3 => Absolute     // 000 -> abs, 001 -> abs, 010- > abs, 011-> abs, 100 ->  abs, 101 -> abs, 110 -> abs, 111 ->abs
            case 4 => IndirectY    // 000 -> ind Y, 001 -> ind Y, 010- > ind Y, 011-> ind Y, 100 ->  ind Y, 101 -> ind Y, 110 -> ind Y, 111 -> ind Y
            case 5 => ZeroPageX    // 000 -> zp X, 001 -> zp X, 010- > zp X, 011-> zp X, 100 ->  zp X, 101 -> zp X, 110 -> zp X, 111 -> zp X
            case 6 => AbsoluteY    // 000 -> abs Y, 001 -> abs Y, 010- > abs Y, 011-> abs Y, 100 ->  abs Y, 101 -> abs Y, 110 -> abs Y, 111 -> abs Y
            case 7 => AbsoluteX    // 000 -> abs X, 001 -> abs X, 010- > abs X, 011-> abs X, 100 ->  abs X, 101 -> abs X, 110 -> abs X, 111 -> abs X
          }
        }
        else if (cc == 2) { // 10
          bbb match {
            case 0 =>     // 000 -> Invalid, 001 -> Invalid, 010- > Invalid, 011-> Invalid, 100 ->  Invalid, 101 -> imeadiate, 110 -> Invalid, 111 -> Invalid
              aaa match {
                case 5 => Immediate
                case _ => Invalid
              }
            case 1 => ZeroPage     // 000 -> zp, 001 -> zp, 010- > zp, 011-> zp, 100 ->  zp, 101 -> zp, 110 -> zp, 111 -> zp
            case 2 =>       // 000 -> acc, 001 -> acc, 010- > acc, 011-> acc, 100 ->  impl, 101 -> impl, 110 -> impl, 111 -> impl
              aaa match {
                case i if 0 to 3 contains i => Accumulator
                case i if 4 to 7 contains i => Implied
                case _ => Invalid
              }

            case 3 => Absolute     // 000 -> abs, 001 -> abs, 010- > abs, 011-> abs, 100 ->  abs, 101 -> abs, 110 -> abs, 111 -> abs
            case 4 => Invalid      // 000 -> Invalid, 001 -> Invalid, 010- > Invalid, 011-> Invalid, 100 ->  Invalid, 101 -> Invalid, 110 -> Invalid, 111 -> Invalid
            case 5 =>     // 000 -> zp X, 001 -> zp X, 010- > zp X, 011-> zp X, 100 -> zp X, 101 -> zp Y, 110 -> zp X, 111 -> zp X
              aaa match {
                case i if 0 to 4 contains i => ZeroPageX
                case i if 5 to 6 contains i => ZeroPageY
                case 7 => ZeroPageX
                case _ => Invalid
              }

            case 6 =>     // 000 -> Invalid, 001 -> Invalid, 010- > Invalid, 011-> Invalid, 100 ->  Impl, 101 -> Impl, 110 -> Invalid, 111 -> Invalid
              aaa match {
                case i if 4 to 5 contains i => ZeroPageX
                case _ => Invalid
              }

            case 7 =>     // 000 -> abs X, 001 ->  abs X, 010- >  abs X, 011->  abs X, 100 -> invalid, 101 -> abs Y, 110 -> , 111 ->
              aaa match {
                case i if ((0 to 3 contains i) || (6 to 7 contains i)) => AbsoluteX
                case 5  => AbsoluteY
                case _ => Invalid
              }
          }
        }
        else if (cc == 3) { // 11
          Invalid
        }
        else {
          Invalid
        }
      }
      new AddressMode(mode)
    }
  }

  sealed trait AddressingMode{
    def bytes: Int
  }

  case object Accumulator extends AddressingMode {
    override def toString: String = "accumulator"

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

  case object None extends AddressingMode {
    override def toString: String = "NONE"
    override def bytes: Int = 0
  }