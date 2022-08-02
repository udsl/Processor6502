package com.udsl.processor6502.cpu.execution:

  import com.typesafe.scalalogging.StrictLogging
  import com.udsl.processor6502.cpu.StatusFlag
  import com.udsl.processor6502.cpu.StatusFlag.*

  class Opcode( private val code: OpcodeValue) extends StrictLogging:
    def v: OpcodeValue = { code }
    override def toString: String = code.toString

    override def equals(obj: Any): Boolean =
      val x = obj.asInstanceOf[OpcodeValue]
      x.mnemonic == code.mnemonic && x.addressMode == code.addressMode

  
  object Opcode extends StrictLogging :
    val aaaMask: Int = Integer.parseInt("11100000", 2)
    val bbbMask: Int = Integer.parseInt("00011100", 2)
    val ccMask: Int = Integer.parseInt("00000011", 2)
    val groupMask: Int = Integer.parseInt("00010000", 2)
      
    def disassemble(opcode: Int): Opcode =
      val code: OpcodeValue =
        cc(opcode) match
          case 0 => c0(aaa(opcode), bbb(opcode))
          case 1 => c1(aaa(opcode), bbb(opcode))
          case 2 => c2(aaa(opcode), bbb(opcode))
          case _ => illegalOpcode
      new Opcode(code)
    
    def cc(i: Int): Int =
      i & ccMask

    def bbb(i: Int): Int =
      (i & bbbMask) >> 2

    def aaa(i: Int): Int =
      (i & aaaMask) >> 5

    def c0(aaa: Int, bbb: Int): OpcodeValue =
      aaa match
        case 0 => c0a0(bbb)
        case 1 => c0a1(bbb)
        case 2 => c0a2(bbb)
        case 3 => c0a3(bbb)
        case 4 => c0a4(bbb)
        case 5 => c0a5(bbb)
        case 6 => c0a6(bbb)
        case 7 => c0a7(bbb)
        case _ => Illegal(NotApplicable)

    def c0a0(bbb: Int): OpcodeValue =
      bbb match
        case 0 => BRK(Implied)
        case 2 => PHP(Implied)
        case 4 => BPL(Relative)
        case 6 => CLC(Implied)
        case _ => Illegal(NotApplicable)

    def c0a1(bbb: Int): OpcodeValue =
      bbb match
        case 0 => JSR(Absolute)
        case 1 => BIT(ZeroPage)
        case 2 => PLP(Implied)
        case 3 => BIT(Absolute)
        case 4 => BMI(Relative)
        case 6 => SEC(Implied)
        case _ => Illegal(NotApplicable)

    def c0a2(bbb: Int): OpcodeValue =
      bbb match
        case 0 => RTI(Implied)
        case 2 => PHA(Implied)
        case 3 => JMP(Absolute)
        case 4 => BVC(Relative)
        case 6 => CLI(Implied)
        case _ => Illegal(NotApplicable)

    def c0a3(bbb: Int): OpcodeValue =
      bbb match
        case 0 => RTS(Absolute)
        case 2 => PLA(Implied)
        case 3 => JMP(Indirect)
        case 4 => BVS(Relative)
        case 6 => SEI(Implied)
        case _ => Illegal(NotApplicable)

    def c0a4(bbb: Int): OpcodeValue =
      bbb match
        case 1 => STY(ZeroPage)
        case 2 => DEY(Implied)
        case 3 => STY(Absolute)
        case 4 => BCC(Relative)
        case 5 => STY(ZeroPageX)
        case 6 => TYA(Implied)
        case _ => Illegal(NotApplicable)

    def c0a5(bbb: Int): OpcodeValue =
      bbb match
        case 0 => LDY(Immediate)
        case 1 => LDY(ZeroPage)
        case 2 => TAY(Implied)
        case 3 => LDY(Absolute)
        case 4 => BCS(Relative)
        case 5 => LDY(ZeroPageX)
        case 6 => CLV(Implied)
        case 7 => LDY(AbsoluteX)
        case _ => Illegal(NotApplicable)

    def c0a6(bbb: Int): OpcodeValue =
      bbb match
        case 0 => CPY(Immediate)
        case 1 => CPY(ZeroPage)
        case 2 => INY(Implied)
        case 3 => CPY(Absolute)
        case 4 => BNE(Relative)
        case 6 => CLD(Implied)
        case _ => Illegal(NotApplicable)

    def c0a7(bbb: Int): OpcodeValue =
      bbb match
        case 0 => CPX(Immediate)
        case 1 => CPX(ZeroPage)
        case 2 => INX(Implied)
        case 3 => CPX(Absolute)
        case 4 => BEQ(Relative)
        case 6 => SED(Implied)
        case _ => Illegal(NotApplicable)

    def c1(aaa: Int, bbb: Int): OpcodeValue =
      aaa match
        case 0 => c1a0(bbb)
        case 1 => c1a1(bbb)
        case 2 => c1a2(bbb)
        case 3 => c1a3(bbb)
        case 4 => c1a4(bbb)
        case 5 => c1a5(bbb)
        case 6 => c1a6(bbb)
        case 7 => c1a7(bbb)
        case _ => Illegal(NotApplicable)


    def c1a0(bbb: Int): OpcodeValue =
      bbb match
        case 0 => ORA(IndirectX)
        case 1 => ORA(ZeroPage)
        case 2 => ORA(Immediate)
        case 3 => ORA(Absolute)
        case 4 => ORA(IndirectY)
        case 5 => ORA(ZeroPageX)
        case 6 => ORA(AbsoluteY)
        case 7 => ORA(AbsoluteX)
        case _ => Illegal(NotApplicable)

    def c1a1(bbb: Int): OpcodeValue =
      bbb match
        case 0 => AND(IndirectX)
        case 1 => AND(ZeroPage)
        case 2 => AND(Immediate)
        case 3 => AND(Absolute)
        case 4 => AND(IndirectY)
        case 5 => AND(ZeroPageX)
        case 6 => AND(AbsoluteY)
        case 7 => AND(AbsoluteX)
        case _ => Illegal(NotApplicable)

    def c1a2(bbb: Int): OpcodeValue =
      bbb match
        case 0 => EOR(IndirectX)
        case 1 => EOR(ZeroPage)
        case 2 => EOR(Immediate)
        case 3 => EOR(Absolute)
        case 4 => EOR(IndirectY)
        case 5 => EOR(ZeroPageX)
        case 6 => EOR(AbsoluteY)
        case 7 => EOR(AbsoluteX)
        case _ => Illegal(NotApplicable)

    def c1a3(bbb: Int): OpcodeValue =
      bbb match
        case 0 => ADC(IndirectX)
        case 1 => ADC(ZeroPage)
        case 2 => ADC(Immediate)
        case 3 => ADC(Absolute)
        case 4 => ADC(IndirectY)
        case 5 => ADC(ZeroPageX)
        case 6 => ADC(AbsoluteY)
        case 7 => ADC(AbsoluteX)
        case _ => Illegal(NotApplicable)

    def c1a4(bbb: Int): OpcodeValue =
      bbb match
        case 0 => STA(IndirectX)
        case 1 => STA(ZeroPage)
        case 3 => STA(Absolute)
        case 4 => STA(IndirectY)
        case 5 => STA(ZeroPageX)
        case 6 => STA(AbsoluteY)
        case 7 => STA(AbsoluteX)
        case _ => Illegal(NotApplicable)

    def c1a5(bbb: Int): OpcodeValue =
      bbb match
        case 0 => LDA(IndirectX)
        case 1 => LDA(ZeroPage)
        case 2 => LDA(Immediate)
        case 3 => LDA(Absolute)
        case 4 => LDA(IndirectY)
        case 5 => LDA(ZeroPageX)
        case 6 => LDA(AbsoluteY)
        case 7 => LDA(AbsoluteX)
        case _ => Illegal(NotApplicable)

    def c1a6(bbb: Int): OpcodeValue =
      bbb match
        case 0 => CMP(IndirectX)
        case 1 => CMP(ZeroPage)
        case 2 => CMP(Immediate)
        case 3 => CMP(Absolute)
        case 4 => CMP(IndirectY)
        case 5 => CMP(ZeroPageX)
        case 6 => CMP(AbsoluteY)
        case 7 => CMP(AbsoluteX)
        case _ => Illegal(NotApplicable)

    def c1a7(bbb: Int): OpcodeValue =
      bbb match
        case 0 => SBC(IndirectX)
        case 1 => SBC(ZeroPage)
        case 2 => SBC(Immediate)
        case 3 => SBC(Absolute)
        case 4 => SBC(IndirectY)
        case 5 => SBC(ZeroPageX)
        case 6 => SBC(AbsoluteY)
        case 7 => SBC(AbsoluteX)
        case _ => Illegal(NotApplicable)


    def c2(aaa: Int, bbb: Int): OpcodeValue =
      aaa match
        case 0 => c2a0(bbb)
        case 1 => c2a1(bbb)
        case 2 => c2a2(bbb)
        case 3 => c2a3(bbb)
        case 4 => c2a4(bbb)
        case 5 => c2a5(bbb)
        case 6 => c2a6(bbb)
        case 7 => c2a7(bbb)
        case _ => Illegal(NotApplicable)


    def c2a0(bbb: Int): OpcodeValue =
      bbb match
        case 1 => ASL(ZeroPage)
        case 2 => ASL(Accumulator)
        case 3 => ASL(Absolute)
        case 5 => ASL(ZeroPageX)
        case 7 => ASL(AbsoluteX)
        case _ => Illegal(NotApplicable)

    def c2a1(bbb: Int): OpcodeValue =
      bbb match
        case 1 => ROL(ZeroPage)
        case 2 => ROL(Accumulator)
        case 3 => ROL(Absolute)
        case 5 => ROL(ZeroPageX)
        case 7 => ROL(AbsoluteX)
        case _ => Illegal(NotApplicable)
          
    def c2a2(bbb: Int): OpcodeValue =
      bbb match
        case 1 => LSR(ZeroPage)
        case 2 => LSR(Accumulator)
        case 3 => LSR(Absolute)
        case 5 => LSR(ZeroPageX)
        case 7 => LSR(AbsoluteX)
        case _ => Illegal(NotApplicable)

    def c2a3(bbb: Int): OpcodeValue =
      bbb match
        case 1 => ROR(ZeroPage)
        case 2 => ROR(Accumulator)
        case 3 => ROR(Absolute)
        case 5 => ROR(ZeroPageX)
        case 7 => ROR(AbsoluteX)
        case _ => Illegal(NotApplicable)

    def c2a4(bbb: Int): OpcodeValue =
      bbb match
        case 1 => STX(ZeroPage)
        case 2 => TXA(Implied)
        case 3 => STX(Absolute)
        case 5 => STX(ZeroPageY)
        case 6 => TXS(Implied)
        case _ => Illegal(NotApplicable)

    def c2a5(bbb: Int): OpcodeValue =
      bbb match
        case 0 => LDX(Immediate)
        case 1 => LDX(ZeroPage)
        case 2 => TAX(Implied)
        case 3 => LDX(Absolute)
        case 5 => LDX(ZeroPageY)
        case 6 => TSX(Implied)
        case 7 => LDX(AbsoluteY)
        case _ => Illegal(NotApplicable)

    def c2a6(bbb: Int): OpcodeValue =
      bbb match
        case 1 => DEC(ZeroPage)
        case 2 => DEX(Implied)
        case 3 => DEC(Absolute)
        case 5 => DEC(ZeroPageX)
        case 7 => DEC(AbsoluteX)
        case _ => Illegal(NotApplicable)

    def c2a7(bbb: Int): OpcodeValue =
      bbb match
        case 1 => INC(ZeroPage)
        case 2 => NOP(Implied)
        case 3 => INC(Absolute)
        case 5 => INC(ZeroPageX)
        case 7 => INC(AbsoluteX)
        case _ => Illegal(NotApplicable)

    def t2(): OpcodeValue =
      NULL(Invalid)

    def illegalOpcode: OpcodeValue =
      logger.info( "Found opcode with illegal bits 1 and 2")
      Illegal(NotApplicable)

  
  sealed trait OpcodeValue:
    val addressMode: AddressingMode
    def mnemonic: String  = ""
    override def toString: String = s"$mnemonic - $addressMode"

  case class NULL(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "NULL"
  
  case class Illegal(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "Undefined instruction"
  
  case class ORA(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "ORA"

  case class JSR(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "JSR"

  case class PLP(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "PLP"

  case class BMI(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "BMI"

  case class SEC(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "SEC"

  case class AND(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "AND"
  
  case class EOR(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "EOR"
  
  case class ADC(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "ADC"
  
  case class STA(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "STA"
  
  case class LDA(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "LDA"
  
  case class CMP(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "CMP"
  
  case class SBC(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "SBC"
  
  case class ASL(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "ASL"
  
  case class ROL(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "ROL"
  
  case class LSR(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "LSR"
  
  case class ROR(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "ROR"
  
  case class STX(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "STX"
  
  case class LDX(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "LDX"
  
  case class DEC(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "DEC"
  
  case class INC(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "INC"

  /**
   * bits 7 and 6 of operand are transfered to bit 7 and 6 of SR (N,V);
   * the zero-flag is set to the result of operand AND accumulator.
   * @param addressMode the mode of addressing a trait give the size of the instruction in bytes.
   */
  case class BIT(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "BIT"

  case class BRK(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "BRK"

  case class PHP(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "PHP"

  case class BPL(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "BPL"

  case class CLC(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "CLC"

  case class JMP(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "JMP"
  
  case class STY(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "STY"
  
  case class LDY(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "LDY"
  
  case class CPY(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "CPY"
  
  case class CPX(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "CPX"

  /**
   * The status register is pulled with the break flag
   * and bit 5 ignored. Then PC is pulled from the stack.
   * @param addressMode the mode of addressing a trait give the size of the instruction in bytes.
   */
  case class RTI(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "RTI"

  case class PHA(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "PHA"

  case class BVC(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "BVC"

  case class CLI(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "CLI"

  case class RTS(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "RTS"

  case class PLA(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "PLA"

  case class BVS(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "BVS"

  case class SEI(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "SEI"

  case class DEY(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "DEY"

  case class BCC(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "BCC"

  case class TAY(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "TAY"

  case class TYA(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "TYA"

  case class INY(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "INY"

  case class BNE(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "BNE"

  case class CLD(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "CLD"

  case class INX(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "INX"

  case class BEQ(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "BEQ"

  case class BCS(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "BCS"

  case class CLV(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "CLV"

  case class SED(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "SED"

  case class TXA(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "TXA"

  case class TXS(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "TXS"

  case class TAX(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "TAX"

  case class TSX(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "TSX"

  case class DEX(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "DEX"

  case class NOP(addressMode: AddressingMode) extends OpcodeValue:
    override def mnemonic: String = "NOP"

