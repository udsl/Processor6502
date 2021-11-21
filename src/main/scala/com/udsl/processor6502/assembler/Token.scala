package com.udsl.processor6502.assembler

class Token( val typeOfToken: AssemblerTokenType, val tokenStr: String, var tokenVal: TokenValue) {
  def intValue : Int =
      return tokenVal.asIntValue

  def setValue(v: String) =
    tokenVal = TokenValue(v)

  def setValue(v: TokenValue) =
    tokenVal = v

  def str : String =
    tokenStr

  override def toString =
    s"TokenType: ${typeOfToken}, Value: ${tokenStr} "

}

object Token {
  def apply(typeOfToken: AssemblerTokenType) : Token =
    new Token(typeOfToken, "", TokenValue(""))

  def apply(typeOfToken: AssemblerTokenType, tokenStr: String) : Token =
    new Token(typeOfToken, tokenStr, TokenValue(""))

  def apply(typeOfToken: AssemblerTokenType, tokenStr: String, tokenVal: TokenValue) : Token =
    new Token(typeOfToken, tokenStr, tokenVal)
}

class TokenValue(var pridictedMode: PredictedAddressingModes, val strVal: String):
  def asIntValue: Int =
    Integer.parseInt(strVal)


object TokenValue:
  def apply =
    new TokenValue(PredictedAddressingModes.NotApplicable, " ")

  def apply(strVal: Array[String]) =
    new TokenValue(PredictedAddressingModes.NoPricitions, strVal.mkString(" "))

  def apply(strVal: String) =
    new TokenValue(PredictedAddressingModes.NoPricitions, strVal)

  def apply(pridictedMode: PredictedAddressingModes, strVal: Array[String]) =
    new TokenValue(pridictedMode, strVal.mkString(" "))

  def apply(pridictedMode: PredictedAddressingModes, strVal: String) =
    new TokenValue(pridictedMode, strVal)


enum AddressingModes(val faceValue: String):
//  The Accumulator is implied as the operand, so no address needs to be specified.
//  Using the ASL (Arithmetic Shift Left) instruction with no operands, the Accumulator is always the value being shifted left.
  case Accumulator extends AddressingModes("Accumulator")
//  The operand is implied, so it does not need to be specified.
//  The operands being implied TXA - X, the source of the transfer, and A, the destination of the transfer.
  case Implied extends AddressingModes("Implied")
//  The operand is used directly to perform the computation.
//  LDA #$22 - $22 is loaded into the Accumulator.
  case Immediate extends AddressingModes("Immediate #")
//  A full 16-bit address is specified and the byte at that address is used to perform the computation.
//  LDX $D010 - the value at address $D010 is loaded into the X register.
  case Absolute extends AddressingModes("Absolute")
//  single byte specifies an address in the first page of memory ($00xx), also known as the zero page, and the byte at that address is used to perform the computation.
//  LDY $02 - the value at address $0002 is loaded into the Y register.
  case ZeroPage extends AddressingModes("ZeroPage")
//  The offset specified is added to the current address stored in the Program Counter (PC). Offsets can range from -128 to +127.
//  BPL $2D - the offset $2D is added to the address in the Program Counter (say $C100). The destination of the branch (if taken) will be $C12D.
  case Relative extends AddressingModes("Relative")
//  The little-endian two-byte value stored at the specified address is used to perform the computation. Only used by the JMP instruction.
//  JMP ($A001) - the addresses $A001 and $A002 are read, returning $FF and $00 respectively. Then address $00FF is then jumped to.
  case AbsoluteIndirect extends AddressingModes("(Absolute)")
//  The value in X is added to the specified address for a sum address. The value at the sum address is used to perform the computation.
//  ADC $C001,X - the value $02 in X is added to $C001 for a sum of $C003. The value $5A at address $C003 is used to perform the add with carry (ADC) operation.
  case AbsoluteIndexedX extends AddressingModes("Absolute,X")
//  The value in Y is added to the specified address for a sum address. The value at the sum address is used to perform the computation.
//  INC $F001,Y - the value $03 in Y is added to $F001 for a sum of $F004. The value $EF at address $F004 is incremented (INC) and $F0 is written back to $F004.
  case AbsoluteIndexedY extends AddressingModes("Absolute,Y")
//  The value in X is added to the specified zero page address for a sum address. The value at the sum address is used to perform the computation.
//  LDA $01,X - the value $02 in X is added to $01 for a sum of $03. The value $A5 at address $0003 is loaded into the Accumulator.
  case ZeroPageX extends AddressingModes("ZeroPage,X")
//  The value in Y is added to the specified zero page address for a sum address. The value at the sum address is used to perform the computation.
//  LDA $01,Y - the value $03 in Y is added to $01 for a sum of $04. The value $E3 at address $0004 is loaded into the Accumulator.
  case ZeroPageY extends AddressingModes("ZeroPage,Y")
//  The value in X is added to the specified zero page address for a sum address. The little-endian address stored at the two-byte pair of sum address (LSB) and sum address plus one (MSB) is loaded and the value at that address is used to perform the computation.
//  STA ($15,X) - the value $02 in X is added to $15 for a sum of $17. The address $D010 at addresses $0017 and $0018 will be where the value $0F in the Accumulator is stored.
  case ZeroPageIndirectX extends AddressingModes("(ZeroPage,X)")
//  The value in Y is added to the address at the little-endian address stored at the two-byte pair of the specified address (LSB) and the specified address plus one (MSB). The value at the sum address is used to perform the computation. Indeed addressing mode actually repeats exactly the Accumulator register's digits.
//  EOR ($2A),Y - the value $03 in Y is added to the address $C235 at addresses $002A and $002B for a sum of $C238. The Accumulator is then exclusive ORed with the value $2F at $C238.
  case ZeroPageIndirectY extends AddressingModes("(ZeroPage),Y")


enum PredictedAddressingModes( val modes: List[AddressingModes]):
  case AccumulatorOrImplied extends PredictedAddressingModes(List(AddressingModes.Accumulator, AddressingModes.Implied))
  case Immediate extends PredictedAddressingModes(List(AddressingModes.Immediate))
  case Absolute extends PredictedAddressingModes(List(AddressingModes.Absolute))
  case ZeroPage extends PredictedAddressingModes(List(AddressingModes.ZeroPage))
  case AbsoluteOrZeroPage extends PredictedAddressingModes(List(AddressingModes.Absolute, AddressingModes.ZeroPage))
  case AbsoluteXOrZeroPageX extends PredictedAddressingModes(List(AddressingModes.AbsoluteIndexedX, AddressingModes.ZeroPageX))
  case AbsoluteYOrZeroPageY extends PredictedAddressingModes(List(AddressingModes.AbsoluteIndexedY, AddressingModes.ZeroPageY))
  case Relative extends PredictedAddressingModes(List(AddressingModes.Relative))
  case ZeroPageIndirectX extends PredictedAddressingModes(List(AddressingModes.ZeroPageIndirectX))
  case ZeroPageIndirectY extends PredictedAddressingModes(List(AddressingModes.ZeroPageIndirectY))
  case AbsoluteIndirect extends PredictedAddressingModes(List(AddressingModes.AbsoluteIndirect))
  case NoPricitions extends PredictedAddressingModes(List.empty)
  case NotApplicable extends PredictedAddressingModes(List.empty)

enum AssemblerTokenType(val desc: String):
  case BlankLineToken extends AssemblerTokenType("BlankLineToken")
  case CommentLineToken extends AssemblerTokenType("CommentLineToken")
  case LineComment extends AssemblerTokenType("LineComment")
  case NoneCommentLine extends AssemblerTokenType("NoneCommentLine")
  case LabelToken extends AssemblerTokenType("LabelToken")
  case CommandToken extends AssemblerTokenType("CommandToken") 
  case InstructionToken extends AssemblerTokenType("InstructionToken")
  case SyntaxErrorToken extends AssemblerTokenType("SyntaxErrorToken")
  case ExceptionToken extends AssemblerTokenType("ExceptionToken")
  case ClearToken extends AssemblerTokenType("ClearToken")

