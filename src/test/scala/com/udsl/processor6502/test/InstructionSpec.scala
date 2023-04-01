package com.udsl.processor6502.test


import com.udsl.processor6502.assembler
import com.udsl.processor6502.cpu.execution.*
import com.udsl.processor6502.cpu.{CpuInstruction, CpuInstructions}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*


class TestData(val code: String, val opcode: Int, val insLength: Int, val addMode: AddressingMode){

}

object TestData{
  def apply(code: String, opcode: Int, insLength: Int, addMode: AddressingMode): TestData = {
    new TestData(code, opcode, insLength, addMode)
  }
}

val dataValidInstructionTest: List[TestData] = List(
  /*
  ADC - Add Memory to Accumulator with Carry
  immediate	    ADC #oper	    69	2
  ZeroPage	    ADC oper	    65	2
  ZeroPage,X	  ADC oper,X	  75	2
  absolute	    ADC oper	    6D	3
  absolute,X	  ADC oper,X	  7D	3
  absolute,Y	  ADC oper,Y	  79	3
  (indirect,X)	ADC (oper,X)	61	2
  (indirect),Y	ADC (oper),Y	71	2
*/
  TestData("ADC", 0x69, 2, Immediate), // #
  TestData("ADC", 0x65, 2, ZeroPage),  // $LL
  TestData("ADC", 0x75, 2, ZeroPageX), // $LL,X
  TestData("ADC", 0x6D, 3, Absolute),  // $LLLL
  TestData("ADC", 0x7D, 3, AbsoluteX), // $LL,X
  TestData("ADC", 0x79, 3, AbsoluteY), // $LL,Y
  TestData("ADC", 0x61, 2, IndirectX), // ($LL,X)
  TestData("ADC", 0x71, 2, IndirectY), //"($LL),Y"

  /*
 AND - AND Memory to Accumulator
 immediate	  AND #oper	    29	2
 ZeroPage	    AND oper	    25	2
 ZeroPage,X	  AND oper,X	  35	2
 absolute	    AND oper	    2D	3
 absolute,X	  AND oper,X	  3D	3
 absolute,Y	  AND oper,Y	  39	3
 (indirect,X)	AND (oper,X)	21	2
 (indirect),Y	AND (oper),Y	31	2
*/
  TestData("AND", 0x29, 2, Immediate), // #
  TestData("AND", 0x25, 2, ZeroPage),  // $LL
  TestData("AND", 0x35, 2, ZeroPageX), // $LL,X
  TestData("AND", 0x2D, 3, Absolute),  // $LLLL
  TestData("AND", 0x3D, 3, AbsoluteX), // $LL,X
  TestData("AND", 0x39, 3, AbsoluteY), // $LL,Y
  TestData("AND", 0x21, 2, IndirectX), // ($LL,X)
  TestData("AND", 0x31, 2, IndirectY), //"($LL),Y"

  /*
 ASL - Add Memory to Accumulator with Carry
 accumulator  ASL A   	    0A	1
 ZeroPage	    ASL oper	    06	2
 ZeroPage,X	  ASL oper,X	  16	2
 absolute	    ASL oper	    0E	3
 absolute,X	  ASL oper,X	  1E	3
*/
  TestData("ASL", 0x0A, 1, Accumulator), // #
  TestData("ASL", 0x06, 2, ZeroPage),  // $LL
  TestData("ASL", 0x16, 2, ZeroPageX), // $LL,X
  TestData("ASL", 0x0E, 3, Absolute),  // $LLLL
  TestData("ASL", 0x1E, 3, AbsoluteX), // $LL,X

  TestData("BCC", 0x90, 2, Relative), // $LL
  TestData("BCS", 0xB0, 2, Relative), // $LL
  TestData("BEQ", 0xF0, 2, Relative), // $LL

  TestData("BIT", 0x24, 2, ZeroPage), // $LL
  TestData("BIT", 0x2C, 3, Absolute), // $LLHH

  TestData("BMI", 0x30, 2, Relative), // $LL
  TestData("BNE", 0xD0, 2, Relative), // $LL

  TestData("BRK", 0x00, 1, Implied), // $LL

  TestData("BVC", 0x50, 2, Relative), // $LL
  TestData("BVS", 0x70, 2, Relative), // $LL


  TestData("CLC", 0x18, 1, Implied),
  TestData("CLD", 0xD8, 1, Implied),
  TestData("CLI", 0x58, 1, Implied),
  TestData("CLV", 0xB8, 1, Implied),

  /**
   * CMP Compare Memory with Accumulator
   *
   * immediate	CMP #oper	C9	2
   * ZeroPage	CMP oper	C5	2
   * ZeroPage,X	CMP oper,X	D5	2
   * absolute	CMP oper	CD	3
   * absolute,X	CMP oper,X	DD	3
   * absolute,Y	CMP oper,Y	D9	3
   * (indirect,X)	CMP (oper,X)	C1	2
   * (indirect),Y	CMP (oper),Y	D1	2
   */
  TestData("CMP", 0xC9, 2, Immediate), // #
  TestData("CMP", 0xC5, 2, ZeroPage),  // $LL
  TestData("CMP", 0xD5, 2, ZeroPageX), // $LL,X
  TestData("CMP", 0xCD, 3, Absolute),  // $LLHH
  TestData("CMP", 0xDD, 3, AbsoluteX), // $LL,X
  TestData("CMP", 0xD9, 3, AbsoluteY), // $LL,X
  TestData("CMP", 0xC1, 2, IndirectX), // $LL,X
  TestData("CMP", 0xD1, 2, IndirectY), // $LL,X

  /**
   * CPX Compare Memory and Index X
   *
   * immediate CPX #oper E0	2
   * ZeroPage	 CPX oper	 E4	2
   * absolute	 CPX oper	 EC	3
   */
  TestData("CPX", 0xE0, 2, Immediate), // #
  TestData("CPX", 0xE4, 2, ZeroPage),  // $LL
  TestData("CPX", 0xEC, 3, Absolute),  // $LLHH


  /**
   * CPY Compare Memory and Index Y
   *
   * immediate CPY #oper C0	2
   * ZeroPage	 CPY oper	 C4	2
   * absolute  CPY oper	 CC	3
   */
  TestData("CPY", 0xC0, 2, Immediate), // #
  TestData("CPY", 0xC4, 2, ZeroPage),  // $LL
  TestData("CPY", 0xCC, 3, Absolute),  // $LLHH

  /**
   * DEC Compare Memory and Index Y
   *
   * ZeroPage   DEC #oper C6	2
   * ZeroPage,X DEC oper	D6	2
   * absolute   DEC oper	CE	3
   * absolute,X DEC oper	DE	3
   */
  TestData("DEC", 0xC6, 2, ZeroPage),
  TestData("DEC", 0xD6, 2, ZeroPageX),
  TestData("DEC", 0xCE, 3, Absolute),
  TestData("DEC", 0xDE, 3, AbsoluteX),

  TestData("DEX", 0xCA, 1, Implied),

  TestData("DEY", 0x88, 1, Implied),

  /**
   * EOR Compare Memory with Accumulator
   *
   * immediate	EOR #oper	49	2
   * ZeroPage	EOR oper	45	2
   * ZeroPage,X	EOR oper,X	55	2
   * absolute	EOR oper	4D	3
   * absolute,X	EOR oper,X	5D	3
   * absolute,Y	EOR oper,Y	59	3
   * (indirect,X)	EOR (oper,X)	41	2
   * (indirect),Y	EOR (oper),Y	51	2
   */
  TestData("EOR", 0x49, 2, Immediate), // #
  TestData("EOR", 0x45, 2, ZeroPage),  // $LL
  TestData("EOR", 0x55, 2, ZeroPageX), // $LL,X
  TestData("EOR", 0x4D, 3, Absolute),  // $LLHH
  TestData("EOR", 0x5D, 3, AbsoluteX), // $LL,X
  TestData("EOR", 0x59, 3, AbsoluteY), // $LL,X
  TestData("EOR", 0x41, 2, IndirectX), // $LL,X
  TestData("EOR", 0x51, 2, IndirectY), // $LL,X

  /**
   * INC Compare Memory and Index Y
   *
   * ZeroPage   INC #oper E6	2
   * ZeroPage,X INC oper	F6	2
   * absolute   INC oper	EE	3
   * absolute,X INC oper	FE	3
   */
  TestData("INC", 0xE6, 2, ZeroPage),
  TestData("INC", 0xF6, 2, ZeroPageX),
  TestData("INC", 0xEE, 3, Absolute),
  TestData("INC", 0xFE, 3, AbsoluteX),

  TestData("INX", 0xE8, 1, Implied),

  TestData("INY", 0xC8, 1, Implied),

  /**
   * JMP Jump to New Location
   *
   * absolute	JMP oper	4C	3
   * indirect	JMP (oper)	6C	3
   */
  TestData("JMP", 0x4C, 3, Absolute),
  TestData("JMP", 0x6C, 3, Indirect),

  TestData("JSR", 0x20, 3, Absolute),

  /**
   * LDA Load Accumulator with Memory
   *
   * immediate	LDA #oper	      A9	2
   * ZeroPage	LDA oper	        A5	2
   * ZeroPage,X	LDA oper,X	    B5	2
   * absolute	LDA oper	        AD	3
   * absolute,X	LDA oper,X	    BD	3
   * absolute,Y	LDA oper,Y	    B9	3
   * (indirect,X)	LDA (oper,X)	A1	2
   * (indirect),Y	LDA (oper),Y	B1	2
   */
  TestData("LDA", 0xA9, 2, Immediate),
  TestData("LDA", 0xA5, 2, ZeroPage),
  TestData("LDA", 0xB5, 2, ZeroPageX),
  TestData("LDA", 0xAD, 3, Absolute),
  TestData("LDA", 0xBD, 3, AbsoluteX),
  TestData("LDA", 0xB9, 3, AbsoluteY),
  TestData("LDA", 0xA1, 2, IndirectX),
  TestData("LDA", 0xB1, 2, IndirectY),

  /**
   * LDX Load Index X with Memory
   *
   * immediate	LDX #oper	  A2	2
   * ZeroPage	LDX oper	    A6	2
   * ZeroPage,Y	LDX oper,Y	B6	2
   * absolute	LDX oper	    AE	3
   * absolute,Y	LDX oper,Y	BE	3
   */
  TestData("LDX", 0xA2, 2, Immediate),
  TestData("LDX", 0xA6, 2, ZeroPage),
  TestData("LDX", 0xB6, 2, ZeroPageY),
  TestData("LDX", 0xAE, 3, Absolute),
  TestData("LDX", 0xBE, 3, AbsoluteY),

  /**
   * LDY Load Index Y with Memory
   *
   * immediate	LDY #oper	  A0	2
   * ZeroPage	LDY oper	    A4	2
   * ZeroPage,Y	LDY oper,Y	B4	2
   * absolute	LDY oper	    Ac	3
   * absolute,Y	LDY oper,Y	Bc	3
   */
  TestData("LDY", 0xA0, 2, Immediate),
  TestData("LDY", 0xA4, 2, ZeroPage),
  TestData("LDY", 0xB4, 2, ZeroPageX),
  TestData("LDY", 0xAC, 3, Absolute),
  TestData("LDY", 0xBC, 3, AbsoluteX),

  /**
   * LSR Shift One Bit Right (Memory or Accumulator)
   *
   * accumulator	LSR A	    4A	1
   * ZeroPage	LSR oper	    46	2
   * ZeroPage,X	LSR oper,X	56	2
   * absolute	LSR oper	    4E	3
   * absolute,X	LSR oper,X	5E	3
   */
  TestData("LSR", 0x4A, 1, Accumulator),
  TestData("LSR", 0x46, 2, ZeroPage),
  TestData("LSR", 0x56, 2, ZeroPageX),
  TestData("LSR", 0x4E, 3, Absolute),
  TestData("LSR", 0x5E, 3, AbsoluteX),

  /**
   * NOP No Operation
   *
   * implied	NOP	EA	1
   */
  TestData("NOP", 0xEA, 1, Implied),


  /**
   * ORA Load Accumulator with Memory
   *
   * immediate	ORA #oper	      09	2
   * ZeroPage	ORA oper	        05	2
   * ZeroPage,X	ORA oper,X	    15	2
   * absolute	ORA oper	        0D	3
   * absolute,X	ORA oper,X	    1D	3
   * absolute,Y	ORA oper,Y	    19	3
   * (indirect,X)	ORA (oper,X)	01	2
   * (indirect),Y	ORA (oper),Y	11	2
   */
  TestData("ORA", 0x09, 2, Immediate),
  TestData("ORA", 0x05, 2, ZeroPage),
  TestData("ORA", 0x15, 2, ZeroPageX),
  TestData("ORA", 0x0D, 3, Absolute),
  TestData("ORA", 0x1D, 3, AbsoluteX),
  TestData("ORA", 0x19, 3, AbsoluteY),
  TestData("ORA", 0x01, 2, IndirectX),
  TestData("ORA", 0x11, 2, IndirectY),

  /**
   * PHA Push Accumulator on Stack
   *
   * implied	PHA	48	1
   */
  TestData("PHA", 0x48, 1, Implied),

  /**
   * PHP Push Processor Status on Stack
   *
   * implied	PHA	08	1
   */
  TestData("PHP", 0x08, 1, Implied),

  /**
   * PLA Pull Accumulator from Stack
   *
   * implied	PLA	68	1
   */
  TestData("PLA", 0x68, 1, Implied),

  /**
   * PLP Pull Processor Status from Stack
   *
   * implied	PLP	28	1
   */
  TestData("PLP", 0x28, 1, Implied),

  /**
   * ROL Rotate One Bit Left (Memory or Accumulator)
   *
   * accumulator	ROL A	      2A	1
   * ZeroPage	    ROL oper	  26	2
   * ZeroPage,X	  ROL oper,X	36	2
   * absolute	    ROL oper	  2E	3
   * absolute,X	  ROL oper,X	3E	3
   */
  TestData("ROL", 0x2A, 1, Accumulator),
  TestData("ROL", 0x26, 2, ZeroPage),
  TestData("ROL", 0x36, 2, ZeroPageX),
  TestData("ROL", 0x2E, 3, Absolute),
  TestData("ROL", 0x3E, 3, AbsoluteX),


  /**
   * ROR Rotate One Bit Right (Memory or Accumulator)
   *
   * accumulator	ROR A	      6A	1
   * ZeroPage	    ROR oper	  66	2
   * ZeroPage,X	  ROR oper,X	76	2
   * absolute	    ROR oper	  6E	3
   * absolute,X	  ROR oper,X	7E	3
   */
  TestData("ROR", 0x6A, 1, Accumulator),
  TestData("ROR", 0x66, 2, ZeroPage),
  TestData("ROR", 0x76, 2, ZeroPageX),
  TestData("ROR", 0x6E, 3, Absolute),
  TestData("ROR", 0x7E, 3, AbsoluteX),

  /**
   * RTI Return from Interrupt
   *
   * implied	RTI	40	1
   */
  TestData("RTI", 0x40, 1, Implied),

  /**
   * RTS Return from Subroutine
   *
   * implied	RTI	60	1
   */
  TestData("RTS", 0x60, 1, Implied),

  /**
   * SBC Subtract Memory from Accumulator with Borrow
   *
   * immediate	SBC #oper	      E9	2
   * ZeroPage	SBC oper	        E5	2
   * ZeroPage,X	SBC oper,X	    F5	2
   * absolute	SBC oper	        ED	3
   * absolute,X	SBC oper,X	    FD	3
   * absolute,Y	SBC oper,Y	    F9	3
   * (indirect,X)	SBC (oper,X)	E1	2
   * (indirect),Y	SBC (oper),Y	F1	2
   */
  TestData("SBC", 0xE9, 2, Immediate),
  TestData("SBC", 0xE5, 2, ZeroPage),
  TestData("SBC", 0xF5, 2, ZeroPageX),
  TestData("SBC", 0xED, 3, Absolute),
  TestData("SBC", 0xFD, 3, AbsoluteX),
  TestData("SBC", 0xF9, 3, AbsoluteY),
  TestData("SBC", 0xE1, 2, IndirectX),
  TestData("SBC", 0xF1, 2, IndirectY),

  /**
   * SEC Set Carry Flag
   *
   * implied	SEC	38	1
   */
  TestData("SEC", 0x38, 1, Implied),

  /**
   * SED Set Decimal Flag
   *
   * implied	SED	38	1
   */
  TestData("SED", 0xF8, 1, Implied),

  /**
   * SEI Set Interrupt Disable Status
   *
   * implied	SEI	78	1
   */
  TestData("SEI", 0x78, 1, Implied),

  /**
   * STA Store Accumulator in Memory
   *
   * ZeroPage	STA oper	        85	2
   * ZeroPage,X	STA oper,X	    95	2
   * absolute	STA oper	        8D	3
   * absolute,X	STA oper,X	    9D	3
   * absolute,Y	STA oper,Y	    99	3
   * (indirect,X)	STA (oper,X)	81	2
   * (indirect),Y	STA (oper),Y	91	2
   */
  TestData("STA", 0x85, 2, ZeroPage),
  TestData("STA", 0x95, 2, ZeroPageX),
  TestData("STA", 0x8D, 3, Absolute),
  TestData("STA", 0x9D, 3, AbsoluteX),
  TestData("STA", 0x99, 3, AbsoluteY),
  TestData("STA", 0x81, 2, IndirectX),
  TestData("STA", 0x91, 2, IndirectY),
  
  /**
   * STX Store X in Memory
   *
   * ZeroPage	STX oper	        86	2
   * ZeroPage,Y	STX oper,Y	    96	2
   * absolute	STX oper	        8E	3
   */
  TestData("STX", 0x86, 2, ZeroPage),
  TestData("STX", 0x96, 2, ZeroPageY),
  TestData("STX", 0x8E, 3, Absolute),

  /**
   * STY Store Y in Memory
   *
   * ZeroPage	STY oper	        84	2
   * ZeroPage,X	STY oper,X	    94	2
   * absolute	STY oper	        8C	3
   */
  TestData("STY", 0x84, 2, ZeroPage),
  TestData("STY", 0x94, 2, ZeroPageX),
  TestData("STY", 0x8C, 3, Absolute),

  /**
   * TAX Transfer Accumulator to Index X
   *
   * implied	TAX	AA	1
   */
  TestData("TAX", 0xAA, 1, Implied),

  /**
   * TAY Transfer Accumulator to Index Y
   *
   * implied	TAY	A8	1
   */
  TestData("TAY", 0xA8, 1, Implied),

  /**
   * TSX Transfer Stack Pointer to Index X
   *
   * implied	TSX	BA	1
   */
  TestData("TSX", 0xBA, 1, Implied),

  /**
   * TXA Transfer Index X to Accumulator
   *
   * implied	TXA	8A	1
   */
  TestData("TXA", 0x8A, 1, Implied),

  /**
   * TXS Transfer Index X to Stack Register
   *
   * implied	TXS	9A	1
   */
  TestData("TXS", 0x9A, 1, Implied),

  /**
   * TYA Transfer Index Y to Accumulator
   *
   * implied	TYA	98	1
   */
  TestData("TYA", 0x98, 1, Implied),

)

class InstructionSpec extends AnyFlatSpec, should.Matchers {
  "An instruction" should " have the correct opcode and addressing mode" in {
    for (data <- dataValidInstructionTest) {
      assert(CpuInstructions.isValidInstruction(data.code), s"Instruction not found ${data.code}")
      val (opcode: Int, bytes:Int)  = CpuInstructions.getInstructionOpcodeBytes(data.code, data.addMode)
      assert(1 to 3 contains bytes, s"Failed to get valid bytes for ${data.code} with ${data.addMode} addressing")
      assert(opcode >= 0,s"Failed to get valid opcode for ${data.code}")
      assert(opcode == data.opcode, s"Invalid opcode for ${data.code} - ${data.addMode}! expected 0x${data.opcode.toHexString.toUpperCase()} got 0x${opcode.toHexString.toUpperCase()}.")
      assert(bytes == data.insLength, s"Invalid instruction length for ${data.code} - ${data.addMode}! expected ${data.insLength} got $bytes.")
    }
  }

}


