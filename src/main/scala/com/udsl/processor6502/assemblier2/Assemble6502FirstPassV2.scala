package com.udsl.processor6502.assemblier2

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.{AssembleLocation, AssemblyData}
import com.udsl.processor6502.assembler.AssembleLocation.currentLocation
import com.udsl.processor6502.cpu.CpuInstruction
import com.udsl.processor6502.cpu.execution.{Accumulator, Absolute, AbsoluteX, AbsoluteY, AddressingMode, Immediate, Implied, Indirect, IndirectX, IndirectY, InstructionSize, Invalid, Relative, Unknown, ZeroPage, ZeroPageX, ZeroPageY}
import com.udsl.processor6502.assemblier2.{Token, TokenisedLine}
/**
 * First pass class - as one would expect does the first pass which resolves any forward references.
 */
trait FirstPassV2:
  def assemble(tokenisedLine: TokenisedLine): FirstPassResult = ???

class Assemble6502FirstPassV2 extends FirstPassV2 with StrictLogging with Assemble6502PassBaseV2 :

  override def assemble(tokenisedLine: TokenisedLine): FirstPassResult =
    logger.info(s"Parsing line ${tokenisedLine.lineNumber} ")
    for (token <- tokenisedLine.tokens)
      token match {
        case BlankLineToken( _ ) => // extends AssemblerTokenType("BlankLineToken")
          logger.info("\tBlankLineToken ")
        case CommentLineToken( _ ) => // extends AssemblerTokenType("CommentLineToken")
          assembleCommentLineToken(token)
        case LineCommentToken( _, _ ) => // extends AssemblerTokenType("LineComment")
          logger.info("\tLineComment ")
        case LabelToken( _, _ ) => // extends AssemblerTokenType("LabelToken")
          procesLabel(token)
        case CommandToken( _, _ ) => // extends AssemblerTokenType("CommandToken")
          assembleCommandToken(token)
        case InstructionToken( _, _ ) => // extends AssemblerTokenType("InstructionToken")
          assembleInstructionToken(token)
        case _ => logger.error(s"unsupported case $token")
      }
    logger.debug(tokenisedLine.sourceLine)

    FirstPassResult(tokenisedLine)

//  def processOrigin(t: Token) : Unit =
//    logger.info(s"\tOrigin Token '${t.mnemonic}'")
//    if (t.fields.length == 1){
//      if isNumeric(t.mnemonic) then
//        AssembleLocation.setAssembleLoc(numericValue(t.mnemonic))
//      else
//        if isLabel(t.mnemonic) && AssemblyData.labelIsDefined(t.mnemonic) then
//          val labelValue = AssemblyData.labelValue(t.mnemonic)
//          AssembleLocation.setAssembleLoc(labelValue)
//        else
//          throw new Exception("Label or ORIG not defined.")
//    }

//  def processValues(t: Token) : Unit =
//    logger.info(s"\tValue Token '${t.mnemonic}'")
  
  def assembleCommentLineToken(t: Token) : Unit =
    logger.info(s"\tCommentLineToken '${t.tokenText}' - ")

  def assembleCommandToken(t: Token) : Unit =
    logger.info(s"\tCommandToken '${t.tokenText}' - ")
    t.tokenText.toUpperCase() match
      case "BYT" => advanceAssemLocForBytes(t.fields)
      case "WRD" => advanceAssemLocForWords(t.fields)
      case "ADDR" => advanceAssemLocForAddresses(t.fields)
      case _ => logger.info(s"\tInvalid command $t ")

//  def processClear(t: Token, tl: TokenisedLine) : Unit =
//    logger.info("Processing CLR command")
//    if tl.lineNumber > 1 then
//      val errorText = "CLR command only valid on first line"
//      logger.error(errorText)
//      throw new Exception(errorText)

  def assembleInstructionToken(token: Token) : Unit =
    logger.info(s"\tInstructionToken '$token' - location: $currentLocation")
    val instruction: CpuInstruction = token.asInstanceOf[InstructionToken].instruction
    // TODO work out addressing mode
    /* By this time we should have an idea of any foreword reference values as they are 'registered' during tockenisation
       though the actual value of a label will not be known.
       Possible values and associated addressing mode:
          missing - just the instruction them accumilator or implied
          numeric - starts with a digit or $ for hex - absolute or zero page
          label - starts with an alph but not ( absolute mode to label
          imeadiate address mode starts with #
          indirect addresing mode starts with ( some for of indexed addressing
          nothing implied addressing
          relative applies to branches only - except for BIT and BRK all instructions starting with B are a branch! */

    val addressingMode: AddressingMode = if token.fields.isEmpty then // Implied addressing mode
      Implied
    else if instruction.name().head.equals('B') && !(instruction.name().equals("BIT") || instruction.name().equals("BRK")) then
      Relative
    else
      val operandField = token.fields.head.toUpperCase()
      operandField.head match {
        case 'A' => Accumulator
        case '#' => Immediate
        case '$' => if Integer.parseInt(operandField.substring(1), 16) > 255 then
                      Absolute
                    else
                      ZeroPage
        case c if c.isDigit => if Integer.parseInt(operandField, 10) > 255 then
                  Absolute
                else
                  ZeroPage
        case d if d.isLetter => if AssemblyData.labelIsDefinedV2(operandField) then // label or defined
                                  AssemblyData.labelValue(operandField) match {
                                    case Some(v) => if v > 255 then Absolute else ZeroPage
                                    case _ => Unknown
                                  }
                                else Invalid

        case '(' => if operandField.endsWith(",X)") then IndirectX
                    else if operandField.endsWith("),Y") then IndirectY
                    else if operandField.endsWith(")") then Indirect
                    else Invalid
        case _ => Unknown
      }
    // is the addressingMode valid for this instruction? then can move the program counter along by the instruction size.
    instruction.getInsDataForAddressingMode(addressingMode) match {
      case Some(i) =>
        AssembleLocation.setMemoryByte(i.opcode) // write the opcode
        AssembleLocation.addInstructionSize(i.bytes - 1) // move on hte current locaion by instruction size -1 as setMemoryByte already moved on by 1.
      case _ => throw new Exception(s"Addressing mode '$addressingMode' not applicable to instruction '${instruction.name()}''")
    }




  /**
   * Addressing modes starting with a numeric value
   *
   * abs	absolute	OPC $LLHH	operand is address $HHLL
   *
   * rel	relative	OPC $BB	branch target is PC + signed offset BB
   * zpg	zeropage	OPC $LL	operand is zeropage address (hi-byte is zero, address = $00LL)
   *
   * abs,X	absolute, X-indexed	OPC $LLHH,X	operand is address; effective address is address incremented by X with carry
   * abs,Y	absolute, Y-indexed	OPC $LLHH,Y	operand is address; effective address is address incremented by Y with carry
   * zpg,X	zeropage, X-indexed	OPC $LL,X	operand is zeropage address; effective address is address incremented by X without carry
   * zpg,Y	zeropage, Y-indexed	OPC $LL,Y	operand is zeropage address; effective address is address incremented by Y without carry
   *
   * @param t    the string representing the operand
   * @param base if base of the number Decimal or hex
   * @return a prediction of the real addressing mode, we only need a prediction. The 2nd pass will have to do the full calc anyway.
   *         The prediction just goves it a starting point saves a bit of time.
   */
  def getNumericMode(t: String, base: Int): AddressingMode =
    val lastChars = t.substring(t.length() - 2).toUpperCase()
    val indexed = lastChars == ",X" || lastChars == ",Y"
    val valStr = if indexed then
      t.substring(t.length - 2)
    else
      t
    val value = Integer.parseInt(valStr, base)
    if value > 256 then
      lastChars match
        case ",X" => AbsoluteX
        case ",Y" => AbsoluteY
        case _ => Absolute
    else
      lastChars match
        case ",X" => ZeroPageX
        case ",Y" => ZeroPageY
        case _ => ZeroPage

  /**
   * For 2nd pass we just need to verify that the label is defined and that if the value is set verify its the same
   * other wise update it.
   *
   * @param t the Token to process
   */
//  def processDefinition(t: Token) : Unit =
//    logger.info(s"\tDefinition of label ${t.tokenText} with value ${t.value}")
//    if AssemblyData.labelIsDefined(t.mnemonic) then
//      val v = AssemblyData.labelValue(t.mnemonic)
//      if v != t.intValue then
//        throw new Exception(s"Definition value changed om 2nd pass was ${t.intValue} now $v")

  def procesLabel(t: Token) : Unit =
    logger.info(s"\tDefining label ${t.tokenText} with value $currentLocation")
    AssemblyData.addLabel(t.tokenText)

  //TODO modify how this works!
  def advanceAssemLocForBytes(fields: Array[String]): Unit =
    logger.debug("advance current assembly location for each byte")
    for (value <- fields)
      AssembleLocation.addInstructionSize(InstructionSize(1))

  def advanceAssemLocForWords(fields: Array[String]): Unit =
    logger.debug("advance current assembly location by 2 for each word")
    for (value <- fields)
      AssembleLocation.addInstructionSize(InstructionSize(2))

  def advanceAssemLocForAddresses(fields: Array[String]): Unit =
    logger.debug("advance current assembly location by 2 for each address")
    for (v <- fields)
      AssembleLocation.addInstructionSize(InstructionSize(2))

object Assemble6502FirstPassV2:
  def apply: Assemble6502FirstPassV2 =
    new Assemble6502FirstPassV2()

