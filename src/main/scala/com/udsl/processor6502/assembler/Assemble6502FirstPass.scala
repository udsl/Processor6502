package com.udsl.processor6502.assembler

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Utilities
import com.udsl.processor6502.assembler.Assemble6502FirstPass.logger
import com.udsl.processor6502.assembler.AssembleLocation.currentLocation
import com.udsl.processor6502.cpu.CpuInstructions


/**
 * Fisrt pass object - as one would expect does the first pass which resolves any forward references.
 */
object Assemble6502FirstPass extends StrictLogging, Assemble6502PassBase :

  def assemble(tokenisedLine: TokenisedLine) : Unit =
    logger.info(s"Parsing line ${tokenisedLine.sourceLine.lineNumber} ")
    for (token <- tokenisedLine.tokens)
      token.typeOfToken match {
        case AssemblerTokenType.BlankLineToken => // extends AssemblerTokenType("BlankLineToken")
          logger.info("\tBlankLineToken ")
        case AssemblerTokenType.CommentLineToken => // extends AssemblerTokenType("CommentLineToken")
          assembleCommentLineToken(token)
        case AssemblerTokenType.LineComment => // extends AssemblerTokenType("LineComment")
          logger.info("\tLineComment ")
        case AssemblerTokenType.NoneCommentLine => // extends AssemblerTokenType("NoneCommentLine")
          logger.info("\tNoneCommentLine ")
        case AssemblerTokenType.LabelToken => // extends AssemblerTokenType("LabelToken")
          procesLabel(token)
        case AssemblerTokenType.CommandToken => // extends AssemblerTokenType("CommandToken")
          assembleCommandToken(token)
        case AssemblerTokenType.InstructionToken => // extends AssemblerTokenType("InstructionToken")
          assembleInstructionToken(token)
        case AssemblerTokenType.SyntaxErrorToken => // extends AssemblerTokenType("SyntaxErrorToken")
          logger.info("\tSyntaxErrorToken ")
        case AssemblerTokenType.ClearToken =>
          logger.info("\tClear Token")
          processClear(token, tokenisedLine)
        case AssemblerTokenType.ValueToken =>
          processValues(token)
        case AssemblerTokenType.OriginToken =>
          processOrigin(token)

        case _ => logger.error(s"unsupported case ${token.typeOfToken}")
      }
    logger.debug(tokenisedLine.sourceLine.source)

  def processOrigin(t: Token) : Unit =
    logger.info("\tOrigin Token")
    val value = Utilities.numericValue(t.tokenStr)
    if value > 0 then
      AssembleLocation.setAssembleLoc(value)

  def processValues(t: Token) : Unit =
    logger.info("\tValue Token")

  def NoneCommentLine(t: Token) : Unit =
    logger.info(s"\tNoneCommentLine '${t.tokenVal.strVal}' - ")

  def assembleCommentLineToken(t: Token) : Unit =
    logger.info(s"\tCommentLineToken '${t.tokenVal.strVal}' - ")

  def assembleCommandToken(t: Token) : Unit =
    logger.info(s"\tCommandToken '${t.tokenVal.strVal}' - ")
    t.tokenStr.toUpperCase() match
      case "ORIG" => AssembleLocation.setAssembleLoc(t.intValue)
      case "BYT" => setBytes(t.tokenVal.strVal)
      case "WRD" => setWords(t.tokenVal.strVal)
      case "ADDR" => setAddresses(t.tokenVal.strVal)
      case _ => logger.info(s"\tInvalid command ${t.tokenStr} ")

  def processClear(t: Token, tl: TokenisedLine) : Unit =
    logger.info("Processing CLR command")
    if tl.sourceLine.lineNumber > 1 then
      val errorText = "CLR command only valid on first line"
      logger.error(errorText)
      throw new Exception(errorText)

  def assembleInstructionToken(t: Token) : Unit =
    logger.info(s"\tInstructionToken '${t.tokenVal.strVal}' - location: $currentLocation")
    // Do we have a valid instruction?
    if !CpuInstructions.isValidInstruction(t.tokenStr) then
      logger.error(s"Invalid instruction ${t.tokenStr}")
    else
      var insSize = 0
      // By this time we should have an idea of any foreword reference values so we can work out the actual addresing mode.
      if t.tokenVal.pridictedMode.modes.length == 1 then
      // only 1 predicted mode so lets assume it right :)
        insSize = t.tokenVal.pridictedMode.modes.head match
          case AddressingMode.Accumulator
               | AddressingMode.Implied  => 1
          case AddressingMode.Immediate
               | AddressingMode.ZeroPage
               | AddressingMode.Relative
               | AddressingMode.ZeroPageX
               | AddressingMode.ZeroPageY
               | AddressingMode.ZeroPageIndirectX
               | AddressingMode.ZeroPageIndirectY => 2
          case AddressingMode.Absolute
               | AddressingMode.Indirect
               | AddressingMode.AbsoluteIndexedX
               | AddressingMode.AbsoluteIndexedY => 3
          case AddressingMode.Invalid => -1
      // Is that addressing mode valid for the instruction?
      // Now we can move the program counter along by the instruction size.
      AssembleLocation.addInstructionSize(insSize)


  def procesLabel(t: Token) : Unit =
    logger.info(s"\tDefining label ${t.tokenStr} with value $currentLocation")
    AssemblyData.addLabel(t.tokenStr)

  def setBytes(v: String): Unit =
    logger.debug("setBytes")
    val values = v.split(",")
    for (value <- values)
      setMemoryByte(value.trim)

  def setWords(v: String): Unit =
    logger.debug("setWords")
    val values = v.split(",")
    for (value <- values)
      setMemoryWord(value.trim)

  def setMemoryWord(v: String): Unit =
    if v.charAt(0).isLetter then // a label
      AssemblyData.addReference(v)
      AssembleLocation.setMemoryWord(0x6363) // word value for 99, 99 decimal
    else
      AssembleLocation.setMemoryWord(if v.charAt(0) == '$' then
        Integer.parseInt(v.substring(1), 16)
      else
        Integer.parseInt(v))

  def setAddresses(v: String): Unit =
    logger.debug("setAddresses")
    val values = v.split(",")
    for (value <- values)
      setMemoryAddress(value.trim)


