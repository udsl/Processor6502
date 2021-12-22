package com.udsl.processor6502.assembler

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Utilities
import com.udsl.processor6502.assembler.Assemble6502FirstPass.logger
import com.udsl.processor6502.assembler.AssembleLocation.currentLocation
import com.udsl.processor6502.cpu.CpuInstructions
import com.udsl.processor6502.cpu.execution.*


/**
 * Fisrt pass object - as one would expect does the first pass which resolves any forward references.
 */
object Assemble6502FirstPass extends StrictLogging, Assemble6502PassBase :

  def assemble(tokenisedLine: TokenisedLine) : Unit =
    logger.info(s"Parsing line ${tokenisedLine.sourceLine.lineNumber} ")
    for (token <- tokenisedLine.tokens)
      token match {
        case BlankLineToken( _, _ ) => // extends AssemblerTokenType("BlankLineToken")
          logger.info("\tBlankLineToken ")
        case CommentLineToken( _, _ ) => // extends AssemblerTokenType("CommentLineToken")
          assembleCommentLineToken(token)
        case LineComment( _, _ ) => // extends AssemblerTokenType("LineComment")
          logger.info("\tLineComment ")
        case NoneCommentLine( _, _ ) => // extends AssemblerTokenType("NoneCommentLine")
          logger.info("\tNoneCommentLine ")
        case LabelToken( _, _ ) => // extends AssemblerTokenType("LabelToken")
          procesLabel(token)
        case CommandToken( _, _ ) => // extends AssemblerTokenType("CommandToken")
          assembleCommandToken(token)
        case InstructionToken( _, _ ) => // extends AssemblerTokenType("InstructionToken")
          assembleInstructionToken(token)
        case SyntaxErrorToken( _, _ ) => // extends AssemblerTokenType("SyntaxErrorToken")
          logger.info("\tSyntaxErrorToken ")
        case ClearToken( _, _ ) =>
          logger.info("\tClear Token")
          processClear(token, tokenisedLine)
        case ValueToken( _, _ ) =>
          processValues(token)
        case OriginToken( _, _ ) =>
          processOrigin(token)
        case DefToken( _, _ ) =>
          processDefinition(token)
        case _ => logger.error(s"unsupported case ${token}")
      }
    logger.debug(tokenisedLine.sourceLine.source)

  def processOrigin(t: AssemblerToken) : Unit =
    logger.info(s"\tOrigin Token '${t.mnemonic}'")
    if (t.fields.length == 1){
      AssembleLocation.setAssembleLoc(Utilities.numericValue(t.fields.head))
    }

  def processValues(t: AssemblerToken) : Unit =
    logger.info(s"\tValue Token '${t.mnemonic}'")

  def NoneCommentLine(t: AssemblerToken) : Unit =
    logger.info(s"\tNoneCommentLine '${t.mnemonic}' - ")

  def assembleCommentLineToken(t: AssemblerToken) : Unit =
    logger.info(s"\tCommentLineToken '${t.mnemonic}' - ")

  def assembleCommandToken(t: AssemblerToken) : Unit =
    logger.info(s"\tCommandToken '${t.mnemonic}' - ")
    t.mnemonic.toUpperCase() match
      case "ORIG" => AssembleLocation.setAssembleLoc(t.intValue)
      case "BYT" => setBytes(t.fields)
      case "WRD" => setWords(t.fields)
      case "ADDR" => setAddresses(t.fields)
      case _ => logger.info(s"\tInvalid command ${t} ")

  def processClear(t: AssemblerToken, tl: TokenisedLine) : Unit =
    logger.info("Processing CLR command")
    if tl.sourceLine.lineNumber > 1 then
      val errorText = "CLR command only valid on first line"
      logger.error(errorText)
      throw new Exception(errorText)

  def assembleInstructionToken(t: AssemblerToken) : Unit =
    logger.info(s"\tInstructionToken '${t}' - location: $currentLocation")
    // Do we have a valid instruction?
    if !CpuInstructions.isValidInstruction(t.value) then
      logger.error(s"Invalid instruction ${t}")
    else
      var insSize = 0
      // By this time we should have an idea of any foreword reference values so we can work out the actual addresing mode.
      if t.predictedAddressingModes.length == 1 then
      // only 1 predicted mode so lets assume it right :)
        insSize = t.predictedAddressingModes.head match
          case Accumulator
               | Implied  => 1
          case Immediate
               | ZeroPage
               | Relative
               | ZeroPageX
               | ZeroPageY
               | IndirectX
               | IndirectY => 2
          case Absolute
               | Indirect
               | AbsoluteX
               | AbsoluteY => 3
          case Invalid | Unknown => -1
      // Is that addressing mode valid for the instruction?
      // Now we can move the program counter along by the instruction size.
      AssembleLocation.addInstructionSize(insSize)


  def processDefinition(t: AssemblerToken) : Unit =
    logger.info(s"\tDefinition of label ${t.value} with value ")
    AssemblyData.addLabel(t.mnemonic, t.value)

  def procesLabel(t: AssemblerToken) : Unit =
    logger.info(s"\tDefining label ${t.mnemonic} with value $currentLocation")
    AssemblyData.addLabel(t.mnemonic)

  def setBytes(fields: Array[String]): Unit =
    logger.debug("setBytes")
    for (value <- fields)
      setMemoryByte(value.trim)

  def setWords(fields: Array[String]): Unit =
    logger.debug("setWords")
    for (value <- fields)
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

  def setAddresses(fields: Array[String]): Unit =
    logger.debug("setAddresses")
    for (value <- fields)
      setMemoryAddress(value.trim)


