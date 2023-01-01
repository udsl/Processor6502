package com.udsl.processor6502.assemblier2

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.{AssembleLocation, AssemblyData}
import com.udsl.processor6502.assembler.AssembleLocation.currentLocation
import com.udsl.processor6502.cpu.CpuInstructions
import com.udsl.processor6502.cpu.execution.InstructionSize

/**
 * Fisrt pass object - as one would expect does the first pass which resolves any forward references.
 */
object Assemble6502FirstPassV2 extends StrictLogging, Assemble6502PassBaseV2 :

  def assemble(tokenisedLine: TokenisedLine): Unit =
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

  def assembleInstructionToken(t: Token) : Unit =
    logger.info(s"\tInstructionToken '$t' - location: $currentLocation")
    // Do we have a valid instruction?
    if !CpuInstructions.isValidInstruction(t.tokenText) then
      logger.error(s"Invalid instruction $t")
    // TODO work out addressing mode
//    else
      // By this time we should have an idea of any foreword reference values so we can work out the actual addressing mode.
//      if t.predictedAddressingModes.length == 1 then
//        // only 1 predicted mode so lets assume it right :)
//        // Now we can move the program counter along by the instruction size.
//        AssembleLocation.addInstructionSize(t.predictedAddressingModes.head.size)


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

