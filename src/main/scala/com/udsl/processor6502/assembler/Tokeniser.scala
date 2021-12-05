package com.udsl.processor6502.assembler

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Utilities
import com.udsl.processor6502.Utilities.*
import com.udsl.processor6502.assembler.AssemblerTokenType.{BlankLineToken, CommentLineToken, ExceptionToken}
import com.udsl.processor6502.cpu.CpuInstructions

import scala.collection.mutable.ListBuffer

object Tokeniser extends StrictLogging :

  def Tokenise(allLines: Array[UntokenisedLine]): List[TokenisedLine] =
    logger.info(
      """
        |**************************
        |*                        *
        |*   Start Tokenisation   *
        |*                        *
        |**************************
        |
        |""".stripMargin)
    val tokenisedLines = new ListBuffer[TokenisedLine]()
    for lineToTokenise <- allLines do
      try
        tokenisedLines.addOne(tokeniseLine(lineToTokenise))
      catch
        case e: Exception =>  addExceptionToken(tokenisedLines, lineToTokenise, s"${e.getMessage}\nOn line ${lineToTokenise.lineNumber}")
        case a => addExceptionToken(tokenisedLines, lineToTokenise, s"${a.getMessage}\nOn line ${lineToTokenise.lineNumber}")
    val f = java.io.File("code.tock")
    writeToFile( f, tokenisedLines.toList)
    tokenisedLines.toList

  private def addExceptionToken(tokenisedLines: ListBuffer[TokenisedLine], source: UntokenisedLine, exceptionMessage: String): Unit =
    val tokenisedLine = TokenisedLine(source)
    tokenisedLine.tokens.addOne(Token(ExceptionToken, exceptionMessage))
    tokenisedLines.addOne(tokenisedLine)

  def tokeniseLine(line: UntokenisedLine): TokenisedLine =
    logger.debug(s"\ntokeniseLine: $line")
    // determine basic line type
    val tokenisedLine = TokenisedLine(line)
    val token = line.source.trim match {
      case "" => Token(AssemblerTokenType.BlankLineToken)
      case a if a.charAt(0) == ';' => Token(AssemblerTokenType.CommentLineToken, line.source.trim, TokenValue.apply)
      case _ => Token(AssemblerTokenType.NoneCommentLine)
    }

    token.typeOfToken match
      case BlankLineToken |  CommentLineToken =>
        tokenisedLine + token
      case _ =>
        // if we have a NoneCommentLine then must be either
        //           operation operant + optional comment
        //       or a command
        // lets deal with the potential comment first
        val commentSplit = line.source.split(";")

        // remove comment and split rest of line into fields using space though these this could also be seperated by ,
        val fields =
          if commentSplit.length > 1 then
            // we have split so must be a ; and therefore a comment which should be at the end of the line
            tokenisedLine + Token(AssemblerTokenType.LineComment, commentSplit.tail.mkString)
            commentSplit.head.split("\\s+")
          else
            line.source.split("\\s+")

        // command | command value | command value[ ],[ ]value | Label |  Label instruction
        if !processCommand(fields, tokenisedLine) then
          val instruction = processLabel(fields, tokenisedLine)
          if !instruction.isEmpty then
            val tokenisedInstruction = processInstruction(instruction, tokenisedLine)
            if tokenisedInstruction.typeOfToken == AssemblerTokenType.InstructionToken then
              processValue(instruction.tail, tokenisedLine, tokenisedInstruction)

    tokenisedLine


  private def processLabel(text: Array[String], tokenisedLine: TokenisedLine ) : Array[String] =
    logger.debug(s"processLabel: ${text.mkString(" ")}")
    val head = text.head
    if head.takeRight(1) == ":" then
      val labelText = head.dropRight(1)
      val token = Token(AssemblerTokenType.LabelToken, labelText)
      tokenisedLine + token
      logger.debug(s"token added: $token")
      text.tail
    else
      text


  private def processCommand(text: Array[String], tokenisedLine: TokenisedLine ) : Boolean =

    def
    getReferenceToken( value: String ): Token =
      logger.debug(s"value - $value")
      if value == null || value.isEmpty then
        Token(AssemblerTokenType.SyntaxErrorToken, "Value not given")
      else
        if isLable(value) then
          // is a label so a reference value
          Token(AssemblerTokenType.ReferenceToken, value)
        else
          if isNumeric(value) then
            Token(AssemblerTokenType.ValueToken, value)
          else
            Token(AssemblerTokenType.SyntaxErrorToken, value)


    logger.debug(s"processCommand: ${text.mkString(" ")}")
    if !text.isEmpty then
      val head = text.head.toUpperCase
      head match {
        case "ADDR" | "BYT" | "WRD" =>
          val data: Array[String] = text.tail
          // At this point we could have the values split on space or not split at all.
          // So we need to look at each value and determine if it can be further split.
          // the easy way join all back together (the previous split will have removed the spaces)
          // Now we have a value that wne split on , will be have no spaces.
          val values = data.mkString("")
          val token = Token(AssemblerTokenType.CommandToken, head, TokenValue(PredictedAddressingModes.NotApplicable, data.mkString("")))
          tokenisedLine + token
          logger.debug(s"token added: $token")
          return true

        case "ORIG" =>
          val value: Array[String] = text.tail
          if value.length == 1 then
            val str = value(0).trim
            if Utilities.isNumeric(str) then
              val token = Token(AssemblerTokenType.OriginToken, str)
              tokenisedLine + token
              logger.debug(s"token added: $token")
            else
              tokenisedLine + Token(AssemblerTokenType.SyntaxErrorToken, "Value for ORIG not numeric")
          else
            tokenisedLine + Token(AssemblerTokenType.SyntaxErrorToken, "Invalid ORIG command!")
          return true

        // clr only valid on the first line
        case "CLR" =>
          val token = Token(AssemblerTokenType.ClearToken, head)
          tokenisedLine + token
          if tokenisedLine.sourceLine.lineNumber > 1 then
            tokenisedLine + Token(AssemblerTokenType.SyntaxErrorToken, head)
          AssemblyData.clear()
          logger.debug(s"token added: $token")
          return true

        case _ =>
      }
    false


  def processInstruction(text: Array[String], tokenisedLine: TokenisedLine ) : Token =
    logger.debug(s"processInstruction: ${text.mkString(" ")}")
    val instruction = text.head.toUpperCase()
    val token = if CpuInstructions.isValidInstruction(instruction) then
      Token(AssemblerTokenType.InstructionToken, instruction)
    else
      Token(AssemblerTokenType.SyntaxErrorToken, s"Invalid instruction: $instruction")
    tokenisedLine + token
    token

  def processValue(text: Array[String], tokenisedLine: TokenisedLine, token: Token ): Unit = {
    logger.debug(s"processValue: ${text.mkString(" ")}")

    logger.debug(s"No operand for ${token.tokenStr}")
    // Possible values and associated adressing mode:
    //      missing - just the instruction them accumilator or implied
    //      numeric - starts with a digit or $ for hex - absolute or zero page
    //      label - starts with an alph but not ( absolute mode to label
    //      imeadiate address mode starts with #
    //      indirect addresing mode starts with ( some for of indexed addressing
    //      nothing implied addressing

    // At this point we only need to tokenise the addressign mode not work out if its valid.
    if text.isEmpty then // applied addrtessing mode
      token.tokenVal.pridictedMode = PredictedAddressingModes.AccumulatorOrImplied
    else if text.length == 1 then
      text(0) match {
        case a if a.charAt(0) == '#' => token.tokenVal.pridictedMode = getPredition(a.substring(1), 10)
        case b if b.charAt(0) == '$' => token.tokenVal.pridictedMode = getPredition(b.substring(1), 16)
        case c if c.charAt(0).isDigit => token.tokenVal.pridictedMode = getPredition(c.substring(1), 10)
        case c if c.charAt(0).isLetter => token.tokenVal.pridictedMode = getPredition(c.substring(1), 16)
        case d if d.charAt(0) == '(' => token.tokenVal.pridictedMode = PredictedAddressingModes.Indirect
        case _ => token.tokenVal.pridictedMode = PredictedAddressingModes.NoPricitions
      }
  }

  def getPredition(t: String, base: Int) : PredictedAddressingModes =
    try {
      val value = Integer.parseInt(t, base)
      if value > 256 then
        PredictedAddressingModes.Absolute
      else
        PredictedAddressingModes.ZeroPage
    } catch {
      case _: Throwable => PredictedAddressingModes.Absolute
    }

