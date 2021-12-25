package com.udsl.processor6502.assembler

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Utilities
import com.udsl.processor6502.Utilities.*
import com.udsl.processor6502.cpu.execution.*
import com.udsl.processor6502.cpu.CpuInstructions
import com.udsl.processor6502.cpu.execution.{Absolute, AddressingMode, ZeroPage}
import com.udsl.processor6502.assembler.{AssemblerToken, BlankLineToken, ClearToken, CommandToken, InstructionToken, LabelToken, OriginToken, SyntaxErrorToken}

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
    tokenisedLine.tokens.addOne(ExceptionToken(exceptionMessage, Array[String]()))
    tokenisedLines.addOne(tokenisedLine)

  def tokeniseLine(line: UntokenisedLine): TokenisedLine =
    logger.debug(s"\ntokeniseLine: $line")
    // determine basic line type
    val tokenisedLine = TokenisedLine(line)
    val token: AssemblerToken = line.source.trim match {
      case "" => BlankLineToken( "", Array[String]())
      case a if a.charAt(0) == ';' => CommentLineToken(line.source.trim, Array[String]())
      case _ => NoneCommentLine(line.source.trim, Array[String]())
    }

    token match
      case BlankLineToken( _, _ ) |  CommentLineToken( _, _ ) =>
        tokenisedLine + token
      case _ =>
        // if we have a NoneCommentLine then must be either
        //       nememic operand + optional comment
        //       or a command
        // lets deal with the potential comment first
        val commentSplit = line.source.trim.split(";")

        // remove comment and split rest of line into fields using space though these this could also be seperated by ,
        val fields =
          if commentSplit.length > 1 then
            // we have split so must be a ; and therefore a comment which should be at the end of the line
            tokenisedLine + LineComment(commentSplit.tail.mkString, commentSplit)
            commentSplit.head.split("\\s+")
          else
            line.source.split("\\s+")

        // command | command value | command value[ ],[ ]value | Label |  Label instruction
        if !processCommand(fields, tokenisedLine) then
          val instruction = processLabel(fields, tokenisedLine)
          if !instruction.isEmpty then
            val tokenisedInstruction = processInstruction(instruction, tokenisedLine)
            if tokenisedInstruction.isInstanceOf[InstructionToken] then
              processValue(tokenisedLine, tokenisedInstruction)

    tokenisedLine


  private def processLabel(text: Array[String], tokenisedLine: TokenisedLine ) : Array[String] =
    logger.debug(s"processLabel: ${text.mkString(" ")}")
    val head = text.head
    if head.substring(head.length - 1) == ":" then
      val labelText = head.substring(0, head.length - 1)
      AssemblyData.addLabel(labelText)
      val token = LabelToken(labelText, text.tail)
      tokenisedLine + token
      logger.debug(s"token added: $token")
      text.tail
    else
      text


  private def processCommand(text: Array[String], tokenisedLine: TokenisedLine ) : Boolean =

//    def getReferenceToken( value: String ): AssemblerToken =
//      logger.debug(s"value - $value")
//      if value == null || value.isEmpty then
//        SyntaxErrorToken( "Value not given")
//      else
//        if isLable(value) then
//          // is a label so a reference value
//          ReferenceToken( value)
//        else
//          if isNumeric(value) then
//            ValueToken( value)
//          else
//            SyntaxErrorToken( value)


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
          val token = CommandToken(head, values.split(","))
          token.addPrediction(Unknown)
          tokenisedLine + token
          logger.debug(s"token added: $token")
          return true

        case "ORIG" =>
          val value: Array[String] = text.tail
          if value.length == 1 then
            val str = value(0).trim
            if Utilities.isNumeric(str) then
              val token = OriginToken(str, value)
              tokenisedLine + token
              logger.info(s"Origin added from numeric literal $str")
            else if Utilities.isLabel(str) && AssemblyData.labelIsDefined(str) then
              val labelValue = AssemblyData.labelValue(str).toString
              val token = OriginToken(labelValue, value)
              tokenisedLine + token
              logger.info(s"Origin added from defined label '$str")
            else
              tokenisedLine + SyntaxErrorToken( "Value for ORIG not numeric or defined label", value)
          else
            tokenisedLine + SyntaxErrorToken("Invalid ORIG command!", value)
          return true

        // clr only valid on the first line
        case "CLR" =>
          val token = ClearToken(head, text.tail)
          tokenisedLine + token
          if tokenisedLine.sourceLine.lineNumber > 1 then
            tokenisedLine + SyntaxErrorToken(head, text.tail)
          AssemblyData.clear()
          logger.debug(s"token added: $token")
          return true

        case "DEF" =>
          val parts: Array[String] = text.tail
          // fist part must be the label being defined
          // 2nd is the value which must not be a label
          if parts.length != 2  then
            tokenisedLine + SyntaxErrorToken("Bad DEF", text)
          else if !isLabel(parts(0)) || !isNumeric(parts(1)) then
            tokenisedLine + SyntaxErrorToken("DEF should be label number", text)
          else
            val value = numericValue(parts(1))
            if (0 to 65535 contains value) then
              AssemblyData.addLabel(parts(0), value)
              val token = DefToken(parts(0), Array[String](parts(1)))
              token.value = parts(1)
              tokenisedLine + token
              logger.debug(s"token added: $token")
            else
              tokenisedLine + SyntaxErrorToken(s"Invalid defined value ${parts(0)} - ${parts(1)}", text)
          return true

        case _ =>
      }
    false


  def processInstruction(text: Array[String], tokenisedLine: TokenisedLine ) : AssemblerToken =
    logger.debug(s"processInstruction: ${text.mkString(" ")}")
    val instruction = text.head.toUpperCase()
    val token = if CpuInstructions.isValidInstruction(instruction) then
      val tail = text.tail
      if tail.length > 1 then
        val head = tail.head
        val fields = if head.substring(head.length() - 1).equals(",") then
          Array[String](tail.mkString)
        else
          text.tail
        InstructionToken(instruction, fields)
      else
        InstructionToken(instruction, text.tail)
    else
      SyntaxErrorToken( s"Invalid instruction: $instruction", text)
    tokenisedLine + token
    token

  def processValue(tokenisedLine: TokenisedLine, token: AssemblerToken ): Unit = {
    logger.debug(s"processValue: ${token.fields}")

    logger.debug(s"No operand for ${token}")
    // Possible values and associated addressing mode:
    //      missing - just the instruction them accumilator or implied
    //      numeric - starts with a digit or $ for hex - absolute or zero page
    //      label - starts with an alph but not ( absolute mode to label
    //      imeadiate address mode starts with #
    //      indirect addresing mode starts with ( some for of indexed addressing
    //      nothing implied addressing

    // At this point we only need to tokenise the addressign mode not work out if its valid.
    if token.fields.isEmpty then // applied addressing mode
      token.addPredictions(List(Accumulator, Implied))
    else
      token.fields.head match {
        case a if a.charAt(0) == '#' => token.addPrediction(Immediate)
        case b if b.charAt(0) == '$' => token.addPredictions(getPredictions(b.substring(1), 16))
        case c if c.charAt(0).isDigit => token.addPredictions(getPredictions(c, 10))
        case d if d.charAt(0).isLetter => token.addPredictions(getLabelPredictions(d))
        case e if e.charAt(0) == '(' => token.addPredictions(getIndirectPredictions(e))
        case _ => token.addPrediction(Unknown)
      }
  }

  /**
   * ind	indirect	OPC ($LLHH)	operand is address; effective address is contents of word at address: C.w($HHLL)
   * X,ind	X-indexed, indirect	OPC ($LL,X)	operand is zeropage address; effective address is word in (LL + X, LL + X + 1), inc. without carry: C.w($00LL + X)
   * ind,Y	indirect, Y-indexed	OPC ($LL),Y	operand is zeropage address; effective address is word in (LL, LL + 1) incremented by Y with carry: C.w($00LL) + Y
   *
   * @param t the string representing the operand
   * @return a prediction of the real addressing mode, we only need a prediction. The 2nd pass will have to do the full calc anyway.
   *         The prediction just goves it a starting point saves a bit of time.
   *
   */
  def getIndirectPredictions(t: String) : List[AddressingMode] =
    // lastChar should be ) or Y
    val lastChar = t.substring(t.length() - 1).toUpperCase()
    lastChar match {
      case ")" => List(Indirect, IndirectX)
      case "Y" => List(IndirectY)
      case _ => List(Unknown)
    }

  /**
   * The operand starts with a letter so it could be almost anny mode with the address being defined with a DEF later in the file
   * so at this point we have no idea if the label even exists! Therefore no way to tell if zero page can only determine if X or Y indexed.
   *
   * All indirect start with ( so cant be any of those and Implied has no operand so that also not possible.
   * Immediate is dealt with prior to calling this method also not possible
   *
   * The indexed options
   * abs,X	absolute, X-indexed	OPC $LLHH,X	operand is address; effective address is address incremented by X with carry
   * abs,Y	absolute, Y-indexed	OPC $LLHH,Y	operand is address; effective address is address incremented by Y with carry
   * zpg,X	zeropage, X-indexed	OPC $LL,X	operand is zeropage address; effective address is address incremented by X without carry
   * zpg,Y	zeropage, Y-indexed	OPC $LL,Y	operand is zeropage address; effective address is address incremented by Y without carry
   *
   * None index options
   * abs	absolute	OPC $LLHH	operand is address $HHLL
   * rel	relative	OPC $BB	branch target is PC + signed offset BB
   * zpg	zeropage	OPC $LL	operand is zeropage address (hi-byte is zero, address = $00LL)
   *
   *
   * @param t the string representing the operand (label)
   * @returna prediction of the real addressing mode, we only need a prediction. The 2nd pass will have to do the full calc anyway.
   *         The prediction just gives it a starting point saves a bit of time.
   */
  def getLabelPredictions(t: String) : List[AddressingMode] =
    // labels can end with an X or a Y but cant contain a comma therefor indexing is indicated by ',X' or ',Y' termination
    val lastChars = t.substring(t.length() - 2).toUpperCase()
    if lastChars == ",X" then
      List(AbsoluteX, ZeroPageX)
    else if lastChars == ",Y" then
      List(AbsoluteY, ZeroPageY)
    else
      List(Absolute, Relative, ZeroPage)

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
   * @param t the string representing the operand
   * @param base if base of the number Decimal or hex
   * @return a prediction of the real addressing mode, we only need a prediction. The 2nd pass will have to do the full calc anyway.
   *         The prediction just goves it a starting point saves a bit of time.
   */
  def getPredictions(t: String, base: Int) : List[AddressingMode] =
    val lastChars = t.substring(t.length() - 2).toUpperCase()
    val indexed = lastChars == ",X" || lastChars == ",Y"
    val valStr = if indexed then
      t.substring(t.length - 2)
    else
      t
    val value = Integer.parseInt(valStr, base)
    if value > 256 then
      lastChars match
        case ",X" => List(AbsoluteX)
        case ",Y" => List(AbsoluteY)
        case _ => List(Absolute)
    else
      lastChars match
        case ",X" => List(ZeroPageX)
        case ",Y" => List(ZeroPageY)
        case _ => List(ZeroPage, Relative)


