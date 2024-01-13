package com.udsl.processor6502.assembler.version1

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Utilities
import com.udsl.processor6502.Utilities.{getExpression, isExpression, isLabel, isNumeric, numericValue}
import com.udsl.processor6502.assembler.AssembleLocation.currentLocation
import com.udsl.processor6502.assembler.{AssemblyData, *}
import com.udsl.processor6502.assembler.AssemblyData.*
import com.udsl.processor6502.cpu.CpuInstructions
import com.udsl.processor6502.cpu.execution.*

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object TokeniserV1 extends StrictLogging :

  def Tokenise(allLines: Array[SourceLine]): List[TokenisedLineV1] =
    logger.info(
      """
        |**************************
        |*                        *
        |*   Start Tokenisation   *
        |*                        *
        |**************************
        |
        |""".stripMargin)
    val tokenisedLines = new ListBuffer[TokenisedLineV1]()
    for lineToTokenise <- allLines do
      Try(tokeniseLine(lineToTokenise)) match
        case Success(line) => tokenisedLines.addOne(line)
        case Failure(ex) => addError(ErrorRecord(ex.getMessage, lineToTokenise))
    tokenisedLines.toList

  def tokeniseLine(line: SourceLine): TokenisedLineV1 =
    logger.debug(s"\ntokeniseLine: $line")
    // determine basic line type
    implicit val tokenisedLine: TokenisedLineV1 = TokenisedLineV1(line)
    val token: Option[AssemblerToken] = line.text.trim match {
      case "" => Some(BlankLineToken( "", Array[String](), line))
      case a if a.charAt(0) == ';' =>
        line.removeComment()
        Some(CommentLineToken(line.text.trim, Array[String](), line))
      case b if b.contains(";")  =>
        line.removeComment()
        Some(LineComment(line.text.trim, Array[String](), line))
      case _ => None
    }

    if token.nonEmpty then
      tokenisedLine + token.get

    // if we have a text then must be one of these formats where [is optional]
    // if not then line must have been a comment and no more processing required.
    //       [label:] nenemic operand
    //       command [command params]
    if line.hasText then
      // split line into fields using space though these could also be seperated by ,
      val fields = line.text.split("\\s+")
  
      // [LABEL:] command | command value | command value[ ],[ ]value | Label |  Label instruction
      processValue( processInstruction( processCommand( processLabel(fields) ) ) )
  
    tokenisedLine


  private def processLabel(text: Array[String])(using tokenisedLine: TokenisedLineV1) : Array[String] =
    logger.debug(s"processLabel: ${text.mkString(" ")}")
    val head: String = text.head
    if head != "" && head.endsWith(":") then
      val labelText = head.dropRight(1)
      if !LabelFactory.addLabel(labelText, currentLocation) then
        addError(ErrorRecord(s"failed to add label $labelText", tokenisedLine.source))
      val token = LabelToken(labelText, text.tail, tokenisedLine.source)
      tokenisedLine + token
      logger.debug(s"token added: $token")
      text.tail
    else
      text


  /**
   * Method to process commands into tokens
   * @param text fields of the line
   * @param tokenisedLine we add additional tokens to this
   * @return returns the list of fields left to process otherwise empty list
   */
  private def processCommand(text: Array[String])(using tokenisedLine: TokenisedLineV1) : Array[String]  =
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
          val token = CommandToken(head, values.split(","), tokenisedLine.source)
          token.addPrediction(Unknown)
          tokenisedLine + token
          logger.debug(s"token added: $token")
          return Array.empty

        case "TXT" =>
          // remove the TXT and the ' from the source, result is the txt and the ending '
          // we nned the ending ' incase we have a comment following
          val textValue = tokenisedLine.source.OriginalText.substring(tokenisedLine.source.OriginalText.indexOf("'") + 1)
          val token = CommandToken(head, Array(textValue.substring(0,textValue.length-1)), tokenisedLine.source)
          token.addPrediction(Unknown)
          tokenisedLine + token
          logger.debug(s"TXT token added: $token")
          return Array.empty

        case "ORIG" =>
          val value: Array[String] = text.tail
          if value.length == 1 then
            val str = value(0).trim
            if Utilities.isNumeric(str) then
              val token = OriginToken(str, value, tokenisedLine.source)
              tokenisedLine + token
              logger.info(s"Origin added from numeric literal $str")
            else if Utilities.isLabel(str) && !LabelFactory.labelIsDefined(str) then
              val labelValue = LabelFactory.labelValue(str).toString
              val token = OriginToken(labelValue, value, tokenisedLine.source)
              tokenisedLine + token
              logger.info(s"Origin added from defined label '$str")
            else
              AssemblyData.addError(ErrorRecord.apply("Value for ORIG not numeric or defined label", tokenisedLine.source))
          else
            AssemblyData.addError(ErrorRecord.apply("Invalid ORIG command!", tokenisedLine.source))
          return Array.empty

        // clr only valid on the first line
        case "CLR" =>
          val token = ClearToken(head, text.tail, tokenisedLine.source)
          tokenisedLine + token
          if tokenisedLine.source.lineNum > 1 then
            AssemblyData.addError(ErrorRecord.apply("clr only valid on the first line", tokenisedLine.source))
          clear()
          logger.debug(s"token added: $token")
          return Array.empty

        case "DEF" =>
          val parts: Array[String] = text.tail
          // fist part must be the label being defined
          // 2nd is the value which is an expression that can contain a label
          if parts.tail.isEmpty then
            AssemblyData.addError(ErrorRecord.apply("Bad DEF - no expression", tokenisedLine.source))
          else if !isLabel(parts.head) then
            addError(ErrorRecord("DEF should define a label", tokenisedLine.source))
          // if we have a tain greate than length 1 then it must be a expression with spaces  
          else if parts.tail.length > 1 && !isExpression(parts.tail.mkString(" ")) then
            addError(ErrorRecord("DEF definition should be an expression, a label or a number", tokenisedLine.source))
          // If its only length 1 it could be an expression without spaces
          else if isExpression(parts.tail.mkString(" ")) then
            // process the expression
            getExpression(parts.tail.mkString(" "))
            
          else
            if isLabel(parts(1)) then
              if !LabelFactory.addLabel(parts(0), parts.tail) then // add label as an expression because it could be a forward reference
                addError(ErrorRecord(s"failed to add label DEF ${parts(0)}", tokenisedLine.source))
              // As this could be a fowards reference and could have already been defined by a previous forward ref no need to check if we are adding it
              LabelFactory.addLabel(parts(1))
              val token = DefToken(parts(0), Array[String](parts(1)), tokenisedLine.source)
              token.value = parts(1)
              tokenisedLine + token
              logger.debug(s"token added: $token")
            else
              val value = numericValue(parts(1))
              if 0 to 65535 contains value.get then
                if !LabelFactory.addLabel(parts(0), value.get) then
                  addError(ErrorRecord(s"failed to add label DEF val${parts(0)}", tokenisedLine.source))
                val token = DefToken(parts(0), Array[String](parts(1)), tokenisedLine.source)
                token.value = parts(1)
                tokenisedLine + token
                logger.debug(s"token added: $token")
              else
                addError(ErrorRecord(s"DEF defining ${parts(0)} with value $value", tokenisedLine.source))

          return Array.empty

        case _ =>
      }
    text



  def processInstruction(text: Array[String])(using tokenisedLine: TokenisedLineV1)  : AssemblerToken =
    logger.debug(s"processInstruction: '${tokenisedLine.source}'")
    if text.isEmpty then return NoTokenToken("", text, tokenisedLine.source)
    
    val instruction = text.head.toUpperCase()

    def getToken(values: Array[String]): AssemblerToken =
      if values.length > 1 then
        val head = values.head
        val fields = if head.substring(head.length() - 1).equals(",") then
          Array[String](values.mkString)
        else
          values.tail
        InstructionToken(instruction, fields, tokenisedLine.source)
      else
        InstructionToken(instruction, values, tokenisedLine.source)

    if !CpuInstructions.isValidInstruction(instruction) then
      addError(ErrorRecord(s"Invalid instruction: $instruction", tokenisedLine.source))
      NoTokenToken(instruction, text.tail, tokenisedLine.source)
    else
      val token = getToken(text.tail)
      tokenisedLine + token
      token

  def processValue(token: AssemblerToken)(using tokenisedLine: TokenisedLineV1) : Unit = {
    logger.debug(s"processValue: ${token.fields.mkString("Array(", ", ", ")")}")
    if token.isInstanceOf[InstructionToken] then
      /*
        Possible values and associated addressing mode:
            missing - just the instruction them accumilator or implied
            numeric - starts with a digit or $ for hex - absolute or zero page
            label - starts with an alph but not ( absolute mode to label
        imeadiate address mode starts with #
        indirect addresing mode starts with ( some for of indexed addressing
        nothing implied addressing

        At this point we only need to tokenise the addressing mode not work out if its valid.
      */
      if token.fields.isEmpty then // Implied addressing mode
        logger.debug(s"No operand for ${token.mnemonic} Implied?")
        token.addPredictions(List(Implied))
      else
        token.fields.head match {
          case l if l.toUpperCase == "A"=> token.addPrediction(Accumulator)
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
   * @return prediction of the real addressing mode, we only need a prediction. The 2nd pass will have to do the full calc anyway.
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


