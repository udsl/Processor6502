package com.udsl.processor6502.assembler

import com.typesafe.scalalogging.LazyLogging
import com.udsl.processor6502.assembler.AssemblerTokenType.{BlankLineToken, CommentLineToken}
//import org.apache.log4j.Logger // Log4j 1
//import org.apache.logging.log4j.LogManager // Log4j 2
//import org.apache.logging.log4j.Logger // Log4j 2
import scala.collection.mutable.ListBuffer

object Tokeniser extends LazyLogging :
//  private val logger = Logger.getLogger(getClass.getName) // Log4j 1
//  private val logger: Logger = LogManager.getLogger(Tokeniser.getClass.getName) // Log4j 2

  val validInstructions = List("ORA","AND","EOR","ADC","STA","LDA","CMP","SBC","ASL","ROL","LSR","ROR","STX","LDX","DEC","INC","BIT","JMP","lue","JMP","STY","LDY","CPY","CPX")

  val tokenisedLines = new ListBuffer[TokenisedLine]()

  def Tokenise(allLines: Array[UntokenisedLine]): List[TokenisedLine] =
    for lineToTokenise <- allLines do
      tokenisedLines.addOne(tokeniseLine(lineToTokenise))
    tokenisedLines.toList

  def tokeniseLine(line: UntokenisedLine) =
    logger.info(s"\ntokeniseLine: ${line}")
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

        // remove comment and split rest of line into fields
        val fields =
          if commentSplit.length > 1 then
            // we have a comment
            tokenisedLine + Token(AssemblerTokenType.LineComment, commentSplit.tail.mkString)
            commentSplit.head.split("\\s+")
          else
            line.source.split("\\s+")

        // command | Label |  Label instruction
        if !processCommand(fields, tokenisedLine) then
          val instruction = processLabel(fields, tokenisedLine)
          if !instruction.isEmpty then
            val tokenisedInstruction = processInstruction(instruction, tokenisedLine)
            if tokenisedInstruction.typeOfToken == AssemblerTokenType.InstructionToken then
              processValue(instruction.tail, tokenisedLine, tokenisedInstruction)

    tokenisedLine


  private def processLabel(text: Array[String], tokenisedLine: TokenisedLine ) : Array[String] =
    logger.info(s"processLabel: ${text.mkString(" ")}")
    val head = text.head
    if head.takeRight(1) == ":" then
      val labelText = head.dropRight(1)
      val token = Token(AssemblerTokenType.LabelToken, labelText)
      tokenisedLine + token
      logger.info(s"token added: ${token}")
      text.tail
    else
      text


  private def processCommand(text: Array[String], tokenisedLine: TokenisedLine ) : Boolean =
    logger.info(s"processCommand: ${text.mkString(" ")}")
    if !text.isEmpty then
      val head = text.head.toUpperCase
      head match {
        case "ORIG" | "BYT" | "WRD" => {
          val value = text.tail
          val token = if value.isEmpty then
            Token(AssemblerTokenType.SyntaxErrorToken, "Value not given")
          else
            Token(AssemblerTokenType.CommandToken, head, TokenValue(value))
          tokenisedLine + token
          logger.info(s"token added: ${token}")
          return true
        }

        case _ => {
        }
      }
    return false


  def processInstruction(text: Array[String], tokenisedLine: TokenisedLine ) : Token =
    logger.info(s"processInstruction: ${text.mkString(" ")}")
    val instruction = text.head.toUpperCase()
    val token = if validInstructions.contains(instruction) then
      Token(AssemblerTokenType.InstructionToken, instruction)
    else
      Token(AssemblerTokenType.SyntaxErrorToken, s"Invalid instruction: ${instruction}")
    tokenisedLine + token
    token

  def processValue(text: Array[String], tokenisedLine: TokenisedLine, token: Token ) =
    logger.info(s"processValue: ${text.mkString(" ")}")

    logger.info(s"No operand for ${token.tokenStr}")
    // Possible values and associated adressing mode:
    //      numeric - starts with a digit or $ for hex - absolute or zero page
    //      label - starts with an alph but not ( absolute mode to label
    //      imeadiate address mode starts with #
    //      indirect addresing mode starts with ( some for of indexed addressing
    //      nothing implied addressing

    // At this point we only need to tokenise the addressign mode not work out if its valid.
    if text.isEmpty then // applied addrtessing mode
      token.tokenVal.pridictedMode = PredictedAddressingModes.Immediate
    else
      token.tokenVal.pridictedMode = PredictedAddressingModes.AbsoluteXOrZeroPageX


