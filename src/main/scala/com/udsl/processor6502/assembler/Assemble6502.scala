package com.udsl.processor6502.assembler

import com.udsl.processor6502.assembler.AssembleLocation.currentLocation
import com.udsl.processor6502.assembler.AssemblerTokenType.{BlankLineToken, CommentLineToken, LabelToken}
import com.udsl.processor6502.cpu.Processor

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import com.typesafe.scalalogging.{LazyLogging, StrictLogging}
import com.udsl.processor6502.Utilities.errorAlert


/**
 * The class produced by the Assemble6502 apply(source: String) method.
 * The apply takes the source string and tockenises it, notmally this would be the job of pass one but this is not
 * an efficency excersise it a sue Scala and ScalaFx excersise so not woried about efficiency :)
 *
 * @param tokenisedLines the source lines tokenised.
 */
class Assemble6502( val tokenisedLines: List[TokenisedLine]) extends StrictLogging {

  def assemble(): Unit =
    try {
      Assemble6502FirstPass.apply(tokenisedLines)
      printTokenisedLines()
      Assemble6502.printLabels()
    }catch {
      case e: Exception => errorAlert(e.getMessage)
      case _  => errorAlert("Unexpected error")
    }

  def printTokenisedLines(): Unit =
    logger.info("\n\nTokenisedLines\n")
    if (tokenisedLines.isEmpty) logger.info("No lines tokenised") else for (line <- tokenisedLines) {
      logger.info(line.toString)
    }
    logger.info("\nend TokenisedLines\n\n")
}

/**
 * This object hold and maintains the current location of the assmbly.
 * As each assembly needs to know this it makes sence for it to be an object.
 * That way any method can have access.
 */
object AssembleLocation extends StrictLogging :
  // The point in memery we are assembling to.
  var currentLocation: Int = 0

  def setAssembleLoc(l: Int):Unit =
    if l > 65535 || l < 0 then
      throw new Exception(s"Bad assmbler Location ${l} ")
    else
      currentLocation = l
      logger.info(s" assmbler Location = ${l} ")

  def setMemoryWord(v: Int): Unit =
    if v > 65535 || v < 0 then
      val errorMessage = s"Bad word value ${v}"
      logger.debug(errorMessage)
      throw new Exception(errorMessage)
    Processor.setMemoryByte(currentLocation, v / 256)
    currentLocation += 1
    Processor.setMemoryByte(currentLocation, v % 256)
    currentLocation += 1

  def setMemoryAddress(adr: Int): Unit =
    if adr > 65535 || adr < 0 then
      val errorMessage = s"Bad address value ${adr}"
      logger.debug(errorMessage)
      throw new Exception(errorMessage)
    Processor.setMemoryByte(currentLocation, adr % 256)
    currentLocation += 1
    Processor.setMemoryByte(currentLocation, adr / 256)
    currentLocation += 1

  def setMemoryByte(v: Int): Unit =
    if v > 256 || v < 0 then
      throw new Exception("Not a byt value")
    Processor.setMemoryByte(currentLocation, v)
    currentLocation += 1


/**
 * The assembler common parts.
 */
object Assemble6502 extends StrictLogging :
  // lebel defined in the companion object so they common to each instance
  // this enables multi file assembly
  val labels = new mutable.HashMap[String, Int]()

  def apply(source: String): Assemble6502 =
    val allLines = for ((str, index) <- source.split("\n").zipWithIndex)
      yield new UntokenisedLine(index + 1, str)
    val asm = new Assemble6502(Tokeniser.Tokenise(allLines) )
    asm

  def printLabels(): Unit =
    
    if (labels.isEmpty) logger.info("No labels defined") else for ((label, address) <- labels) {
      logger.info(s"${label} address ${address}")
    }

  def addLabel(name: String): Unit =
    if (labels.contains(name))
      throw new Exception("Label already defined")
    labels.addOne(name, currentLocation)


/**
 * Fisrt pass object - as one would expect does the first pass which resolves any forward references.
 */
object Assemble6502FirstPass extends StrictLogging :

  def apply(tokenisedLines: List[TokenisedLine]): Unit =
    for (tokenisedLine <- tokenisedLines)
      try
        assemble(tokenisedLine)
      catch
        case e: Exception => throw new Exception(s"${e.getMessage}\nOn line ${tokenisedLine.sourceLine.lineNumber}" )
        case a => logger.error(s"Unknown exception! ${a}\nOn line ${tokenisedLine.sourceLine.lineNumber}")

  def assemble(tokenisedLine: TokenisedLine) : Unit =
    logger.info(s"line ${tokenisedLine.sourceLine.lineNumber} ")
    for (token <- tokenisedLine.tokens)
      token.typeOfToken match {
        case AssemblerTokenType.BlankLineToken => // extends AssemblerTokenType("BlankLineToken")
          logger.info("BlankLineToken ")
        case AssemblerTokenType.CommentLineToken => // extends AssemblerTokenType("CommentLineToken")
          AssembleCommentLineToken(token)
        case AssemblerTokenType.LineComment => // extends AssemblerTokenType("LineComment")
          logger.info("LineComment ")
        case AssemblerTokenType.NoneCommentLine => // extends AssemblerTokenType("NoneCommentLine")
          logger.info("NoneCommentLine ")
        case AssemblerTokenType.LabelToken => // extends AssemblerTokenType("LabelToken")
          procesLabel(token)
        case AssemblerTokenType.CommandToken => // extends AssemblerTokenType("CommandToken")
          AssembleCommandToken(token)
        case AssemblerTokenType.InstructionToken => // extends AssemblerTokenType("InstructionToken")
          AssembleInstructionToken(token)
        case AssemblerTokenType.SyntaxErrorToken => // extends AssemblerTokenType("SyntaxErrorToken")
          logger.info("SyntaxErrorToken ")
        case AssemblerTokenType.ClearToken =>
          logger.info("Clear Token")
          processClear(token, tokenisedLine)
      }
    logger.info(tokenisedLine.sourceLine.source)

  def AssembleCommentLineToken(t: Token) : Unit =
    logger.info(s"CommentLineToken '${t.tokenVal.strVal}' - ")

  def AssembleCommandToken(t: Token) : Unit =
    logger.info(s"CommandToken '${t.tokenVal.strVal}' - ")
    t.tokenStr.toUpperCase() match
      case "ORIG" => AssembleLocation.setAssembleLoc(t.intValue)
      case "BYT" => setBytes(t.tokenVal.strVal)
      case "WRD" => setWords(t.tokenVal.strVal)
      case "ADDR" => setAddresses(t.tokenVal.strVal)
      case _ => logger.info(s" Invalid command ${t.tokenStr} ")

  def processClear(t: Token, tl: TokenisedLine) : Unit =
    logger.info("Processing CLR command")
    if (tl.sourceLine.lineNumber > 1) then
      val errorText = "CLR command only valid on first line"
      logger.error(errorText)
      throw new Exception(errorText)

  def AssembleInstructionToken(t: Token) : Unit =
    logger.info(s"InstructionToken '${t.tokenVal.strVal}' - location: ${currentLocation}")

  def procesLabel(t: Token) : Unit =
    logger.info(s"Defining label ${t.tokenStr} with value ${currentLocation}")
    Assemble6502.addLabel(t.tokenStr)

  def setBytes(v: String): Unit =
    logger.info("setBytes")
    val values = v.split(",")
    for (value <- values)
      setMemoryByte(value.trim)

  def setWords(v: String): Unit =
    logger.info("setWords")
    val values = v.split(",")
    for (value <- values)
      setMemoryWord(value.trim)

  def setMemoryWord(v: String): Unit =
    if v.charAt(0).isLetter then // a label
      addFowardReference(v)
      AssembleLocation.setMemoryWord(0x6363) // word value for 99, 99 decimal
    else
      AssembleLocation.setMemoryWord(if v.charAt(0) == '$' then
        Integer.parseInt(v.substring(1), 16)
      else
        Integer.parseInt(v))

  def setAddresses(v: String): Unit =
    logger.info("setAddresses")
    val values = v.split(",")
    for (value <- values)
      setMemoryAddress(value.trim)

  def setMemoryAddress(v: String): Unit =
    AssembleLocation.setMemoryAddress( if v.charAt(0) == '$' then
      Integer.parseInt(v.substring(1), 16)
    else
      Integer.parseInt(v))

  def setMemoryByte(v: String): Unit =
    AssembleLocation.setMemoryByte(if v.charAt(0) == '$' then
      Integer.parseInt(v.substring(1), 16)
    else
      Integer.parseInt(v))


  //TODO
  def addFowardReference(ref: String): Unit =
    logger.info(s"addFowardReference to '${ref}' @ ${currentLocation}")
