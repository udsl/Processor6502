package com.udsl.processor6502.assembler

import com.udsl.processor6502.assembler.AssembleLocation.currentLocation
import com.udsl.processor6502.assembler.AssemblerTokenType.{BlankLineToken, CommentLineToken, LabelToken, ExceptionToken, SyntaxErrorToken}
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
    if hasSyntaxError() || hasException() then
      errorAlert("Assemly Error", "Syntax or exceptions encontered")
      listExceptions()
      listSyntaxErrors()
    else
      Assemble6502FirstPass.apply(tokenisedLines)
      Assemble6502SecondPass.apply(tokenisedLines)
      printTokenisedLines()
      Assemble6502.printLabels()

  def listExceptions(): Unit =
    val exceptions = tokenisedLines.filter(x => x.tokens.exists(p = y => y.typeOfToken == ExceptionToken))
    logger.info(
      """
        |*****************
        |*               *
        |*   EXCEPTIONS  *
        |*               *
        |*****************
        |
        |""".stripMargin )
    logger.info(s"  exceptions found: ${if exceptions.length == 0 then "ZERO" else exceptions.length}!")

  def listSyntaxErrors(): Unit =
    val syntax = tokenisedLines.filter(x => x.tokens.exists(p = y => y.typeOfToken == SyntaxErrorToken))
    logger.info(
      """
        |*****************
        |*               *
        |*   SYNTAX ERR  *
        |*               *
        |*****************
        |
        |""".stripMargin )
    logger.info(s"  syntax errors found ${if syntax.length == 0 then "ZERO" else syntax.length}!")


  def hasException() : Boolean =
    tokenisedLines.exists(x => x.tokens.exists(p = y => y.typeOfToken == ExceptionToken))

  def hasSyntaxError() : Boolean =
    tokenisedLines.exists(x => x.tokens.exists(p = y => y.typeOfToken == SyntaxErrorToken))

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
 * The assembler common parts.
 */
trait Assemble6502PassBase extends StrictLogging :
  logger.info("Base definition for pass objects")

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


/**
 * Fisrt pass object - as one would expect does the first pass which resolves any forward references.
 */
object Assemble6502FirstPass extends Assemble6502PassBase :

  def apply(tokenisedLines: List[TokenisedLine]): Unit =
    for (tokenisedLine <- tokenisedLines)
      try
        assemble(tokenisedLine)
      catch
        case e: Exception => throw new Exception(s"${e.getMessage}\nOn line ${tokenisedLine.sourceLine.lineNumber}" )
        case a => logger.error(s"Unknown exception! ${a}\nOn line ${tokenisedLine.sourceLine.lineNumber}")

  def assemble(tokenisedLine: TokenisedLine) : Unit =
    logger.info(s"\nParsing line ${tokenisedLine.sourceLine.lineNumber} ")
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

  def NoneCommentLine(t: Token) : Unit =
    logger.info(s"NoneCommentLine '${t.tokenVal.strVal}' - ")

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

  //TODO
  def addFowardReference(ref: String): Unit =
    logger.info(s"addFowardReference to '${ref}' @ ${currentLocation}")



/**
 * Second pass object - Using the the data generated by the first pass does the actual assembly.
 */
object Assemble6502SecondPass extends Assemble6502PassBase :
  val validInstructions = List("ORA","AND","EOR","ADC","STA","LDA","CMP","SBC","ASL","ROL","LSR","ROR","STX","LDX","DEC","INC","BIT","JMP","lue","JMP","STY","LDY","CPY","CPX")

  def apply(tokenisedLines: List[TokenisedLine]): Unit =
    for (tokenisedLine <- tokenisedLines)
      try
        assemble(tokenisedLine)
      catch
        case e: Exception => throw new Exception(s"${e.getMessage}\nOn line ${tokenisedLine.sourceLine.lineNumber}" )
        case a => logger.error(s"Unknown exception! ${a}\nOn line ${tokenisedLine.sourceLine.lineNumber}")

  def assemble(tokenisedLine: TokenisedLine) : Unit =
    logger.info(s"\nAssembling ${tokenisedLine.sourceLine.lineNumber} ")