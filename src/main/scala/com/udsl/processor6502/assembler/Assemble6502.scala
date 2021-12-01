package com.udsl.processor6502.assembler

import com.udsl.processor6502.assembler.AssembleLocation.currentLocation
import com.udsl.processor6502.assembler.AssemblerTokenType.{BlankLineToken, CommentLineToken, ExceptionToken, LabelToken, ReferenceToken, SyntaxErrorToken}
import com.udsl.processor6502.cpu.{CpuInstructions, Processor}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Utilities
import com.udsl.processor6502.Utilities.errorAlert
import com.udsl.processor6502.assembler.Assemble6502.logger
import com.udsl.processor6502.assembler.Assemble6502FirstPass.{AssembleCommandToken, AssembleCommentLineToken, AssembleInstructionToken, logger, procesLabel, processClear}
import com.udsl.processor6502.assembler.Assemble6502SecondPass.assemble


/**
 * The class produced by the Assemble6502 apply(source: String) method.
 * The apply takes the source string and tockenises it, notmally this would be the job of pass one but this is not
 * an efficency excersise it a sue Scala and ScalaFx excersise so not woried about efficiency :)
 *
 * @param tokenisedLines the source lines tokenised.
 */
class Assemble6502( val tokenisedLines: List[TokenisedLine]) extends StrictLogging {

  def assemble(): Unit =
    if hasSyntaxError || hasException then
      errorAlert("Assembly Error", "Syntax or exceptions encontered")
      listExceptions()
      listSyntaxErrors()
    else
      // Create the references form the tokens
      val references = tokenisedLines.filter(x => x.tokens.exists(y => y.typeOfToken == ReferenceToken)).map(a => a.tokens) //.filter(b => b.      filter(c => c.typeOfToken == ReferenceToken))
      val referenceTokens = references.flatMap( _ )
      for r <- references do
        val r1 = r.toList
        val r2 = r1.filter(x => x.typeOfToken == ReferenceToken)
        for a <- r2 do
          addReference(a.tokenStr)

      // Now we have the references but before we can check we need the labels.
      firstPass()
      // Now we can check.
      if verification() then
        secondPass()
      printTokenisedLines()
      AssemblyData.printLabels()
      AssemblyData.printRefs()

  def addReference(value: String): Unit =
    AssemblyData.addReference(value)

  /**
   * Verify that the first pass is without error
   */
  def verification(): Boolean =
    val syntax = tokenisedLines.filter(x => x.tokens.exists(p = y => y.typeOfToken == SyntaxErrorToken))
    if syntax.nonEmpty then
      errorAlert("Errors", "Syntax errors")
    // Check that all references have associated labels defined
    val (result, errors) = AssemblyData.isValid
    if !result then
      errorAlert( "Validation failure", errors.mkString("\n"))
    syntax.isEmpty & result

  def firstPass(): Unit =
    for (tokenisedLine <- tokenisedLines)
      try
        Assemble6502FirstPass.assemble(tokenisedLine)
      catch
        case e: Exception => throw new Exception(s"${e.getMessage}\nOn line ${tokenisedLine.sourceLine.lineNumber}" )
        case a => logger.error(s"Unknown exception! $a\nOn line ${tokenisedLine.sourceLine.lineNumber}")

  def secondPass(): Unit =
    for (tokenisedLine <- tokenisedLines)
      try
        Assemble6502SecondPass.assemble(tokenisedLine)
      catch
        case e: Exception => throw new Exception(s"${e.getMessage}\nOn line ${tokenisedLine.sourceLine.lineNumber}" )
        case a => logger.error(s"Unknown exception! $a\nOn line ${tokenisedLine.sourceLine.lineNumber}")

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
    logger.info(s"  exceptions found: ${if exceptions.isEmpty then "ZERO" else exceptions.length}!")

  def listSyntaxErrors(): Unit =
    val syntax = tokenisedLines.filter(x => x.hasSyntaxError)
    logger.info(
      """
        |*****************
        |*               *
        |*   SYNTAX ERR  *
        |*               *
        |*****************
        |
        |""".stripMargin )
    logger.info(s"  syntax errors found ${if syntax.isEmpty then "ZERO" else syntax.length}!")
    for syn <- syntax do
      logger.info(s"${syn.tokens}")

  def hasException: Boolean =
    tokenisedLines.exists(x => x.tokens.exists(y => y.typeOfToken == ExceptionToken))

  def hasSyntaxError: Boolean =
    tokenisedLines.exists(x => x.hasSyntaxError)

  def printTokenisedLines(): Unit =
    logger.debug("\n\nTokenisedLines\n")
    if (tokenisedLines.isEmpty) logger.info("No lines tokenised") else for (line <- tokenisedLines) {
      logger.debug(line.toString)
    }
    logger.debug("\nend TokenisedLines\n\n")
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
      throw new Exception(s"Bad assmbler Location $l ")
    else
      currentLocation = l
      logger.debug(s" assmbler Location = $l ")

  def setMemoryWord(v: Int): Unit =
    if v > 65535 || v < 0 then
      val errorMessage = s"Bad word value $v"
      logger.debug(errorMessage)
      throw new Exception(errorMessage)
    setMemoryByte(v / 256)
    setMemoryByte(v % 256)

  def setMemoryAddress(adr: Int): Unit =
    if adr > 65535 || adr < 0 then
      val errorMessage = s"Bad address value $adr"
      logger.debug(errorMessage)
      throw new Exception(errorMessage)
    setMemoryByte(adr % 256)
    setMemoryByte(adr / 256)

  def setMemoryByte(v: Int): Unit =
    if v > 256 || v < 0 then
      throw new Exception("Not a byt value")
    Processor.setMemoryByte(currentLocation, v)
    currentLocation += 1

  def addInstructionSize(insSize: Int) : Unit =
    if insSize < 1 || insSize > 3 then
      throw new Exception(s"No instruction with size $insSize")
    currentLocation += insSize


object Assemble6502 extends StrictLogging :

  def apply(source: String): Assemble6502 =
    logger.info("\n\n***** Starting Assembly *****\n\n")
    val allLines = for ((str, index) <- source.split("\n").zipWithIndex)
      yield new UntokenisedLine(index + 1, str)
    val asm = new Assemble6502(Tokeniser.Tokenise(allLines) )
    asm


object AssemblyData extends StrictLogging:
  // labels defined in a base object so they common others
  // this enables multi file assembly
  val labels = new mutable.HashMap[String, Int]()
  var references = new ListBuffer[Reference]()

  def clear(): Unit =
    labels.clear()
    references.clear()

  def isValid: (Boolean, List[String]) =
    if AssemblyData.labels.isEmpty && AssemblyData.references.nonEmpty then
      val error = "No labels but have references!"
      logger.debug(error)
      return (false, List(error))
    var result = true
    var errors = ListBuffer[String]()
    for r <- AssemblyData.references do
      if !AssemblyData.labels.contains(r.name) then
        val error = s"Reference ${r.name} not found in labels."
        logger.debug(error)
        errors.addOne(error)
        result = false
    logger.debug(s"Have ${AssemblyData.labels.size} labels and ${AssemblyData.references.size} references")
    (result, errors.toList)

  def addReference(ref: String): Unit =
    logger.debug(s"addReference to '$ref' @ $currentLocation")
    references += Reference(ref);

  def addLabel(name: String): Unit =
    if (labels.contains(name))
      throw new Exception(s"Label '$name' already defined")
    labels.addOne(name, currentLocation)

  def printRefs(): Unit =
    logger.info(
      """
        |*****************
        |*               *
        |*   References  *
        |*               *
        |*****************
        |
        |""".stripMargin )
    if references.isEmpty then
      logger.info("No references made")
    else
      for ref <- references do
        logger.info(s"Have reference to ${ref.name} value ${ref.value}")


  def printLabels(): Unit =
    logger.info(
      """
        |*****************
        |*               *
        |*     Labels    *
        |*               *
        |*****************
        |
        |""".stripMargin )
    if (labels.isEmpty) logger.info("No labels defined") else for ((label, address) <- labels) {
      logger.info(s"\t$label address $address")
    }


/**
 * The assembler common parts.
 */
trait Assemble6502PassBase:

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
object Assemble6502FirstPass extends StrictLogging, Assemble6502PassBase :

  def assemble(tokenisedLine: TokenisedLine) : Unit =
    logger.info(s"Parsing line ${tokenisedLine.sourceLine.lineNumber} ")
    for (token <- tokenisedLine.tokens)
      token.typeOfToken match {
        case AssemblerTokenType.BlankLineToken => // extends AssemblerTokenType("BlankLineToken")
          logger.info("\tBlankLineToken ")
        case AssemblerTokenType.CommentLineToken => // extends AssemblerTokenType("CommentLineToken")
          AssembleCommentLineToken(token)
        case AssemblerTokenType.LineComment => // extends AssemblerTokenType("LineComment")
          logger.info("\tLineComment ")
        case AssemblerTokenType.NoneCommentLine => // extends AssemblerTokenType("NoneCommentLine")
          logger.info("\tNoneCommentLine ")
        case AssemblerTokenType.LabelToken => // extends AssemblerTokenType("LabelToken")
          procesLabel(token)
        case AssemblerTokenType.CommandToken => // extends AssemblerTokenType("CommandToken")
          AssembleCommandToken(token)
        case AssemblerTokenType.InstructionToken => // extends AssemblerTokenType("InstructionToken")
          AssembleInstructionToken(token)
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

  def AssembleCommentLineToken(t: Token) : Unit =
    logger.info(s"\tCommentLineToken '${t.tokenVal.strVal}' - ")

  def AssembleCommandToken(t: Token) : Unit =
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

  def AssembleInstructionToken(t: Token) : Unit =
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
          case AddressingModes.Accumulator
               | AddressingModes.Implied  => 1
          case AddressingModes.Immediate
               | AddressingModes.ZeroPage
               | AddressingModes.Relative
               | AddressingModes.ZeroPageX
               | AddressingModes.ZeroPageY
               | AddressingModes.ZeroPageIndirectX
               | AddressingModes.ZeroPageIndirectY => 2
          case AddressingModes.Absolute
               | AddressingModes.AbsoluteIndirect
               | AddressingModes.AbsoluteIndexedX
               | AddressingModes.AbsoluteIndexedY => 3
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




/**
 * Second pass object - Using the the data generated by the first pass does the actual assembly.
 */
object Assemble6502SecondPass extends StrictLogging, Assemble6502PassBase :

  def assemble(tokenisedLine: TokenisedLine) : Unit =
    logger.info(s"\n\n2nd Pass ${tokenisedLine.sourceLine.lineNumber} ")
//    for (token <- tokenisedLine.tokens)
//      token.typeOfToken match {
//        case AssemblerTokenType.BlankLineToken => // extends AssemblerTokenType("BlankLineToken")
//          logger.info("\tBlankLineToken ")
//        case AssemblerTokenType.CommentLineToken => // extends AssemblerTokenType("CommentLineToken")
//          AssembleCommentLineToken(token)
//        case AssemblerTokenType.LineComment => // extends AssemblerTokenType("LineComment")
//          logger.info("\tLineComment ")
//        case AssemblerTokenType.NoneCommentLine => // extends AssemblerTokenType("NoneCommentLine")
//          logger.info("\tNoneCommentLine ")
//        case AssemblerTokenType.LabelToken => // extends AssemblerTokenType("LabelToken")
//          procesLabel(token)
//        case AssemblerTokenType.CommandToken => // extends AssemblerTokenType("CommandToken")
//          AssembleCommandToken(token)
//        case AssemblerTokenType.InstructionToken => // extends AssemblerTokenType("InstructionToken")
//          AssembleInstructionToken(token)
//        case AssemblerTokenType.SyntaxErrorToken => // extends AssemblerTokenType("SyntaxErrorToken")
//          logger.info("\tSyntaxErrorToken ")
//        case AssemblerTokenType.ClearToken =>
//          logger.info("\tClear Token")
//          processClear(token, tokenisedLine)
//
//        case _ => logger.error(s"unsupported case ${token.typeOfToken}")
//      }


class Reference( val name: String):
  def hasValue: Boolean =
    Reference.hasValue(this)

  def value : Option[Int] =
    Reference.getValue(this)


object Reference:
  def apply(name: String) : Reference =
    new Reference(name)

  def hasValue(instance: Reference): Boolean =
    AssemblyData.labels.contains(instance.name)

  def getValue(instance: Reference) : Option[Int] =
    AssemblyData.labels.get(instance.name)
