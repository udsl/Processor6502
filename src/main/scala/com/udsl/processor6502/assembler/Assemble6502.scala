package com.udsl.processor6502.assembler

import com.udsl.processor6502.assembler.AssembleLocation.currentLocation
import com.udsl.processor6502.assembler.AssemblerTokenType.{BlankLineToken, CommentLineToken, ExceptionToken, LabelToken, ReferenceToken, SyntaxErrorToken}
import com.udsl.processor6502.cpu.{CpuInstructions, Processor}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Dialogues.errorAlert
import com.udsl.processor6502.assembler.Assemble6502.logger
import com.udsl.processor6502.assembler.Assemble6502FirstPass.{assembleCommandToken, assembleCommentLineToken, assembleInstructionToken, logger, procesLabel, processClear, setAddresses, setBytes, setWords}
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
//    val syntax = tokenisedLines.filter(x => x.hasSyntaxError)
    val syntax = tokenisedLines.flatMap(_.tokens.filter( _.typeOfToken == SyntaxErrorToken))
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
      logger.info(s"${syn.tokenStr}")

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
