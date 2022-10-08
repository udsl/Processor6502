package com.udsl.processor6502.assembler

import com.udsl.processor6502.assembler.AssembleLocation.currentLocation
import com.udsl.processor6502.assembler.{BlankLineToken, CommentLineToken, LabelToken, ReferenceToken}
import com.udsl.processor6502.cpu.{CpuInstructions, Memory, Processor}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Dialogues.errorAlert
import com.udsl.processor6502.Utilities.numericValue
import com.udsl.processor6502.assembler.Assemble6502.logger
import com.udsl.processor6502.assembler.Assemble6502FirstPass.{advanceAssemLocForAddresses, advanceAssemLocForBytes, advanceAssemLocForWords, assembleCommandToken, assembleCommentLineToken, assembleInstructionToken, logger, procesLabel, processClear}
import com.udsl.processor6502.assembler.Assemble6502SecondPass.assemble
import com.udsl.processor6502.cpu.execution.InstructionSize


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
      val references = tokenisedLines.filter(x => x.tokens.contains(ReferenceToken)).map(a => a.tokens) //.filter(b => b.      filter(c => c.typeOfToken == ReferenceToken))
      val referenceTokens = references.flatMap( _ )
      for r <- references do
        val r1 = r.toList
        val r2 = r1.filter(x => x == ReferenceToken)
        for a <- r2 do
          addReference(a.value)

      // Now we have the references but before we can check we need the labels.
      firstPass()
      // Now we can check.
      if verification() then
        secondPass()
      //printTokenisedLines()
      AssemblyData.printLabels()
      AssemblyData.printRefs()
      listExceptions()
      listSyntaxErrors()

  def addReference(value: String): Unit =
    AssemblyData.addReference(value)

  /**
   * Verify that the first pass is without error
   */
  def verification(): Boolean =
    if Parser.sytaxErrorList.nonEmpty then
      errorAlert("Errors", "Syntax errors")
    // Check that all references have associated labels defined
    val (result, errors) = AssemblyData.isValid
    if !result then
      errorAlert( "Validation failure", errors.mkString("\n"))
    Parser.sytaxErrorList.isEmpty & result

  def firstPass(): Unit =
    for (tokenisedLine <- tokenisedLines)
      Assemble6502FirstPass.assemble(tokenisedLine)


  def secondPass(): Unit =
    for (tokenisedLine <- tokenisedLines)
      val resultToken = Assemble6502SecondPass.assemble(tokenisedLine)
      resultToken match
        case NoTokenToken( _, _ ) =>
        case _ =>
          tokenisedLine.tokens.addOne(resultToken)

  def listExceptions(): Unit =
    logger.info(
      """
        |*****************
        |*               *
        |*   EXCEPTIONS  *
        |*               *
        |*****************
        |
        |""".stripMargin )
    logger.info(s"  exceptions found: ${if Tokeniser.exceptionList.isEmpty then "ZERO" else Tokeniser.exceptionList.length}!")
    for exp <- Tokeniser.exceptionList do
      logger.info(s"Exception \"${exp.exceptionMessage}\" at: ${exp.lineNumber} - '${exp.sourceText}'\n")


  def listSyntaxErrors(): Unit =
    logger.info(
      """
        |*****************
        |*               *
        |*   SYNTAX ERR  *
        |*               *
        |*****************
        |
        |""".stripMargin )
    logger.info(s"  syntax errors found ${if Parser.sytaxErrorList.isEmpty then "ZERO" else s"in ${Parser.sytaxErrorList.length} lines"}!")
    for syn <- Parser.sytaxErrorList do
      logger.info(s"Syntax error \"${syn.errorMessage}\" at: ${syn.lineNumber} - '${syn.sourceText}'\n")

  def hasException: Boolean =
    Tokeniser.exceptionList.nonEmpty

  def hasSyntaxError: Boolean =
    Parser.sytaxErrorList.nonEmpty

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
  val memoryAccess: Memory = Memory.apply

  def setAssembleLoc(l: Int):Unit =
    if l > 65535 || l < 0 then
      throw new Exception(s"Bad assembler Location $l ")
    else
      currentLocation = l
      logger.debug(s" assembler Location = $l ")

  def setMemoryWord(v: Int): Unit =
    if v > 65535 || v < 0 then
      val errorMessage = s"Bad word value $v"
      logger.debug(errorMessage)
      throw new Exception(errorMessage)
    setMemoryByte(v / 256, s"WRD $v")
    setMemoryByte(v % 256)


  def setMemoryAddress(adr: Int): Unit =
    if adr > 65535 || adr < 0 then
      val errorMessage = s"Bad address value $adr"
      logger.debug(errorMessage)
      throw new Exception(errorMessage)
    
    memoryAccess.setMemoryToAddress(currentLocation, adr)
    currentLocation += 2

  def setMemoryByte(v: Int, disassembly: String): Unit =
    memoryAccess.setMemoryByte(currentLocation, v, disassembly)
    currentLocation += 1

  def setMemoryByte(v: Int): Unit =
    memoryAccess.setMemoryByte(currentLocation, v)
    currentLocation += 1

  def getMemoryByte(v: Int): Int =
    memoryAccess.getMemoryByte(v)

  /**
   * Update the current assemble location by the size of the instruction that was assembled.
   * 
   * @param insSize the size of the instruction 1, 2 0r 3 bytes only
   */
  def addInstructionSize(insSize: InstructionSize) : Unit =
    currentLocation += insSize.bytes


object Assemble6502 extends StrictLogging :

  /**
   * Assemble the code
   * @param source lines of text to assemble \n terminated
   * @return an assembler instance, initialised with tokenised source lines 
   */
  def apply(source: String): Assemble6502 =
    logger.info("\n\n***** Starting Assembly *****\n\n")
    val allLines = for ((str, index) <- source.split("\n").zipWithIndex)
      yield new UntokenisedLine(index + 1, str.trim)
    val asm = new Assemble6502(Tokeniser.Tokenise(allLines) )
    asm

  def apply(source: String, location: Int): Assemble6502 =
    logger.info(s"\n\n***** Starting Assembly for $location *****\n\n")
    val allLines = for ((str, index) <- s"ORIG $location\n$source".split("\n").zipWithIndex)
      yield new UntokenisedLine(index + 1, str.trim)
    val asm = new Assemble6502(Tokeniser.Tokenise(allLines) )
    asm


object AssemblyData extends StrictLogging:
  // labels defined in a base object so they common others
  // this enables multi file assembly
  val labels = new mutable.HashMap[String, (Int, Boolean)]()
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
    val errors = ListBuffer[String]()
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
    labels.get(name) match
      case Some((v, bool)) =>
        labels.addOne(name, (currentLocation, true))
      case None =>
        labels.addOne(name, (-1, false))

  def labelIsDefined(name: String): Boolean =
    labels.get(name) match
      case Some((v, bool)) =>
        bool
      case None =>
        false

  def labelValue(name: String): Int =
    labels.get(name) match
      case Some((v, bool)) =>
        v
      case None =>
        -1

  def addLabel(name: String, value: Int): Unit =
    labels.get(name) match
      case Some((v, bool)) =>
        if bool && v != value then
          throw new Exception(s"Label '$name' already defined")
        else
          labels.addOne(name, (value, true))
      case None =>
        labels.addOne(name, (value, true))

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
    AssembleLocation.setMemoryAddress( numericValue(v))

  def setMemoryAddress(v: Int): Unit =
    AssembleLocation.setMemoryAddress(v)
    
  def setMemoryByte(v: String): Unit =
    AssembleLocation.setMemoryByte(if v.charAt(0) == '$' then
      Integer.parseInt(v.substring(1), 16)
    else
      Integer.parseInt(v))

  def setMemoryByte(v: Int): Unit =
    AssembleLocation.setMemoryByte(v)

  def setMemoryByte(v: Int, disassembly: String): Unit =
    AssembleLocation.setMemoryByte(v, disassembly)

class Reference( val name: String):
  def hasValue: Boolean =
    Reference.hasValue(this)

  def value : Option[(Int, Boolean)] =
    Reference.getValue(this)


object Reference:
  def apply(name: String) : Reference =
    new Reference(name)

  def hasValue(instance: Reference): Boolean =
    AssemblyData.labels.contains(instance.name)

  def getValue(instance: Reference) : Option[(Int, Boolean)] =
    AssemblyData.labels.get(instance.name)
