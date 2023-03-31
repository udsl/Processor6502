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
