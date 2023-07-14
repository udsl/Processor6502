package com.udsl.processor6502.assembler

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.version1.Assemble6502.logger
import com.udsl.processor6502.assembler.version1.{Assemble6502FirstPass, Assemble6502SecondPass, TokenisedLine, TokeniserV1, UntokenisedLine}
import com.udsl.processor6502.config.ConfigDatum

import reflect.Selectable.reflectiveSelectable
import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.language.reflectiveCalls
import scala.util.Try

sealed trait Assembler:
  def startAssembly(): Unit

class SourceFileAssembler(val sourceFile: File, val assemblerData: AssemblerDataStructure) extends Assembler, StrictLogging:

  def processLine(line: String): Unit =
    logger.info(s"processing line '$line'")
    assemblerData.currentLine += 1
    val lineToTokenise = UntokenisedLine(assemblerData.currentLine, line)
    val tokisedLine = TokeniserV1.tokeniseLine(lineToTokenise)
    assemblerData.tokenisedList += tokisedLine
    Assemble6502FirstPass.assemble(tokisedLine)

  def startAssembly(): Unit =
    logger.info(s"Starting file assembly")
    val bufferedSource = Source.fromFile(sourceFile)
    for (line <- bufferedSource.getLines) {
      processLine(line)
    }
    bufferedSource.close
    doSecondPass()

  def doSecondPass(): Unit =
    for (tokisedLine <- assemblerData.tokenisedList) {
      Assemble6502SecondPass.assemble(tokisedLine)
    }

class SourceAssembler(val source: String, val assemblerData: AssemblerDataStructure) extends Assembler, StrictLogging:
  def processLine(line: String): Unit =
    logger.info(s"processing line '$line'")
    assemblerData.currentLine += 1
    val lineToTokenise = UntokenisedLine(assemblerData.currentLine, line)
    val tokisedLine = TokeniserV1.tokeniseLine(lineToTokenise)
    assemblerData.tokenisedList += tokisedLine
    Assemble6502FirstPass.assemble(tokisedLine)

  def startAssembly(): Unit =
      logger.info(s"Starting source assembly")
      val bufferedSource = Source.fromString(source)
      for (line <- bufferedSource.getLines) {
        processLine(line)
      }
      bufferedSource.close

class AssemblerDataStructure extends StrictLogging :
  val tokenisedList: ArrayBuffer[TokenisedLine] = ArrayBuffer.empty[TokenisedLine]
  var currentLine: Int = 0

object AssemblerDataStructure extends StrictLogging :
  def apply() : AssemblerDataStructure =
    new AssemblerDataStructure()


object Assembler extends StrictLogging :

  def apply(sourceFile: File): Assembler =
    logger.info("\n\n***** Starting File Assembly *****\n\n")
    new SourceFileAssembler(sourceFile, AssemblerDataStructure.apply())

  def apply(source: String): Assembler =
    logger.info("\n\n***** Starting Source Assembly *****\n\n")
    new SourceAssembler(source, AssemblerDataStructure.apply())

