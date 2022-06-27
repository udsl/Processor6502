package com.udsl.processor6502.assembler

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.Assemble6502.logger
import com.udsl.processor6502.config.ConfigDatum

import reflect.Selectable.reflectiveSelectable
import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.language.reflectiveCalls
import scala.util.Try

sealed trait Assembler:
  def startAssembly(): Unit

class SourceFileAssembler(val sourceFile: File) extends Assembler, StrictLogging:

  def startAssembly(): Unit =
    logger.info(s"Starting file assembly")
    val bufferedSource = Source.fromFile(sourceFile)
    for (line <- bufferedSource.getLines) {
      Assembler.processLine(line)
    }
    bufferedSource.close
    doSecondPass()

  def doSecondPass(): Unit =
    for (tokisedLine <- Assembler.tokenisedList) {
      Assemble6502SecondPass.assemble(tokisedLine)
    }

class SourceAssembler(val source: String) extends Assembler, StrictLogging:
  def startAssembly(): Unit =
    logger.info(s"Starting source assembly")
    val bufferedSource = Source.fromString(source)
    for (line <- bufferedSource.getLines) {
      Assembler.processLine(line)
    }
    bufferedSource.close


object Assembler extends StrictLogging :
  val tokenisedList: ArrayBuffer[TokenisedLine] = ArrayBuffer.empty[TokenisedLine]
  var currentLine: Int = 0


  def apply(sourceFile: File): Assembler =
    logger.info("\n\n***** Starting File Assembly *****\n\n")
    init()
    new SourceFileAssembler(sourceFile)

  def apply(source: String): Assembler =
    logger.info("\n\n***** Starting Source Assembly *****\n\n")
    init()
    new SourceAssembler(source)

  private def init():Unit =
    logger.info("Resetting companion values")
    tokenisedList.clear()
    currentLine = 0

  def processLine(line: String): Unit =
    logger.info(s"processing line '$line'")
    currentLine += 1
    val lineToTokenise = UntokenisedLine(currentLine, line)
    val tokisedLine = Tokeniser.tokeniseLine(lineToTokenise)
    tokenisedList += tokisedLine
    Assemble6502FirstPass.assemble(tokisedLine)

