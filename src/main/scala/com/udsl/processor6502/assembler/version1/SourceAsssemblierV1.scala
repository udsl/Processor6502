package com.udsl.processor6502.assembler.version1

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.application.Main.userLog
import com.udsl.processor6502.assembler.version1.SourceAsssemblierV1.assemblerData
import com.udsl.processor6502.assembler.{AssembleLocation, Assembler, AssemblerDataStructureV1, SourceLine}
import com.udsl.processor6502.config.AppOptions

import java.io.File
import scala.io.Source
import scala.util.Using

class SourceAsssemblierV1(val source: Iterator[String] ) extends Assembler, StrictLogging:
  override def version = 1

  def startAssembly(): Unit =
    logger.info(s"SourceAsssemblierV1 starting assembly")
    userLog("Assembly starting")
    // record current assembly location
    val strtloc = AssembleLocation.currentLocation
    for (line <- source)
      val lineToTokenise = SourceLine(line.strip(), assemblerData.currentLine)
      val tokisedLine = TokeniserV1.tokeniseLine(lineToTokenise)
      assemblerData.tokenisedList += tokisedLine
      Assemble6502FirstPass.assemble(tokisedLine)
    // first pass complete, restore AssembleLocation.currentLocation to saved start location and ORIG commands will also be processed in second pass
    AssembleLocation.currentLocation = strtloc
    for (tokisedLine <- assemblerData.tokenisedList)
      Assemble6502SecondPass.assemble(tokisedLine)


object SourceAsssemblierV1:
  val assemblerData: AssemblerDataStructureV1 = AssemblerDataStructureV1.apply()

  private def sourceIter(source: String): Iterator[String] =
    var sourceIter: Iterator[String] = Iterator()
    Using(Source.fromString(source)) { reader =>
      sourceIter = reader.getLines
    }
    sourceIter

  private def sourceIter(sourceFile: File): Iterator[String] =
    assemblerData.init()
    val source = Source.fromFile(sourceFile)
    val lines = try source.mkString finally source.close()
    sourceIter(lines)

  def apply(source: String): SourceAsssemblierV1 =
    def verboseString =
      if AppOptions.userLoggingVerbosity > 1 then s"\n$source"
      else ""
    userLog(s"Initialising assembler V1 with string$verboseString")
    new SourceAsssemblierV1(sourceIter(source))

  def apply(sourceFile: File): SourceAsssemblierV1 =
    def verboseString =
      if AppOptions.userLoggingVerbosity > 1 then s"\n${sourceFile.getName}"
      else ""
    userLog(s"Initialising assembler V1 with string$verboseString")
    new SourceAsssemblierV1(sourceIter(sourceFile))