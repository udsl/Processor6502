package com.udsl.processor6502.assembler.version1

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.{Assembler, AssemblerDataStructure}
import java.io.File
import scala.io.Source
import scala.util.Using

class SourceAsssemblierV1(val source: Iterator[String], val assemblerData: AssemblerDataStructure) extends Assembler, StrictLogging:

  def startAssembly(): Unit =
    logger.info(s"Starting file assembly")
    for (line <- source)
      val lineToTokenise = UntokenisedLine(assemblerData.currentLine, line)
      val tokisedLine = TokeniserV1.tokeniseLine(lineToTokenise)
      assemblerData.tokenisedList += tokisedLine
      Assemble6502FirstPass.assemble(tokisedLine)
    for (tokisedLine <- assemblerData.tokenisedList)
      Assemble6502SecondPass.assemble(tokisedLine)


object SourceAsssemblierV1:
  val assemblerData: AssemblerDataStructure = AssemblerDataStructure.apply()

  private def sourceIter(source: String): Iterator[String] =
    var sourceIter: Iterator[String] = Iterator()
    Using(Source.fromString(source)) { reader =>
      sourceIter = reader.getLines
    }
    sourceIter

  private def sourceIter(sourceFile: File): Iterator[String] =
    val source = Source.fromFile(sourceFile)
    val lines = try source.mkString finally source.close()
    sourceIter(lines)

  def apply(source: String): SourceAsssemblierV1 =
    new SourceAsssemblierV1(sourceIter(source), assemblerData)

  def apply(sourceFile: File): SourceAsssemblierV1 =
    new SourceAsssemblierV1(sourceIter(sourceFile), assemblerData)