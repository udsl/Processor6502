package com.udsl.processor6502.assembler.version2

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.{Assembler, AssemblerDataStructureV2}

import java.io.File
import scala.io.Source
import scala.util.Using

class SourceAsssemblierV2(val source: Iterator[String]) extends Assembler, StrictLogging:
  override def version = 2

  def startAssembly(): Unit =
    logger.info(s"SourceAsssemblierV2 starting  assembly")
    var lineNum = 1
    for (line <- source)
      val tokisedLine = TokeniserV2.tockenise(line, lineNum)
      lineNum += 1
//      assemblerData.tokenisedList += tokisedLine
//      Assemble6502FirstPass.assemble(tokisedLine)
//    for (tokisedLine <- assemblerData.tokenisedList)
//      Assemble6502SecondPass.assemble(tokisedLine)

object SourceAsssemblierV2:
  val assemblerData: AssemblerDataStructureV2 = AssemblerDataStructureV2.apply()

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

  def apply(source: String): SourceAsssemblierV2 =
    new SourceAsssemblierV2(sourceIter(source))

  def apply(sourceFile: File): SourceAsssemblierV2 =
    new SourceAsssemblierV2(sourceIter(sourceFile))