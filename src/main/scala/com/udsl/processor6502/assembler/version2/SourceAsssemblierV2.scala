package com.udsl.processor6502.assembler.version2

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.{Assembler, AssemblerDataStructure}

import java.io.File
import scala.io.Source
import scala.util.Using

class SourceAsssemblierV2(val source: Iterator[String], val assemblerData: AssemblerDataStructure) extends Assembler, StrictLogging:

  def startAssembly(): Unit =
    logger.info(s"SourceAsssemblierV2 starting  assembly")


object SourceAsssemblierV2:
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

  def apply(source: String): SourceAsssemblierV2 =
    new SourceAsssemblierV2(sourceIter(source), assemblerData)

  def apply(sourceFile: File): SourceAsssemblierV2 =
    new SourceAsssemblierV2(sourceIter(sourceFile), assemblerData)