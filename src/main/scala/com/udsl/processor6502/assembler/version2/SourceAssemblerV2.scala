package com.udsl.processor6502.assembler.version2

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.{Assembler, AssemblyData, SourceLine}

import java.io.{File, FileInputStream}
import java.nio.file.{Files, Paths}
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}

class SourceAssemblerV2(val sourceLines: List[SourceLine], val firstPass: Assemble6502FirstPassV2, val secondPass: Assemble6502SecondPassV2) extends Assembler with StrictLogging :
  override def version: Int = 2

  import SourceAssemblerV2.*

  override def startAssembly(): Unit =
    logger.info("starting V2")
    AssemblyData.clear()
    val x = tokenisation.map(firstPass.assemble)
    logger.info(s"complete $x")


  def tokenisation: List[TokenisedLineV2] =
    sourceLines.map(tokonise)

  def parse(tokenisedLine: TokenisedLineV2): Unit =
    logger.info(s"Parsing line ${tokenisedLine.source.lineNum} ")

  private def tokonise(line: SourceLine) : TokenisedLineV2 =
    TokeniserV2.tockenise(line)


object SourceAssemblerV2 :

  def apply(sourceLines: List[String]) : SourceAssemblerV2 =
    new SourceAssemblerV2(
      List.from(sourceLines
        .zip(LazyList.from(1))
        .map(f => SourceLine(f._1, f._2))),
      Assemble6502FirstPassV2.apply,
      Assemble6502SecondPassV2.apply
    )

  def apply(sourceFile: File) : SourceAssemblerV2 =
    val s = Source.fromFile(sourceFile)
    new SourceAssemblerV2(
      List.from(s.getLines().toList
        .zip(LazyList.from(1))
        .map(f => SourceLine(f._1, f._2))),
      Assemble6502FirstPassV2.apply,
      Assemble6502SecondPassV2.apply
    )


