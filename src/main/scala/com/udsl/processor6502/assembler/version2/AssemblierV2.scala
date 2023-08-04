package com.udsl.processor6502.assembler.version2

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.AssemblyData
import java.io.FileInputStream
import java.nio.file.{Files, Paths}
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}
import com.udsl.processor6502.assembler.SourceLine

class AssemblierV2(val sourceLines: LazyList[SourceLine], val firstPass: Assemble6502FirstPassV2, val secondPass: Assemble6502SecondPassV2) extends StrictLogging :
  import AssemblierV2.*
  
  def assemble(): Unit =
    logger.info("starting V2")
    AssemblyData.clear()
    val x = tokenisation.map(firstPass.assemble)
    logger.info(s"complete $x")


  def tokenisation: LazyList[TokenisedLineV2] =
    sourceLines.map(tokonise)

  def parse(tokenisedLine: TokenisedLineV2): Unit =
    logger.info(s"Parsing line ${tokenisedLine.source.lineNum} ")

  private def tokonise(line: SourceLine) : TokenisedLineV2 =
    TokeniserV2.tockenise(line)


object AssemblierV2 :

  def apply(sourceLines: List[String]) : AssemblierV2 =
    val x: Seq[(String, Int)] = sourceLines.zip(LazyList.from(1))
    val y: Seq[SourceLine] = x.map(f => SourceLine(f._1, f._2))
    val s = sourceLines.zip(LazyList.from(1)).map(f => SourceLine(f._1, f._2))
    new AssemblierV2(LazyList.from(sourceLines.zip(LazyList.from(1)).map(f => SourceLine(f._1, f._2))), Assemble6502FirstPassV2.apply, Assemble6502SecondPassV2.apply)

  def apply(sourceFilename: String) : AssemblierV2 =
    val s = Source.fromFile(sourceFilename)
    new AssemblierV2(LazyList.from(s.getLines().zip(LazyList.from(1)).map(f => SourceLine(f._1, f._2))), Assemble6502FirstPassV2.apply, Assemble6502SecondPassV2.apply)

  def apply(sourceLines: List[String], parser: Assemble6502FirstPassV2): AssemblierV2 =
    new AssemblierV2(LazyList.from(sourceLines.zip(LazyList.from(1))).map(f => SourceLine(f._1, f._2)), parser, Assemble6502SecondPassV2.apply)

  def apply(sourceFilename: String, parser: Assemble6502FirstPassV2): AssemblierV2 =
    val s = Source.fromFile(sourceFilename)
    new AssemblierV2(LazyList.from(s.getLines().zip(LazyList.from(1))).map(f => SourceLine(f._1, f._2)), parser, Assemble6502SecondPassV2.apply)



