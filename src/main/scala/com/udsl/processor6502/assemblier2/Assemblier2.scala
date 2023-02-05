package com.udsl.processor6502.assemblier2

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.AssemblyData
import com.udsl.processor6502.assemblier2
import com.udsl.processor6502.assemblier2.{Assemble6502FirstPassV2, Assemble6502SecondPassV2}
import com.udsl.processor6502.assemblier2.{Assemblier2, Token, Tokeniser}

import java.io.FileInputStream
import java.nio.file.{Files, Paths}
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}

class Assemblier2(val sourceLines: LazyList[(String, Int)], val firstPass: Assemble6502FirstPassV2, val secondPass: Assemble6502SecondPassV2) extends StrictLogging :
  import Assemblier2.*
  
  def assemble(): Unit =
    println("starting")
    AssemblyData.clear()
    val x = tokenisation.map(firstPass.assemble)
    println(s"complete $x")


  def tokenisation: LazyList[TokenisedLine] =
    sourceLines.map(tokonise)

  def parse(tokenisedLine: TokenisedLine): Unit =
    logger.info(s"Parsing line ${tokenisedLine.lineNumber} ")

  private def tokonise(text: String, line: Int) : TokenisedLine =
    Tokeniser.tockenise(text, line)


object Assemblier2 :

  def apply(sourceLines: List[String]) : Assemblier2 =
    new Assemblier2(LazyList.from(sourceLines.zip(LazyList.from(1))), Assemble6502FirstPassV2.apply, Assemble6502SecondPassV2.apply)

  def apply(sourceFilename: String) : Assemblier2 =
    val s = Source.fromFile(sourceFilename)
    new Assemblier2(LazyList.from(s.getLines().zip(LazyList.from(1))), Assemble6502FirstPassV2.apply, Assemble6502SecondPassV2.apply)

  def apply(sourceLines: List[String], parser: Assemble6502FirstPassV2): Assemblier2 =
    new Assemblier2(LazyList.from(sourceLines.zip(LazyList.from(1))), parser, Assemble6502SecondPassV2.apply)

  def apply(sourceFilename: String, parser: Assemble6502FirstPassV2): Assemblier2 =
    val s = Source.fromFile(sourceFilename)
    new Assemblier2(LazyList.from(s.getLines().zip(LazyList.from(1))), parser, Assemble6502SecondPassV2.apply)



