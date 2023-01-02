package com.udsl.processor6502.assemblier2

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.AssemblyData
import com.udsl.processor6502.assemblier2.Assemble6502FirstPassV2.assemble
import com.udsl.processor6502.assemblier2.{Assemblier2, Token, Tokeniser}

import java.io.FileInputStream
import java.nio.file.{Files, Paths}
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try, Using}

class Assemblier2(val sourceLines: LazyList[(String, Int)]) extends StrictLogging :
  import Assemblier2.*

  def assemble(): Unit =
    println("starting")
    AssemblyData.clear()
    tokenisation.foreach(Assemble6502FirstPassV2.assemble)
    println("complete")


  def tokenisation: LazyList[TokenisedLine] =
    sourceLines.map(tokonise)

  def firstPass(tokenisedLine: TokenisedLine): Unit =
    logger.info(s"Parsing line ${tokenisedLine.lineNumber} ")

  private def tokonise(text: String, line: Int) : TokenisedLine =
    Tokeniser.tockenise(text, line)


object Assemblier2 :

  def apply(sourceLines: List[String]) : Assemblier2 =
    new Assemblier2(LazyList.from(sourceLines.zip(LazyList.from(1))))

  def apply(sourceFilename: String) : Assemblier2 =
    val s = Source.fromFile(sourceFilename)
    new Assemblier2(LazyList.from(s.getLines().zip(LazyList.from(1))))



