package com.udsl.processor6502.assemblier2

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assemblier2.{Token, Tokeniser, Assemblier2}

import java.io.FileInputStream
import java.nio.file.{Files, Paths}
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try, Using}

class Assemblier2(val sourceLines: LazyList[(String, Int)]) extends StrictLogging :
  import Assemblier2.*


  def souceLineCount : Int =
    sourceLines.length

  def assemble(): List[TokenisedLine] =
    println("starting")
    val tokenised = tokenisation
    println("complete")
    tokenised

  def tokenisation: List[TokenisedLine] =
    sourceLines.map(tokonise).toList

  private def tokonise(text: String, line: Int) : TokenisedLine =
    Tokeniser.tockenise(text, line)


object Assemblier2 :

  def apply(sourceLines: List[String]) : Assemblier2 =
    new Assemblier2(LazyList.from(sourceLines.zip(LazyList.from(1))))

  def apply(sourceFilename: String) : Assemblier2 =
    new Assemblier2(
      Using(Source.fromFile(sourceFilename)) { s =>
        LazyList.from(s.getLines().toList.zip(LazyList.from(1)))
      }.get
    )



