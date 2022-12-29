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
    val tokenised = sourceLines.map(tokonise).toList
    println("complete")
    tokenised

  def tokonise(text: String, line: Int) : TokenisedLine =
    Tokeniser.tockenise(text, line)


object Assemblier2 :

  def apply(sourceLines: List[(String, Int)]) : Assemblier2 =
    val asm = new Assemblier2(LazyList.from(sourceLines))
    asm

  def apply(sourceFilename: String) : Assemblier2 =

    //val string = new String(Files.readAllBytes(Paths.get(sourceFilename)))
    val x = Using(Source.fromFile(sourceFilename)) { s =>
      s.getLines().toList.zipWithIndex
    }

    val asm = new Assemblier2(LazyList.from(x.get))
    asm



