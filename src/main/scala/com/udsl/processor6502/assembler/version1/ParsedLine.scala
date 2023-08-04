package com.udsl.processor6502.assembler.version1

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.SourceLine

import scala.collection.mutable.ListBuffer

class ParsedLine(val source: SourceLine) extends StrictLogging :
  val tokens = new ListBuffer[AssemblerToken]()

  def +(other: AssemblerToken): tokens.type = {
    tokens += other
  }
  
  override def toString: String =
    var str: String = s"${source.toString } has ${tokens.length} tokens: \n"
    for t <- tokens do
      str += s"\t$t - ${t.predictedAddressingModes}\n"
    str



object ParsedLine:
  def apply(sourceLine: SourceLine) : ParsedLine =
    new ParsedLine(sourceLine)
