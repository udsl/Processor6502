package com.udsl.processor6502.assembler

import com.typesafe.scalalogging.StrictLogging

import scala.collection.mutable.ListBuffer

class ParsedLine(val source: String, val lineNumber: Int) extends StrictLogging :
  val tokens = new ListBuffer[AssemblerToken]()

  def +(other: AssemblerToken): tokens.type = {
    tokens += other
  }
  
  override def toString: String =
    var str: String = s"Line number: ${lineNumber} has ${tokens.length} tokens,  Source: '${source}', Tokens: \n"
    for t <- tokens do
      str += s"\t${t} - ${t.predictedAddressingModes}\n"
    str



object ParsedLine:
  def apply(line: String, lineNumber: Int) : ParsedLine =
    new ParsedLine(line, lineNumber)
