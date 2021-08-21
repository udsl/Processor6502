package com.udsl.processor6502.assembler

import com.udsl.processor6502.assembler.AssemblerTokenType.LabelToken

import scala.Console.println
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Assemble6502 {
  var currentLocation: Int = 0
  var labels = new mutable.HashMap[String, Int]()
  var tokenisedLines = new ListBuffer[TokenisedLine]()

  def assemble( source: String, location: Int): Unit ={
    currentLocation = location
    println(s"Assembling to ${location}, Source:\n${source}")
    // split into lines, remove training spaces and any resulting blank lines
//    val allLines = source.split("\n").toList.map( _.trim).filter( _ != "")
    // Remove any comment lines
//    val noneCommentLines = allLines.filter( _.take(1) != ";")
//    println(s"Source has ${noneCommentLines.length} lines")
//    println(s"noneCommentLines:\n${noneCommentLines.mkString("\n")}")
//    // Remove any training comments from lines
//    val commentsRemoved = noneCommentLines.map( s => { if (s.contains(";")) {s.take(s.indexOf(";")) } else { s } } )
//    println(s"commentsRemoved:\n${noneCommentLines.mkString("\n")}")
//    for (line <- commentsRemoved)
//    {
//      // Build tokenised list
//      val tokenisedLine = LineTokens(1)
//      tokeniseLine(line, tokenisedLine)
//    }

    // Create a list of lines
    val allLines = for ((str, index) <- source.split("\n").zipWithIndex)
      yield new UntokenisedLine(index + 1, str)
    // Create tokens from the line list
    for ( lineToTokenise <- allLines){
      tokenisedLines.addOne(tokeniseLine(lineToTokenise))
    }
  }

  private def tokeniseLine(line: UntokenisedLine): TokenisedLine = {
    println(s"\ntokeniseLine: ${line}")
    val tokenisedLine = TokenisedLine(line)
    line.source.trim match {
      case "" => tokenisedLine + Token(AssemblerTokenType.BlankLineToken)
      case a if a.charAt(0) == ';' => tokenisedLine + Token(AssemblerTokenType.CommentLineToken)
      case _ => tokenisedLine + Token(AssemblerTokenType.NoneCommentLine)
    }
    // split the line on white space
    val fields = line.source.split("\\s+")
    val instruction = processLabel(fields, tokenisedLine)
    processValue(instruction.tail, tokenisedLine)
    processOpcode(instruction.head, tokenisedLine)
    tokenisedLine
  }

  private def processLabel(text: Array[String], tokenisedLine: TokenisedLine ) : Array[String] ={
    println(s"processLabel: ${text.mkString(" ")}")
    val head = text.head
    if (head.takeRight(1) == ":"){
      val labelText = head.dropRight(1)
      tokenisedLine + Token(AssemblerTokenType.LabelToken)
      text.tail
    }
    text
  }

  def processOpcode(text: String, tokenisedLine: TokenisedLine  ) ={
    println(s"processOpcode: ${text}")
  }

  def processValue(text: Array[String], tokenisedLine: TokenisedLine  ) = {
    println(s"processValue: ${text.mkString(" ")}")

  }

  def printLabels = {
    println
    if (labels.isEmpty) println("No labels defined") else for ((label, address) <- labels) {
      println(s"${label} address ${address}")
    }
  }

  def printTokenisedLines = {
    println("\nprintTokenisedLines")
    if (tokenisedLines.isEmpty) println("No lines tokenised") else for (line <- tokenisedLines) {
      println(line.toString)
    }
  }
}

