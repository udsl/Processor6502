package com.udsl.processor6502.assembler

import com.udsl.processor6502.assembler.Assemble6502.tokeniseLine
import com.udsl.processor6502.assembler.AssemblerTokenType.{BlankLineToken, CommentLineToken, LabelToken}

import scala.Console.println
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Assemble6502 {
  var currentLocation: Int = 0
  var labels = new mutable.HashMap[String, Int]()
  var tokenisedLines = new ListBuffer[TokenisedLine]()

  def assemble(source: String): Unit = {
    // Create a list of lines
    val allLines = for ((str, index) <- source.split("\n").zipWithIndex)
      yield new UntokenisedLine(index + 1, str)
    for ( lineToTokenise <- allLines){
      tokenisedLines.addOne(tokeniseLine(lineToTokenise))
    }

    println
    println

    for line <- tokenisedLines do
      println(line)

  }

  def assemble( source: String, location: Int): Unit ={
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
    val allLines = for ((str: String, index: Int) <- source.split("\n").zipWithIndex)
      yield new UntokenisedLine(index + 1, str)
    // Create tokens from the line list
    for ( lineToTokenise <- allLines){
      tokenisedLines.addOne(tokeniseLine(lineToTokenise))
    }

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


object Assemble6502 {
  val validInstructions = List("ORA","AND","EOR","ADC","STA","LDA","CMP","SBC","ASL","ROL","LSR","ROR","STX","LDX","DEC","INC","BIT","JMP","lue","JMP","STY","LDY","CPY","CPX")

  def apply(): Assemble6502 =
    val asm = new Assemble6502()
    asm


  def tokeniseLine(line: UntokenisedLine) =
    println(s"\ntokeniseLine: ${line}")
    // determine basic line type
    val tokenisedLine = TokenisedLine(line)
    val token = line.source.trim match {
      case "" => Token(AssemblerTokenType.BlankLineToken)
      case a if a.charAt(0) == ';' => Token(AssemblerTokenType.CommentLineToken, line.source.trim, TokenValue.apply)
      case _ => Token(AssemblerTokenType.NoneCommentLine)
    }

    token.typeOfToken match
      case BlankLineToken |  CommentLineToken =>
        tokenisedLine + token
      case _ =>
        // if we have a NoneCommentLine then must be either
        //           operation operant + optional comment
        //       or a command
        // lets deal with the potential comment first
        val commentSplit = line.source.split(";")

        // remove comment and split rest of line into fields
        val fields =
          if commentSplit.length > 1 then
            // we have a comment
            tokenisedLine + Token(AssemblerTokenType.LineComment, commentSplit.tail.mkString)
            commentSplit.head.split("\\s+")
          else
            line.source.split("\\s+")

        // command | Label |  Label instruction
        if !processCommand(fields, tokenisedLine) then
          val instruction = processLabel(fields, tokenisedLine)
          if !instruction.isEmpty then
            val tokenisedInstruction = processInstruction(instruction, tokenisedLine)
            if tokenisedInstruction.typeOfToken == AssemblerTokenType.InstructionToken then
              processValue(instruction.tail, tokenisedLine, tokenisedInstruction)

    tokenisedLine


  private def processLabel(text: Array[String], tokenisedLine: TokenisedLine ) : Array[String] =
    println(s"processLabel: ${text.mkString(" ")}")
    val head = text.head
    if head.takeRight(1) == ":" then
      val labelText = head.dropRight(1)
      val token = Token(AssemblerTokenType.LabelToken, labelText)
      tokenisedLine + token
      println(s"token added: ${token}")
      text.tail
    else
      text


  private def processCommand(text: Array[String], tokenisedLine: TokenisedLine ) : Boolean =
    println(s"processCommand: ${text.mkString(" ")}")
    if !text.isEmpty then
      val head = text.head.toUpperCase
      head match {
        case "ORIG" | "BYT" | "WRD" => {
          val value = text.tail
          val token = if value.isEmpty then
            Token(AssemblerTokenType.SyntaxErrorToken, "Value not given")
          else
            Token(AssemblerTokenType.CommandToken, head, TokenValue(value))
          tokenisedLine + token
          println(s"token added: ${token}")
          return true
        }

        case _ => {
        }
      }
    return false


  def processInstruction(text: Array[String], tokenisedLine: TokenisedLine ) : Token =
    println(s"processInstruction: ${text.mkString(" ")}")
    val instruction = text.head.toUpperCase()
    val token = if validInstructions.contains(instruction) then
      Token(AssemblerTokenType.InstructionToken, instruction)
    else
      Token(AssemblerTokenType.SyntaxErrorToken, s"Invalid instruction: ${instruction}")
    tokenisedLine + token
    token

  def processValue(text: Array[String], tokenisedLine: TokenisedLine, token: Token ) =
    println(s"processValue: ${text.mkString(" ")}")

    println(s"No operand for ${token.tokenStr}")
    // Possible values and associated adressing mode:
    //      numeric - starts with a digit or $ for hex - absolute or zero page
    //      label - starts with an alph but not ( absolute mode to label
    //      imeadiate address mode starts with #
    //      indirect addresing mode starts with ( some for of indexed addressing
    //      nothing implied addressing

    // At this point we only need to tokenise the addressign mode not work out if its valid.
    if text.isEmpty then // applied addrtessing mode
      token.tokenVal.pridictedMode = PredictedAddressingModes.Immediate
    else
      token.tokenVal.pridictedMode = PredictedAddressingModes.AbsoluteXOrZeroPageX
}

