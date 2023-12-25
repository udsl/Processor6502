package com.udsl.processor6502.assembler.version2

import com.udsl.processor6502.Utilities.isLabel
import com.udsl.processor6502.assembler.{AssemblyData, LabelFactory, SourceLine}
import com.udsl.processor6502.cpu.{CpuInstruction, CpuInstructions}

object TokeniserV2 :
  def tockenise(line: SourceLine) : TokenisedLineV2 =
    val tokenisedLine = TokenisedLineV2.apply(line)

    val toTokenise = line.text.trim
    // Blank line
    if toTokenise == "" then
      tokenisedLine.add(BlankLineTokenV2.apply(Array("")))
    // line comment
    else if toTokenise.head == ';' then
      tokenisedLine.add(CommentLineTokenV2.apply(Array(toTokenise.tail)))
    else
      // Does this line have a comment, comment could have a ; in it so cant split
      val semicolonAt = toTokenise.indexOf(';')

      val beforeComment = if semicolonAt > 0 then
        // The comment is the sub string from semicolonAt otherwise it would have been a comment line above.
        // We cant use split because the comment itself may contain a ';'
        tokenisedLine.add(LineCommentTokenV2.apply(toTokenise.substring(semicolonAt).trim, Array()))
        toTokenise.substring(0, semicolonAt).trim
      else
        toTokenise

      // is it a command
      val beforeCommentSplit = beforeComment.split("\\s+")
      if !(beforeCommentSplit.head.toUpperCase() match {
        case "ADDR" | "BYT" | "WRD" | "ORIG" | "CLR" | "DEF" | "TXT" =>
          tokenisedLine.add(CommandTokenV2.apply(beforeCommentSplit.head.toUpperCase(),
            if beforeCommentSplit.tail.length > 0 then Array(beforeCommentSplit.tail.mkString(" ")) else Array()))
          true
        case _ => false
      }) then
        // Dose this line have a label ie does the beforeCommentSplit.head end with ':'
        val ins: String = if beforeCommentSplit.head.endsWith(":") then
          val labelText = beforeCommentSplit.head.dropRight(1)
          if isLabel(labelText) then
            tokenisedLine.add(LabelTokenV2.apply(labelText,
              if beforeCommentSplit.tail.length > 0 then Array(beforeCommentSplit.tail.mkString(" ")) else Array()))
            LabelFactory.addLabel(labelText, beforeCommentSplit.tail)
          else
            tokenisedLine.add(SytaxErrorTokenV2.apply(s"Bad label test ${beforeCommentSplit.head}", beforeCommentSplit))
          beforeCommentSplit.tail.mkString(" ")
        else
          beforeComment

        // is this line an instruction
        val fields = ins.trim.split("\\s+")
        try
          tokenisedLine.add(InstructionTokenV2.apply(fields.head, fields.tail))
        catch
          case e: Exception =>
            tokenisedLine.add(SytaxErrorTokenV2.apply(e.getMessage, fields.tail))
    
    tokenisedLine

