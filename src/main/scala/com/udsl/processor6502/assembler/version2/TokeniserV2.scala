package com.udsl.processor6502.assembler.version2

import com.udsl.processor6502.assembler.ErrorRecord
import com.udsl.processor6502.Utilities.{getExpression, isExpression, isLabel, splitExpression}
import com.udsl.processor6502.assembler.AssembleLocation.currentLocation
import com.udsl.processor6502.assembler.AssemblyData.addError
import com.udsl.processor6502.assembler.{AssemblyData, LabelFactory, SourceLine, SyntaxErrorRecord}
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
        case "ADDR" | "BYT" | "WRD" | "ORIG" =>
          tokenisedLine.add(CommandTokenV2.apply(beforeCommentSplit.head.toUpperCase(),
            if beforeCommentSplit.tail.length > 0 then Array(beforeCommentSplit.tail.mkString(" ")) else Array()))
          true

        case  "CLR" =>
          // CLR only valid on first line
          if line.lineNum > 1 then
            addError(ErrorRecord.apply("CLR only valid on first line", line))
          tokenisedLine.add(CommandTokenV2.apply(beforeCommentSplit.head.toUpperCase(),
            if beforeCommentSplit.tail.length > 0 then Array(beforeCommentSplit.tail.mkString(" ")) else Array()))
          true

        case "DEF" =>
          val parts: Array[String] = beforeCommentSplit.tail
          // fist part must be the label being defined
          // 2nd is the value which is an expression that can contain a label
          if parts.tail.isEmpty then
            addError(ErrorRecord.apply("Bad DEF - no expression", line))
          else if !isLabel(parts.head) then
            addError(ErrorRecord("DEF should define a label", line))
            // if we have a tain greate than length 1 then it must be a expression with spaces
          else if parts.tail.length > 1 && !isExpression(parts.tail.mkString(" ")) then
            addError(ErrorRecord("DEF definition should be an expression, a label or a number", line))
            // If its only length 1 it could be an expression without spaces
          else if isExpression(parts.tail.mkString(" ")) || isLabel(parts.tail.mkString(" ")) then
            // process the expression
            val expression = parts.tail
            // need to split the expression into parts
            var expresionParts = List[String]()
            for(exp: String <- expression)
              if isExpression(exp) then
                // its a sub expression that needs to be split up
                expresionParts = expresionParts.appendedAll(splitExpression(exp))
              else
                expresionParts = expresionParts.appended(exp) 
              
            LabelFactory.addLabel(parts.head, expresionParts.toArray)
            
            tokenisedLine.add(CommandDefTokenV2(parts.head, expresionParts.toArray))
          true

        case "TXT" =>
          val txt = toTokenise.substring(1, toTokenise.indexOf("'"))
          tokenisedLine.add(CommandTokenV2.apply(beforeCommentSplit.head.toUpperCase(),
            Array(txt.substring(0,txt.length-1))))
          true
        case _ => false
      }) then
        // Dose this line have a label ie does the beforeCommentSplit.head end with ':'
        val ins: String = if beforeCommentSplit.head.endsWith(":") then
          val labelText = beforeCommentSplit.head.dropRight(1)
          if isLabel(labelText) then
            beforeCommentSplit.tail.length match
              case 0 =>
                // Just a label
                tokenisedLine.add(LabelTokenV2.apply(labelText, Array()))
                LabelFactory.addLabel(labelText, currentLocation)
              case 1 =>
                // instruction
                tokenisedLine.add(LabelTokenV2.apply(labelText, Array()))
                LabelFactory.addLabel(labelText, currentLocation)
              case _ =>
                // instruction  with operand or command, if it a command them dela with it
                beforeCommentSplit.tail.head.toUpperCase() match
                  case "ADDR" | "BYT" | "WRD" =>
                    // we have a labeled value command, could be an expression
                    val expressionString = beforeCommentSplit.tail.tail.mkString(" ")
                    if isExpression(expressionString) then
                      tokenisedLine.add(LabelTokenV2.apply(labelText,
                        if beforeCommentSplit.tail.tail.length > 0 then beforeCommentSplit.tail.tail else Array()))
                    else // need label token & command tocken
                      tokenisedLine.add(LabelTokenV2.apply(labelText, Array()))
                      tokenisedLine.add(CommandTokenV2.apply(beforeCommentSplit.tail.head.toUpperCase(),
                        if beforeCommentSplit.tail.tail.length > 0 then Array(beforeCommentSplit.tail.tail.mkString(" ")) else Array()))

                  case _ =>
                    tokenisedLine.add(LabelTokenV2.apply(labelText, Array()))
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

