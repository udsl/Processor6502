package com.udsl.processor6502.assembler.version1

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Utilities.*
import com.udsl.processor6502.assembler.version1.Assemble6502FirstPass.*
import com.udsl.processor6502.assembler.version1.Assemble6502SecondPass.logger
import com.udsl.processor6502.assembler.*
import com.udsl.processor6502.assembler.AssemblyData.addSyntaxError
import com.udsl.processor6502.cpu.CpuInstructions
import com.udsl.processor6502.cpu.CpuInstructions.{getInstruction, isValidInstruction}
import com.udsl.processor6502.cpu.execution.*
import com.udsl.processor6502.{NumericFormatType, Utilities}


/**
 * Second pass object - Using the the data generated by the first pass does the actual assembly.
 */
object Assemble6502SecondPass extends StrictLogging, Assemble6502BasePass :

  def assemble(tokenisedLine: TokenisedLineV1) : Unit =
    logger.info(s"\n\n2nd Pass ${tokenisedLine.source.lineNum} ")
    for (token <- tokenisedLine.tokens)
      token match
        case BlankLineToken( _, _, _ ) => // extends AssemblerTokenType("BlankLineToken")
          logger.info("\tBlankLineToken ")
        case CommentLineToken( _, _, _ ) => // extends AssemblerTokenType("CommentLineToken")
          logger.info("\tCommentLineToken ")
        case LineComment( _, _, _ ) => // extends AssemblerTokenType("LineComment")
          logger.info("\tLineComment ")
        case LabelToken( _, _, _ ) => // extends AssemblerTokenType("LabelToken")
          procesLabel(token)
        case CommandToken( _, _, _ ) => // extends AssemblerTokenType("CommandToken")
          assembleCommandToken(token)
        case InstructionToken( _, _, _ ) => // extends AssemblerTokenType("InstructionToken")
          assembleInstructionToken(token, tokenisedLine)
        case ClearToken( _, _, _ ) =>
          logger.info("\tClear Token - but we dont clear on 2nd pass!")
        case OriginToken( _, _, _ ) =>
          processOrigin(token)
        case _ => logger.error(s"unsupported case $token")

  def procesLabel(token: AssemblerToken): Unit =
    logger.info("\tprocesLabel ")
    AssemblyData.labels.get(token.mnemonic) match
      case Some((v, bool)) =>
        if bool then token.value = v.toString
      case _ =>
        throw new RuntimeException(s"Label not defined in second pass '${token.mnemonic}'")

  def assembleCommandToken(t: AssemblerToken): Unit =
    logger.info(s"\tassembleCommandToken '$t' - ")
    t.mnemonic.toUpperCase() match
      case "BYT" => setBytes(t.fields)
      case "WRD" => setWords(t.fields)
      case "ADDR" => setAddresses(t.fields)
      case _ => logger.info(s"\tInvalid mnemonic ${t.mnemonic} ")

  def processOrigin(t: AssemblerToken): Unit =
    Utilities.numericValue(t.mnemonic) match
      case Some(value) =>
        if 0 to 65535 contains value then
          AssembleLocation.setAssembleLoc(value)
          logger.info(s"\tOrigin Token 2nd pass: $value")
        else
          throw new Exception(s"Invalid origin value $value")
      case None =>
        throw new Exception("Origin vale not given")


  def assembleInstructionToken(t: AssemblerToken, tl: TokenisedLineV1): AssemblerToken =
    def getValue(operand: String): Option[Int] =
      numericValue(operand).orElse({
        AssemblyData.labels.get(operand) match
          case Some((v, bool)) =>
            Some(v)
          case None =>
            addSyntaxError(SyntaxErrorRecord(s"Undefined label '$operand'", tl.source))
            None
      })

    def getOperandValue: Int =
      val operand = if t.fields.head.charAt(0) == '#' then
        t.fields.head.substring(1)
      else
        t.fields.head
      getValue(operand).get

    def getIndexedOperandValue: Int =
      val operand = t.fields.head.substring(0, t.fields.head.length - 2)
      getValue(operand).get

    /**
     * options: (indirect), (indirect,X) or (indirect),Y
     *
     * @return
     */
    def getIndirectOperandValue: Int =
      val op1 = t.fields.head.substring(1) // remove the leading '('
      if op1.toUpperCase.endsWith("),Y")  || op1.toUpperCase.endsWith(",X)") then // (indirect),Y or (indirect,X)
        getValue(op1.substring(0, op1.length() - 3)).get
      else
        getValue(op1.substring(0, op1.length() - 1)).get // (indirect)

    def validateAddressingMode: (AddressingMode, Int) =
      val sortedModes = t.predictedAddressingModes.sortBy(addrMode => addrMode.size.bytes)

      var res: Option[(AddressingMode, Int)] = None

      for mode: AddressingMode <- sortedModes do
        mode match {
          case Immediate =>
            if CpuInstructions.isAddressingModeValid(t.mnemonic, Immediate) then
              val operandValue = getOperandValue
              if t.fields.head.charAt(0) == '#' && (0 to 255 contains operandValue) then
                res = Some( (Immediate, operandValue) )

          case Absolute | ZeroPage =>
            val operandValue = getOperandValue
            if operandValue > 255 && CpuInstructions.isAddressingModeValid(t.mnemonic, Absolute) then
              res = Some(  (Absolute, operandValue) )
            else
              res = Some(  (ZeroPage, operandValue) )

          case ZeroPageX =>
            if t.fields.head.toUpperCase.contains(",X") && CpuInstructions.isAddressingModeValid(t.mnemonic, ZeroPageX) then
              val operandValue = getIndexedOperandValue
              if 0 to 255 contains operandValue then
                res = Some( (ZeroPageX, operandValue) )

          case ZeroPageY =>
            if t.fields.head.toUpperCase.contains(",Y") && CpuInstructions.isAddressingModeValid(t.mnemonic, ZeroPageY) then
              val operandValue = getIndexedOperandValue
              if 0 to 255 contains operandValue then
                res = Some( (ZeroPageY, operandValue) )

          case AbsoluteX =>
            if t.fields.head.toUpperCase.contains(",X") && CpuInstructions.isAddressingModeValid(t.mnemonic, AbsoluteX) then
              val operandValue = getIndexedOperandValue
              if (0 to 255 contains operandValue) && CpuInstructions.isAddressingModeValid(t.mnemonic, ZeroPageX) then
                res = Some( (ZeroPageX, operandValue) )// prediction was AbsoluteX but actually ZeroPageX, why labels need updating.
              else
                res = Some( (AbsoluteX, operandValue))

          case AbsoluteY =>
            if t.fields.head.toUpperCase.contains(",Y") && CpuInstructions.isAddressingModeValid(t.mnemonic, AbsoluteY) then
              val operandValue = getIndexedOperandValue
              if (0 to 255 contains operandValue) && CpuInstructions.isAddressingModeValid(t.mnemonic, ZeroPageY) then
                res = Some( (ZeroPageY, operandValue) )// prediction was AbsoluteX but actually ZeroPageX, why labels need updating.
              else
                res = Some( (AbsoluteY, operandValue) )


          case IndirectX =>
            if t.fields.head.toUpperCase.contains(",X)") && CpuInstructions.isAddressingModeValid(t.mnemonic, IndirectX) then
              val operandValue = getIndirectOperandValue
              if 0 to 255 contains operandValue then
                res = Some( (IndirectX, operandValue) )

          case IndirectY =>
            if t.fields.head.toUpperCase.contains("),Y") && CpuInstructions.isAddressingModeValid(t.mnemonic, IndirectY) then
              val operandValue = getIndirectOperandValue
              if 0 to 255 contains operandValue then
                res = Some( (IndirectY, operandValue))

          case Indirect =>
            if t.fields.head.charAt(0) == '(' && t.fields.head.endsWith(")") && CpuInstructions.isAddressingModeValid(t.mnemonic, Indirect) then // if it has a comma then its not Indirect
              val operandValue = getIndirectOperandValue
              res = Some( (Indirect, operandValue))

          case Implied =>
            if t.fields.length == 0 && CpuInstructions.isAddressingModeValid(t.mnemonic, Implied) then
              res = Some( (Implied, -1) )// there is no operand

          case Accumulator =>
            if !t.fields.isEmpty && "A".equals(t.fields.head.toUpperCase)  && CpuInstructions.isAddressingModeValid(t.mnemonic, Accumulator) then
              res = Some( (Accumulator, -1) )// there is no operand

          case Relative =>
            // check that mode is appropriate
            if CpuInstructions.isAddressingModeValid(t.mnemonic, Relative) then
              // Target of branch can be a byte value or a label destination
              val operandValue = getOperandValue
              if isLabel(t.fields.head) then // its an address to branch to
                // This offset should be from the following instraction because the 6502 would have increament the PC on each fetch
                // so if we are going backwards need those extra 2 bytes bit if going wards
                // pc is already incremented by 2 so need an offset that is 2 less.
                val offset = operandValue - AssembleLocation.currentLocation
                if offset < 0 && (offset - 2 > -128) then
                  res = Some( (Relative, offset - 2))
                if offset > 0 && offset > 130 then
                  res = Some( (Relative, offset + 2))
                logger.info(s"Relative address out of range ${t.mnemonic}")
              else
                if operandValue > 0 && operandValue < 256 then
                  res = Some( (Relative, operandValue))

          case _ =>
        }
      res match
        case Some( (a, b) )  => (a, b)
        case None => (Invalid, -1)

    logger.info(s"assembleInstructionToken - ${t.mnemonic}")
    
    if isValidInstruction(t.mnemonic) then
      val (addrMode: AddressingMode, value: Int) = validateAddressingMode
      logger.info(s"validateAddressingMode - $addrMode")
      addrMode match
        case Invalid =>
          addSyntaxError(SyntaxErrorRecord(s"Invalid addressing mode for '${t.mnemonic}'", tl.source))
        case _ =>
          //TODO
          // Need details of the instruction for the disassembly byte string
          // mnemonic, value and number bytes
          CpuInstructions.getInstructionOpcodeBytes(t.mnemonic, addrMode).foreach((opcode, bytes) =>
            setMemoryByte(opcode, constructSourceLine(t.mnemonic, addrMode, (value % 256, value / 256)))
            // now we know how long the instruction should be
            bytes match
              case 1 =>
                // implied or accumulator so nothing else to write.
              case 2 =>
                // Imeadiate, zeropage etc
                setMemoryByte(value)
              case 3 =>
                // Absolute
                setMemoryAddress(value)
              case _ =>
                throw new Exception(s"SYSTEM ERROR INVALID BYTES FOR INSTRUCTION - ${t.mnemonic}"))
    NoTokenToken("", Array[String](), SourceLine())

  def setBytes(fields: Array[String]): Unit =
    logger.debug("setBytes")
    for (value <- fields)
      setMemoryByte(value.trim)

  def setWords(fields: Array[String]): Unit =
    logger.debug("setWords")
    for (value <- fields)
      setMemoryWord(value.trim)

  def setMemoryWord(v: String): Unit =
    if v.charAt(0).isLetter then // a label
      AssemblyData.addReference(v)
      AssembleLocation.setMemoryWord(0x6363) // word value for 99, 99 decimal
    else
      AssembleLocation.setMemoryWord(if v.charAt(0) == '$' then
        Integer.parseInt(v.substring(1), 16)
      else
        Integer.parseInt(v))

  def setAddresses(fields: Array[String]): Unit =
    logger.debug("setAddresses")
    for (v <- fields)
      val value = v.trim
      setMemoryAddress(numericValue(value).getOrElse({
        AssemblyData.labelValue(value).getOrElse( throw new Exception(s"Invalid value address '$value'"))
      }), true)