package com.udsl.processor6502.assembler

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.{NumericFormatType, Utilities}
import com.udsl.processor6502.Utilities.{constructSourceLine, isLabel, isNumeric, numToByteString, numToWordString, numericValue}
import com.udsl.processor6502.assembler.Assemble6502FirstPass.{advanceAssemLocForAddresses, advanceAssemLocForBytes, advanceAssemLocForWords, logger, processClear}
import com.udsl.processor6502.assembler.Assemble6502SecondPass.logger
import com.udsl.processor6502.assembler.Parser.addSyntaxError
import com.udsl.processor6502.cpu.CpuInstructions
import com.udsl.processor6502.cpu.CpuInstructions.{getInstruction, isValidInstruction}
import com.udsl.processor6502.cpu.execution.{Absolute, AbsoluteX, AbsoluteY, Accumulator, AddressingMode, Immediate, Implied, Indirect, IndirectX, IndirectY, Invalid, NotApplicable, Relative, Unknown, ZeroPage, ZeroPageX, ZeroPageY}


/**
 * Second pass object - Using the the data generated by the first pass does the actual assembly.
 */
object Assemble6502SecondPass extends StrictLogging, Assemble6502PassBase :

  def assemble(tokenisedLine: TokenisedLine) : AssemblerToken =
    logger.info(s"\n\n2nd Pass ${tokenisedLine.lineNumber} ")
    for (token <- tokenisedLine.tokens)
      token match {
        case BlankLineToken( _, _ ) => // extends AssemblerTokenType("BlankLineToken")
          logger.info("\tBlankLineToken ")
        case CommentLineToken( _, _ ) => // extends AssemblerTokenType("CommentLineToken")
          logger.info("\tCommentLineToken ")
        case LineComment( _, _ ) => // extends AssemblerTokenType("LineComment")
          logger.info("\tLineComment ")
        case NoneCommentLine( _, _ ) => // extends AssemblerTokenType("NoneCommentLine")
          logger.info("\tNoneCommentLine ")
        case LabelToken( _, _ ) => // extends AssemblerTokenType("LabelToken")
          procesLabel(token)
        case CommandToken( _, _ ) => // extends AssemblerTokenType("CommandToken")
          assembleCommandToken(token)
        case InstructionToken( _, _ ) => // extends AssemblerTokenType("InstructionToken")
          return assembleInstructionToken(token, tokenisedLine)
        case ClearToken( _, _ ) =>
          logger.info("\tClear Token - but we dont clear on 2nd pass!")
        case OriginToken( _, _ ) =>
          processOrigin(token)

        case _ => logger.error(s"unsupported case $token")
      }
    NoTokenToken("", Array[String]())

  
  def procesLabel(token: AssemblerToken): Unit =
    logger.info("\tprocesLabel ")

  def assembleCommandToken(t: AssemblerToken): Unit =
    logger.info(s"\tassembleCommandToken '$t' - ")
    t.mnemonic.toUpperCase() match
      case "BYT" => setBytes(t.fields)
      case "WRD" => setWords(t.fields)
      case "ADDR" => setAddresses(t.fields)
      case _ => logger.info(s"\tInvalid mnemonic ${t.value} ")

  def processOrigin(t: AssemblerToken): Unit =
    logger.info("\tOrigin Token 2nd pass")
    val value = Utilities.numericValue(t.mnemonic)
    if 0 to 65535 contains value then
      AssembleLocation.setAssembleLoc(value)


  def assembleInstructionToken(t: AssemblerToken, tl: TokenisedLine): AssemblerToken =
    def getValue(operand: String): Int =
      if isNumeric(operand) then
        numericValue(operand)
      else // only other possibility is a label
        AssemblyData.labels.get(operand) match
          case Some((v, bool)) =>
            v
          case None =>
            addSyntaxError(SyntaxErrorRecord(s"Undefined label '$operand'", tl))
            -1

    def getOperandValue: Int =
      val operand = if t.fields.head.charAt(0) == '#' then
        t.fields.head.substring(1)
      else
        t.fields.head
      getValue(operand)

    def getIndexedOperandValue: Int =
      val operand = t.fields.head.substring(0, t.fields.head.length - 2)
      getValue(operand)

    /**
     * options: (indirect), (indirect,X) or (indirect),Y
     *
     * @return
     */
    def getIndirectOperandValue: Int =
      val op1 = t.fields.head.toUpperCase.substring(1) // remove the leading '('
      if op1.substring(op1.length() - 3) == "),Y"  || op1.substring(op1.length() - 3) == ",X)" then // (indirect),Y or (indirect,X)
        getValue(op1.substring(0, op1.length() - 3))
      else
        getValue(op1.substring(0, op1.length() - 1)) // (indirect)

    def validateAddressingMode: (AddressingMode, Int) =
      val sortedModes = t.predictedAddressingModes.sortBy(addrMode => addrMode.size.bytes)
      for mode: AddressingMode <- sortedModes do
        mode match {
          case Immediate =>
            if CpuInstructions.isAddressingModeValid(t.mnemonic, Immediate) then
              val operandValue = getOperandValue
              if t.fields.head.charAt(0) == '#' && (0 to 255 contains operandValue) then
                return (Immediate, operandValue)

          case Absolute | ZeroPage =>
            val operandValue = getOperandValue
            if operandValue > 255 && CpuInstructions.isAddressingModeValid(t.mnemonic, Absolute) then
              return (Absolute, operandValue)
            else
              return (ZeroPage, operandValue)

          case ZeroPageX =>
            if t.fields.head.toUpperCase.contains(",X") && CpuInstructions.isAddressingModeValid(t.mnemonic, ZeroPageX) then
              val operandValue = getIndexedOperandValue
              if 0 to 255 contains operandValue then
                return (ZeroPageX, operandValue)

          case ZeroPageY =>
            if t.fields.head.toUpperCase.contains(",Y") && CpuInstructions.isAddressingModeValid(t.mnemonic, ZeroPageY) then
              val operandValue = getIndexedOperandValue
              if 0 to 255 contains operandValue then
                return (ZeroPageY, operandValue)

          case AbsoluteX =>
            if t.fields.head.toUpperCase.contains(",X") && CpuInstructions.isAddressingModeValid(t.mnemonic, AbsoluteX) then
              val operandValue = getIndexedOperandValue
              if (0 to 255 contains operandValue) && CpuInstructions.isAddressingModeValid(t.mnemonic, ZeroPageX) then
                return (ZeroPageX, operandValue) // prediction was AbsoluteX but actually ZeroPageX, why labels need updating.
              else
                return (AbsoluteX, operandValue)

          case AbsoluteY =>
            if t.fields.head.toUpperCase.contains(",Y") && CpuInstructions.isAddressingModeValid(t.mnemonic, AbsoluteY) then
              val operandValue = getIndexedOperandValue
              if (0 to 255 contains operandValue) && CpuInstructions.isAddressingModeValid(t.mnemonic, ZeroPageY) then
                return (ZeroPageY, operandValue) // prediction was AbsoluteX but actually ZeroPageX, why labels need updating.
              else
                return (AbsoluteY, operandValue)


          case IndirectX =>
            if t.fields.head.toUpperCase.contains(",X)") && CpuInstructions.isAddressingModeValid(t.mnemonic, IndirectX) then
              val operandValue = getIndirectOperandValue
              if 0 to 255 contains operandValue then
                return (IndirectX, operandValue)

          case IndirectY =>
            if t.fields.head.toUpperCase.contains("),Y") && CpuInstructions.isAddressingModeValid(t.mnemonic, IndirectY) then
              val operandValue = getIndirectOperandValue
              if 0 to 255 contains operandValue then
                return (IndirectY, operandValue)

          case Indirect =>
            if !t.fields.head.contains(",") && CpuInstructions.isAddressingModeValid(t.mnemonic, Indirect) then // if it has a comma then its not Indirect
              val operandValue = getIndirectOperandValue
              return (Indirect, operandValue)

          case Implied =>
            if t.fields.length == 0 && CpuInstructions.isAddressingModeValid(t.mnemonic, Implied) then
              return (Implied, -1) // there is no operand

          case Accumulator =>
            if !t.fields.isEmpty && "A".equals(t.fields.head.toUpperCase)  && CpuInstructions.isAddressingModeValid(t.mnemonic, Accumulator) then
              return (Accumulator, -1) // there is no operand

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
                  return (Relative, offset - 2)
                if offset > 0 && offset > 130 then
                  return (Relative, offset + 2)
                logger.info(s"Relative address out of range ${t.mnemonic}")
              else
                if operandValue > 0 && operandValue < 256 then
                  return (Relative, operandValue)

          case _ =>
        }
      (Invalid, -1)

    logger.info(s"assembleInstructionToken - ${t.mnemonic}")
    if isValidInstruction(t.mnemonic) then
      val (addrMode: AddressingMode, value: Int) = validateAddressingMode
      logger.info(s"validateAddressingMode - $addrMode")
      addrMode match
        case Invalid =>
          addSyntaxError(SyntaxErrorRecord(s"Invalid addressing mode for '${t.mnemonic}'", tl))
        case _ =>
          //TODO
          // Need details of the instruction for the disassembly byte string
          // mnemonic, value and number bytes
          val (opcode: Int, bytes:Int) = CpuInstructions.getInstructionOpcodeBytes(t.mnemonic, addrMode)
          setMemoryByte(opcode, constructSourceLine(t.mnemonic, addrMode, (value % 256, value / 256)))
          // now we know how long the instruction shoud be
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
              throw new Exception(s"SYSTEM ERROR INVALID BYTES FOR INSTRUCTION - ${t.mnemonic}")
    NoTokenToken("", Array[String]())

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
      if isNumeric(value) then
        setMemoryAddress(numericValue(value))
      else if isLabel(value) then
        AssemblyData.labelIsDefined(value)
        setMemoryAddress(AssemblyData.labelValue(value))
      else
        throw new Exception(s"Invalid value address '$value'")