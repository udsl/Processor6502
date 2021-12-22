package com.udsl.processor6502.assembler

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Utilities
import com.udsl.processor6502.Utilities.{isLabel, isNumeric, numericValue}
import com.udsl.processor6502.assembler.Assemble6502FirstPass.{processClear, setAddresses, setBytes, setWords}
import com.udsl.processor6502.assembler.Assemble6502SecondPass.logger
import com.udsl.processor6502.cpu.CpuInstructions
import com.udsl.processor6502.cpu.CpuInstructions.{getInstruction, isValidInstruction}
import com.udsl.processor6502.cpu.execution.{Absolute, AbsoluteX, AbsoluteY, Accumulator, AddressingMode, Immediate, Implied, Indirect, IndirectX, IndirectY, Invalid, Relative, ZeroPage, ZeroPageX, ZeroPageY}


/**
 * Second pass object - Using the the data generated by the first pass does the actual assembly.
 */
object Assemble6502SecondPass extends StrictLogging, Assemble6502PassBase :

  def assemble(tokenisedLine: TokenisedLine) : Unit =
    logger.info(s"\n\n2nd Pass ${tokenisedLine.sourceLine.lineNumber} ")
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
          assembleInstructionToken(token)
        case SyntaxErrorToken( _, _ ) => // extends AssemblerTokenType("SyntaxErrorToken")
          logger.info("\tSyntaxErrorToken ")
        case ClearToken( _, _ ) =>
          logger.info("\tClear Token")
          processClear(token, tokenisedLine)

        case _ => logger.error(s"unsupported case ${token.value}")
      }

  def procesLabel(token: AssemblerToken): Unit =
    logger.info("\tprocesLabel ")

  def assembleCommandToken(t: AssemblerToken): Unit =
    logger.info(s"\tassembleCommandToken '${t}' - ")
    t.mnemonic.toUpperCase() match
      case "ORIG" => AssembleLocation.setAssembleLoc(t.intValue)
      case "BYT" => setBytes(t.fields)
      case "WRD" => setWords(t.fields)
      case "ADDR" => setAddresses(t.fields)
      case _ => logger.info(s"\tInvalid mnemonic ${t.value} ")

  def processOrigin(t: AssemblerToken): Unit =
    logger.info("\tOrigin Token 2nd pass")
    val value = Utilities.numericValue(t.fields.head)
    if value > 0 then
      AssembleLocation.setAssembleLoc(value)

  def assembleInstructionToken(t: AssemblerToken): Unit =
    def getValue(operand: String): Int =
      if isNumeric(operand) then
        numericValue(operand)
      else // only other possibility is a label
        val labelValue = AssemblyData.labels.getOrElse(operand, -1)
        labelValue

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
      val sortedModes = t.predictedAddressingModes.sortBy(_.bytes)
      for mode: AddressingMode <- sortedModes do
        mode match {
          case Immediate =>
            val operandValue = getOperandValue
            if t.fields.head.charAt(0) == '#' && (0 to 255 contains operandValue) then
              return (Immediate, operandValue)

          case Absolute | ZeroPage =>
            val operandValue = getOperandValue
            if operandValue > 255 then
              return (Absolute, operandValue)
            else
              return (ZeroPage, operandValue)

          case ZeroPageX =>
            val operandValue = getIndexedOperandValue
            if (0 to 255 contains operandValue) then
              return (ZeroPageX, operandValue)

          case AbsoluteX =>
            if t.fields.head.toUpperCase.contains(",X") then
              val operandValue = getIndexedOperandValue
              if (0 to 255 contains operandValue) then
                return (ZeroPageX, operandValue) // prediction was AbsoluteX but actually ZeroPageX, why labels need updating.
              else
                return (AbsoluteX, operandValue)

          case AbsoluteY =>
            if t.fields.head.toUpperCase.contains(",Y") then
              val operandValue = getIndexedOperandValue
              return (AbsoluteY, operandValue) // no ZeroPageY

          case IndirectX =>
            if t.fields.head.toUpperCase.contains(",X)") then
              val operandValue = getIndirectOperandValue
              if (0 to 255 contains operandValue) then
                return (IndirectX, operandValue)

          case IndirectY =>
            if t.fields.head.toUpperCase.contains("),Y") then
              val operandValue = getIndirectOperandValue
              if (0 to 255 contains operandValue) then
                return (IndirectY, operandValue)

          case Indirect =>
            if !t.fields.head.contains(",") then // if it has a comma then its not Indirect
              val operandValue = getIndirectOperandValue
              return (Indirect, operandValue)

          case Implied | Accumulator =>
            if t.fields.length == 0 then
              return (Implied, -1) // there is no operand

          case Relative =>
            // Target of branch can be a byte value or a label destination
            val operandValue = getOperandValue
            if isLabel(t.fields.head) then // its an address to branch to
              val offset = operandValue - AssembleLocation.currentLocation
              if offset > -128 && offset < 128 then
                return (Relative, offset)
            else
              if operandValue > 0 && operandValue < 256 then
                return (Relative, operandValue)

          case _ =>
        }
      (Invalid, -1)

    if isValidInstruction(t.mnemonic) then
      val (addrMode: AddressingMode, value: Int) = validateAddressingMode
      val (opcode: Int, bytes:Int) = CpuInstructions.getInstructionOpcodeBytes(t.mnemonic, addrMode)
      if opcode < 0 then
          throw new Exception(s"Invalid instruction/Addressing mode - mnemonic: ${t.mnemonic}, operand: ${t.fields.mkString(", ")}, predicted: ${t.predictedAddressingModes}, actual: $addrMode" )
      setMemoryByte(opcode)
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
          throw new Exception("SYSTEM ERROR INVALID BYTES FOR INSTRUCTION!")


