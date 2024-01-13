package com.udsl.processor6502.assembler

enum ErrType:
  case SYNTAX, PARSING, ASSEMBLE

trait ErrorRecord(val errorMessage: String):
  def errType: ErrType

class SyntaxErrorRecord(val lineNumber: Int, val sourceText: String, override val errorMessage: String) extends ErrorRecord(errorMessage):
  def errType: ErrType = ErrType.SYNTAX

class ParsingErrorRecord(val sourceText: String, override val errorMessage: String) extends ErrorRecord(errorMessage):
  def errType: ErrType = ErrType.PARSING

class AssembleErrorRecord(override val errorMessage: String) extends ErrorRecord(errorMessage):
  def errType: ErrType = ErrType.ASSEMBLE


object ErrorRecord:
  def apply(errorMessage: String, source: SourceLine): ErrorRecord =
    new SyntaxErrorRecord(source.lineNum, source.text, errorMessage)

  def apply(errorMessage: String, sourceText: String): ErrorRecord =
    new ParsingErrorRecord(sourceText, errorMessage)

  def apply(errorMessage: String): ErrorRecord =
    new AssembleErrorRecord(errorMessage)

