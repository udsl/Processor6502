package com.udsl.processor6502.assembler

import com.udsl.processor6502.assembler.version1.{TokenisedLine, UntokenisedLine}

class AssembleExceptionRecord(val lineNumber: Int, val sourceText: String, val exceptionMessage: String)

object AssembleExceptionRecord:
  def apply(exceptionMessage: String, source: UntokenisedLine): AssembleExceptionRecord =
    new AssembleExceptionRecord(source.lineNumber, source.sourceText, exceptionMessage)

class SyntaxErrorRecord(val lineNumber: Int, val sourceText: String, val errorMessage: String)

object SyntaxErrorRecord:
  def apply(errorMessage: String, source: ParsedLine): SyntaxErrorRecord =
    new SyntaxErrorRecord(source.lineNumber, source.sourceText, errorMessage)

  def apply(errorMessage: String, source: TokenisedLine): SyntaxErrorRecord =
    new SyntaxErrorRecord(source.lineNumber, source.sourceText, errorMessage)

