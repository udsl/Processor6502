package com.udsl.processor6502.assembler

class AssembleExceptionRecord(val lineNumber: Int, val sourceText: String, val exceptionMessage: String)

object AssembleExceptionRecord:
  def apply(exceptionMessage: String, source: SourceLine): AssembleExceptionRecord =
    new AssembleExceptionRecord(source.lineNum, source.text, exceptionMessage)

class SyntaxErrorRecord(val lineNumber: Int, val sourceText: String, val errorMessage: String)

object SyntaxErrorRecord:
  def apply(errorMessage: String, source: SourceLine): SyntaxErrorRecord =
    new SyntaxErrorRecord(source.lineNum, source.text, errorMessage)


