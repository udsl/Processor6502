package com.udsl.processor6502.assembler

class AssemblerException(message: String, reason: String) extends Exception(message) {
  
  def this(message: String, reason: String, cause: Throwable) =
    this(message, reason)
    initCause(cause)

  def this(message: String, cause: Throwable) =
    this(message, Option(cause).map(_.toString).orNull, cause)

  def this() =
    this(null: String, "Unknown")

  def getReason: String =
    reason
}
