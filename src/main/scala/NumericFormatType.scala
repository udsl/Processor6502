package com.udsl.processor6502

object NumericFormatType extends Enumeration {
  val Decimal: NumericFormatType.Value = Value("Dec")
  val Binary: NumericFormatType.Value = Value("Bin")
  val Octal: NumericFormatType.Value = Value("Oct")
  val HexDecimal: NumericFormatType.Value = Value("Hex")

}
