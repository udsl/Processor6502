package com.udsl.processor6502
package UI

import scalafx.beans.property.ObjectProperty


object NumericFormatType extends Enumeration {
  val Decimal: NumericFormatType.Value = Value("Dec")
  val Binary: NumericFormatType.Value = Value("Bin")
  val Octal: NumericFormatType.Value = Value("Oct")
  val HexDecimal: NumericFormatType.Value = Value("Hex")
}

class NumericFormatClass{
  var value: NumericFormatType.Value = NumericFormatType.Decimal
}

class NumericFormatProperty extends ObjectProperty[NumericFormatType.Value]{

}

