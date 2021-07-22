package com.udsl.processor6502

import scalafx.beans.property.ObjectProperty
import scalafx.scene.input.KeyCode.Decimal


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
  def fromStr(str: String): Unit = {
    str.toLowerCase match {
      case "bin" => value = NumericFormatType.Binary
      case "oct" => value = NumericFormatType.Octal
      case "hex" => value = NumericFormatType.HexDecimal
      case _ => value = NumericFormatType.Decimal
    }
  }

}

