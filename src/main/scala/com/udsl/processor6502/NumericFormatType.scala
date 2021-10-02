package com.udsl.processor6502:

  import scalafx.beans.property.ObjectProperty
//  import scalafx.scene.input.KeyCode.Decimal

  enum NumericFormatType:
    case DEC, BIN, OCT, HEX

//  enum NumericFormatType(val str: String):
//    case Decimal extends NumericFormatType("dec")
//    case Binary extends NumericFormatType("bin")
//    case Octal extends NumericFormatType("oct")
//    case HexDecimal extends NumericFormatType("hex")

  //object NumericFormatType extends Enumeration {
  //  val Decimal: NumericFormatType.Value = Value("Dec")
  //  val Binary: NumericFormatType.Value = Value("Bin")
  //  val Octal: NumericFormatType.Value = Value("Oct")
  //  val HexDecimal: NumericFormatType.Value = Value("Hex")
  //}

  class NumericFormatClass{
    var value: NumericFormatType = NumericFormatType.DEC
  }

  class NumericFormatProperty extends ObjectProperty[NumericFormatType]:
    def fromStr(str: String): Unit = {
      str.toLowerCase match {
        case "bin" => value = NumericFormatType.BIN
        case "oct" => value = NumericFormatType.OCT
        case "hex" => value = NumericFormatType.HEX
        case _ => value = NumericFormatType.DEC
      }
    }


