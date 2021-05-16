package com.udsl.processor6502
package CPU

import UI.NumericFormatType

import com.udsl.processor6502.UI.NumericFormatSelector.numericFormatProperty
import scalafx.beans.property.{IntegerProperty, ObjectProperty}

class ByteValue extends ObjectProperty[NumericFormatType.Value]{
    val MAX_BYTE_VALUE: Int = 256

    val _byte: IntegerProperty = IntegerProperty(0)

    def byte_= (b: Int): Unit = {
        if (b < 0 || b > MAX_BYTE_VALUE) throw new Exception("Byte value out of range.")
        _byte.value = b
    }

    def byte: Int = _byte.value

    override def toString: String = {
        val value = _byte.value
        (numericFormatProperty.value) match {
            case NumericFormatType.HexDecimal => value.toHexString.toUpperCase
            case NumericFormatType.Octal => value.toOctalString
            case NumericFormatType.Binary => value.toBinaryString
            case NumericFormatType.Decimal => value.toString
        }
    }

}

object ByteValue {
    def apply: ByteValue = {
        val b_ = new ByteValue
        b_.byte = 0
        b_
    }

    def apply(b: Int): ByteValue = {
        val b_ = new ByteValue
        b_.byte = b
        b_
    }
}