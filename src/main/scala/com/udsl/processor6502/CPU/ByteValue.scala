package com.udsl.processor6502.CPU

import com.udsl.processor6502.CPU.ByteValue.validate
import com.udsl.processor6502.NumericFormatType
import com.udsl.processor6502.UI.NumericFormatSelector.numericFormatProperty
import scalafx.beans.property.IntegerProperty

class ByteValue {

    val _byte: IntegerProperty = IntegerProperty(0)

    def byte_= (b: Int): Unit = {
        validate( b )
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
    val MAX_BYTE_VALUE: Int = 256

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

    def validate( b: Int) = {
        if (b < 0 || b > MAX_BYTE_VALUE) throw new Exception(s"Value out of range for BYTE: ${b}.")
    }
}