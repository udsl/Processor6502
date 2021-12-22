package com.udsl.processor6502.cpu

import com.udsl.processor6502.cpu.ByteValue.validate
import com.udsl.processor6502.NumericFormatType
import com.udsl.processor6502.ui.NumericFormatSelector.numericFormatProperty
import com.udsl.processor6502.Utilities.{numToByteString, numToString}
import scalafx.beans.property.IntegerProperty

class ByteValue {

    val _byte: IntegerProperty = IntegerProperty(0)

    def byte_= (b: Int): Unit = {
        validate( b )
        _byte.value = b
    }

    def byte: Int = _byte.value

    override def toString: String =
        numToString( _byte.value)

    def toDisplayString(format: NumericFormatType): String =
      numToByteString(byte, format)
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