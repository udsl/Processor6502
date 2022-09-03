package com.udsl.processor6502.cpu

import com.udsl.processor6502.cpu.ByteValue.validate
import com.udsl.processor6502.NumericFormatType
import com.udsl.processor6502.ui.NumericFormatSelector.numericFormatProperty
import com.udsl.processor6502.Utilities.{numToByteString, numToString}
import com.udsl.processor6502.cpu.execution.NotApplicable
import scalafx.beans.property.IntegerProperty

class ByteValue {
    val _byte: IntegerProperty = IntegerProperty(0)
    private var disassembly: String = ""

    def byte_= (b: Int): Unit = {
        validate( b )
        _byte.value = b
    }

    def setDisassembly(theDisassembly: String): Unit =
        disassembly = theDisassembly.trim

    def getDisassembly: String =
        disassembly

    def asSignedValue:Int =
        ByteValue.asSignedValue(_byte.value)
          
    def byte: Int = _byte.value

    override def toString: String =
        numToString( _byte.value)

    def toDisplayString(format: NumericFormatType): String =
      numToByteString(byte, format)

    def asSerilisedString: String =
       s"${_byte.value}:$disassembly"

}

object ByteValue {
    val MAX_BYTE_VALUE: Int = 255
    val MIN_BYTE_VALUE: Int = -127

    def apply: ByteValue = {
        val b_ = new ByteValue
        b_.byte = 0
        b_
    }

    // Byte values are signed or unsigned but always stored as unsigned
    def apply(b: Int): ByteValue = {
        val b_ = new ByteValue
        b_.byte = if b < 0 then b & 255 else b
        b_
    }

    def apply(b: Int, theDisassembly: String): ByteValue = {
        val b_ = new ByteValue
        b_.byte = if b < 0 then b & 255 else b
        b_.setDisassembly(theDisassembly)
        b_
    }

    def validate( b: Int): Unit = {
        if (b < MIN_BYTE_VALUE || b > MAX_BYTE_VALUE) throw new Exception(s"Value out of range for BYTE: $b.")
    }

    def asSignedValue(value: Int):Int =
        validate(value)
        if value > 128 then
            -(256 - value)
        else
            value

}