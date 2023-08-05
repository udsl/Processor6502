package com.udsl.processor6502.cpu

import com.udsl.processor6502.cpu.ByteValue.validate
import com.udsl.processor6502.NumericFormatType
import com.udsl.processor6502.ui.NumericFormatSelector.numericFormatProperty
import com.udsl.processor6502.Utilities.{numToByteString, numToString}
import com.udsl.processor6502.cpu.execution.NotApplicable
import scalafx.beans.property.IntegerProperty

class ByteValue(val value: Option[Int]) {
    private var disassembly: String = ""

    def setDisassembly(theDisassembly: String): Unit =
        disassembly = theDisassembly.trim

    def getDisassembly: String =
        disassembly

    def asSignedValue:Int =
        ByteValue.asSignedValue(byte)
          
    def byte: Int = value.getOrElse(throw Exception("Byte not set"))

    override def toString: String =
        numToString(byte)

    def toDisplayString(format: NumericFormatType): String =
      value.map( f => numToByteString(f, format) ).getOrElse("UNSET")


    def asSerilisedString: String =
       s"$byte:$disassembly"

}

object ByteValue {
    val MAX_BYTE_VALUE: Int = 255
    val MIN_BYTE_VALUE: Int = -127

    def apply: ByteValue =
        new ByteValue(None)

    // Byte values are signed or unsigned but always stored as unsigned
    def apply(b: Int): ByteValue =
        validate(b)
        new ByteValue( if b < 0 then Some(b & 255) else Some(b) )


    def apply(b: Int, theDisassembly: String): ByteValue = {
        validate(b)
        val b_ = new ByteValue( if b < 0 then Some(b & 255) else Some(b) )
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