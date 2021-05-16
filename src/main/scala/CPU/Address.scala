package com.udsl.processor6502.CPU

import com.udsl.processor6502.UI.NumericFormatSelector.{numToString, numericFormatProperty}
import com.udsl.processor6502.UI.NumericFormatType
import scalafx.beans.property.{IntegerProperty, ObjectProperty}
import scalafx.event.subscriptions.Subscription

class Address extends ObjectProperty[NumericFormatType.Value]{
    val MAX_ADDRESS: Int = 65535

    val _addr: IntegerProperty = IntegerProperty(0)

    def addr_= (a: Int): Unit = {
        if (a < 0 || a > MAX_ADDRESS) throw new Exception("Address out of range.")
        _addr.value = a
    }

    def addr: Int = _addr.value

    override def toString: String = {
        val value = _addr.value
        (numericFormatProperty.value) match {
            case NumericFormatType.HexDecimal => value.toHexString.toUpperCase
            case NumericFormatType.Octal => value.toOctalString
            case NumericFormatType.Binary => value.toBinaryString
            case NumericFormatType.Decimal => value.toString
        }
    }

    /**
     * Get the address as a Little Endian String if hex. otherwise as a number
     * @return the string representation
     */
    def toAddressString: String = {
        (numericFormatProperty.value) match {
            case NumericFormatType.HexDecimal => numToString(getLo) + numToString(getHi)
            case _ => toString
        }
    }


    /**
     * Value abs mod 256 and short so never negative
     * @return positive lower 8 bits a positive value in a short
     */
    def getLo: Short = {
        (_addr.value.abs % 256).toShort
    }

    /**
     * Value abs / 256 gives upper 24 bits, mod 256 disposes of upper 16 bits leaving leaving an 8 bit value
     * effectively masking with 00000000000000001111111100000000 and making it a short
     * @return  higher 8 bits of 16 bit address as a positive value in a short
     */
    def getHi: Short = {
        ((_addr.value.abs / 256) % 256).toShort
    }

    def addOnchange( callback: () => Unit ): Unit ={
        val subscription: Subscription =_addr.onChange {

        }
    }
}

object Address {
    def apply(a: Int): Address ={
        val a_ = new Address
        a_.addr = a
        a_
    }
}
