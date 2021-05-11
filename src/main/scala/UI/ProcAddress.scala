package com.udsl.processor6502
package UI


class ProcAddress (private var _address: Int = 0) {
    def address: Int = _address

    def address_= (addr: Int): Unit = {
        println(s"addr = $addr")
        if (addr < 0 || addr > 65535) throw new Exception( s"Invalid address $addr")
        _address = addr
    }

    def getHi : ProcByte = {
        ProcByte( (address / 256).toByte )
    }

    def hasHighByte: Boolean = {
        address > 255
    }

    def getLo: ProcByte = {
        ProcByte( address .toByte )
    }

    def asNumString : String = {
        NumericFormatSelector.addressToString( this )
    }

    def asAddressString : String = {
        NumericFormatSelector.addressToString( this )
    }
}

object ProcAddress {

    def apply( shortIn: Int ) : ProcAddress = {
        val b = new ProcAddress
        b.address = shortIn
        b
    }

}

