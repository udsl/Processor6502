package com.udsl.processor6502

class ProcByte {
    var byte : Byte = 0

    def asNumString : String = {
        NumericFormatSelector.numToString( byte )
    }
}

object ProcByte {

    def apply( byteIn: Byte ) : ProcByte = {
        val b = new ProcByte
        b.byte = byteIn
        b
    }
}


