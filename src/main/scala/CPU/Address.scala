package com.udsl.processor6502.CPU

import com.udsl.processor6502.UI.NumericFormatType
import scalafx.beans.property.{IntegerProperty, ObjectProperty}

class Address extends ObjectProperty[NumericFormatType.Value]{
    val MAX_ADDRESS: Int = 65535

    val _addr: IntegerProperty = IntegerProperty(0)

    def addr_= (a: Int): Unit = {
        if (a < 0 || a > MAX_ADDRESS) throw new Exception("Address out of range.")
        _addr.value = a
    }

    def addr: Int = _addr.value
}

object Address {
    def apply(a: Int): Address ={
        val a_ = new Address
        a_.addr = a
        a_
    }
}
