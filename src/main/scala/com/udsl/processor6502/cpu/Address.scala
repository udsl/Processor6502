package com.udsl.processor6502.cpu:

    import com.udsl.processor6502.cpu.Address.validate
    import com.udsl.processor6502.NumericFormatType
    import com.udsl.processor6502.ui.NumericFormatSelector.numericFormatProperty
    import com.udsl.processor6502.Utilities.numToString
    import scalafx.beans.property.IntegerProperty

    class Address {
        val _addr: IntegerProperty = IntegerProperty(0)

        def addr_= (a: Int): Unit = {
            validate( a )
            _addr.value = a
        }

        def addr: Int = _addr.value

        override def toString: String = {
            numToString(_addr.value)
        }

        /**
         * Get the address as a Little Endian String if hex. otherwise as a number
         * @return the string representation
         */
        def toAddressString(format: NumericFormatType): String = {
            format match {
                case NumericFormatType.HEX => addr.toHexString
                case _ => addr.toString
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
    }

    object Address {
        val MAX_ADDRESS: Int = 65535

        def apply(a: Int): Address ={
            val a_ = new Address
            a_.addr = a
            a_
        }

        def validate( a: Int): Unit = {
            if (a < 0 || a > MAX_ADDRESS) throw new Exception("Address out of range.")
        }
    }
