package com.udsl.processor6502.CPU

import com.udsl.processor6502.CPU.Address.validate
import com.udsl.processor6502.NumericFormatType
import com.udsl.processor6502.UI.NumericFormatSelector.numericFormatProperty
import com.udsl.processor6502.Utilities.numToString
import scalafx.beans.property.IntegerProperty

class EightBitRegister {
  val _ebr: IntegerProperty = IntegerProperty(0)

  def ebr_= (ebr: Int): Unit = {
    validate( ebr )
    _ebr.value = ebr
  }

  def ebr: Int = _ebr.value

  override def toString: String = {
    numToString(_ebr.value)
  }

  /**
   * Get the value as a String
   * @return the string representation
   */
  def toValueString: String = {
    (numericFormatProperty.value) match {
      case NumericFormatType.HexDecimal => numToString(_ebr.value)
      case _ => toString
    }
  }
}

object EightBitRegister {
  val MAX_VALUE: Int = 255
  val MIN_VALUE: Int = 0

  def apply(init: Int): EightBitRegister ={
    validate(init)
    val ebr_ = new EightBitRegister
    ebr_.ebr = init
    ebr_
  }

  def set( ebr: Int): Unit = {
    validate(ebr)
  }

  def validate( ebr: Int): Unit = {
    if (ebr < MIN_VALUE || ebr > MAX_VALUE) throw new Exception("Register out of range.")
  }
}
