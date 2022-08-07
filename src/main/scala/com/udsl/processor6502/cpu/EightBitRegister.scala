package com.udsl.processor6502.cpu:

  import com.udsl.processor6502.NumericFormatType
  import com.udsl.processor6502.ui.NumericFormatSelector.numericFormatProperty
  import com.udsl.processor6502.Utilities.numToString
  import com.udsl.processor6502.cpu.EightBitRegister.validate
  import com.udsl.processor6502.cpu.execution.ExecutionUnit.isTesting
  import com.udsl.processor6502.ui.popups.Executor
  import scalafx.application.Platform
  import scalafx.beans.property.IntegerProperty

  class EightBitRegister(private val name: String) :
    val _ebr: IntegerProperty = IntegerProperty(0)
    var updateValue: Int = 0
    def ebr_= (ebr: Int): Unit = {
      validate( ebr )
      _ebr.value = ebr
    }

    def update(): Unit =
      if isTesting() then
          performUpdate()
      else
        Platform.runLater(() -> {
          performUpdate()
        })

    def performUpdate(): Unit =
      if updateValue != _ebr.value then
        _ebr.value = ebr

    def ebr: Int = _ebr.value

    def asHex: String = s"0x${_ebr.value.toHexString.toUpperCase}"

    def asDebugStr: String = s"${_ebr.value} - 0x${_ebr.value.toHexString.toUpperCase}"

    override def toString: String = {
      numToString(_ebr.value)
    }

    /**
     * Get the value as a String
     * @return the string representation
     */
    def toValueString: String =
      numericFormatProperty.value match {
        case NumericFormatType.DEC => numToString(_ebr.value)
        case _ => toString
      }

  object EightBitRegister :
    val MAX_VALUE: Int = 255
    val MIN_VALUE: Int = 0

    def apply(init: Int, name: String): EightBitRegister =
      validate(init)
      val ebr_ = new EightBitRegister(name)
      ebr_.ebr = init
      ebr_


    def set( ebr: Int): Unit =
      validate(ebr)

    def validate( ebr: Int): Unit =
      if (ebr < MIN_VALUE || ebr > MAX_VALUE) throw new Exception(s"Register value $ebr out of range.")

