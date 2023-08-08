package com.udsl.processor6502.cpu.execution

import com.udsl.processor6502.NumericFormatType
import com.udsl.processor6502.Utilities.numToByteString

/**
 * Parameter order lowByrte then hiByte because 6502 is little endian
 *
 * @param _lowByte the low byte vale
 * @param _hiByte the high byte vale
 */
class Operand( val _lowByte: Option[Int], val _hiByte: Option[Int]):
  def hasValue: Boolean = _lowByte.isDefined // if no lowByte cant have a hiByte

  def hasLowByte: Boolean = _lowByte.isDefined // if no lowByte cant have a hiByte
  def hasHiByte: Boolean = _hiByte.isDefined // if no lowByte cant have a hiByte

  def has16BitValue: Boolean = hasLowByte && hasHiByte

  def lowByte: Int =
    if hasValue then
      _lowByte.get
    else
      throw new RuntimeException("Bad operand value")
  
  def lowByteAsHexStr: String =
    if hasLowByte then
      numToByteString(_lowByte.get, NumericFormatType.HEX)
    else
      "None"
      
  def hiByteAsHexStr: String =
    if hasHiByte then
      numToByteString(_hiByte.get, NumericFormatType.HEX)
    else
      "None"
    
  def hiByte: Int =
    if hasHiByte then
      _hiByte.get
    else
      throw new RuntimeException("Bad operand value")
  
  def asByte: Int =
      if hasValue then
        _lowByte.get
      else
        throw new RuntimeException("Bad operand value")
  
  def asSignedByte: Int =
    val value = asByte
    if value > 128 then
      -(256 - value)
    else
      value

  def asWord: (Int, Int) =
    if has16BitValue then
      (_hiByte.get, _lowByte.get) // High byte first
    else
      throw new RuntimeException("Bad operand value")

  /**
   * For an address the first byte in memory is the low byte but for a word its a 16 bit number not an address value
   * so the first byte is the most significant byte.
   *
   * @return the word value
   */
  def asWordValue: Int =
    if has16BitValue then
      (_lowByte.get * 256) + _hiByte.get
    else
      throw new RuntimeException("Bad operand value")

  def asAddress: (Int, Int) =
    if has16BitValue then
      (_lowByte.get, _hiByte.get) // Low byte first
    else
      throw new RuntimeException("Bad operand value")

  def asAddressValue: Int =
    as16BitValue

  def as16BitValue: Int =
    if has16BitValue then
      (_hiByte.get * 256) + _lowByte.get
    else
      throw new RuntimeException("Bad operand value")


object Operand:
  val MAX_BYTE_VALUE: Int = 255
  val MIN_BYTE_VALUE: Int = -127

  // A byte can be negative
  def validate(b: Int): Unit =
    if (b < MIN_BYTE_VALUE || b > MAX_BYTE_VALUE) throw new Exception(s"Value out of range for a BYTE: $b.")

  // Components of a 16bit value can not be negative
  def validate(lowByte: Int, hiByte: Int): Unit =
    if (lowByte < 0 || lowByte > MAX_BYTE_VALUE) throw new Exception(s"Value out of range for a LOW-BYTE: $lowByte.")
    if (hiByte < 0 || hiByte > MAX_BYTE_VALUE) throw new Exception(s"Value out of range for a HIGH-BYTE: $hiByte.")

  def apply() =
    new Operand( None, None)

  def apply(lowByte: Int): Operand =
    validate(lowByte)
    new Operand(if lowByte < 0 then Some(lowByte & 255) else Some(lowByte), None)

  def apply(lowByte: Int, hiByte: Int): Operand =
    validate(lowByte, hiByte)
    new Operand(Some(lowByte), Some(hiByte))
