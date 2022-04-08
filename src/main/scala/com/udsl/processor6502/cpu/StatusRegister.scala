package com.udsl.processor6502.cpu
  
import com.udsl.processor6502.cpu.StatusRegister.{BREAK_FLAG_MASK, CARRY_FLAG_MASK, DECIMAL_FLAG_MASK, INTERRUPT_FLAG_MASK, NEGATIVE_FLAG_MASK, OVERFLOW_FLAG_MASK, UNUSED_FLAG_MASK, ZERO_FLAG_MASK}
  
class StatusRegister(name: String) extends EightBitRegister(name: String):

  def value: Int = _ebr.value

  def value_= (value: Int): Unit = {
    ebr = value
  }


  def testFlag( flag: StatusRegisterFlags) : Boolean = {
    flag match {
      case StatusRegisterFlags.Negative => (ebr & NEGATIVE_FLAG_MASK) > 0
      case StatusRegisterFlags.Overflow => (ebr & OVERFLOW_FLAG_MASK) > 0
      case StatusRegisterFlags.Break => (ebr & BREAK_FLAG_MASK) > 0
      case StatusRegisterFlags.Decimal => (ebr & DECIMAL_FLAG_MASK) > 0
      case StatusRegisterFlags.Interrupt => (ebr & INTERRUPT_FLAG_MASK) > 0
      case StatusRegisterFlags.Zero => (ebr & ZERO_FLAG_MASK) > 0
      case StatusRegisterFlags.Carry => (ebr & CARRY_FLAG_MASK) > 0
    }
  }

  def setFlag( flag: StatusRegisterFlags): Unit = {
    flag match {
      case StatusRegisterFlags.Negative => ebr = ebr | NEGATIVE_FLAG_MASK
      case StatusRegisterFlags.Overflow => ebr = ebr | OVERFLOW_FLAG_MASK
      case StatusRegisterFlags.Break => ebr = ebr | BREAK_FLAG_MASK
      case StatusRegisterFlags.Decimal => ebr = ebr | DECIMAL_FLAG_MASK
      case StatusRegisterFlags.Interrupt => ebr = ebr | INTERRUPT_FLAG_MASK
      case StatusRegisterFlags.Zero => ebr = ebr | ZERO_FLAG_MASK
      case StatusRegisterFlags.Carry => ebr = ebr | CARRY_FLAG_MASK
    }
  }

  /**
   * If toSet the flag is reset (cleared), if false then the flag is set.
   *
   * @param flag the flag to update
   * @param toSet should it be set or reset
   */
  def updateFlag( flag: StatusRegisterFlags, toSet: Boolean): Unit = {
    flag match {
      case StatusRegisterFlags.Negative =>
        if toSet then
          ebr = ebr | NEGATIVE_FLAG_MASK
        else
          ebr = ebr  & (255 ^ NEGATIVE_FLAG_MASK)
      case StatusRegisterFlags.Overflow =>
        if toSet then
          ebr = ebr | OVERFLOW_FLAG_MASK
        else
          ebr = ebr  & (255 ^ OVERFLOW_FLAG_MASK)
      case StatusRegisterFlags.Break =>
        if toSet then
          ebr = ebr | BREAK_FLAG_MASK
        else
          ebr = ebr  & (255 ^ BREAK_FLAG_MASK)
      case StatusRegisterFlags.Decimal =>
        if toSet then
          ebr = ebr | DECIMAL_FLAG_MASK
        else
          ebr = ebr  & (255 ^ DECIMAL_FLAG_MASK)
      case StatusRegisterFlags.Interrupt =>
        if toSet then
          ebr = ebr | INTERRUPT_FLAG_MASK
        else
          ebr = ebr  & (255 ^ INTERRUPT_FLAG_MASK)
      case StatusRegisterFlags.Zero =>
        if toSet then
          ebr = ebr | ZERO_FLAG_MASK
        else
          ebr = ebr  & (255 ^ ZERO_FLAG_MASK)
      case StatusRegisterFlags.Carry =>
        if toSet then
          ebr = ebr | CARRY_FLAG_MASK
        else
          ebr = ebr  & (255 ^ CARRY_FLAG_MASK)
    }
  }

  /**
   * To clear the bit and with the negative mask which we can mahe by xor the mask with 255
   *
   * @param flag  the flag to clear
   */
  def clearFlag( flag: StatusRegisterFlags): Unit = {
    flag match {
      case StatusRegisterFlags.Negative => ebr = ebr & (255 ^ NEGATIVE_FLAG_MASK)
      case StatusRegisterFlags.Overflow => ebr = ebr & (255 ^ OVERFLOW_FLAG_MASK)
      case StatusRegisterFlags.Break => ebr = ebr & (255 ^ BREAK_FLAG_MASK)
      case StatusRegisterFlags.Decimal => ebr = ebr & (255 ^ DECIMAL_FLAG_MASK)
      case StatusRegisterFlags.Interrupt => ebr = ebr & (255 ^ INTERRUPT_FLAG_MASK)
      case StatusRegisterFlags.Zero => ebr = ebr & (255 ^ ZERO_FLAG_MASK)
      case StatusRegisterFlags.Carry => ebr = ebr & (255 ^ CARRY_FLAG_MASK)
    }
  }

  def reset(): Unit =
    ebr = 32


object StatusRegister {
  val NEGATIVE_FLAG_MASK = 128;
  val OVERFLOW_FLAG_MASK = 64;
  val UNUSED_FLAG_MASK = 32;
  val BREAK_FLAG_MASK = 16;
  val DECIMAL_FLAG_MASK = 8;
  val INTERRUPT_FLAG_MASK = 4;
  val ZERO_FLAG_MASK = 2;
  val CARRY_FLAG_MASK = 1;

  def apply() : StatusRegister = {
    val sr_ = new StatusRegister("Status Register")
    sr_.reset()
    sr_
  }
}

enum StatusRegisterFlags(val str: String):
  case Negative extends StatusRegisterFlags("Negative")
  case Overflow extends StatusRegisterFlags("Overflow")
  case Break extends StatusRegisterFlags("Break")
  case Decimal extends StatusRegisterFlags("Decimal")
  case Interrupt extends StatusRegisterFlags("Interrupt")
  case Zero extends StatusRegisterFlags("Zero")
  case Carry extends StatusRegisterFlags("Carry")

