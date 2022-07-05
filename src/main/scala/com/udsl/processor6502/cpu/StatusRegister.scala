package com.udsl.processor6502.cpu
  

class StatusRegister(name: String) extends EightBitRegister(name: String):

  def value: Int = _ebr.value

  def value_= (value: Int): Unit = {
    ebr = value
  }


  def testFlag( flag: StatusFlag) : Boolean =
    (ebr & flag.mask) > 0

  def setFlag( flag: StatusFlag): Unit =
    ebr = ebr | flag.mask

  /**
   * If toSet the flag is reset (cleared), if false then the flag is set.
   *
   * @param flag the flag to update
   * @param toSet should it be set or reset
   */
  def updateFlag(flag: StatusFlag, toSet: Boolean): Unit =
    if toSet then
      setFlag(flag)
    else
      clearFlag(flag)

  /**
   * To clear the bit and with the negative mask which we can mahe by xor the mask with 255
   *
   * @param flag  the flag to clear
   */
  def clearFlag( flag: StatusFlag): Unit =
    ebr = ebr & (255 ^ flag.mask)

  def reset(): Unit =
    ebr = 32


object StatusRegister {
//  val NEGATIVE_FLAG_MASK = 128;
//  val OVERFLOW_FLAG_MASK = 64;
//  val UNUSED_FLAG_MASK = 32;
//  val BREAK_FLAG_MASK = 16;
//  val DECIMAL_FLAG_MASK = 8;
//  val INTERRUPT_FLAG_MASK = 4;
//  val ZERO_FLAG_MASK = 2;
//  val CARRY_FLAG_MASK = 1;

  def apply() : StatusRegister = {
    val sr_ = new StatusRegister("Status Register")
    sr_.reset()
    sr_
  }
}

enum StatusFlag(val mask: Int, val str: String):
  case Negative extends StatusFlag(0x80, "Negative")
  case Overflow extends StatusFlag(0x40, "Overflow")
  case Unused extends StatusFlag(0x20, "Unused")
  case Break extends StatusFlag(0x10, "Break")
  case Decimal extends StatusFlag(0x8, "Decimal")
  case Interrupt extends StatusFlag(0x4, "Interrupt")
  case Zero extends StatusFlag(0x2, "Zero")
  case Carry extends StatusFlag(0x1, "Carry")

