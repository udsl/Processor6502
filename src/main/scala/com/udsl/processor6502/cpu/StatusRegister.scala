package com.udsl.processor6502.cpu

import scala.language.postfixOps


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
   * To clear the bit AND with 0xFF mask xor with the bit mask
   *
   * @param flag  the flag to clear
   */
  def clearFlag( flag: StatusFlag): Unit =
    ebr = ebr & (0xFF ^ flag.mask)

  def reset(): Unit =
    ebr = 32




object StatusRegister:

  def apply() : StatusRegister =
    val sr_ = new StatusRegister("Status Register")
    sr_.reset()
    sr_

  def testValueForFlag( value: Int, flag: StatusFlag) : Boolean =
    (value & flag.mask) > 0

  def asFlagsString( value: Int) : String =
    var res: String = ""
    for (flag: StatusFlag <- StatusFlag.values) {
      flag match
        case StatusFlag.Unused => ()
        case _ =>
          if testValueForFlag( value, flag) then
            if res.nonEmpty then
              res += "|"
            res += flag.str
    }
    res


enum StatusFlag(val mask: Int, val str: String):
  case Negative extends StatusFlag(0x80, "Negative")
  case Overflow extends StatusFlag(0x40, "Overflow")
  case Unused extends StatusFlag(0x20, "Unused")
  case Break extends StatusFlag(0x10, "Break")
  case Decimal extends StatusFlag(0x8, "Decimal")
  case Interrupt extends StatusFlag(0x4, "Interrupt")
  case Zero extends StatusFlag(0x2, "Zero")
  case Carry extends StatusFlag(0x1, "Carry")

