package com.udsl.processor6502.assembler

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.AssembleLocation.logger
import com.udsl.processor6502.cpu.Memory
import com.udsl.processor6502.cpu.execution.InstructionSize

/**
 * This object hold and maintains the current location of the assmbly.
 * As each assembly needs to know this it makes sence for it to be an object.
 * That way any method can have access.
 */
object AssembleLocation extends StrictLogging :
  // The point in memery we are assembling to.
  val defaultLocation = 0x200 // Defaults to immediately after the stack
  var currentLocation: Int = defaultLocation
  val memoryAccess: Memory = Memory.apply

  def setAssembleLoc(l: Int): Unit =
    if l > 65535 || l < 0 then
      throw new Exception(s"Bad assembler Location $l ")
    else
      currentLocation = l
      logger.debug(s" assembler Location = $l ")

  def setAssembleLoc(l: Option[Int]): Unit =
    setAssembleLoc(l.getOrElse(throw new Exception(s"setAssembleLoc to option of nothing ")))
    
  def setMemoryWord(v: Int): Unit =
    if v > 65535 || v < 0 then
      val errorMessage = s"Bad word value $v"
      logger.debug(errorMessage)
      throw new Exception(errorMessage)
    setMemoryByte(v / 256, s"WRD $v")
    setMemoryByte(v % 256)


  def setMemoryAddress(adr: Int): Unit =
    if adr > 65535 || adr < 0 then
      val errorMessage = s"Bad address value $adr"
      logger.debug(errorMessage)
      throw new Exception(errorMessage)

    memoryAccess.setMemoryToAddress(currentLocation, adr)
    currentLocation += 2

  def setMemoryByte(v: Int, disassembly: String): Unit =
    memoryAccess.setMemoryByte(currentLocation, v, disassembly)
    currentLocation += 1

  def setMemoryByte(v: Int): Unit =
    memoryAccess.setMemoryByte(currentLocation, v)
    currentLocation += 1

  def getMemoryByte(v: Int): Int =
    memoryAccess.getMemoryByte(v)

  /**
   * Update the current assemble location by the size of the instruction that was assembled.
   *
   * @param insSize the size of the instruction 1, 2 0r 3 bytes only
   */
  def addInstructionSize(insSize: InstructionSize) : Unit =
    currentLocation += insSize.bytes

  def addInstructionSize(bytes: Int): Unit =
    currentLocation += bytes

