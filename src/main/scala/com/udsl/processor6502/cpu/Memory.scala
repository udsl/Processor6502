package com.udsl.processor6502.cpu

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.NumericFormatType
import com.udsl.processor6502.assembler.AssembleLocation.currentLocation
import scalafx.collections.ObservableBuffer

import java.io.{BufferedWriter, FileWriter}
import scala.collection.mutable.ListBuffer

class Memory extends StrictLogging:
  logger.info("Memory access class created")

  def setMemoryByte(location: Int, value: Int): Unit = {
    Memory.setMemoryByte(location, value)
  }

  def setMemoryByte(location: Int, value: Int, disassembly: String): Unit = {
    Memory.setMemoryByte(location, value, disassembly)
  }

  def getMemoryByte(location: Int): Int = {
    Memory.getMemoryByte(location)
  }

  def setMemoryToAddress(location: Int, address: Int): Unit =
    Memory.setMemoryToAddress(location, address)

  def getMemoryAsAddress(location: Int): Int = {
    Memory.getMemoryAsAddress(location)
  }

  def getMemoryWrd(location: Int): Int = {
    Memory.getMemoryAsAddress(location)
  }


trait VectorChangeListener:
  def vectorChange( changeList: (String, Int)): Unit

object Memory extends StrictLogging:
  val NMI_VECTOR = 0xFFFA
  val RESET_VECTOR = 0xFFFC
  val INTERRUPT_VECTOR = 0xFFFE

  private val memory = new ObservableBuffer[MemoryCell]()
  memory.addAll((0 to 65535).toList.map(m => MemoryCell(m)))

  private var vectorChangeListeners = List[VectorChangeListener]()

  def addVectorChangeListener(changeListener: VectorChangeListener): Unit =
    vectorChangeListeners = vectorChangeListeners :+ changeListener

  def apply: Memory =
    new Memory

  private def setMemoryByte(address: Int, value: Int): Unit = {
    if value > 255 || value < -127 then
      throw new Exception(s"Not a valid byte value (-127 to +255): $value")
    memory(address) = MemoryCell(address,
      if value < 0 then
        value & 255
      else value)
  }

  private def setMemoryByte(location: Int, value: Int, disassembly: String): Unit = {
    logger.info(s"Updating $location to $value with disassembly '$disassembly'")
    memory(location) = MemoryCell(location,
      if value < 0 then
        value & 255
      else value,
      disassembly)
  }

  private def getMemoryByte(location: Int): Int = {
    memory(location).getValue
  }

  private def notifyVectorChangeListeners(vectorName: String, newValue: Int): Unit ={
    for listener: VectorChangeListener <- vectorChangeListeners do
      listener.vectorChange( (vectorName, newValue) )
  }

  private def getMemoryAsAddress(location: Int): Int =
    val loByte = getMemoryByte(location)
    val hiByte = getMemoryByte(location + 1)
    loByte + (hiByte * 256)

  private def setMemoryToAddress(location: Int, address: Int): Unit =
    logger.info(s"Updating contents of $location to address value $address")
    setMemoryByte(location, address % 256 )
    setMemoryByte(location + 1, (address / 256) % 256 )
    location match
      case NMI_VECTOR => notifyVectorChangeListeners("NMI", address)
      case INTERRUPT_VECTOR => notifyVectorChangeListeners("IRQ", address)
      case RESET_VECTOR => notifyVectorChangeListeners("RST", address)
      case _ =>

  def getMemory: ObservableBuffer[MemoryCell] =
    memory

  def saveMemoryImage(): Unit =
      logger.info("Saving memory image!")
      val file = java.io.File("mem.dmp")
      val bw = new BufferedWriter(new FileWriter(file))

      val cells = memory.toList.map[Int](f => f.getValue)
      val memoryImage = new ListBuffer[String]()
      val image = new StringBuilder()
      var count = 0
      var lines = 0
      for( x <- cells)
        image.append(x)
        if count == 9 then
          count = 0
          image.append("\n")
          bw.write(image.toString())
          image.clear()
          lines += 1
        else
          image.append(", ")
          count += 1
      bw.write(image.toString().dropRight(2)) // write the last line - the comma and space
      bw.close()
      logger.info(s"Memory size: ${memory.size}, Lines: $lines")


class MemoryCell(private val location: Address, private var value: ByteValue = ByteValue.apply) {

  override def toString: String =
    s"[${location.toAddressString(MemoryCell.currentMemoryFormat)}] ${value.toDisplayString(MemoryCell.currentMemoryFormat)} ${value.getDisassembly}"

  def getValue: Int = {
    value._byte.value
  }

  def getLocation: Int = {
    location.addr
  }
}


object MemoryCell:
  private var currentMemoryFormat = NumericFormatType.DEC

  def apply(index: Int): MemoryCell =
    Address.validate(index)
    val m = new MemoryCell(Address(index))
    m


  def apply(index: Int, byt: Int): MemoryCell =
    Address.validate(index)
    ByteValue.validate(byt)
    val m = new MemoryCell(Address(index), ByteValue(byt))
    m


  def apply(index: Int, byt: Int, disassembly: String): MemoryCell =
    Address.validate(index)
    ByteValue.validate(byt)
    val b = ByteValue(byt, disassembly)
    val m = new MemoryCell(Address(index), b)
    m


  def changeDisplayMode( displayMode: NumericFormatType): Unit =
    currentMemoryFormat = displayMode

