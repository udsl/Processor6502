package com.udsl.processor6502.cpu

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.FileIOUtilities.{selectMemoryImageFileToLoad, selectMemoryImageFileToSave}
import com.udsl.processor6502.Utilities.isNumeric
import com.udsl.processor6502.{NumericFormatType, Utilities}
import com.udsl.processor6502.assembler.AssembleLocation.currentLocation
import scalafx.collections.ObservableBuffer

import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter}
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

  // Memory words are in hi byte low byte order (reverse of addresses)
  def getMemoryWrd(location: Int): Int = {
    Memory.getMemoryWord(location)
  }

  def setMemoryWrd(location: Int, value: Int): Unit = {
    Memory.setMemoryWord(location, value)
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

  /**
   * Address is low byte hi byte order (little endian)
   */
  private def setMemoryToAddress(location: Int, address: Int): Unit =
    logger.info(s"Updating contents of $location to address value $address")
    setMemoryByte(location, address & 0xFF, s"ADDR $address" )
    setMemoryByte(location + 1, (address >> 8) & 0xFF)
    location match
      case NMI_VECTOR => notifyVectorChangeListeners("NMI", address)
      case INTERRUPT_VECTOR => notifyVectorChangeListeners("IRQ", address)
      case RESET_VECTOR => notifyVectorChangeListeners("RST", address)
      case _ =>

  /**
   * word is hi byte low byte order (reverse of address)
  */
  private def setMemoryWord(location: Int, word: Int): Unit =
    logger.info(s"Updating contents of $location to word value $word")
    setMemoryByte(location, (word  >> 8) & 0xFF)
    setMemoryByte(location + 1, word & 0xFF )

  private def getMemoryWord(location: Int): Int =
    logger.info(s"Retrieving contents of $location")
    (getMemoryByte(location) * 256) + getMemoryByte(location + 1)


  def getMemory: ObservableBuffer[MemoryCell] =
    memory

  def loadMemoryImage(): Unit =
    logger.info("Loading memory image!")

    val file = selectMemoryImageFileToLoad
    val br = new BufferedReader(new FileReader(file))

    for l <- Iterator.continually(br.readLine()).takeWhile(_ != null) do
      val m = MemoryCell(MemoryCellRecord(l) )
      memory.update(m.getLocation, m)
    br.close()

  private def doSaveImage( file: File, start: Int = 0, end: Int = 65535): Unit =
    val bw = new BufferedWriter(new FileWriter(file))

    for cell <- memory do
      bw.write(s"${cell.asSerialisedString()}\n")
    bw.close()
    logger.info(s"Memory image size written: ${end - start}, from: $start to $end")

  def saveMemoryImage(): Unit =
    logger.info("Saving memory image!")
    selectMemoryImageFileToSave match
      case Some(file) => doSaveImage( file )
      case _ =>

class MemoryCell(private val location: Address, private var value: ByteValue = ByteValue.apply):

  override def toString: String =
    val disasemblyDisplay = if value.getDisassembly.nonEmpty then s" - ${value.getDisassembly}" else ""
    s"[${location.toAddressString(MemoryCell.currentMemoryFormat)}] ${value.toDisplayString(MemoryCell.currentMemoryFormat)}$disasemblyDisplay"

  def asSerialisedString(): String =
    s"$location:${value.asSerilisedString}"

  def asString: String =
    s"$location:$value"

  def getValue: Int =
    value._byte.value

  def getLocation: Int =
    location.addr

  def getByte: ByteValue = value



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

  def apply(mcr: MemoryCellRecord): MemoryCell =
    apply(mcr.index, mcr.byt, mcr.dis)

  def changeDisplayMode( displayMode: NumericFormatType): Unit =
    currentMemoryFormat = displayMode

case class MemoryCellRecord(index:Int, byt:Int, dis:String)

object MemoryCellRecord:

  def validate(str:String): (Int, Int, String) =
    val v = str.split(":")
    if v.length > 3 then throw new Exception(s"Invalid MemoryCellRecord: required 2 Int plus optional string. got - $str")
    if v.length >= 2 then
      if !isNumeric(v(0)) then throw new Exception(s"Invalid MemoryCellRecord: field 1 is not numeric - ${v(0)}")
      if !isNumeric(v(1)) then throw new Exception(s"Invalid MemoryCellRecord: field 2 is not numeric - ${v(1)}")
    val v2 = if v.length == 2 then "" else v(2)
    (v(0).toInt, v(1).toInt, v2)
  def apply(str:String): MemoryCellRecord =
    val v = validate(str)
    MemoryCellRecord(v._1, v._2, v._3)



