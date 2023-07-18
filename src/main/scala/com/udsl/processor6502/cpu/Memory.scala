package com.udsl.processor6502.cpu

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Dialogues.theStage
import com.udsl.processor6502.FileIOUtilities.{selectMemoryImageFileToLoad, selectMemoryImageFileToSave}
import com.udsl.processor6502.Main.stage
import com.udsl.processor6502.Utilities.{currentFormat, isNumeric, stringToNum}
import com.udsl.processor6502.{NumericFormatType, Utilities}
import com.udsl.processor6502.assembler.AssembleLocation.currentLocation
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control.{ButtonType, Dialog, Label, TextField}
import scalafx.scene.layout.GridPane

import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter}
import scala.collection.mutable
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

  def validateAddress(addr: Int): Unit=
    if addr < 0 || addr > 65535 then
      throw new RuntimeException(s"Address $addr out of range 0 - 65535")

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

  def getCells(start: Int, Length: Int) : List[MemoryCell] =
    validateAddress(start)
    validateAddress(start + Length - 1)
    val res: ListBuffer[MemoryCell] = ListBuffer[MemoryCell]()
    val memArray = memory.toArray
    for (i <- start to  start+Length)
      res += memArray(i)
    res.toList


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
    logger.info(s"Saving Memory image from: $start to $end")
    val bw = new BufferedWriter(new FileWriter(file))

    for (i <- start to end) do
      val cell = memory(i)
      bw.write(s"${cell.asSerialisedString()}\n")
    bw.close()
    logger.info(s"Memory image size written: ${end - start}, from: $start to $end")

  case class SaveResult(start: Int, end: Int)

  def saveMemoryImage(): Unit =
    logger.info("Saving memory image!")

    val dialog = new Dialog[SaveResult]() {
      initOwner(theStage)
      title = "Set Memory Save Range"
      headerText = s"Set the start and end address for the save.\nCurrent active number format ${currentFormat.toString}"
    }

    val setRangeButtonType = new ButtonType("Set Range", ButtonData.OKDone)
    dialog.dialogPane().getButtonTypes.setAll(setRangeButtonType, ButtonType.Cancel)

    val start = new TextField() {
      promptText = "start location"
    }
    val end = new TextField() {
      promptText = "end location"
    }

    val grid = new GridPane() {
      hgap = 10
      vgap = 10
      padding = Insets(20, 100, 10, 10)

      add(new Label("Start:"), 0, 0)
      add(start, 1, 0)
      add(new Label("End:"), 0, 1)
      add(end, 1, 1)
    }

    dialog.dialogPane().setContent(grid)

    dialog.resultConverter = dialogButton =>
      if (dialogButton == setRangeButtonType)
        SaveResult(stringToNum(start.text()), stringToNum(end.text()))
      else
        null

    dialog.showAndWait() match {
      case Some(SaveResult(s, e)) =>
        selectMemoryImageFileToSave match
          case Some(file) => doSaveImage(file, s, e)
          case _ =>
      case _ => logger.info("memory dialogue cancelled")
    }

class MemoryCell(private val location: Address, private var value: ByteValue = ByteValue.apply):

  override def toString: String =
    val disasemblyDisplay = if value.getDisassembly.nonEmpty then s" - ${value.getDisassembly}" else ""
    s"[${location.toAddressString(MemoryCell.currentMemoryFormat)}] ${value.toDisplayString(MemoryCell.currentMemoryFormat)}$disasemblyDisplay"

  def asSerialisedString(): String =
    s"${location.addr}:${value.asSerilisedString}"

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



