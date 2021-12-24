package com.udsl.processor6502.cpu:

  import com.typesafe.scalalogging.StrictLogging
  import com.udsl.processor6502.{NumericFormatType, Utilities}
  import com.udsl.processor6502.Utilities.{numToString, writeStringToFile, writeToFile}
  import com.udsl.processor6502.cpu.execution.{Opcode, OpcodeValue}
  import scalafx.collections.ObservableBuffer

  import java.io.{BufferedWriter, FileWriter}
  import java.util
  import scala.collection.mutable.ListBuffer


  object Processor extends StrictLogging{
    private val memory = new ObservableBuffer[MemoryCell]()
    memory.addAll(Array.fill[MemoryCell](65536)(MemoryCell(getAndIncIndex)))

    private var indexer: Int = 0

    // Note vectors are Little Endian so low byte come first
    val NMI_VECTOR_LO_ADDRESS_BYTE = 0xFFFA
    val NMI_VECTOR_HI_ADDRESS_BYTE = 0xFFFB
    val RESET_VECTOR_LO_ADDRESS_BYTE = 0xFFFC
    val RESET_VECTOR_HI_ADDRESS_BYTE = 0xFFFD
    val IRQ_VECTOR_LO_ADDRESS_BYTE = 0xFFFE
    val IRQ_VECTOR_HI_ADDRESS_BYTE = 0xFFFF

    val pc: Address = Address(0)
    val sp: StackPointer = StackPointer.apply()
    val ix: IndexX = IndexX.apply()
    val iy: IndexY = IndexY.apply()
    val ac: Accumulator = Accumulator.apply()
    val sr: StatusRegister = StatusRegister.apply()

    val resetVector: Address = Address(0)
    val nmiVector: Address = Address(0)
    val irqVector: Address = Address(0)

    def saveMemoryImage: Unit =
      logger.info("Saving memory image!")
      val file = java.io.File("mem.dmp")
      val bw = new BufferedWriter(new FileWriter(file))

      val cells = memory.toList.map[Int](f => f.getValue())
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

    def getAndIncIndex: Int = {
      val res = indexer
      indexer += 1
      res
    }

    def getMemory: ObservableBuffer[MemoryCell] = {
      memory
    }

    def nmi = {
      pushPc
      //        Push SR.
      //        Set IRQ disable in status.
      //        PC is loaded
      pc.addr = nmiVector.addr
    }

    def irq = {
      pushPc
      //        Push SR.
      //        PC is loaded
      pc.addr = irqVector.addr
    }


    def reset = {
      pushPc
      //        Push SR.
      //        Set IRQ disable in status.
      //        PC is loaded
      pc.addr = resetVector.addr
    }

    def setPC(address: Int): Unit = {
      pc.addr = address
    }

    def setMemoryByte(address: Int, value: Int): Unit = {
      memory(address) = MemoryCell(address, value)
    }

    def setMemoryByte(address: Int, value: Int, disassembly: String): Unit = {
      memory(address) = MemoryCell(address, value, disassembly)
    }

    def getMemoryByte(address: Int): Int = {
      memory(address).getValue()
    }

    def getNextInstruction: OpcodeValue =
      Opcode.disassemble(memory(pc.addr).getValue()).v

    def getNextInstructionOperand: (Int, Int) =
      (memory(pc.addr + 1).getValue(), memory(pc.addr + 2).getValue())

    /**
     * Used to update executer on PC change
     * @param address
     * @return
     */
    def getInstruction(address: Int): OpcodeValue =
      Opcode.disassemble(memory(address).getValue()).v

    def getInstructionOperand(address: Int): (Int, Int) =
      (memory(address + 1).getValue(), memory(address + 2).getValue())


    private def pushPc = {
      // Push MSB
      sp.pushByte(pc.getHi)
      // Push LSB
      sp.pushByte(pc.getLo)
    }

    private def popPc = {
      // Pop MSB
      val msb = sp.popByte()
      // Pop LSB
      val lsb = sp.popByte()
      val poped = (msb * 256) + lsb
      pc.addr = poped
    }

    private def pushSr = {
    }

    private def pushByte(byt: Int) = {

    }
  }

  class MemoryCell(private val location: Address, private var value: ByteValue = ByteValue.apply) {

    override def toString: String =
      s"[${location.toAddressString(MemoryCell.currentMemoryFormat)}] ${value.toDisplayString(MemoryCell.currentMemoryFormat)} ${value.getDisassembly}"

    def getValue(): Int = {
      value._byte.value
    }

    def getLocation(): Int = {
      location.addr
    }
  }

  object MemoryCell {
    private var currentMemoryFormat = NumericFormatType.DEC

    def apply(index: Int): MemoryCell = {
      Address.validate(index)
      val m = new MemoryCell(Address(index))
      m
    }

    def apply(index: Int, byt: Int): MemoryCell = {
      Address.validate(index)
      ByteValue.validate(byt)
      val m = new MemoryCell(Address(index), ByteValue(byt))
      m
    }

    def apply(index: Int, byt: Int, disassembly: String): MemoryCell = {
      Address.validate(index)
      ByteValue.validate(byt)
      val b = ByteValue(byt, disassembly)
      val m = new MemoryCell(Address(index), b)
      m
    }

    def changeDisplayMode( displayMode: NumericFormatType): Unit =
      currentMemoryFormat = displayMode
  }
