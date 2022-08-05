package com.udsl.processor6502.cpu:

  import com.typesafe.scalalogging.StrictLogging
  import com.udsl.processor6502.{NumericFormatType, Utilities}
  import com.udsl.processor6502.Utilities.{numToString, writeStringToFile, writeToFile}
  import com.udsl.processor6502.cpu.Memory.{INTERRUPT_VECTOR, NMI_VECTOR, RESET_VECTOR}
  import com.udsl.processor6502.cpu.execution.Opcode
  import scalafx.collections.ObservableBuffer

  import java.io.{BufferedWriter, FileWriter}
  import java.util
  import scala.collection.mutable.ListBuffer


  object Processor extends StrictLogging{

    val memoryAccess: Memory = Memory.apply

    val pc: Address = Address(0)
    val sp: StackPointer = StackPointer.apply()
    val ix: IndexX = IndexX.apply()
    val iy: IndexY = IndexY.apply()
    val ac: Accumulator = Accumulator.apply()
    val sr: StatusRegister = StatusRegister.apply()

    def resetVector: Int = memoryAccess.getMemoryAsAddress(RESET_VECTOR)
    def nmiVector: Int = memoryAccess.getMemoryAsAddress(NMI_VECTOR)
    def irqVector: Int = memoryAccess.getMemoryAsAddress(INTERRUPT_VECTOR)

    def nmi(): Unit = {
      pushPc
      //        Push SR.
      //        Set IRQ disable in status.
      //        PC is loaded from NMI vector
      pc.addr = nmiVector
    }

    def irq(): Unit = {
      pushPc
      //        Push SR.
      //        Push PC.
      //        PC is loaded from IRQ vector
      pc.addr = irqVector
    }

    def reset(): Unit = {
      // Set status.
      sr.reset()
      // load pc from vector
      pc.addr = resetVector
    }

    def setPC(address: Int): Unit = {
      pc.addr = address
    }

    def getNextInstruction: Opcode =
      Opcode(memoryAccess.getMemoryByte(pc.addr))

    def getNextInstructionOperand: (Int, Int) =
      (memoryAccess.getMemoryByte(pc.addr + 1), memoryAccess.getMemoryByte(pc.addr + 2))

    /**
     * Used to update executor on PC change
     * @param address the location in memory of the instruction to retrieve
     * @return the retrieved instruction
     */
    def getInstruction(address: Int): Opcode =
      Opcode(memoryAccess.getMemoryByte(address))

    def getInstructionOperand(address: Int): (Int, Int) =
      (memoryAccess.getMemoryByte(address + 1), memoryAccess.getMemoryByte(address + 2))

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
      val popped = (msb * 256) + lsb
      pc.addr = popped
    }

    private def pushSr = {
      sp.pushByte(sr.ebr)
    }
  }