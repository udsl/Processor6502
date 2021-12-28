package com.udsl.processor6502.cpu:

  import com.typesafe.scalalogging.StrictLogging
  import com.udsl.processor6502.{NumericFormatType, Utilities}
  import com.udsl.processor6502.Utilities.{numToString, writeStringToFile, writeToFile}
  import com.udsl.processor6502.cpu.Memory.{INTERRUPT_VECTOR, NMI_VECTOR, RESET_VECTOR}
  import com.udsl.processor6502.cpu.execution.{Opcode, OpcodeValue}
  import scalafx.collections.ObservableBuffer

  import java.io.{BufferedWriter, FileWriter}
  import java.util
  import scala.collection.mutable.ListBuffer


  object Processor extends StrictLogging{

    val memoryAccess = Memory.apply

    val pc: Address = Address(0)
    val sp: StackPointer = StackPointer.apply()
    val ix: IndexX = IndexX.apply()
    val iy: IndexY = IndexY.apply()
    val ac: Accumulator = Accumulator.apply()
    val sr: StatusRegister = StatusRegister.apply()

    def resetVector = memoryAccess.getMemoryAsAddress(RESET_VECTOR)
    def nmiVector = memoryAccess.getMemoryAsAddress(NMI_VECTOR)
    def irqVector = memoryAccess.getMemoryAsAddress(INTERRUPT_VECTOR)

    def nmi = {
      pushPc
      //        Push SR.
      //        Set IRQ disable in status.
      //        PC is loaded
      pc.addr = nmiVector
    }

    def irq = {
      pushPc
      //        Push SR.
      //        PC is loaded
      pc.addr = irqVector
    }

    def reset = {
      //        Set IRQ disable in status.
      sr.reset()
      //        PC is loaded
      pc.addr = resetVector
    }

    def setPC(address: Int): Unit = {
      pc.addr = address
    }

    def getNextInstruction: OpcodeValue =
      Opcode.disassemble(memoryAccess.getMemoryByte(pc.addr)).v

    def getNextInstructionOperand: (Int, Int) =
      (memoryAccess.getMemoryByte(pc.addr + 1), memoryAccess.getMemoryByte(pc.addr + 2))

    /**
     * Used to update executer on PC change
     * @param address
     * @return
     */
    def getInstruction(address: Int): OpcodeValue =
      Opcode.disassemble(memoryAccess.getMemoryByte(address)).v

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
      val poped = (msb * 256) + lsb
      pc.addr = poped
    }

    private def pushSr = {
      sp.pushByte(sr.ebr)
    }

//    private def pushByte(byt: Int) = {
//
//    }
  }