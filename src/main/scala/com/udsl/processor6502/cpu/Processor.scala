package com.udsl.processor6502.cpu

import com.udsl.processor6502.cpu.execution.Instruction
import scalafx.collections.ObservableBuffer


object Processor {
    private val memory = new ObservableBuffer[MemoryCell]()
    memory.addAll( Array.fill[MemoryCell](65536)(MemoryCell(getAndIncIndex)) )

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

    def setPC(address: Int): Unit ={
        pc.addr = address
    }

    def setMemoryByte(address: Int, value: Int): Unit = {
        memory(address) = MemoryCell(address, value)
    }

    def getMemoryByte(address: Int): Int = {
        memory(address).getValue()
    }

    def getNextInstruction(): Instruction = {
        Instruction(memory(pc.addr).getValue())
    }

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

    private def pushByte( byt: Int) = {

    }
}

class MemoryCell( private val location: Address, private var value: ByteValue = ByteValue.apply) {

    override def toString: String = {
        s"[${ location.toString}] ${value}"
    }

    def getValue(): Int ={
        value._byte.value
    }
}

object MemoryCell {
    def apply(index: Int): MemoryCell = {
        Address.validate( index )
        val m = new MemoryCell( Address(index) )
        m
    }

    def apply(index: Int, byt: Int): MemoryCell = {
        Address.validate( index )
        ByteValue.validate(byt)
        val m = new MemoryCell( Address(index), ByteValue(byt) )
        m
    }
}

