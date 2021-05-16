package com.udsl.processor6502
package CPU

import scalafx.collections.ObservableBuffer


object Processor {
    private val memory: IndexedSeq[MemoryCell] = IndexedSeq.fill[MemoryCell](65536)(new MemoryCell(getAndIncIndex))

    private var indexer: Int = 0

    val pc: Address = Address(0)

    var resetVector: Address = Address(0)
    var nmiVector: Address = Address(0)
    var irqVector: Address = Address(0)


    def getAndIncIndex: Int = {
        val res = indexer
        indexer += 1
        res
    }

    def getMemory: ObservableBuffer[MemoryCell] = {
        ObservableBuffer(memory)
    }

    def reset = {
        pc.addr = resetVector.addr
    }

    def setPC(address: Int): Unit ={
        pc.addr = address
    }

}

class MemoryCell(index: Int) {
    var value: ByteValue = ByteValue.apply
    val location: Address = Address(index)

    override def toString: String = {
        s"[${ location.toString}] ${value}"
    }
}

