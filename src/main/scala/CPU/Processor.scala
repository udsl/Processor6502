package com.udsl.processor6502
package CPU

import UI.{ProcAddress, ProcByte}

import scalafx.collections.ObservableBuffer


object Processor {
    private val memory: IndexedSeq[MemoryCell] = IndexedSeq.fill[MemoryCell](65536)(new MemoryCell(getAndIncIndex))

    private var indexer: Int = 0

    var pc: Address = Address(0)

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
}

class MemoryCell(index: Int) {
    var value: ProcByte = ProcByte( 0 )
    val location: ProcAddress = ProcAddress(index)

    override def toString: String = {
        s"[${ location.asAddressString}] ${value.asNumString}"
    }
}

