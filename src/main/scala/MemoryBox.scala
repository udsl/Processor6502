package com.udsl.processor6502

import scalafx.collections.ObservableBuffer
import scalafx.event.subscriptions.Subscription
import scalafx.geometry.{Insets, Orientation}
import scalafx.scene.control.{Label, ListView}
import scalafx.scene.layout.{StackPane, VBox}

class MemoryBox extends VBox {
    private var currentMemoryFormat = NumericFormatType.Decimal

    implicit def toShort(x: Int): Short = x.toShort

    val subscription: Subscription = NumericFormatSelector.numericFormatProperty.onChange {
        (_, oldValue, newValue) =>
            currentMemoryFormat = newValue
            memoryView.refresh()
    }

    val memoryBoxCaption: Label = new Label {
        text = "Memory View"
        style = "-fx-font: 12 arial;-fx-font-weight:bold"
        padding = Insets(0, 0, 0, 80)
    }

    var indexer: Int = 0

    def getAndIncIndex: Int = {
        val res = indexer
        indexer += 1
        res
    }

    val memory: IndexedSeq[MemoryCell] = IndexedSeq.fill[MemoryCell](65536)(new MemoryCell(getAndIncIndex))

    val memoryView: ListView[MemoryCell] = new ListView[MemoryCell] {
        items = ObservableBuffer(memory)
        orientation = Orientation.Vertical
    }

    val st = new StackPane {
        padding = Insets(10)
        children = memoryView
    }

    padding = Insets(20)
    spacing = 8
    children = List(memoryBoxCaption, st)

    class MemoryCell(index: Int) {
        var value: ProcByte = ProcByte( 0 )
        val location: ProcAddress = ProcAddress(index)

        override def toString: String = {
            s"[${ location.asAddressString}] ${value.asNumString}"
        }
    }


}

