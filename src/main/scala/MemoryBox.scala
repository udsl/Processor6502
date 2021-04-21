package com.udsl.processor6502

import scalafx.collections.ObservableBuffer
import scalafx.event.subscriptions.Subscription
import scalafx.geometry.{Insets, Orientation}
import scalafx.scene.control.{Label, ListView}
import scalafx.scene.layout.{StackPane, VBox}

import scala.annotation.switch

class MemoryBox(val numericFormat: NumericFormatSelector) extends VBox {
    private var currentMemoryFormat = NumericFormatType.Decimal

    val subscription: Subscription = numericFormat.numericFormatProperty.onChange {
        (_, oldValue, newValue) =>
            currentMemoryFormat = newValue
            memoryView.refresh()
    }

    val memoryBoxCaption: Label = new Label {
        text = "Memory View"
        style = "-fx-font: 12 arial;-fx-font-weight:bold"
        padding = Insets(0, 0, 0, 80)
    }

    var indexer: Int = -1

    def getIndex: Int = {
        indexer += 1
        indexer
    }

    val memory: IndexedSeq[MemoryCell] = IndexedSeq.fill[MemoryCell](65536)(new MemoryCell(getIndex))

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
        var value: Byte = _
        val location: Int = index

        def numToString(address: Int): String = {
            (currentMemoryFormat: @switch) match {
                case NumericFormatType.HexDecimal => address.toHexString.toUpperCase
                case NumericFormatType.Octal => address.toOctalString
                case NumericFormatType.Binary => address.toBinaryString
                case NumericFormatType.Decimal => address.toString
            }
        }

        override def toString: String = {
            s"[${numToString(location)}] $value"
        }
    }

}

