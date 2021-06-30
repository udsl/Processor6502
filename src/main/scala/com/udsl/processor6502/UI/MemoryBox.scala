package com.udsl.processor6502.UI

import com.udsl.processor6502.cpu.{MemoryCell, Processor}
import com.udsl.processor6502.NumericFormatType
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

    val memoryView: ListView[MemoryCell] = new ListView[MemoryCell] {
        items = Processor.getMemory
        orientation = Orientation.Vertical
    }

    val st = new StackPane {
        padding = Insets(10)
        children = memoryView
    }

    padding = Insets(20)
    spacing = 8
    children = List(memoryBoxCaption, st)
}

