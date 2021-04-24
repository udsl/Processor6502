package com.udsl.processor6502

import scalafx.geometry.Insets
import scalafx.scene.control.Label
import scalafx.scene.layout.{HBox, StackPane, VBox}

class StatusRegister extends VBox {
    val display = new StackPane {
        val titleBox = new HBox {
            val title: Label = new Label {
                text = "  Status Flags  "
                style = "-fx-content-display: top; -fx-background-color: white; -fx-translate-y: -12; -fx-translate-x: 8;"
            }
            prefWidth = title.prefWidth.value
            children = List(title)
        }

        val statusBits = new VBox {
            val bits = new VBox {
                val neg = new StatusFlag("Negative")
                val ovr = new StatusFlag("Overflow")
                val u = new StatusFlag("Unused", 1, true)
                val brk = new StatusFlag("Break")
                val dec = new StatusFlag("Decimal")
                val inter = new StatusFlag("Interrupt")
                val zero = new StatusFlag("Zero")
                val carry = new StatusFlag("Carry")
                padding = Insets(4, 4, 4, 4)
                children = List(neg, ovr, u, brk, dec, inter, zero, carry)
            }
            style = "-fx-content-display: top; -fx-border-insets: -2 -2 -2 -2; -fx-background-color: white; -fx-border-color: grey; -fx-border-width: 2;"
            children = List(bits)
        }

        children = List(statusBits, titleBox)
    }

    padding = Insets(18, 18, 18, 0)
    children = List(display)
}

class StatusFlag( statusName: String, initalValue: Int = 0, readOnly: Boolean = false) extends HBox {

    println(s"Creating StatusFlag '$statusName'")
    var curentValue: Int = initalValue
    val isReadOnly: Boolean = readOnly
    val label: Label = new Label(statusName){
        prefWidth = 70
    }
    val value = new Label(initalValue.toString)

    children = List(label, value)

    def update( newValue: Int): Unit ={
        if (!isReadOnly) {
            val v = if (newValue >= 1) 1 else 0
            if (curentValue != v) {
                curentValue = v
                value.text = v.toString
            }
        }
    }
}