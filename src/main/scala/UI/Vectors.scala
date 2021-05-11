package com.udsl.processor6502
package UI

import scalafx.geometry.Insets
import scalafx.scene.control.{Label, TextField, Tooltip}
import scalafx.scene.layout.{HBox, StackPane, VBox}

class Vectors extends VBox {
    def nmiChanges( newVectorDest: Int): Unit ={
        val lo: Byte = newVectorDest.toByte
        val hi: Byte = (newVectorDest / 256).toByte
        println(s"NMI updated to $lo, $hi")
    }

    def irqChanges( newVectorDest: Int): Unit ={
        val lo: Byte = newVectorDest.toByte
        val hi: Byte = (newVectorDest / 256).toByte
        println(s"IRQ updated to $lo, $hi")
    }

    def resetChanges( newVectorDest: Int): Unit ={
        val lo: Byte = newVectorDest.toByte
        val hi: Byte = (newVectorDest / 256).toByte
        println(s"RESET updated to $lo, $hi")
    }

    val display = new StackPane {
        val titleBox = new HBox {
            val title: Label = new Label {
                text = "  Vectors  "
                style = "-fx-content-display: top; -fx-background-color: white; -fx-translate-y: -12; -fx-translate-x: 8;"
            }
            prefWidth = title.prefWidth.value
            children = List(title)
        }

        val theVectors = new VBox {
             style = "-fx-content-display: top; -fx-border-insets: -2 -2 -2 -2; -fx-background-color: white; -fx-border-color: grey; -fx-border-width: 2;"
        }

        val v = new VBox {
            val nmi = new Vector("NMI", 0xFFFA, nmiChanges)
            val reset = new Vector("RESET", 0xFFFC, resetChanges)
            val irq = new Vector("IRQ/BRK", 0xFFFE, irqChanges)
            padding = Insets(4, 4, 4, 4)
            children = List(nmi, reset, irq)
        }

        children = List(theVectors, titleBox, v)
    }

    padding = Insets(18, 18, 18, 0)
    children = List(display)
}

class Vector( vectorName: String, vectorAddress: Int, onChange: (Int) => Unit = null, initalValue: Int = 0) extends HBox {

    println(s"Creating vector '$vectorName' location '$vectorAddress")
    val changeHandler: (Int) => Unit = onChange

    var curentValue: Int = initalValue
    val tooltip = new Tooltip(s"Location $vectorAddress")
    val label: Label = new Label(vectorName){
        prefWidth = 70
    }

    label.setTooltip(tooltip)

    val value: TextField = new TextField {
        text = initalValue.toString

        text.onChange({
            (_, oldValue, newValue) =>
                println(s"Vector $vectorName changed ${oldValue} => ${newValue}")
                if (changeHandler != null){
                    if (newValue != null && !newValue.isBlank) {
                        changeHandler(Integer.parseInt(newValue))
                    }
                }
                else{
                    println("No change handler")
                }
        })

    }


    value.setTooltip(tooltip)

    children = List(label, value)
}