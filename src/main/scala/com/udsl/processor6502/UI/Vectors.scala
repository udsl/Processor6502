package com.udsl.processor6502.UI

import com.udsl.processor6502.CPU.Processor
import com.udsl.processor6502.CPU.Processor.{RESET_VECTOR_HI_ADDRESS_BYTE, RESET_VECTOR_LO_ADDRESS_BYTE}
import com.udsl.processor6502.Utilities.{getAddressSettingDialogue, stringToNum}
import scalafx.application.Platform
import scalafx.event.subscriptions.Subscription
import scalafx.geometry.Insets
import scalafx.scene.control._
import scalafx.scene.layout.{HBox, StackPane, VBox}

class Vectors extends VBox {
    def nmiChanges( newVectorDest: Int): Unit ={
        val lo: Int = newVectorDest % 256
        val hi: Int = (newVectorDest / 256) % 256
        println(s"NMI updated to $lo, $hi")
    }

    def irqChanges( newVectorDest: Int): Unit ={
        val lo: Int = newVectorDest % 256
        val hi: Int = (newVectorDest / 256) % 256
        println(s"IRQ updated to $lo, $hi")
    }

    def resetChanges( newVectorDest: Int): Unit ={
        val lo: Int = newVectorDest % 256
        val hi: Int = (newVectorDest / 256) % 256
        println(s"RESET updated to $lo, $hi")
        Processor.resetVector.addr = newVectorDest
        Processor.setMemoryByte(RESET_VECTOR_LO_ADDRESS_BYTE, lo)
        Processor.setMemoryByte(RESET_VECTOR_HI_ADDRESS_BYTE, hi)
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

        val rstSubscription: Subscription = Processor.resetVector._addr.onChange {
            (_, oldValue, newValue) => {
                println(s"rstSubscription fired: ${Processor.resetVector.toString} ${oldValue} ${newValue}")
                Platform.runLater(() -> {
                    v.reset.value.setText(Processor.resetVector.toString)
                })
            }
        }

    }

    padding = Insets(18, 18, 18, 0)
    children = List(display)
}

class Vector( vectorName: String, vectorAddress: Int, onChange: (Int) => Unit, initalValue: Int = 0) extends HBox {

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
        disable = true
        prefWidth = 120
    }

    val setButton: Button = new Button {
        text = "set"
        onAction = _ => {
            println("Setting PC!")
            val dialog: TextInputDialog = getAddressSettingDialogue(s"New ${vectorName} Vector")

            val result = dialog.showAndWait()
            result match {
                case Some(value) => changeHandler(stringToNum(value))
                case None       => println("Dialog was canceled.")
            }
        }
    }


    value.setTooltip(tooltip)

    children = List(label, value, setButton)
}