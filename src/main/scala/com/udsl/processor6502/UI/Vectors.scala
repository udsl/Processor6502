package com.udsl.processor6502.UI

import com.udsl.processor6502.CPU.Processor
import com.udsl.processor6502.CPU.Processor.{IRQ_VECTOR_HI_ADDRESS_BYTE, IRQ_VECTOR_LO_ADDRESS_BYTE, NMI_VECTOR_HI_ADDRESS_BYTE, NMI_VECTOR_LO_ADDRESS_BYTE, RESET_VECTOR_HI_ADDRESS_BYTE, RESET_VECTOR_LO_ADDRESS_BYTE}
import com.udsl.processor6502.Utilities.{getAddressSettingDialogue, stringToNum}
import com.udsl.processor6502.config.{DataCollector, DataSource}
import scalafx.application.Platform
import scalafx.event.subscriptions.Subscription
import scalafx.geometry.Insets
import scalafx.scene.control._
import scalafx.scene.layout.{HBox, StackPane, VBox}

import scala.collection.mutable.ArrayBuffer

class Vectors extends VBox {
    def nmiChanges( newVectorDest: Int): Unit ={
        val lo: Int = newVectorDest % 256
        val hi: Int = (newVectorDest / 256) % 256
        println(s"NMI updated to $lo, $hi")
        Processor.nmiVector.addr = newVectorDest
        Processor.setMemoryByte(NMI_VECTOR_LO_ADDRESS_BYTE, lo)
        Processor.setMemoryByte(NMI_VECTOR_HI_ADDRESS_BYTE, hi)
    }

    def irqChanges( newVectorDest: Int): Unit ={
        val lo: Int = newVectorDest % 256
        val hi: Int = (newVectorDest / 256) % 256
        println(s"IRQ updated to $lo, $hi")
        Processor.irqVector.addr = newVectorDest
        Processor.setMemoryByte(IRQ_VECTOR_LO_ADDRESS_BYTE, lo)
        Processor.setMemoryByte(IRQ_VECTOR_HI_ADDRESS_BYTE, hi)
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

        val irqSubscription: Subscription = Processor.irqVector._addr.onChange {
            (_, oldValue, newValue) => {
                println(s"rstSubscription fired: ${Processor.irqVector.toString} ${oldValue} ${newValue}")
                Platform.runLater(() -> {
                    v.irq.value.setText(Processor.irqVector.toString)
                })
            }
        }

        val nmiSubscription: Subscription = Processor.nmiVector._addr.onChange {
            (_, oldValue, newValue) => {
                println(s"rstSubscription fired: ${Processor.nmiVector.toString} ${oldValue} ${newValue}")
                Platform.runLater(() -> {
                    v.nmi.value.setText(Processor.nmiVector.toString)
                })
            }
        }
    }

    padding = Insets(18, 18, 18, 0)
    children = List(display)
}

class Vector( vectorName: String, vectorAddress: Int, onChange: (Int) => Unit, initalValue: Int = 0) extends HBox with DataSource{

    DataCollector.registerDataSource(this)

    println(s"Creating vector '$vectorName' location '$vectorAddress")
    val changeHandler: (Int) => Unit = onChange

    var currentValue: Int = initalValue
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



    def updated( str: String): Unit ={
        currentValue = stringToNum(str)
        changeHandler(currentValue)
    }

    val setButton: Button = new Button {
        text = "set"
        onAction = _ => {
            println("Setting PC!")
            val dialog: TextInputDialog = getAddressSettingDialogue(s"New ${vectorName} Vector", currentValue)

            val result = dialog.showAndWait()
            result match {
                case Some(value) => updated(value)
                case None       => println("Dialog was canceled.")
            }
        }
    }

    setButton.setTooltip(tooltip)

    children = List(label, value, setButton)

    override def getData(collector: ArrayBuffer[String]): Unit = {
        println(s"Collecting for vector $vectorName $currentValue")
        collector += s"$vectorName: $currentValue"
    }
}