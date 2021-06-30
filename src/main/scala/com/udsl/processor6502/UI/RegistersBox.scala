package com.udsl.processor6502.UI

import com.udsl.processor6502.cpu.Processor
import scalafx.application.Platform
import scalafx.event.subscriptions.Subscription
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{Button, Label, TextField, TextInputDialog, Tooltip}
import scalafx.scene.layout.{HBox, VBox}
import com.udsl.processor6502.Utilities.{currentFormat, getAddressSettingDialogue, stringToNum}
import com.udsl.processor6502.config.{DataCollector, DataSource}

import scala.collection.mutable.ArrayBuffer

class RegistersBox extends VBox with DataSource {

    DataCollector.registerDataSource( this)

    def getData(): Unit ={
        println("getData called on RegistersBox")
    }

    val pc: TextField = new TextField {
        text = Processor.pc.toString
        disable = true
    }

    val pcSubscription: Subscription = Processor.pc._addr.onChange {
        (_, oldValue, newValue) => {
            println(s"PC subscription fired - ${oldValue}, ${newValue}")
            updateDisplayedValues
        }
    }

    private def updateDisplayedValues: Unit = {
        Platform.runLater(() -> {
            pc.setText(Processor.pc.toString)
            sp.setText(Processor.sp.toString)
        })
    }

    val subscription: Subscription = NumericFormatSelector.numericFormatProperty.onChange {
        (_, oldValue, newValue) => {
            println("Num format subscription fired")
            currentFormat = newValue
            updateDisplayedValues
        }
    }

    val registersCaption: Label = new Label {
        text = "Processor Registers"
        style = "-fx-font-weight:bold"
        padding = Insets(0, 0, 0, 80)
    }

    val programCounter: HBox = new HBox {
        val label: Label = new Label("PC:") {
            style = "-fx-font-weight:bold"
            alignmentInParent = Pos.BaselineLeft
            padding = Insets(10, 0, 0, 0)
        }
        label.setPrefWidth(90)
        pc.setPrefWidth(120)
        val setButton: Button = new Button {
            text = "set"
            onAction = _ => {
                println("Setting PC!")
                val dialog: TextInputDialog = getAddressSettingDialogue("New Program Counter", Processor.pc.addr)

                val result = dialog.showAndWait()
                result match {
                    case Some(value) => Processor.pc.addr = stringToNum(value)
                    case None       => println("Dialog was canceled.")
                }
            }
        }
        children = List(label, pc, setButton)
    }

    val sp: TextField = new TextField{
        text = Processor.sp.toString
        prefColumnCount = 8
        disable = true
    }

    val stackPointer: HBox = new HBox {
        val label: Label = new Label("Stack:") {
            style = "-fx-font-weight:bold"
            alignmentInParent = Pos.BottomLeft
            padding = Insets(10, 0, 0, 0)
        }
        label.setPrefWidth(90)
        children = List(label, sp)
    }

    val spSubscription: Subscription = Processor.sp._ebr.onChange {
        (_, oldValue, newValue) => {
            println(s"SP subscription fired - ${oldValue}, ${newValue}")
            updateDisplayedValues
        }
    }

    val acc: TextField = new TextField{
        prefColumnCount = 8
        disable = true
    }

    val accumulator: HBox = new HBox {
        val label: Label = new Label("Accumulator:") {
            style = "-fx-font-weight:bold"
            alignmentInParent = Pos.BottomLeft
            padding = Insets(10, 0, 0, 0)
        }
        label.setPrefWidth(90)
        children = List(label, acc)
    }

    val inx: TextField = new TextField{
        prefColumnCount = 8
        disable = true
    }

    val indexX: HBox = new HBox {
        val label: Label = new Label("Index X:") {
            style = "-fx-font-weight:bold"
            alignmentInParent = Pos.BottomLeft
            padding = Insets(10, 0, 0, 0)
        }
        label.setPrefWidth(90)
        children = List(label, inx)
    }

    val iny: TextField = new TextField{
        prefColumnCount = 8
        disable = true
    }

    val indexY: HBox = new HBox {
        val label: Label = new Label("Index Y:") {
            style = "-fx-font-weight:bold"
            alignmentInParent = Pos.BottomLeft
            padding = Insets(10, 0, 0, 0)
        }
        label.setPrefWidth(90)
        children = List(label, iny)
    }

/*
Status Register

Carry Flag
The carry flag is set if the last operation caused an overflow from bit 7 of the result or an underflow from bit 0. This condition is set during arithmetic, comparison and during logical shifts. It can be explicitly set using the 'Set Carry Flag' (SEC) instruction and cleared with 'Clear Carry Flag' (CLC).

Zero Flag
The zero flag is set if the result of the last operation as was zero.

Interrupt Disable
The interrupt disable flag is set if the program has executed a 'Set Interrupt Disable' (SEI) instruction. While this flag is set the processor will not respond to interrupts from devices until it is cleared by a 'Clear Interrupt Disable' (CLI) instruction.

Decimal Mode
While the decimal mode flag is set the processor will obey the rules of Binary Coded Decimal (BCD) arithmetic during addition and subtraction. The flag can be explicitly set using 'Set Decimal Flag' (SED) and cleared with 'Clear Decimal Flag' (CLD).

Break Command
The break command bit is set when a BRK instruction has been executed and an interrupt has been generated to process it.

Overflow Flag
The overflow flag is set during arithmetic operations if the result has yielded an invalid 2's complement result (e.g. adding to positive numbers and ending up with a negative result: 64 + 64 => -128). It is determined by looking at the carry between bits 6 and 7 and between bit 7 and the carry flag.

Negative Flag
The negative flag is set if the result of the last operation had bit 7 set to a one.
*/

    val status = new StatusRegisterView

    val vectors = new Vectors

    val buttonBox: HBox = new HBox {
        spacing = 20
        val resetButton: Button = new Button {
            text = "Reset"
            onAction = _ => {
                println("Resetting!")
                Processor.reset
            }
        }
        resetButton.setTooltip(new Tooltip("Perform the reset operation"))

        val nmiButton: Button = new Button {
            text = "NMI"
            onAction = _ => {
                println("NMI!")
                Processor.nmi
            }
        }
        nmiButton.setTooltip(new Tooltip("Perform the NMI operation"))

        val irqButton: Button = new Button {
            text = "IRQ"
            onAction = _ => {
                println("IRQ!")
                Processor.irq
            }
        }
        irqButton.setTooltip(new Tooltip("Perform the IRQ operation"))

        children = List(resetButton, nmiButton, irqButton)
    }

    padding = Insets(20)
    spacing = 8
    children = List(registersCaption, programCounter, stackPointer, accumulator, indexX, indexY, status, vectors, buttonBox)

    override def getData(collector: ArrayBuffer[String]): Unit = {
        println("Collecting from RegisterBox")
    }
}
