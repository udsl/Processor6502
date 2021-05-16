package com.udsl.processor6502
package UI

;

import CPU.Processor

import scalafx.application.Platform
import scalafx.event.subscriptions.Subscription
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{Button, Label, TextField, TextInputDialog}
import scalafx.scene.layout.{HBox, VBox}

class RegistersBox extends VBox {
    private var currentFormat = NumericFormatType.Decimal

    def stringToNum(text: String): Int = {
        currentFormat match {
            case NumericFormatType.HexDecimal => Integer.parseInt(text, 16)
            case NumericFormatType.Octal => Integer.parseInt(text, 8)
            case NumericFormatType.Binary => Integer.parseInt(text, 2)
            case NumericFormatType.Decimal => Integer.parseInt(text, 10)
            case _ => -1
        }
    }

    val pc: TextField = new TextField {
        text = Processor.pc.toString
        disable = true
    }

    val pcSubscription: Subscription = Processor.pc._addr.onChange {
        (_, oldValue, newValue) =>
            updateDisplayedValues
    }

    private def updateDisplayedValues: Unit = {
        Platform.runLater(() -> {
            pc.setText(Processor.pc.toString)
        })
    }

    val subscription: Subscription = NumericFormatSelector.numericFormatProperty.onChange {
        (_, oldValue, newValue) =>
            currentFormat = newValue
            updateDisplayedValues
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
        val setButton: Button = new Button {
            text = "set"
            onAction = _ => {
                println("Setting PC!")
                val dialog: TextInputDialog = getPcSetDialogue

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
        prefColumnCount = 8
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

    val acc: TextField = new TextField{
        prefColumnCount = 8
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

    val status = new StatusRegister

    val vectors = new Vectors

    val buttonBox: HBox = new HBox {
        spacing = 20
        val resetButton: Button = new Button {
            text = "Reset"
            onAction = _ => {
                println("Resetting!")
            }
        }

        val nmiButton: Button = new Button {
            text = "NMI"
            onAction = _ => {
                println("NMI!")
            }
        }

        val irqButton: Button = new Button {
            text = "IRQ"
            onAction = _ => {
                println("IRQ!")
            }
        }

        children = List(resetButton, nmiButton, irqButton)
    }

    padding = Insets(20)
    spacing = 8
    children = List(registersCaption, programCounter, stackPointer, accumulator, indexX, indexY, status, vectors, buttonBox)

    def getPcSetDialogue: TextInputDialog = {
        var lastChange = ""
        var lastFormat: NumericFormatType.Value = currentFormat
        var lastKeyValid = false
        var lastKeyCode = 8

        new TextInputDialog(defaultValue = s"${Processor.pc.toString}") {
            initOwner(Main.stage)
            title = "New Program Counter"
            headerText = s"Current format is: ${currentFormat}"
            contentText = "New value:"

            editor.onKeyPressed = e => {
                val et = e.getEventType
                et.getName
                val k = e.getCode
                lastKeyCode = k.getCode
                lastKeyValid = (currentFormat match {
                    case NumericFormatType.HexDecimal => "ABCDEF0123456789".contains(k.getChar)
                    case NumericFormatType.Octal => "01234567".contains(k.getChar)
                    case NumericFormatType.Binary => "10".contains(k.getChar)
                    case NumericFormatType.Decimal => "0123456789".contains(k.getChar)
                    case _ => false
                }) || (k.getCode == 8) || (k.getCode == 127) // TODO add other key codes for valid keys
                println(s"${et.getName} occurred on pc ${e.getCode} which is ${if (lastKeyValid) "VALID" else "INVALID"}")
            }

            editor.text.onChange({
                (_, oldValue, newValue) =>
                    println(s"${oldValue} => ${newValue} last keyCode: ${lastKeyCode}")
                    if (!oldValue.equals(newValue)) {
                        if (!lastKeyValid) {
                            Platform.runLater(new Runnable() {
                                override def run(): Unit = {
                                    //TODO deal with repositioning the cursor!
                                    editor.text = lastChange
                                }
                            })
                        }
                        else if (!lastChange.equals(newValue)) { // Only the is a change in the text
                            println(s"$oldValue => $newValue")
                            if (lastFormat.equals(currentFormat)) { // not due to changing format
                                if (newValue.length > oldValue.length) { // if text is longer then user has types a char
                                    // That char could be at the end or inserted anywhere
                                    val change = newValue.toUpperCase
                                    val k = change diff oldValue
                                    if (currentFormat match {
                                        case NumericFormatType.HexDecimal => "ABCDEF0123456789".contains(k) && change.length <= 4
                                        case NumericFormatType.Octal => "01234567".contains(k) && change.length <= 6
                                        case NumericFormatType.Binary => "10".contains(k) && change.length <= 16
                                        case NumericFormatType.Decimal => "0123456789".contains(k) && change.length <= 5 && stringToNum(change) <= 65535
                                        case _ => false
                                    }) {
                                        editor.text.value = change
                                        lastChange = change
                                    }
                                    else {
                                        editor.text = oldValue
                                    }
                                }
                                else { // they have deleted one
                                    lastChange = newValue.toUpperCase
                                }
                            }
                            else { // format has changed
                                lastFormat = currentFormat
                                lastChange = newValue.toUpperCase
                            }
                        }
                    }
            })
        }
    }
}
