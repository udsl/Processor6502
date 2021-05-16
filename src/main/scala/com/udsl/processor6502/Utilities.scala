package com.udsl.processor6502

import com.udsl.processor6502.CPU.Processor
import scalafx.application.Platform
import scalafx.scene.control.TextInputDialog

object Utilities {
    var currentFormat: NumericFormatType.Value = NumericFormatType.Decimal

    def stringToNum(text: String): Int = {
        currentFormat match {
            case NumericFormatType.HexDecimal => Integer.parseInt(text, 16)
            case NumericFormatType.Octal => Integer.parseInt(text, 8)
            case NumericFormatType.Binary => Integer.parseInt(text, 2)
            case NumericFormatType.Decimal => Integer.parseInt(text, 10)
            case _ => -1
        }
    }


    def getAddressSettingDialogue(dialogueTitle: String): TextInputDialog = {
        var lastChange = ""
        var lastFormat: NumericFormatType.Value = currentFormat
        var lastKeyValid = false
        var lastKeyCode = 8

        new TextInputDialog(defaultValue = s"${Processor.pc.toString}") {
            initOwner(Main.stage)
            title = dialogueTitle
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
