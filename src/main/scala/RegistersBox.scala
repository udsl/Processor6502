package com.udsl.processor6502;

import javafx.animation.Animation.Status
import javafx.event.EventHandler
import javafx.scene.input.KeyEvent
import scalafx.application.Platform
import scalafx.beans.property.ObjectProperty
import scalafx.event.subscriptions.Subscription
import scalafx.scene.layout.{HBox, VBox}
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{Label, TextField}
import scalafx.scene.input.KeyCode

import scala.annotation.switch


class RegistersBox(val numericFormat: NumericFormatSelector) extends VBox {
    private var currentValue : Int = 65535 //_
    private var currentFormat : String = "Dec"

    def getCurrentPc: Int = {
        currentValue
    }

    def getCurrentFormat: String = {
        currentFormat
    }

    def stringToNum(text: String) : Int = {
        (currentFormat: @switch) match {
            case "Hex" => Integer.parseInt(text, 16)
            case "Oct" => Integer.parseInt(text, 8)
            case "Bin" => Integer.parseInt(text, 2 )
            case "Dec" => Integer.parseInt(text, 10)
            case _ => -1
        }}

    val pc: TextField = new TextField {
        var lastChange = ""
        var lastFormat: String = currentFormat
        text = currentValue.toString

        text.onChange({
            (_, oldValue, newValue) =>
              if (!lastChange.equals(newValue) ){ // Only the is a change in the text
                  println(s"$oldValue => $newValue")
                  if (lastFormat.equals(currentFormat)) { // not due to changing format
                      if (newValue.length > oldValue.length) { // if text is longer then user has types a char
                          // That char could be at the end or inserted anywhere
                          val change = newValue.toUpperCase
                          val k = change diff oldValue
                          if ((currentFormat: @switch) match {
                              case "Hex" => "ABCDEF0123456789".contains(k) && change.length <= 4
                              case "Oct" => "01234567".contains(k) && change.length <= 6
                              case "Bin" => "10".contains(k) && change.length <= 16
                              case "Dec" => "0123456789".contains(k) && change.length <= 5 && stringToNum(change) <= 65535
                              case _ => false
                          }) {
                              text.value = change
                              lastChange = change
                              currentValue = stringToNum(change)
                          }
                          else {
                              text = oldValue
                          }
                      }
                      else { // they have deleted one
                          lastChange = newValue.toUpperCase
                          if (lastChange.isEmpty) {
                              currentValue = -1
                          }
                          else {
                              currentValue = stringToNum(lastChange)
                          }
                      }
                  }
                  else{ // format has changed
                      lastFormat = currentFormat
                      lastChange = newValue.toUpperCase
                  }
              }
        })
     }

    def formatListemer = { (o: javafx.beans.value.ObservableValue[_ <: String], oldVal: String, newVal: String) =>
        currentFormat = newVal
        println("formatListemer: '" + currentFormat + "'")
    }

    private def updateDisplayedValues(): Unit ={
        Platform.runLater(() -> {
             (currentFormat: @switch) match {
                case "Hex" => pc.setText(currentValue.toHexString.toUpperCase)
                case "Oct" => pc.setText(currentValue.toOctalString)
                case "Bin" => pc.text.setValue(currentValue.toBinaryString)
                case "Dec" => pc.setText(currentValue.toString)
                case _ => println("Nothing to match")
            }
        })
    }

    val subscription: Subscription = numericFormat.numFormatText.onChange {
        (_, oldValue, newValue) =>
            currentFormat = newValue
            updateDisplayedValues()
    }

    val registersCaption: Label = new Label {
        text = "Processor Registers"
        padding = Insets(0,0,0,80)
    }

    val programCounter: HBox = new HBox {
        val label: Label = new Label("PC:") {
            style = "-fx-font-weight:bold"
            alignmentInParent = Pos.BottomLeft
        }

        children = List(label, pc)
    }

    padding = Insets(20)
    children = List(registersCaption, programCounter)
}
