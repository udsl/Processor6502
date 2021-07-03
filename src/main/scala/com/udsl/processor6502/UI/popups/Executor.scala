package com.udsl.processor6502.UI.popups

import com.udsl.processor6502.cpu.{Processor, StatusRegisterFlags}
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, TextField}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.stage.{Modality, Stage}

class Executor extends Stage{
  title = "Executor - Popup"
  width = 400
  height = 200
  resizable = false

  initOwner(JFXApp.ActiveApp.stage)
  initModality(Modality.ApplicationModal)

  scene = new Scene {
    root = {
      new VBox {
        val label: Label = new Label("Just a label")

        val pushButton: Button = new Button {
          text = "Push"
          onAction = _ => {
            println("Executing push!")
            Processor.sp.pushByte(99)
          }
        }

        def togle(flag: StatusRegisterFlags.Value) = {
          print(flag)
          val state = Processor.sr.testFlag(flag)
          println(s"  $state")
          if (state) Processor.sr.clearFlag(flag) else Processor.sr.setFlag(flag)
        }

        val flagTest = new HBox {
          val toggleButton: Button = new Button {
            text = "Togle"
            onAction = _ => {
              println("Executing toggle!")
              val c = toToggle.text.value.toString.toUpperCase().take(1)
              c match {
                case "N" => togle(StatusRegisterFlags.Negative)
                case "O" => togle(StatusRegisterFlags.Overflow)
                case "B" => togle(StatusRegisterFlags.Break)
                case "D" => togle(StatusRegisterFlags.Decimal)
                case "I" => togle(StatusRegisterFlags.Interrupt)
                case "Z" => togle(StatusRegisterFlags.Zero)
                case "C" => togle(StatusRegisterFlags.Carry)
                case _ => println("WTF!")
              }
            }
          }

          val toToggle = new TextField {
          }
          children = List(toggleButton, toToggle)
        }

        children = List(label, pushButton, flagTest)
      }
    }
  }
}
