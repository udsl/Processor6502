package com.udsl.processor6502.UI.popups

import com.udsl.processor6502.cpu.Processor
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label}
import scalafx.scene.layout.VBox
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

        children = List(label, pushButton)
      }
    }
  }
}
