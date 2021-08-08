package com.udsl.processor6502.UI.popups

import com.udsl.processor6502.Assembler.Assemble6502.{assemble, printLabels, printTokenisedLines}
import com.udsl.processor6502.Utilities.numToString
import com.udsl.processor6502.cpu.Processor
import scalafx.application.JFXApp
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, TextArea}
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.stage.{Modality, Stage}

class LineAssembler( val location: Int ) extends Stage{
  title = "Line Assembler - Popup"
  width = 400
  height = 200
  resizable = false

  initOwner(JFXApp.ActiveApp.stage)
  initModality(Modality.ApplicationModal)

  scene = new Scene {
    root = {
        val titleBox = new HBox {
          val label: Label = new Label(s"Assembling for location: ${numToString(location)}")

          children = List(label)
        }

      val textBox  = new HBox {
        val textArea = new TextArea()
        children = List(textArea)
      }

      val buttons = new HBox {
        val closeButton = new Button {
          text = "Close"
          onAction = _ => {
            close()
          }
        }
        val assembleButton = new Button {
          text = "Assemble"
          onAction = _ => {
            assemble(textBox.textArea.text.value, location)
            printTokenisedLines
          }
        }
        children = List(closeButton, assembleButton)
      }

      new BorderPane {
        maxWidth = 400
        maxHeight = 300
        padding = Insets(20)
        top = titleBox
        center = textBox
        bottom = buttons
      }
    }
  }
}
