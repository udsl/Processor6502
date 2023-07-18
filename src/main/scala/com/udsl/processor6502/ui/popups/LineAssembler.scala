package com.udsl.processor6502.ui.popups

import com.udsl.processor6502.Utilities.numToString
import scalafx.application.JFXApp
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, TextArea}
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.stage.{Modality, Stage}

class LineAssemblerPopup(val location: Int) extends Stage {
  title = "Line Assembler - Popup"
  width = 400
  height = 200
  resizable = false

  initOwner(new Stage)
  initModality(Modality.ApplicationModal)

  val textArea = new TextArea()

  scene = new Scene {
    root = {
      val titleBox: HBox = new HBox {
        val label: Label = new Label(s"Assembling for location: ${numToString(location)}")

        children = List(label)
      }

      val textBox = new HBox {
        children = List(textArea)
      }

      val buttons: HBox = new HBox {
        val closeButton: Button = new Button {
          text = "Close"
          onAction = _ => {
            close()
          }
        }

        val assembleButton: Button = new Button {
          text = "Assemble"
          onAction = _ => {
//              val assember = Assemble6502.apply(textArea.text.value, location)
//              assember.assemble()
//              assember.printTokenisedLines()
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

