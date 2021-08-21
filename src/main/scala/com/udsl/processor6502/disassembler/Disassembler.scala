package com.udsl.processor6502.disassembler

import com.udsl.processor6502.Utilities.numToString
import scalafx.application.JFXApp
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.Label
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.stage.{Modality, Stage}

class Disassembler( val loc: Int) extends Stage {
  title = "Disassembler"

  initOwner(JFXApp.ActiveApp.stage)
  initModality(Modality.ApplicationModal)

  scene = new Scene {
    root = {
      val titleBox = new HBox {
        val label: Label = new Label(s"Disassembling for location: ${numToString(loc)}")

        children = List(label)
      }

      new BorderPane {
        maxWidth = 400
        maxHeight = 300
        padding = Insets(20)
        top = titleBox
      }
    }
  }
}

object Disassembler {
 def apply( loc: Int){
   val disassembler = new Disassembler(loc)
   disassembler.showAndWait()
  }
}
