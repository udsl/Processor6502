package com.udsl.processor6502

import javafx.event.{ActionEvent, EventHandler}
import scalafx.Includes._
import scalafx.application.{JFXApp, Platform}
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Orientation, Pos}
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, _}


object Main extends JFXApp {

  stage = new JFXApp.PrimaryStage {
    title = "6502 Processor"
    width = 600
    height = 800
    resizable = false


    scene = new Scene {
      root = {
        val memoryBox: VBox = new VBox {
           val memeoryBoxCaption = new Label {
            text = "Memory View"
             padding = Insets(0,0,0,80)
          }

          val seq = Seq("Row 1", "Row 2", "Long Row 3", "Row 4", "Row 5", "Row 6", "Row 7",
            "Row 8", "Row 9", "Row 10", "Row 11", "Row 12", "Row 13", "Row 14", "Row 15",
            "Row 16", "Row 17", "Row 18", "Row 19", "Row 20")

          val memoryView = new ListView[String] {
            items = ObservableBuffer(seq)
            orientation = Orientation.Vertical
          }

          val st = new StackPane {
            padding = Insets(10)
            children = memoryView
          }

          children = List(memeoryBoxCaption, st)
        }

        val numericFormat = new NumericFormatSelector()

        val registersBox = new RegistersBox(numericFormat)

        new BorderPane {
          maxWidth = 400
          maxHeight = 300
          padding = Insets(20)
          top = numericFormat
          left = registersBox
//          center = numFormatLabel
          right = memoryBox
        }
      }
    }
  }
}