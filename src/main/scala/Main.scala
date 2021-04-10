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
        val numericFormat = new NumericFormatSelector()

        val registersBox = new RegistersBox(numericFormat)

        val memoryBox = new MemoryBox(numericFormat)

        new BorderPane {
          maxWidth = 400
          maxHeight = 300
          padding = Insets(20)
          top = numericFormat
          left = registersBox
          right = memoryBox
        }
      }
    }
  }
}