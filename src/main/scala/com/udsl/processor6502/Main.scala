package com.udsl.processor6502

import com.udsl.processor6502.UI.{FooterBox, MemoryBox, NumericFormatSelector, RegistersBox}
import scalafx.application.JFXApp
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.layout._


object Main extends JFXApp {

  def getData(): Unit = {
    println("getData called on Main")
  }
    val registersBox = new RegistersBox()

    stage = new JFXApp.PrimaryStage {
        title = "6502 Processor"
        width = 600
        height = 800
        resizable = false

        scene = new Scene {
             root = {
                val numericFormat = NumericFormatSelector.apply

//               val registersBox = new RegistersBox()

                val memoryBox = new MemoryBox()

                val footer = new FooterBox()

                new BorderPane {
                    maxWidth = 400
                    maxHeight = 300
                    padding = Insets(20)
                    top = numericFormat
                    left = registersBox
                    right = memoryBox
                    bottom = footer
                }
            }
        }
    }
}