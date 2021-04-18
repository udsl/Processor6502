package com.udsl.processor6502

import scalafx.application.JFXApp
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.layout._


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