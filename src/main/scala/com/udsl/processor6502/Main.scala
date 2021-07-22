package com.udsl.processor6502

import com.udsl.processor6502.UI.{FooterBox, MemoryBox, NumericFormatSelector, RegistersBox}
import scalafx.application.JFXApp
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.layout._
import scalafx.stage.FileChooser

import java.io.File


object Main extends JFXApp {
    val registersBox = new RegistersBox()
    val numericFormat: NumericFormatSelector = NumericFormatSelector.apply

    stage = new JFXApp.PrimaryStage {
        title = "6502 Processor"
        width = 600
        height = 800
        resizable = false

        scene = new Scene {
             root = {
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

  def selectConfigFile: File = {
    val chooser = new FileChooser
    chooser.showOpenDialog(stage)
  }

  def getSaveFile: File = {
    val chooser = new FileChooser
    chooser.showSaveDialog(stage)
  }

}