package com.udsl.processor6502:

  import com.udsl.processor6502.ui.{FooterBox, MemoryBox, NumericFormatSelector, RegistersBox}
  import scalafx.application.{JFXApp3}
  import scalafx.geometry.Insets
  import scalafx.scene.Scene
  import scalafx.scene.layout.*
  import scalafx.stage.FileChooser

  import java.io.File


  object Main extends JFXApp3:

    def start(): Unit = {
      val registersBox = new RegistersBox()

      stage = new JFXApp3.PrimaryStage {
        title.value = "6502 Processor"
        width = 700
        height = 800
        resizable = false

        scene = new Scene {
          root =
            val memoryBox = new MemoryBox()

            val footer = new FooterBox()

            new BorderPane {
              maxWidth = 400
              maxHeight = 300
              padding = Insets(20)
              top = NumericFormatSelector.apply
              left = registersBox
              right = memoryBox
              bottom = footer
            }
        }
      }
    }

    def selectConfigFile(): File = {
      val chooser = new FileChooser
      chooser.showOpenDialog(stage)
    }

    def getSaveFile(): File = {
      val chooser = new FileChooser
      chooser.showSaveDialog(stage)
    }

