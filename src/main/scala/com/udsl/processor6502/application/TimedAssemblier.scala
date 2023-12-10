package com.udsl.processor6502.application

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.FileIOUtilities.selectSourceFileToLoad
import com.udsl.processor6502.application.Main.{memoryBox, stage}
import com.udsl.processor6502.assembler.Assembler
import com.udsl.processor6502.config.AppOptions.assmVersion
import com.udsl.processor6502.ui.NumericFormatSelector
import scalafx.application.JFXApp3
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, RadioButton, TextArea, ToggleGroup}
import scalafx.scene.layout.{BorderPane, HBox, VBox}

object TimedAssemblier  extends JFXApp3 with StrictLogging {

  def currentVersion: Int = assmVersion

  def isCurrent(id: String): Boolean =
    id match {
      case "Original" => currentVersion == 1
      case _ => currentVersion == 2
    }

  def setCurrentVersion(): Unit =
    assmVersion = currentVersion

  var textArea: TextArea = null

  def logProgress(message: String): Unit =
    logger.info(message)
    val txt = textArea.text.value +"\n" + message
    textArea.text.value = txt

  def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title.value = "Timed 6502 Assemblier"
      width = 500
      height = 400
      resizable = false

      scene = new Scene {
        root = {
          val titleBox: VBox = new VBox {
            val label: Label = new Label("Assemblier version")

            val selBox: HBox = new HBox {
              val sourceProcessorVersionGroup = new ToggleGroup()

              val v1Assm: RadioButton = new RadioButton {
                  minWidth = 100
                  maxWidth = 200
                  maxHeight = 50
                  text = "Original Assm"
                  id = "Original"
                  toggleGroup = sourceProcessorVersionGroup
                  selected = isCurrent(id.get())
                }

              val v2Assm: RadioButton = new RadioButton {
                  minWidth = 100
                  maxWidth = 200
                  maxHeight = 50
                  text = "New Assm"
                  id = "New"
                  toggleGroup = sourceProcessorVersionGroup
                  selected = isCurrent(id.get())
                }

              children = List(v1Assm, v2Assm)
            }

            children = List(label, selBox)
          }

          val output: HBox = new HBox {
            padding = Insets(20, 20, 20, 20)
            textArea = new TextArea()
            children = List(textArea)
          }

          val buttons: HBox = new HBox {
            val saveButton: Button = new Button {
              text = "Assemble"
              onAction = _ => {
                // Do assemble using selected version
                setCurrentVersion()
                selectSourceFileToLoad match
                  case Some(file) =>
                    logProgress(s"Assembling file\n$file")
                    logProgress(s"Using assemblier version $currentVersion")
                    val startTime = System.currentTimeMillis()
                    val sfa: Assembler = Assembler.apply(file)
                    val fileLoaded = System.currentTimeMillis()
                    sfa.startAssembly()
                    val completed  = System.currentTimeMillis()
                    logProgress(s"File loaded after: ${fileLoaded-startTime}ms, completed after: ${completed-startTime}ms")
                    logProgress(s"Assembly took ${completed-fileLoaded}ms")
                  case _ =>
                    logProgress(s"Failed to load a file to assemble.")
              }
            }

            val cancelButton: Button = new Button {
              text = "Close"
              onAction = _ => {
                close()
              }
            }
            children = List(saveButton, cancelButton)
          }

          new BorderPane {
            maxWidth = 400
            maxHeight = 400
            padding = Insets(20)
            top = titleBox
            center = output
            bottom = buttons
          }
        }
      }
    }
  }
}
