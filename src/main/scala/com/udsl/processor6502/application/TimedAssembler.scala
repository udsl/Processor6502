package com.udsl.processor6502.application

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.FileIOUtilities.selectSourceFileToLoad
import com.udsl.processor6502.application.Main.stage
import com.udsl.processor6502.application.TimedAssembler.textArea
import com.udsl.processor6502.assembler.{Assembler, AssemblyData, SyntaxErrorListener, SyntaxErrorRecord}
import com.udsl.processor6502.config.AppOptions.assmVersion
import scalafx.application.JFXApp3
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, RadioButton, TextArea, ToggleGroup}
import scalafx.scene.layout.{BorderPane, HBox, VBox}

import scala.io.Source
import scala.language.postfixOps

object TimedAssembler extends JFXApp3 with StrictLogging{

  def currentVersion: Int = assmVersion

  def isCurrent(id: String): Boolean =
    id match {
      case "Original" => currentVersion == 1
      case _ => currentVersion == 2
    }

  def setCurrentVersion(): Unit =
    assmVersion = currentVersion

  var textArea: TextArea = _

  def logProgress(message: String): Unit =
    logger.info(message)
    val txt = textArea.text.value +"\n" + message
    textArea.text.value = txt

  def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title.value = "Timed 6502 assembler"
      width = 500
      height = 400
      resizable = false

      scene = new Scene {
        root = {
          val titleBox: VBox = new VBox {
            val label: Label = new Label("Assembler Version")

            val selBox: HBox = new HBox {
              padding = Insets(8, 0, 0,8)
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

          val sourceBox: VBox = new VBox {
            padding = Insets(20,0, 20, 0)
            val label: Label = new Label("Results")

            val output: HBox = new HBox {
              padding = Insets(20)
              textArea = new TextArea()
              children = List(textArea)
            }

            children = List(label, output)
          }

          AssemblyData.addSyntaxErrorListener(new Listener(textArea))

          val buttons: HBox = new HBox {
            val assembleButton: Button = new Button {
              text = "Assemble"
              onAction = _ => {
                // Do assemble using selected version
                setCurrentVersion()
                selectSourceFileToLoad match
                  case Some(file) =>
                    textArea.text = ""
                    logProgress(s"Assembling file\n$file")
                    logProgress(s"Using assembler version $currentVersion")
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
            children = List(assembleButton, cancelButton)
          }

          new BorderPane {
            maxWidth = 400
            maxHeight = 400
            padding = Insets(20)
            top = titleBox
            center = sourceBox
            bottom = buttons
          }
        }
      }
    }
  }
}

class Listener(val textArea: TextArea) extends SyntaxErrorListener:
  def doNotify(syn: SyntaxErrorRecord): Unit =
    val txt = textArea.text.value + "\n" + syn.errorMessage +" line " + syn.lineNumber
    textArea.text.value = txt


