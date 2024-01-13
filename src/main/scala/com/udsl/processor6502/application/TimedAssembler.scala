package com.udsl.processor6502.application

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.FileIOUtilities.selectSourceFileToLoad
import com.udsl.processor6502.application.Main.stage
import com.udsl.processor6502.application.TimedAssembler.textArea
import com.udsl.processor6502.assembler.ErrType.{ASSEMBLE, PARSING, SYNTAX}
import com.udsl.processor6502.assembler.{AssembleErrorRecord, Assembler, AssemblyData, ErrorListener, ErrorRecord, ParsingErrorRecord, SyntaxErrorRecord}
import com.udsl.processor6502.config.AppOptions.assmVersion
import scalafx.application.JFXApp3
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, RadioButton, TextArea, ToggleGroup}
import scalafx.scene.layout.{BorderPane, HBox, VBox}

import scala.io.Source
import scala.language.postfixOps

object TimedAssembler extends JFXApp3 with StrictLogging{

  var currentVersion: Int = assmVersion

  def isCurrent(id: String): Boolean =
    id match {
      case "Original" => currentVersion == 1
      case _ => currentVersion == 2
    }

  def setCurrentVersion(id: String): Unit =
    id match {
      case "Original" => currentVersion = 1
      case _ => currentVersion = 2
    }
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
          val sourceProcessorVersionGroup = new ToggleGroup()
          val titleBox: VBox = new VBox {
            val label: Label = new Label("Assembler Version")

            val selBox: HBox = new HBox {
              padding = Insets(8, 0, 0,8)
              
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

          sourceProcessorVersionGroup.selectToggle(sourceProcessorVersionGroup.toggles.head)
          sourceProcessorVersionGroup.selectedToggle.onChange {
            val rb = sourceProcessorVersionGroup.selectedToggle.get.asInstanceOf[javafx.scene.control.ToggleButton]
            if (rb != null) setCurrentVersion(rb.getId)
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

          AssemblyData.addErrorListener(new Listener(textArea))

          val buttons: HBox = new HBox {
            val assembleButton: Button = new Button {
              text = "Assemble"
              onAction = _ => {
                // Do assemble using selected version
                selectSourceFileToLoad match
                  case Some(file) =>
                    textArea.text = ""
                    logProgress(s"Assembling file\n$file")
                    logProgress(s"Using assembler version $currentVersion")
                    logProgress("initialising. . .")
                    AssemblyData.clear()
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

class Listener(val textArea: TextArea) extends ErrorListener:
  private def updateDisplay(txt: String): Unit =
    textArea.text.value = textArea.text.value + txt
    
  def doNotify(err: ErrorRecord): Unit =
    err.errType match
      case SYNTAX => 
        val syn = err.asInstanceOf[SyntaxErrorRecord]
        updateDisplay( s"\n${syn.errorMessage} line ${syn.lineNumber} -> '${syn.sourceText}'")

      case PARSING =>
        val par = err.asInstanceOf[ParsingErrorRecord]
        updateDisplay( s"\n${par.errorMessage} line text '${par.sourceText}'" )

      case ASSEMBLE =>
        val ass = err.asInstanceOf[AssembleErrorRecord]
        updateDisplay(s"\nAssemble error text '${ass.errorMessage}'")
     


