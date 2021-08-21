package com.udsl.processor6502.assembler

import com.udsl.processor6502.Main
import com.udsl.processor6502.Utilities.confirmation
import scalafx.application.JFXApp
import scalafx.event.EventHandler
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, TextArea}
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.stage.{Modality, Stage}

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source

class CodeEditor extends Stage {
  title = "Code Editor"
  width = 400
  height = 500
  maxWidth = 600
  maxHeight = 800

  initOwner(JFXApp.ActiveApp.stage)
  initModality(Modality.ApplicationModal)

  var currentFile: File = null
  var textChanged = false

  def titleText = s"Code editor - editing ${if (currentFile == null) "UNDEFINED" else currentFile.getName}"

  val textArea = new TextArea() {
    maxWidth = 800
    prefColumnCount = 1000
    onKeyTyped.delegate.setValue(_ => { textChanged = text.value.nonEmpty } )
  }

  val label: Label = new Label(titleText)

  scene = new Scene {
    root = {
      val titleBox = new HBox {
        children = List(label)
      }

      val textBox = new HBox {
        maxWidth = 600
        style = "-fx-background-color: #336699;"
        children = List(textArea)
      }

      val buttons = new HBox {
        padding = Insets(15, 12, 15, 12)
        spacing = 10
        style = "-fx-background-color: #336699;"

        val closeButton = new Button {
          text = "Close"
          onAction = _ => {
            close()
          }
        }

        val saveButton = new Button {
          text = "Save"
          onAction = _ => {
            save()
          }
        }

        val loadButton = new Button {
          text = "" +
            "Load"
          onAction = _ => {
            load()
          }
        }

        val reLoadButton = new Button {
          text = "" +
            "Reload"
          onAction = _ => {
            reLoad()
          }
        }
        children = List(saveButton, loadButton, reLoadButton, closeButton)
      }

      new BorderPane {
        maxWidth = 400
        maxHeight = 800
        padding = Insets(20)
        top = titleBox
        center = textBox
        bottom = buttons
      }
    }
  }

  def saveAs(): Unit = {
    val saveFile = Main.getSaveFile
    if (saveFile != null) {
      currentFile = saveFile
    }
    witeToSaveFile()
  }

  def save(): Unit = {
    if (currentFile == null) {
      saveAs()
    }
    else {
      witeToSaveFile()
    }
  }

  def witeToSaveFile(): Unit = {
    val editorText = textArea.text.value
    val bw = new BufferedWriter(new FileWriter(currentFile))
    bw.write(editorText)
    bw.write("\n")
    bw.close()
    textChanged = false
    label.setText(titleText)
  }

  def load(): Unit = {
    val sourceFile = Main.selectConfigFile
    if (sourceFile != null) {
      currentFile = sourceFile
      label.setText(titleText)
      reLoad()
    }
  }

  def reLoad(): Unit = {
    if (currentFile != null) {
      val bufferedSource = Source.fromFile(currentFile)
      textArea.text.value = bufferedSource.mkString
    }
  }

  override def close(): Unit = {
    if (!textChanged || confirmation("Abandon text changes")) {
      super.close()
    }
  }
}

object CodeEditor {
  def showOdeEditor(): Unit = {
    val editor = new CodeEditor()
    editor.showAndWait()
  }
}