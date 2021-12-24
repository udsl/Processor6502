package com.udsl.processor6502.assembler

import com.udsl.processor6502.Dialogues.{confirmation, selectSourceFileToLoad}
import com.udsl.processor6502.{Main, Utilities}
import scalafx.application.JFXApp
import scalafx.event.EventHandler
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, MenuButton, MenuItem, TextArea}
import scalafx.scene.layout.{BorderPane, GridPane, HBox, VBox}
import scalafx.stage.{Modality, Stage}

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source
import scalafx.event.EventIncludes.eventClosureWrapperWithZeroParam


class CodeEditor extends Stage {
  title = "Code Editor"
  width = 400
  height = 500
  maxWidth = 600
  maxHeight = 800

  initOwner(new Stage)
  initModality(Modality.None)

  onCloseRequest = () => {
    CodeEditor.close()
  }

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

      val closeButton = new Button {
        text = "Close"
        onAction = _ => {
          close()
        }
      }

      val saveMenuItem = new MenuItem {
        text = "Save"

        onAction = _ => {
          save()
        }
      }

      val saveAsMenuItem = new MenuItem {
        text = "Save As"

        onAction = _ => {
          saveAs()
        }
      }

      val saveButton = new MenuButton{
        text = "Save"
        items = List(saveMenuItem, saveAsMenuItem)
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

      val assembleButton = new Button {
        text = "" +
          "Assemble"
        onAction = _ => {
          assemble()
        }
      }

      val buttonBox = new VBox {
        padding = Insets(4, 0, 4, 0)
        val gridPane = new GridPane();
        gridPane.setHgap(4)
        gridPane.setVgap(4)

        gridPane.add(saveButton, 0, 0, 1, 1);
        gridPane.add(loadButton, 2, 0, 1, 1);
        gridPane.add(reLoadButton, 4, 0, 1, 1);
        gridPane.add(closeButton, 6, 0, 1, 1);
        gridPane.add(assembleButton, 15, 0, 1, 1);

        children = List(gridPane)
      }

      new BorderPane {
        maxWidth = 400
        maxHeight = 800
        padding = Insets(20)
        top = titleBox
        center = textBox
        bottom = buttonBox
      }
    }
  }

  def saveAs(): Unit = {
    val saveFile = Utilities.selectSourceFileToSave
    if (saveFile != null) {
      currentFile = saveFile
    }
    writeToSaveFile()
  }

  def save(): Unit = {
    if (currentFile == null) {
      saveAs()
    }
    else {
      writeToSaveFile()
    }
  }

  def writeToSaveFile(): Unit = {
    val editorText = textArea.text.value
    val bw = new BufferedWriter(new FileWriter(currentFile))
    bw.write(editorText)
    bw.write("\n")
    bw.close()
    textChanged = false
    label.setText(titleText)
    CodeEditor.toFront()
  }

  def load(): Unit = {
    val sourceFile = selectSourceFileToLoad
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
    CodeEditor.toFront()
  }

  def assemble(): Unit = {
    val asm = Assemble6502.apply(textArea.text.value)
    asm.assemble()
  }

  override def close(): Unit = {
    if (!textChanged || confirmation("Abandon text changes")) {
      super.close()
    }
  }
}

object CodeEditor {
  var codeEditor: Option[CodeEditor] = None

  def close(): Unit =
    codeEditor = None

  def toBack(): Unit =
    codeEditor match
      case Some(_) =>
        codeEditor.get.toBack()
      case _ =>

  def toFront(): Unit =
    codeEditor match
      case Some(_) =>
        codeEditor.get.toFront()
      case _ =>

  def showCodeEditor(): Unit =
    codeEditor match
      case Some(_) =>
        codeEditor.get.toFront()
      case _ =>
        codeEditor = Some(CodeEditor())
        codeEditor.get.show()
}