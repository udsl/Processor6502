package com.udsl.processor6502.ui

import com.udsl.processor6502.Dialogues.confirmation
import com.udsl.processor6502.FileIOUtilities.{selectSourceFileToLoad, selectSourceFileToSave}
import com.udsl.processor6502.Utilities
import com.udsl.processor6502.application.Main
import com.udsl.processor6502.assembler.Assembler
import scalafx.application.JFXApp
import scalafx.event.EventHandler
import scalafx.event.EventIncludes.eventClosureWrapperWithZeroParam
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.*
import scalafx.scene.layout.{BorderPane, GridPane, HBox, VBox}
import scalafx.stage.{Modality, Stage, WindowEvent}

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source


class CodeEditor extends Stage {
  title = "Code Editor"
  width = 400
  height = 500
  maxWidth = 600
  maxHeight = 800

  initOwner(new Stage)
  initModality(Modality.None)

  onCloseRequest = () => {
    if (!textChanged || confirmation("Abandon text changes")) {
      doClose(true)
    }
  }

  var currentFile: Option[File] = Option.empty
  var textChanged = false

  def titleText = s"Code editor - editing ${currentFile.getOrElse("UNDEFINED")}"

  val textArea: TextArea = new TextArea() {
    maxWidth = 800
    prefColumnCount = 1000
    onKeyTyped.delegate.setValue(_ => {
      textChanged = text.value.nonEmpty
    })
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
          doClose()
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

      val saveButton = new MenuButton {
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

      val buttonBox: VBox = new VBox {
        padding = Insets(4, 0, 4, 0)
        val gridPane = new GridPane()
        gridPane.setHgap(4)
        gridPane.setVgap(4)

        gridPane.add(saveButton, 0, 0, 1, 1)
        gridPane.add(loadButton, 2, 0, 1, 1)
        gridPane.add(reLoadButton, 4, 0, 1, 1)
        gridPane.add(closeButton, 6, 0, 1, 1)
        gridPane.add(assembleButton, 15, 0, 1, 1)

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

  def saveAs(): Unit =
    currentFile = selectSourceFileToSave
    writeToSaveFile()


  def save(): Unit =
    if currentFile.isEmpty then
      saveAs()
    else
      writeToSaveFile()

  private def writeToSaveFile(): Unit =
    val editorText = textArea.text.value
    val bw = new BufferedWriter(new FileWriter(currentFile.get))
    bw.write(editorText)
    bw.write("\n")
    bw.close()
    textChanged = false
    label.setText(titleText)
    CodeEditor.bringToFront()

  private def loadFromFile( file: File): Unit =
    val bufferedSource = Source.fromFile(file)
    textArea.text.value = bufferedSource.mkString
    CodeEditor.bringToFront()

  def load(): Unit =
    selectSourceFileToLoad match
      case Some(file) =>
        currentFile = Some(file)
        loadFromFile(file)
      case None =>

  def reLoad(): Unit =
    currentFile match
      case Some(file) =>
        loadFromFile(file)
      case None =>
        load()
  /**
   * Perform assembly of the text in the code editor
   */
  def assemble(): Unit =
    val asm = Assembler.apply(textArea.text.value)
    asm.startAssembly()

  def doClose(confirmed: Boolean = false): Unit =
    if confirmed || !textChanged || confirmation("Abandon text changes") then
      CodeEditor.close()
      super.close()
    else
      CodeEditor.showCodeEditor()
}

object CodeEditor:
  var codeEditor: Option[CodeEditor] = None

  def close(): Unit =
    codeEditor = None

  def sendToBack(): Unit =
    codeEditor match
      case Some(_) =>
        codeEditor.get.toBack()
      case _ =>

  def bringToFront(): Unit =
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
