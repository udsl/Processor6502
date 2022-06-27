package com.udsl.processor6502.ui

import scalafx.stage.{Modality, Stage}
import scalafx.event.EventIncludes.eventClosureWrapperWithZeroParam
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, MenuButton, MenuItem, TextArea}
import scalafx.scene.layout.{BorderPane, GridPane, HBox, VBox}

class ProgressIndicator extends Stage {
  title = "Code Editor"
  width = 400
  height = 500
  maxWidth = 800
  maxHeight = 1000

  initOwner(new Stage)
  initModality(Modality.None)

  onCloseRequest = () => {
    ProgressIndicator.close()
  }

  val textArea = new TextArea() {
    maxWidth = 800
    prefColumnCount = 1000
//    onKeyTyped.delegate.setValue(_ => { textChanged = text.value.nonEmpty } )
  }

  def updateProgress(text: String): Unit =
    textArea.text.value += text

  scene = new Scene {
    root = {
      val titleBox = new HBox {
        children = List(new Label("Process"))
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

      new BorderPane {
        maxWidth = 400
        maxHeight = 800
        padding = Insets(20)
        top = titleBox
        center = textBox
        bottom = closeButton
      }
    }
  }

}


object ProgressIndicator {
  var progressIndicator: Option[ProgressIndicator] = None

  def close(): Unit =
    progressIndicator = None

  def toBack(): Unit =
    progressIndicator match
      case Some(_) =>
        progressIndicator.get.toBack()
      case _ =>

  def toFront(): Unit =
    progressIndicator match
      case Some(_) =>
        progressIndicator.get.toFront()
      case _ =>

  def showProgress(): Unit =
    progressIndicator match
      case Some(_) =>
        progressIndicator.get.toFront()
      case _ =>
        progressIndicator = Some(ProgressIndicator())
        progressIndicator.get.show()

  def addProgress(text: String): Unit =
    progressIndicator match
      case Some(_) =>
        progressIndicator.get.updateProgress(text)
      case _ => ()



}