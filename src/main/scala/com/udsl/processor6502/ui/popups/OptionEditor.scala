package com.udsl.processor6502.ui.popups

import com.udsl.processor6502.Utilities.numToString
import com.udsl.processor6502.assembler.version1.Assemble6502
import com.udsl.processor6502.config.DataCollector.writeConfigData
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, TextArea}
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.stage.{Modality, Stage}

class OptionEditor extends Stage {
  title = "Edit Options"
  width = 400
  height = 200
  resizable = false

  initOwner(new Stage)
  initModality(Modality.ApplicationModal)

  scene = new Scene {
    root = {
      val titleBox: HBox = new HBox {
        val label: Label = new Label("View / Edit ConfigurationOptions Options")

        children = List(label)
      }

      val buttons: HBox = new HBox {
        val saveButton: Button = new Button {
          text = "Save"
          onAction = _ => {
            if writeConfigData() then
              close()
          }
        }

        val cancelButton: Button = new Button {
          text = "Cancel"
          onAction = _ => {
            close()
          }
        }
        children = List(saveButton, cancelButton)
      }

      new BorderPane {
        maxWidth = 400
        maxHeight = 300
        padding = Insets(20)
        top = titleBox
        bottom = buttons
      }
    }
  }

  override def close(): Unit =
    OptionEditor.close()
    super.close()
}

object OptionEditor:
  var optionEditor: Option[OptionEditor] = None

  def close(): Unit =
    optionEditor = None

  def sendToBack(): Unit =
    optionEditor match
      case Some(_) =>
        optionEditor.get.toBack()
      case _ =>

  def bringToFront(): Unit =
    optionEditor match
      case Some(_) =>
        optionEditor.get.toFront()
      case _ =>

  def showOptionEditor(): Unit =
    optionEditor match
      case Some(_) =>
        optionEditor.get.toFront()
      case _ =>
        optionEditor = Some(OptionEditor())
        optionEditor.get.show()
