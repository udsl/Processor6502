package com.udsl.processor6502.ui.popups

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Utilities.{getConfigValue, numToString}
import com.udsl.processor6502.config.AppOptions.assmVersion
import com.udsl.processor6502.config.DataCollector.writeConfigData
import com.udsl.processor6502.config.{ConfigDatum, DataConsumer, DataProvider}
import com.udsl.processor6502.ui.popups.OptionEditor.isCurrent
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, RadioButton, TextArea, ToggleGroup}
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.stage.{Modality, Stage}

import scala.collection.mutable.ListBuffer

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
        val label: Label = new Label("View / Edit Configuration Options")

        children = List(label)
      }

      val sourceProcessorVersionGroup = new ToggleGroup()

      val optionsBox: HBox = new HBox {
        children = List(
          new RadioButton {
            minWidth = 100
            maxWidth = 200
            maxHeight = 50
            text = "Original"
            id = "Original"
            toggleGroup = sourceProcessorVersionGroup
            selected = isCurrent(id.get())
          },
          new RadioButton {
            minWidth = 100
            maxWidth = 200
            maxHeight = 50
            text = "New"
            id = "New"
            toggleGroup = sourceProcessorVersionGroup
            selected = isCurrent(id.get())
          })
      }

      def update(id: String): Unit =
        id match
          case "New" => assmVersion = 2
          case "Original" => assmVersion = 1

      sourceProcessorVersionGroup.selectedToggle.onChange {
        val rb = sourceProcessorVersionGroup.selectedToggle.get.asInstanceOf[javafx.scene.control.ToggleButton]
        if (rb != null) update(rb.getId)
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
        center = optionsBox
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

  def currentVersion: Int = assmVersion

  def isCurrent(id: String): Boolean =
    id match {
      case "Original" => currentVersion == 1
      case _ => currentVersion == 2
    }

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
