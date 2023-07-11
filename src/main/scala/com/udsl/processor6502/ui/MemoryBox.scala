package com.udsl.processor6502.ui

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Dialogues.{confirmation, errorAlert, getNumberSettingDialogue}
import com.udsl.processor6502.cpu.{Memory, MemoryCell, Processor}
import com.udsl.processor6502.{NumericFormatType, Utilities}
import com.udsl.processor6502.ui.popups.{Executor, LineAssemblerPopup}
import com.udsl.processor6502.cpu.MemoryCell.*
import com.udsl.processor6502.disassembler.Disassembler
import javafx.collections.FXCollections
import javafx.scene.input.ContextMenuEvent
import scalafx.event.{ActionEvent, EventHandler}
import scalafx.event.subscriptions.Subscription
import scalafx.geometry.{Insets, Orientation}
import scalafx.scene.control.{Button, ContextMenu, Label, ListView, MenuButton, MenuItem, TextField, Tooltip}
import scalafx.scene.input.{KeyEvent, MouseEvent}
import scalafx.scene.layout.{HBox, StackPane, VBox}
import com.udsl.processor6502.Utilities.{numToString, numericValue, stringToNum}
import scalafx.application.Platform

import scala.language.implicitConversions

class MemoryBox extends VBox, ScrollToView, StrictLogging:

  implicit def toShort(x: Int): Short = x.toShort

  var currentViewLocation: Int = 0

  viewLocationUpdateDisplay()

  NumericFormatSelector.numericFormatProperty.onChange {
    (_, _, newValue) =>
      changeDisplayMode(newValue)
      memoryView.refresh()
      viewLocationUpdateDisplay()
  }

  val viewLocation: TextField = new TextField {
    text = Processor.sp.toString
    disable = true
    prefColumnCount = 6
  }

  def viewLocationUpdateDisplay(): Unit =
    Platform.runLater(() -> {
      viewLocation.text = numToString(currentViewLocation)
    })

  val memoryBoxCaption: Label = new Label {
    text = "Memory View"
    style = "-fx-font: 12 arial;-fx-font-weight:bold"
    padding = Insets(0, 0, 0, 80)
  }

  val memoryView: ListView[MemoryCell] = new ListView[MemoryCell] {
    items = Memory.getMemory
    orientation = Orientation.Vertical
  }

  val lineAss = new MenuItem("Line assembler")
  lineAss.onAction =  _ => {
    val selected = memoryView.selectionModel.apply().getSelectedItems.get(0)
    logger.info(s"menu action - '$selected'")
    val lineAssembler = new LineAssemblerPopup(selected.getLocation)
    lineAssembler.showAndWait()
  }

  memoryView.contextMenu = new ContextMenu( lineAss )

  val st: StackPane = new StackPane {
    padding = Insets(10)
    children = memoryView
  }

  val memoryButtons: HBox = new HBox{
    spacing = 20

    val saveMemoryImage: MenuItem = new MenuItem("Save Image"){
      text = "Save Image"
      onAction = _ => {
        Memory.saveMemoryImage()
      }
    }

    val loadMemoryImage: MenuItem = new MenuItem("Load Image"){
      onAction = _ => {
        if confirmation("Loading image wil overwrite all current memory!") then
          Memory.loadMemoryImage()
      }
    }

    val memoryImageuButton: MenuButton = new MenuButton("Memory Image", null){
      items = List( saveMemoryImage, loadMemoryImage )
    }

    memoryImageuButton.setTooltip(new Tooltip(s"Save or load memory image."))

    children = List(memoryImageuButton)
  }


  val viewPCButtonBox: HBox = new HBox{
    spacing = 20

    val viewPcButton: Button = new Button {
      text = "View PC"
      onAction = _ => {
        logger.info("Viewing PC location!")
        memoryView.scrollTo(Processor.pc.addr)
      }
    }

    viewPcButton.setTooltip(new Tooltip(s"Scroll memory view to current PC."))
    children = List( viewPcButton)
  }

  val viewButtons: HBox = new HBox{
    spacing = 20

    val viewLocationButton: Button = new Button {
      text = "View Location"
      onAction = _ => {
        logger.info(s"Viewing location ${viewLocation.text.value}!")
        val loc: Int =
          numericValue(viewLocation.text.value).get
        memoryView.scrollTo(loc)
      }
    }

    val setViewLocationButton: Button = new Button {
      text = "Set"
      onAction = _ => {
        logger.info("Setting disassembly location")

        val dialog = getNumberSettingDialogue(s"Setting memory view location", currentViewLocation)

        val result: Option[String] = dialog.showAndWait()
        result match
          case Some(value) =>
            val validationResult = Utilities.verifyNumberEntry(value)
            if !validationResult._1 then
              errorAlert("Input Error", validationResult._2)
            else
              currentViewLocation = stringToNum(value)
              viewLocationUpdateDisplay()
          case None => logger.info("Dialog was canceled.")
      }
    }

    viewLocationButton.setTooltip(new Tooltip(s"Scroll memory view to given location."))
    setViewLocationButton.setTooltip(new Tooltip(s"Set location to scroll to."))
    children = List( viewLocationButton, viewLocation, setViewLocationButton)
  }


  padding = Insets(20)
  spacing = 8
  children = List(memoryBoxCaption, st, memoryButtons, viewPCButtonBox, viewButtons)

  def doScroll(scrollTo: Int): Unit =
    memoryView.scrollTo(scrollTo)
