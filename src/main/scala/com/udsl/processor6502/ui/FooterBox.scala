package com.udsl.processor6502.ui

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Dialogues.{errorAlert, getNumberSettingDialogue}
import com.udsl.processor6502.FileIOUtilities.{readConfigFile, selectSourceFileToLoad, writeConfigFile}
import com.udsl.processor6502.Utilities.{currentFormat, getConfigValue, numToString, numericValue, stringToNum, verifyNumberEntry}
import com.udsl.processor6502.assembler.Assembler
import com.udsl.processor6502.config.DataSupplier.provideData
import com.udsl.processor6502.config.{ConfigDatum, DataCollector, DataSupplier}
import com.udsl.processor6502.disassembler.Disassembler
import com.udsl.processor6502.ui.NumericFormatSelector.updateDisplay
import com.udsl.processor6502.ui.popups.OptionEditor
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.print.PaperSource.Main
import scalafx.scene.control.{Button, Label, MenuButton, MenuItem, TextField, Tooltip}
import scalafx.scene.layout.{GridPane, HBox}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

trait ScrollToView:
  def doScroll(scrollTo: Int): Unit

class FooterBox() extends GridPane, StrictLogging:

  disassemblyLocationUpdateDisplay()


  NumericFormatSelector.numericFormatProperty.onChange {
    (_, _, _) =>
      disassemblyLocationUpdateDisplay()
  }

  val saveItem: MenuItem = new MenuItem("Save Config"){
    onAction = _ => {
      logger.info(s"Save Button pressed")
      DataCollector.writeConfigData()
    }
  }

  val loadItem: MenuItem = new MenuItem("Load Config"){
    onAction = _ => {
      logger.info(s"Load Button pressed")
      DataSupplier.provideData()
    }
  }

  val optionItem: MenuItem = new MenuItem("Edit Options") {
    onAction = _ => {
      logger.info(s"Options Button pressed")
      OptionEditor.showOptionEditor()
    }
  }

  val menuButton: MenuButton = new MenuButton("Config Action", null){
    items = List( optionItem, saveItem, loadItem )
  }

  menuButton.setTooltip(new Tooltip(s"Save or load a configuration."))
  GridPane.setConstraints(menuButton, 10, 0, 5, 1)


  val openEditor: MenuItem = new MenuItem("Open Editor"){
    onAction = _ => {
      logger.info("Editing code!")
      CodeEditor.showCodeEditor()
    }
  }


  val assembleFile: MenuItem = new MenuItem("Assemble from file"){
    onAction = _ => {
      selectSourceFileToLoad match
        case Some(file) =>
          logger.info(s"Assembling file $file")
          val sfa: Assembler = Assembler.apply(file)
          sfa.startAssembly()
        case _ =>
    }
  }

  val codeActionButton: MenuButton = new MenuButton("Code Action", null){
    items = List( openEditor, assembleFile )
  }

  codeActionButton.setTooltip(new Tooltip(s"Open code editor or assemble from file."))
  GridPane.setConstraints(codeActionButton, 30, 0, 5, 1)


  val disassembleButton: Button = new Button {
    text = "Disassemble from"
    onAction = _ => {
      if disassembleLocation.text.value == "" then
        errorAlert("Input Error", "Disassemble location not set")
      else {
        val loc: Int = Integer.parseInt(disassembleLocation.text.value)
        logger.info(s"Disassembling location $loc!")
        Disassembler.disassemble()

        for stv <- scrolToViewHanlers do
          stv.doScroll(loc)

        com.udsl.processor6502.Main.memoryBox.get.memoryView.delegate.refresh()
      }
    }
  }

  GridPane.setConstraints(disassembleButton, 50, 0, 3, 1)

  val disassembleLocation: TextField = new TextField {
    prefColumnCount = 6
    disable = true
  }
  GridPane.setConstraints(disassembleLocation, 53, 0, 2, 1)

  val setDisassembleLocationButton: Button = new Button {
    text = "Set"
    onAction = _ => {
      logger.info("Setting disassembly location")

      val dialog = getNumberSettingDialogue(s"Setting disassembly location", Disassembler.disassemblyLocation)

      val result: Option[String] = dialog.showAndWait()
      result match
        case Some(value) =>
          val validationResult = verifyNumberEntry(value)
          if !validationResult._1 then
            errorAlert("Input Error", validationResult._2)
          else
            Disassembler.disassemblyLocation = stringToNum(value)
            disassemblyLocationUpdateDisplay()
        case None => logger.info("Dialog was canceled.")
    }
  }
  GridPane.setConstraints(setDisassembleLocationButton, 57, 0, 2, 1)

  setDisassembleLocationButton.setTooltip(new Tooltip(s"Set the location to start disassembly, continues till first ZERO byte or end of memory."))

  hgap = 4
  vgap = 16
  margin = Insets(8)

  children ++= Seq(menuButton, codeActionButton, disassembleButton, disassembleLocation, setDisassembleLocationButton)

  private var scrolToViewHanlers = Set[ScrollToView]()

  def registerScrolToViewEventHandler( handler: ScrollToView): Unit =
    scrolToViewHanlers = scrolToViewHanlers + handler

  private def disassemblyLocationUpdateDisplay(): Unit =
    Platform.runLater(() -> {
      disassembleLocation.text = numToString(Disassembler.disassemblyLocation)
    })

