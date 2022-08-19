package com.udsl.processor6502.ui

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Dialogues.*
import com.udsl.processor6502.Utilities
import com.udsl.processor6502.Utilities.{currentFormat, getConfigValue, numToString, numericValue, stringToNum}
import com.udsl.processor6502.assembler.Assembler
import com.udsl.processor6502.config.DataSupplier.provideData
import com.udsl.processor6502.config.{ConfigDatum, DataCollector}
import com.udsl.processor6502.ui.NumericFormatSelector.updateDisplay
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.print.PaperSource.Main
import scalafx.scene.control.{Button, Label, MenuButton, MenuItem, TextField, Tooltip}
import scalafx.scene.layout.{GridPane, HBox}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

trait ScrollToView:
  def doScroll(scrollTo: Int): Unit

class FooterBox() extends GridPane, StrictLogging:

  var currentDisassemblyLocation = 0

  disassemblyLocationUpdateDisplay()


  NumericFormatSelector.numericFormatProperty.onChange {
    (_, oldValue, newValue) =>
      disassemblyLocationUpdateDisplay()
  }

  val saveItem: MenuItem = new MenuItem("Save"){
    onAction = _ => {
      logger.info(s"Save Button pressed")

      val out: ListBuffer[ConfigDatum] = ListBuffer[ConfigDatum]()
      out ++= DataCollector.collectData()

      val formatStr = currentFormat.toString
      out += ConfigDatum.apply("format", formatStr)

      writeConfigFile(out.toList)
    }
  }

  val loadItem: MenuItem = new MenuItem("Load"){
    onAction = _ => {
      logger.info(s"Load Button pressed")
      val lines = readConfigFile
      if lines.nonEmpty then
        updateDisplay(
          getConfigValue(lines, "format") match
            case Some(value) => value
            case _ => "DEC"
        )
        provideData(lines)
    }
  }

  val menuButton: MenuButton = new MenuButton("Config Action", null){
    items = List( saveItem, loadItem )
  }

  menuButton.setTooltip(new Tooltip(s"Save or load configuration."))
  GridPane.setConstraints(menuButton, 20, 0, 2, 1)


  val openEditor: MenuItem = new MenuItem("Open Editor"){
    onAction = _ => {
      logger.info("Editing code!")
      CodeEditor.showCodeEditor()
    }
  }

  val disassemble: MenuItem = new MenuItem("Disassemble from location"){
    onAction = _ => {
      if disassembleLocation.text.value == "" then
        errorAlert("Input Error", "Disassemble location not set")
      else {

        val loc: Int = Integer.parseInt(disassembleLocation.text.value)
        logger.info(s"Disassembling location ${loc}!")
        for stv <- scrolToViewHanlers do
          stv.doScroll(loc)
      }
    }
  }

  val assembleFile: MenuItem = new MenuItem("Assemble from file"){
    onAction = _ => {
      val sourceFile = selectSourceFileToLoad
      if sourceFile != null then
        logger.info(s"Assembling file $sourceFile")
        val sfa: Assembler = Assembler.apply(sourceFile)
        sfa.startAssembly()
    }
  }

  val codeActionButton: MenuButton = new MenuButton("Code Action", null){
    items = List( openEditor, assembleFile, disassemble )
  }

  codeActionButton.setTooltip(new Tooltip(s"Open code editor, assemble from file or disassemble code."))
  GridPane.setConstraints(codeActionButton, 40, 0, 2, 1)

  val disassembleLable: Label = new Label("Disassemble from:") {
    alignmentInParent = Pos.BaselineLeft
    padding = Insets(10, 0, 0, 0)
  }
  disassembleLable.setPrefWidth(110)

  GridPane.setConstraints(disassembleLable, 50, 0, 2, 1)

  val disassembleLocation: TextField = new TextField {
    prefColumnCount = 6
    disable = true
  }
  GridPane.setConstraints(disassembleLocation, 53, 0, 2, 1)

  val setDisassembleLocationButton: Button = new Button {
    text = "Set"
    onAction = _ => {
      logger.info("Setting disassembly location")

      val dialog = getNumberSettingDialogue(s"Setting disassembly location", currentDisassemblyLocation)

      val result: Option[String] = dialog.showAndWait()
      result match
        case Some(value) =>
          val validationResult = Utilities.verifyNumberEntry(value)
          if !validationResult._1 then
            errorAlert("Input Error", validationResult._2)
          else
            currentDisassemblyLocation = stringToNum(value)
            disassemblyLocationUpdateDisplay()
        case None => logger.info("Dialog was canceled.")
    }
  }
  GridPane.setConstraints(setDisassembleLocationButton, 56, 0, 2, 1)


  hgap = 4
  vgap = 16
  margin = Insets(8)

  children ++= Seq(menuButton, codeActionButton, disassembleLable, disassembleLocation, setDisassembleLocationButton)

  private var scrolToViewHanlers = Set[ScrollToView]()

  def registerScrolToViewEventHandler( handler: ScrollToView): Unit =
    scrolToViewHanlers = scrolToViewHanlers + handler

  private def disassemblyLocationUpdateDisplay(): Unit =
    Platform.runLater(() -> {
      disassembleLocation.text = numToString(currentDisassemblyLocation)
    })

