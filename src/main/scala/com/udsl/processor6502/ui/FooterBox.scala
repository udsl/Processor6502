package com.udsl.processor6502.ui

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Dialogues.*
import com.udsl.processor6502.Utilities.{currentFormat, getConfigValue}
import com.udsl.processor6502.assembler.Assembler
import com.udsl.processor6502.config.DataSupplier.provideData
import com.udsl.processor6502.config.{ConfigDatum, DataCollector}
import com.udsl.processor6502.ui.NumericFormatSelector.updateDisplay
import scalafx.geometry.{Insets, Pos}
import scalafx.print.PaperSource.Main
import scalafx.scene.control.{Button, Label, MenuButton, MenuItem, TextField, Tooltip}
import scalafx.scene.layout.{GridPane, HBox}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class FooterBox(val memoryBox: MemoryBox) extends GridPane, StrictLogging{

  val saveItem: MenuItem = new MenuItem("Save"){
    onAction = _ => {
      logger.info(s"Save Button pressed")

      var out: ListBuffer[ConfigDatum] = ListBuffer[ConfigDatum]()
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
  GridPane.setConstraints(menuButton, 23, 0, 2, 1)


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
        memoryBox.memoryView.scrollTo(loc)
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
  GridPane.setConstraints(codeActionButton, 45, 0, 2, 1)

  val  disassembleLable: Label = new Label("Disassemble from:") {
    alignmentInParent = Pos.BaselineLeft
    padding = Insets(10, 0, 0, 0)
  }
  disassembleLable.setPrefWidth(110)

  GridPane.setConstraints(disassembleLable, 50, 0, 2, 1)

  val disassembleLocation: TextField = new TextField {
    prefColumnCount = 6
  }
  GridPane.setConstraints(disassembleLocation, 53, 0, 2, 1)

  hgap = 4
  vgap = 16
  margin = Insets(8)

  children ++= Seq(menuButton, codeActionButton, disassembleLable, disassembleLocation)
}
