package com.udsl.processor6502.ui

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Dialogues.*
import com.udsl.processor6502.Utilities.{currentFormat, getConfigValue}
import com.udsl.processor6502.config.DataSupplier.provideData
import com.udsl.processor6502.config.{ConfigDatum, DataCollector}
import com.udsl.processor6502.ui.NumericFormatSelector.updateDisplay
import scalafx.event.EventIncludes.handle
import scalafx.geometry.Insets
import scalafx.print.PaperSource.Main
import scalafx.scene.control.{Button, MenuButton, MenuItem, Tooltip}
import scalafx.scene.layout.{GridPane, HBox}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class FooterBox extends GridPane, StrictLogging{

  val saveItem = new MenuItem("Save"){
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

  hgap = 4
  vgap = 16
  margin = Insets(18)
  children ++= Seq(menuButton)
}
