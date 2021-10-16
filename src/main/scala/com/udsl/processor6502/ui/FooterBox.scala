package com.udsl.processor6502.ui

import com.udsl.processor6502.Utilities.{currentFormat, getConfigValue, readConfigFile, writeConfigFile}
import com.udsl.processor6502.config.DataSupplier.provideData
import com.udsl.processor6502.config.{ConfigDatum, DataCollector}
import com.udsl.processor6502.ui.NumericFormatSelector.updateDisplay
import scalafx.geometry.Insets
import scalafx.print.PaperSource.Main
import scalafx.scene.control.Button
import scalafx.scene.layout.{GridPane, HBox}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class FooterBox extends GridPane{

  val saveButton: Button = new Button {
    text = "Save Config"
    onAction = _ => {
      println(s"Save Button pressed")

      var out: ListBuffer[ConfigDatum] = ListBuffer[ConfigDatum]()
      out ++= DataCollector.collectData()

      val formatStr = currentFormat.toString
      out += ConfigDatum.apply("format", formatStr)

      writeConfigFile(out.toList)
    }
  }
  GridPane.setConstraints(saveButton, 13, 0, 2, 1)

  val loadButton: Button = new Button {
    text = s"Load Config"
    onAction = _ => {
      println(s"Load Button pressed")
      val lines = readConfigFile

      updateDisplay(getConfigValue(lines, "format", "Dec"))

      provideData(lines)
    }
  }
  GridPane.setConstraints(loadButton, 33, 0, 2, 1)

    hgap = 4
    vgap = 16
    margin = Insets(18)
    children ++= Seq(saveButton, loadButton)
}
