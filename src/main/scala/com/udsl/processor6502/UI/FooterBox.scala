package com.udsl.processor6502.UI

import com.udsl.processor6502.Utilities.{currentFormat, writeFile}
import com.udsl.processor6502.config.DataCollector
import scalafx.geometry.Insets
import scalafx.print.PaperSource.Main
import scalafx.scene.control.Button
import scalafx.scene.layout.{GridPane, HBox}

import scala.collection.mutable.ArrayBuffer

class FooterBox extends GridPane{

  val saveButton: Button = new Button {
    text = "Save"
    onAction = _ => {
      println(s"Save Button pressed")

      var out = DataCollector.collectData()

      val formatStr = currentFormat.toString
      out += s"format: $formatStr"

      out += "first line"
      out += "last line"
      writeFile("processor6502.save", out.toSeq)
    }
  }
  GridPane.setConstraints(saveButton, 13, 0, 2, 1)

  val loadButton: Button = new Button {
    text = "Load"
    onAction = _ => {
      println(s"Load Button pressed")
    }
  }
  GridPane.setConstraints(loadButton, 33, 0, 2, 1)

    hgap = 4
    vgap = 16
    margin = Insets(18)
    children ++= Seq(saveButton, loadButton)
}
