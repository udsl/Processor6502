package com.udsl.processor6502

import scalafx.beans.property.StringProperty
import scalafx.geometry.Insets
import scalafx.scene.control.{Label, RadioButton, ToggleGroup}
import scalafx.scene.layout.{HBox, VBox}

class NumericFormatSelector extends VBox {
  val numericalFormatGroup = new ToggleGroup()
  val numFormatText = new StringProperty()

  val checks: HBox =  new HBox {
    //Radio Button Toggle Group
    children = List(
      new RadioButton {
        minWidth = 100
        maxWidth = 200
        maxHeight = 50
        text = NumericFormatType.Decimal.toString
        id = NumericFormatType.Decimal.toString
        toggleGroup = numericalFormatGroup
        selected = true
      },
      new RadioButton {
        minWidth = 100
        maxWidth = 200
        maxHeight = 50
        text = NumericFormatType.HexDecimal.toString
        id = NumericFormatType.HexDecimal.toString
        toggleGroup = numericalFormatGroup
      },
      new RadioButton {
        minWidth = 100
        maxWidth = 200
        maxHeight = 50
        text = NumericFormatType.Octal.toString
        id = NumericFormatType.Octal.toString
        toggleGroup = numericalFormatGroup
      },
      new RadioButton {
        minWidth = 100
        maxWidth = 200
        maxHeight = 50
        text = NumericFormatType.Binary.toString
        id = NumericFormatType.Binary.toString
        toggleGroup = numericalFormatGroup
      }
    )
  }

  numericalFormatGroup.selectToggle(numericalFormatGroup.toggles(0))
  numericalFormatGroup.selectedToggle.onChange {
    val rb = numericalFormatGroup.selectedToggle.get.asInstanceOf[javafx.scene.control.ToggleButton]
    if (rb != null) numFormatText.value = rb.getId
  }

  padding = Insets(20)
  children = List(Label("Number formats"), checks)
}
