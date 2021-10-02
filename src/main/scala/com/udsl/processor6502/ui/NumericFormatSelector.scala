package com.udsl.processor6502.ui:

  import com.udsl.processor6502.NumericFormatType.*
  import com.udsl.processor6502.{NumericFormatProperty, NumericFormatType}
  import scalafx.geometry.Insets
  import scalafx.scene.control.{Label, RadioButton, Toggle, ToggleGroup}
  import scalafx.scene.layout.{HBox, VBox}

  object NumericFormatSelector {
    val numericFormatProperty = new NumericFormatProperty()
    numericFormatProperty.value = NumericFormatType.DEC

    var instances = List[NumericFormatSelector]()

    def apply: NumericFormatSelector = {
      if (instances.isEmpty) {
        val v = new NumericFormatSelector
        v.numericFormatProperty = numericFormatProperty
        instances = instances.appended(v)
      }
      instances.last
    }

    def updateDisplay(n: String): Unit =
      if (instances.isEmpty) {
        throw new Exception("Instance not available")
      }
      instances.last.updateDisplay(n)
  }

  class NumericFormatSelector extends VBox {
    val numericalFormatGroup = new ToggleGroup()
    var numericFormatProperty: NumericFormatProperty = _

    val checks: HBox = new HBox {
      //Radio Button Toggle Group
      children = List(
        new RadioButton {
          minWidth = 100
          maxWidth = 200
          maxHeight = 50
          text = NumericFormatType.DEC.toString
          id = NumericFormatType.DEC.toString
          toggleGroup = numericalFormatGroup
          selected = true
        },
        new RadioButton {
          minWidth = 100
          maxWidth = 200
          maxHeight = 50
          text = NumericFormatType.HEX.toString
          id = NumericFormatType.HEX.toString
          toggleGroup = numericalFormatGroup
        },
        new RadioButton {
          minWidth = 100
          maxWidth = 200
          maxHeight = 50
          text = NumericFormatType.OCT.toString
          id = NumericFormatType.OCT.toString
          toggleGroup = numericalFormatGroup
        },
        new RadioButton {
          minWidth = 100
          maxWidth = 200
          maxHeight = 50
          text = NumericFormatType.BIN.toString
          id = NumericFormatType.BIN.toString
          toggleGroup = numericalFormatGroup
        }
      )
    }

    def update(n: String): Unit = {
      numericFormatProperty.value = NumericFormatType.valueOf(n)
    }

    def updateDisplay(n: String): Unit = {
      numericFormatProperty.value = NumericFormatType.valueOf(n)
      val tog = numericalFormatGroup.getToggles.filtered({ s => s.asInstanceOf[javafx.scene.control.ToggleButton].getId == n })
      if (tog.size() > 0) {
        numericalFormatGroup.selectToggle(tog.get(0))
      }
    }

    numericalFormatGroup.selectToggle(numericalFormatGroup.toggles(0))
    numericalFormatGroup.selectedToggle.onChange {
      val rb = numericalFormatGroup.selectedToggle.get.asInstanceOf[javafx.scene.control.ToggleButton]
      if (rb != null) update(rb.getId)
    }

    padding = Insets(20)
    children = List(new Label(text = "Number formats") {
      style = "-fx-font: 14 arial;-fx-font-weight:bold"
      padding = Insets(8, 8, 8, 0)
    }, checks)
  }