package com.udsl.processor6502.UI

import com.udsl.processor6502.{NumericFormatProperty, NumericFormatType}
import scalafx.geometry.Insets
import scalafx.scene.control.{Label, RadioButton, ToggleGroup}
import scalafx.scene.layout.{HBox, VBox}

object NumericFormatSelector {
    val numericFormatProperty = new NumericFormatProperty()
    numericFormatProperty.value = NumericFormatType.Decimal

    def apply : NumericFormatSelector = {
        val v = new NumericFormatSelector
        v.numericFormatProperty = numericFormatProperty
        v
    }

//    def addressToString(value: Address): String = {
//        val theAddress = value.addr
//        (numericFormatProperty.value) match {
//            case NumericFormatType.HexDecimal => theAddress.toHexString.toUpperCase
//            case NumericFormatType.Octal => theAddress.toOctalString
//            case NumericFormatType.Binary => theAddress.toBinaryString
//            case NumericFormatType.Decimal => theAddress.toString
//        }
//    }
//

//    def numToString(num: Int): String = {
//        val value = num.abs
//        (numericFormatProperty.value) match {
//            case NumericFormatType.HexDecimal => value.toHexString.toUpperCase
//            case NumericFormatType.Octal => value.toOctalString
//            case NumericFormatType.Binary => value.toBinaryString
//            case NumericFormatType.Decimal => value.toString
//        }
//    }

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

    def update(n: String): Unit = {
        println("for test")
        numericFormatProperty.value = NumericFormatType.withName(n)
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