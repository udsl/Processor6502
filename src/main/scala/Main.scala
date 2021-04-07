package com.udsl.processor6502

import javafx.event.{ActionEvent, EventHandler}
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Orientation, Pos}
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, _}


object Main extends JFXApp {

  stage = new JFXApp.PrimaryStage {
    title = "6502 Processor"
    width = 600
    height = 800
    resizable = false

    val pc: TextField = new TextField {
      maxWidth = 200
      text = "128"
    }

    def stringToNum(oldValue: String, text: String) : Int = {
      if (oldValue == "Hex") Integer.parseInt(text, 16)
      else if (oldValue == "Oct") Integer.parseInt(text, 8)
      else if (oldValue == "Bin") Integer.parseInt(text, 2 )
      else Integer.parseInt(text, 10) // default format is decimal
    } : Int

    scene = new Scene {
      root = {
        // grid1 places the children by specifying the rows and columns in GridPane.setConstraints()
        val registersBox: VBox = new VBox {
          val registersCaption: Label = new Label {
            text = "Processor Registers"
            padding = Insets(0,0,0,80)
          }

          val label1: Label = new Label("PC:") {
            style = "-fx-font-weight:bold"
            alignmentInParent = Pos.BottomLeft
          }
          GridPane.setConstraints(label1, 0, 0, 1, 1)

          val pc: TextField = new TextField {
//            promptText = "Hi! I am Scalafx TextField"
            maxWidth = 200

            override def onAction_=(v: EventHandler[ActionEvent]): Unit = super.onAction_=(v)
          }

          GridPane.setConstraints(pc, 2, 0, 1, 1)

          val registersGrid = new GridPane {
            hgap = 4
            vgap = 6
            margin = Insets(18)
            children ++= Seq(label1, pc)
          }

          children ++= Seq(registersCaption, registersGrid)
        }

        val memoryBox: VBox = new VBox {
           val memeoryBoxCaption = new Label {
            text = "Memory View"
             padding = Insets(0,0,0,80)
          }

          val seq = Seq("Row 1", "Row 2", "Long Row 3", "Row 4", "Row 5", "Row 6", "Row 7",
            "Row 8", "Row 9", "Row 10", "Row 11", "Row 12", "Row 13", "Row 14", "Row 15",
            "Row 16", "Row 17", "Row 18", "Row 19", "Row 20")

          val memoryView = new ListView[String] {
            items = ObservableBuffer(seq)
            orientation = Orientation.Vertical
          }

          val st = new StackPane {
            padding = Insets(10)
            children = memoryView
          }

          children = List(memeoryBoxCaption, st)
        }

        val numericFormat = new NumericFormatSelector()
        val numFormatLabel = new Label {
          text <== numericFormat.numFormatText
        }

        val subscription = numericFormat.numFormatText.onChange {
          (_, oldValue, newValue) =>
            numFormatLabel.text = newValue
            val n = stringToNum( oldValue, pc.text.value )
            if (newValue == "Hex") pc.text = n.toHexString
            if (newValue == "Dec") pc.text = n.toString
            if (newValue == "Oct") pc.text = n.toOctalString
            if (newValue == "Bin") pc.text = n.toBinaryString
        }


        new BorderPane {
          maxWidth = 400
          maxHeight = 300
          padding = Insets(20)
          top = numericFormat
          left = registersBox
          center = numFormatLabel
          right = memoryBox
        }
      }
    }
  }
}