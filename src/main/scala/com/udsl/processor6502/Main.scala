package com.udsl.processor6502

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Dialogues.theStage
import com.udsl.processor6502.ui.popups.Executor
import com.udsl.processor6502.ui.{CodeEditor, FooterBox, MemoryBox, NumericFormatSelector, RegistersBox}
import javafx.beans.value.ChangeListener
import javafx.event.ActionEvent
import scalafx.application.JFXApp3
import scalafx.beans.value.ObservableValue
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.layout.*
import scalafx.stage.FileChooser
import scalafx.event.EventIncludes.eventClosureWrapperWithZeroParam
import scalafx.beans.binding.BindingIncludes.closure2ChangedListener
import javafx.beans.value.ChangeListener
import java.io.File


object Main extends JFXApp3 with StrictLogging {
  var memoryBox: Option[MemoryBox] = None
  
  def start(): Unit = {
    val registersBox = new RegistersBox()
    memoryBox = Some(new MemoryBox())
    val footer = new FooterBox()

    stage = new JFXApp3.PrimaryStage {
      title.value = "6502 Processor"
      width = 700
      height = 800
      resizable = false

      scene = new Scene {
        root =
//          val memoryBox = new MemoryBox()
//          val footer = new FooterBox()

          footer.registerScrolToViewEventHandler(memoryBox.get)

          new BorderPane {
            maxWidth = 400
            maxHeight = 300
            padding = Insets(20)
            top = NumericFormatSelector.apply
            left = registersBox
            right = memoryBox.get
            bottom = footer
          }
      }
    }

    theStage = stage

    val mainFocus = (o: javafx.beans.Observable, oldVal: java.lang.Boolean, newVal: java.lang.Boolean) => {
      def foo(o: javafx.beans.Observable, oldVal: java.lang.Boolean, newVal: java.lang.Boolean) =
        logger.info(newVal.toString)

      foo(o, oldVal, newVal)
    }

    val x = stage.focusedProperty()
    x.addListener(mainFocus)
  }
}

