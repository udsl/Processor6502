package com.udsl.processor6502.application

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Dialogues.theStage
import com.udsl.processor6502.config.AppOptions
import com.udsl.processor6502.ui.popups.Executor
import com.udsl.processor6502.ui.*
import javafx.beans.value.ChangeListener
import javafx.event.ActionEvent
import javafx.scene.image.Image
import scalafx.application.JFXApp3
import scalafx.beans.binding.BindingIncludes.closure2ChangedListener
import scalafx.beans.value.ObservableValue
import scalafx.event.EventIncludes.eventClosureWrapperWithZeroParam
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.layout.*
import scalafx.stage.FileChooser

import java.io.File


object Main extends JFXApp3 with StrictLogging {
  var memoryBox: Option[MemoryBox] = None

  def userLog(logMessage: String): Unit =
    if AppOptions.userLoggingEnabled then
      logger.info(logMessage)

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

    val icon: Image = new Image("chip-40.png")
    stage.getIcons.add(icon)

    theStage = stage

    val mainFocus = (o: javafx.beans.Observable, oldVal: java.lang.Boolean, newVal: java.lang.Boolean) => {
      def foo(o: javafx.beans.Observable, oldVal: java.lang.Boolean, newVal: java.lang.Boolean): Unit =
        logger.debug("Stage getting focus")

      foo(o, oldVal, newVal)
    }

    val x = stage.focusedProperty()
    x.addListener(mainFocus)
  }
}

