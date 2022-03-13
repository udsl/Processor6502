package com.udsl.processor6502.ui:
  
  import com.typesafe.scalalogging.StrictLogging
  import com.udsl.processor6502.cpu.{Memory, MemoryCell, Processor}
  import com.udsl.processor6502.NumericFormatType
  import com.udsl.processor6502.ui.popups.{Executor, LineAssemblerPopup}
  import com.udsl.processor6502.assembler.CodeEditor
  import com.udsl.processor6502.cpu.MemoryCell.*
  import com.udsl.processor6502.disassembler.Disassembler
  import javafx.collections.FXCollections
  import javafx.scene.input.ContextMenuEvent
  import scalafx.event.{ActionEvent, EventHandler}
  import scalafx.event.subscriptions.Subscription
  import scalafx.geometry.{Insets, Orientation}
  import scalafx.scene.control.{Button, ContextMenu, Label, ListView, MenuItem, TextField, Tooltip}
  import scalafx.scene.input.{KeyEvent, MouseEvent}
  import scalafx.scene.layout.{HBox, StackPane, VBox}
  import com.udsl.processor6502.Utilities.numericValue

  import scala.language.implicitConversions
  
  class MemoryBox extends VBox, StrictLogging {

    implicit def toShort(x: Int): Short = x.toShort

    val subscription: Subscription = NumericFormatSelector.numericFormatProperty.onChange {
      (_, oldValue, newValue) =>
        changeDisplayMode(newValue)
        memoryView.refresh()
    }
  
    val memoryBoxCaption: Label = new Label {
      text = "Memory View"
      style = "-fx-font: 12 arial;-fx-font-weight:bold"
      padding = Insets(0, 0, 0, 80)
    }
  
    val memoryView: ListView[MemoryCell] = new ListView[MemoryCell] {
      items = Memory.getMemory
      orientation = Orientation.Vertical
    }
  
    val lineAss = new MenuItem("Line assembler")
    lineAss.onAction =  _ => {
      val selected = memoryView.selectionModel.apply().getSelectedItems.get(0)
      logger.info(s"menu action - '${selected}'")
//      logger.info(selected.getLocation())
      val lineAssembler = new LineAssemblerPopup(selected.getLocation)
      lineAssembler.showAndWait()
    }
  
    memoryView.contextMenu = new ContextMenu( lineAss )
  
    val st = new StackPane {
      padding = Insets(10)
      children = memoryView
    }
  
    val viewButtons = new HBox{
      spacing = 20
  
      val viewPcButton: Button = new Button {
        text = "View PC"
        onAction = _ => {
          logger.info("Viewing PC location!")
          memoryView.scrollTo(Processor.pc.addr)
        }
      }
      viewPcButton.setTooltip(new Tooltip("Click to view PC location"))
  
      val viewLocationButton: Button = new Button {
        text = "View "
        onAction = _ => {
          logger.info(s"Viewing location ${toView.text}!")
          val loc: Int =
            numericValue(toView.text.value)
          memoryView.scrollTo(loc)
        }
      }
      viewPcButton.setTooltip(new Tooltip("Click to view this location"))
  
      val toView: TextField = new TextField {
        text = Processor.sp.toString
        prefColumnCount = 6
      }
      children = List( viewPcButton, viewLocationButton, toView)
    }
  
    val codeButtons = new HBox{
      spacing = 20
  
      val viewCodeEditorButton: Button = new Button {
        text = "Edit Code"
        onAction = _ => {
          logger.info("Editing code!")
          CodeEditor.showCodeEditor()
        }
      }
      viewCodeEditorButton.setTooltip(new Tooltip("Open code window"))
  
      val disassembleButton: Button = new Button {
        text = "Disassemble "
        disable = true
  
        onAction = _ => {
          val loc: Int = Integer.parseInt(disassembleLocation.text.value)
          logger.info(s"Disassembling location ${loc}!")
          memoryView.scrollTo(loc)
        }
      }
  
      disassembleButton.setTooltip(new Tooltip("Disassemble from location"))
  
      val disassembleLocation: TextField = new TextField {
        prefColumnCount = 6
  
         onKeyTyped = e => {
          disassembleButton.disable = text.value.isEmpty
        }
      }
  
      children = List( viewCodeEditorButton, disassembleButton, disassembleLocation)
    }

    val memoryButtons = new HBox{
      spacing = 20

      val saveMemoryImageButton: Button = new Button {
        text = "Save Image"
        onAction = _ => {
          Memory.saveMemoryImage()
        }
      }
      saveMemoryImageButton.setTooltip(new Tooltip("Save a memory image"))

      children = List(saveMemoryImageButton)
    }

    padding = Insets(20)
    spacing = 8
    children = List(memoryBoxCaption, st, memoryButtons, viewButtons, codeButtons)
  }
  
