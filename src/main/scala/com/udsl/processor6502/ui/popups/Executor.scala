import com.udsl.processor6502.ui.popups.Executor
package com.udsl.processor6502.ui.popups:

  import com.udsl.processor6502.Main
  import com.udsl.processor6502.cpu.execution.ExecutionUnit
  import com.udsl.processor6502.cpu.{Processor, StatusRegisterFlags}
  import scalafx.application.JFXApp
  import scalafx.scene.Scene
  import scalafx.scene.control.{Button, Label, TextField}
  import scalafx.scene.layout.{BorderPane, HBox, VBox}
  import scalafx.stage.{Modality, Stage}
  import scalafx.event.EventIncludes.eventClosureWrapperWithZeroParam
  import scalafx.geometry.Insets

  class Executor extends Stage{
    title = "Executor / Debugger"
    width = 400
    height = 200
    resizable = false

    initOwner(new Stage)
    initModality(Modality.None)

    onCloseRequest = () => {
      Executor.close()
    }

    scene = new Scene {
      root = {
        val label: Label = new Label("Just a label")
  
        val pushButton: Button = new Button {
          text = "Step"
          onAction = _ => {
            println("Executing STEP!")
            Executor.executionUnit.singleStep()
          }
        }

        def togle(flag: StatusRegisterFlags) = {
          print(flag)
          val state = Processor.sr.testFlag(flag)
          println(s"  $state")
          if (state) Processor.sr.clearFlag(flag) else Processor.sr.setFlag(flag)
        }

        val flagTest = new HBox {
          val toggleButton: Button = new Button {
            text = "Togle"
            onAction = _ => {
              println("Executing toggle!")
              val c = toToggle.text.value.toString.toUpperCase().take(1)
              c match {
                case "N" => togle(StatusRegisterFlags.Negative)
                case "O" => togle(StatusRegisterFlags.Overflow)
                case "B" => togle(StatusRegisterFlags.Break)
                case "D" => togle(StatusRegisterFlags.Decimal)
                case "I" => togle(StatusRegisterFlags.Interrupt)
                case "Z" => togle(StatusRegisterFlags.Zero)
                case "C" => togle(StatusRegisterFlags.Carry)
                case _ => println("WTF!")
              }
            }
          }

          val toToggle = new TextField {
          }
          children = List(toggleButton, toToggle)
        }

        new BorderPane {
          maxWidth = 400
          maxHeight = 200
          padding = Insets(20)
          top = label
//          left = registersBox
          right = pushButton
          bottom = flagTest
        }
      }
    }
  }

  object Executor {
    var executor: Option[Executor] = None
    val executionUnit: ExecutionUnit = ExecutionUnit.apply
    
    def close(): Unit =
      executor = None

    def toBack(): Unit =
      executor match
        case Some(_) =>
          executor.get.toBack()
        case _ =>


    def showExecutor(): Unit =
      executor match
        case Some(_) =>
          executor.get.toFront()
        case _ =>
          executor = Some(Executor())
          executor.get.show()

  }