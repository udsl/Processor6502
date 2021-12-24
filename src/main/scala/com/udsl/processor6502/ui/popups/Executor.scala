import com.udsl.processor6502.ui.popups.Executor
package com.udsl.processor6502.ui.popups:

  import com.typesafe.scalalogging.StrictLogging
  import com.udsl.processor6502.{Main, Observer}
  import com.udsl.processor6502.cpu.execution.ExecutionUnit
  import com.udsl.processor6502.cpu.{Processor, StatusRegisterFlags}
  import scalafx.application.JFXApp
  import scalafx.scene.Scene
  import scalafx.scene.control.{Button, Label, TextField}
  import scalafx.scene.layout.{BorderPane, HBox, VBox}
  import scalafx.stage.{Modality, Stage}
  import scalafx.event.EventIncludes.eventClosureWrapperWithZeroParam
  import scalafx.event.subscriptions.Subscription
  import scalafx.geometry.Insets

  class Executor extends Stage, Observer[ExecutionUnit], StrictLogging{
    title = "Executor / Debugger"
    width = 400
    height = 200
    resizable = false

    initOwner(new Stage)
    initModality(Modality.None)

    onCloseRequest = () => {
      Executor.close()
    }

    val currentInsLabel: Label = new Label(Executor.executionUnit.get.decodeInstruction())

    val currentInsBox = new HBox{
      spacing = 20

      val currentLabel: Label = new Label("Current Instruction")


      children = List(currentLabel, currentInsLabel)
    }

    val pcSubscription: Subscription = Processor.pc._addr.onChange {
      (_, oldValue, newValue) => {
        logger.info(s"PC subscription fired - ${oldValue}, ${newValue}")
        currentInsLabel.text = Executor.executionUnit.get.decodeInstruction()
      }
    }

    override def receiveUpdate(subject: ExecutionUnit ) =
      logger.info(s"Recived notification of ExecutionUnit update")

    scene = new Scene {
      root = {
        val pushButton: Button = new Button {
          text = "Step"
          onAction = _ => {
            logger.info("Executing STEP!")
            Executor.executionUnit.get.singleStep()
          }
        }

        def togle(flag: StatusRegisterFlags) = {
          logger.info(flag.toString)
          val state = Processor.sr.testFlag(flag)
          logger.info(s"  $state")
          if (state) Processor.sr.clearFlag(flag) else Processor.sr.setFlag(flag)
        }

        val flagTest = new HBox {
          val toggleButton: Button = new Button {
            text = "Togle"
            onAction = _ => {
              logger.info("Executing toggle!")
              val c = toToggle.text.value.toString.toUpperCase().take(1)
              c match {
                case "N" => togle(StatusRegisterFlags.Negative)
                case "O" => togle(StatusRegisterFlags.Overflow)
                case "B" => togle(StatusRegisterFlags.Break)
                case "D" => togle(StatusRegisterFlags.Decimal)
                case "I" => togle(StatusRegisterFlags.Interrupt)
                case "Z" => togle(StatusRegisterFlags.Zero)
                case "C" => togle(StatusRegisterFlags.Carry)
                case _ => logger.info("WTF!")
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
          top = currentInsBox
//          left = registersBox
          right = pushButton
          bottom = flagTest
        }
      }
    }
  }

  object Executor {
    var executor: Option[Executor] = None
    private var executionUnit: Option[ExecutionUnit] = None
    
    def close(): Unit =
      executor = None
      executionUnit = None

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
          // Need to get an ExecutionUnit before we create the screen that uses it.
          if executionUnit.isEmpty then
            executionUnit = Some(ExecutionUnit.apply)
          executor = Some(Executor())
          executor.get.show()

  }