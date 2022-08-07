import com.udsl.processor6502.ui.popups.Executor
package com.udsl.processor6502.ui.popups:

  import com.typesafe.scalalogging.StrictLogging
  import com.udsl.processor6502.{Main, Observer}
  import com.udsl.processor6502.cpu.execution.ExecutionUnit
  import com.udsl.processor6502.cpu.{Processor, StatusFlag}
  import com.udsl.processor6502.ui.popups.Executor.executor
  import scalafx.application.{JFXApp, Platform}
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

    val currentInsBox: HBox = new HBox{
      spacing = 20

      val currentLabel: Label = new Label("Current Instruction")


      children = List(currentLabel, currentInsLabel)
    }

    Processor.pc._addr.onChange {
      (_, oldValue, newValue) => {
        logger.debug(s"PC subscription fired - $oldValue, $newValue")
        Executor.executor match
          case Some(_) =>
            Platform.runLater(() -> {
              currentInsLabel.text = Executor.executionUnit.get.decodeInstruction()
            })
          case _ =>
            // No executor
      }
    }

    override def receiveUpdate(subject: ExecutionUnit ): Unit =
      logger.info(s"Recived notification of ExecutionUnit update")

    scene = new Scene {
      root = {
        val pushButtons: HBox = new HBox {
          spacing = 20
          val stepButton: Button = new Button {
            text = "Step"
            onAction = _ => {
              logger.info("Executing STEP!")
              Executor.executionUnit.get.singleStep()
            }
          }

          val startButton: Button = new Button {
            text = "Run"
            onAction = _ => {
              logger.info("Executing START!")
              Executor.executionUnit.get.start()
            }
          }

          val runSlowButton: Button = new Button {
            text = "Run slow"
            onAction = _ => {
              logger.info("Executing START!")
              Executor.executionUnit.get.startSlow()
            }
          }

          children = List(startButton, runSlowButton, stepButton)
        }

        def togle(flag: StatusFlag): Unit = {
          logger.info(flag.toString)
          val state = Processor.sr.testFlag(flag)
          logger.info(s"  $state")
          if (state) Processor.sr.clearFlag(flag) else Processor.sr.setFlag(flag)
        }

        val flagTest: HBox = new HBox {
          val toggleButton: Button = new Button {
            text = "Togle"
            onAction = _ => {
              logger.info("Executing toggle!")
              val c = toToggle.text.value.toUpperCase().take(1)
              c match {
                case "N" => togle(StatusFlag.Negative)
                case "O" => togle(StatusFlag.Overflow)
                case "B" => togle(StatusFlag.Break)
                case "D" => togle(StatusFlag.Decimal)
                case "I" => togle(StatusFlag.Interrupt)
                case "Z" => togle(StatusFlag.Zero)
                case "C" => togle(StatusFlag.Carry)
                case _ => logger.info("WTF!")
              }
            }
          }

          val toToggle: TextField = new TextField {
          }
          children = List(toggleButton, toToggle)
        }

        new BorderPane {
          maxWidth = 400
          maxHeight = 200
          padding = Insets(20)
          top = currentInsBox
//          left = registersBox
          right = pushButtons
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