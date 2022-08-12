package com.udsl.processor6502.ui.popups

import com.udsl.processor6502.ui.popups.Executor
import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.{Main, Observer}
import com.udsl.processor6502.cpu.execution.{ExecutionUnit, Opcode, NULL_Instruction}
import com.udsl.processor6502.cpu.{Processor, StatusFlag}
import com.udsl.processor6502.ui.popups.Executor.{executionUnit, executor}
import scalafx.application.{JFXApp, Platform}
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, TextField}
import scalafx.scene.layout.{BorderPane, HBox, VBox}
import scalafx.stage.{Modality, Stage}
import scalafx.event.EventIncludes.eventClosureWrapperWithZeroParam
import scalafx.event.subscriptions.Subscription
import scalafx.geometry.Insets

class Executor extends Stage, Observer[ExecutionUnit], StrictLogging:
  title = "Executor / Debugger"
  width = 400
  height = 200
  resizable = false

  initOwner(new Stage)
  initModality(Modality.None)

  onCloseRequest = () => {
    Executor.close()
  }

  def getCurrentInsDecoded(): String =
    Executor.executionUnit.get.opcode match {
      case NULL_Instruction =>
        // not set so get from PC
        Executor.executionUnit.get.loadInstructionAtPc()
      case _ =>
    }
    Executor.executionUnit.get.decodeInstruction()

  val currentInsLabel: Label = new Label(getCurrentInsDecoded())

  val currentInsBox: HBox = new HBox{
    spacing = 20
    val currentLabel: Label = new Label("Current Instruction")
    children = List(currentLabel, currentInsLabel)
  }

  Processor.pc._addr.onChange {
    executionUnit match
      case Some(eu) =>
        currentInsLabel.text = eu.decodeInstruction()
      case None =>
        logger.info(s"executionUnit not set!")
  }

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

      new BorderPane {
        maxWidth = 400
        maxHeight = 200
        padding = Insets(20)
        top = currentInsBox
        right = pushButtons
      }
    }
  }

  override def receiveUpdate(subject: ExecutionUnit ): Unit =
    logger.info(s"Received notification of ExecutionUnit update")


object Executor:
  var executor: Option[Executor] = None
  private var executionUnit: Option[ExecutionUnit] = None

  def close(): Unit =
    executor = None
    executionUnit = None

  def gotoBack(): Unit =
    executor match
      case Some(_) =>
        executor.get.toBack()
      case _ =>

  def showExecutor(): Unit =
    executor match
      case Some(_) => {
        executor.get.toFront()
      }
      case _ => {
        // Need to get an ExecutionUnit before we create the screen that uses it.
        if executionUnit.isEmpty then
          executionUnit = Some(ExecutionUnit.apply)
        executor = Some(Executor())
        executor.get.show()
      }
