package com.udsl.processor6502.ui

import scalafx.scene.control.Button

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Utilities.getConfigValue
import com.udsl.processor6502.config.DataAgentRegistration.registerDataSource
import com.udsl.processor6502.config.{ConfigDatum, DataConsumer, DataProvider}
import com.udsl.processor6502.cpu.{Processor, StatusFlag}
import javafx.application.Platform
import scalafx.event.subscriptions.Subscription
import scalafx.geometry.Insets
import scalafx.scene.control.Label
import scalafx.scene.layout.{HBox, StackPane, VBox}

import scala.collection.mutable.ListBuffer

class StatusRegisterView extends VBox, StrictLogging:

  Processor.sr._ebr.onChange {
    (_, oldValue, newValue) => {
      logger.info(s"Status register subscription fired - $oldValue, $newValue")
      Platform.runLater(() => {
        updateDisplayedValues(newValue.intValue())
      })
    }
  }

  def updateDisplayedValues(newValue: Int): Unit =
    setFlag( StatusFlag.Negative, (StatusFlag.Negative.mask & newValue) > 0)
    setFlag( StatusFlag.Overflow, (StatusFlag.Overflow.mask & newValue) > 0)
    setFlag( StatusFlag.Break, (StatusFlag.Break.mask & newValue) > 0)
    setFlag( StatusFlag.Decimal, (StatusFlag.Decimal.mask & newValue) > 0)
    setFlag( StatusFlag.Interrupt, (StatusFlag.Interrupt.mask & newValue) > 0)
    setFlag( StatusFlag.Zero, (StatusFlag.Zero.mask & newValue) > 0)
    setFlag( StatusFlag.Carry, (StatusFlag.Carry.mask & newValue) > 0)


  val neg = new StatusFlagControl("Negative")
  val ovr = new StatusFlagControl("Overflow")
  val u = new StatusFlagControl("Unused", true, true)
  val brk = new StatusFlagControl("Break")
  val dec = new StatusFlagControl("Decimal")
  val inter = new StatusFlagControl("Interrupt")
  val zero = new StatusFlagControl("Zero")
  val carry = new StatusFlagControl("Carry")

  val display: StackPane = new StackPane:
    val titleBox: HBox = new HBox {
      val title: Label = new Label {
        text = "  Status Flags  "
        style = "-fx-font-weight:bold; -fx-content-display: top; -fx-background-color: white; -fx-translate-y: -12; -fx-translate-x: 8;"
      }
      prefWidth = title.prefWidth.value
      children = List(title)
    }

    val statusBits: VBox = new VBox {
      style = "-fx-content-display: top; -fx-border-insets: -2 -2 -2 -2; -fx-background-color: white; -fx-border-color: grey; -fx-border-width: 2;"
    }

    val theBits: VBox = new VBox {
      val bits: VBox = new VBox {
        padding = Insets(4, 4, 4, 4)
        children = List(neg, ovr, u, brk, dec, inter, zero, carry)
      }
      children = List(bits)
    }
    children = List(statusBits, titleBox, theBits)

  padding = Insets(18, 18, 18, 0)
  children = List(display)

  private def setFlag(flag: StatusFlag, value: Boolean): Unit =
    flag match{
      case StatusFlag.Negative => neg.update(value)
      case StatusFlag.Overflow => ovr.update(value)
      case StatusFlag.Break => brk.update(value)
      case StatusFlag.Decimal => dec.update(value)
      case StatusFlag.Interrupt => inter.update(value)
      case StatusFlag.Zero => zero.update(value)
      case StatusFlag.Carry => carry.update(value)
      case _ => ()
    }


class StatusFlagControl(val statusName: String, initalValue: Boolean = false, val isReadOnly: Boolean = false) extends HBox, DataProvider, DataConsumer, StrictLogging:

  registerDataSource( this)

  logger.info(s"Creating StatusFlag '$statusName'")
  var currentValue: Boolean = initalValue
  val label: Label = new Label(statusName){
    prefWidth = 70
  }
  val value: Label = new Label(if (initalValue) "1" else "0"){
    prefWidth = 70
  }

  val toggleButton: Button = new Button{
    text = "Togle"
    prefHeight = 8
    onAction = _ => {
      logger.info(s"$statusName - status: $currentValue")
      if !isReadOnly then
        currentValue = !currentValue
        Processor.sr.updateFlag(StatusFlag.valueOf(statusName), currentValue)
        value.text = if (currentValue) "1" else "0"
    }
  }
  children = List(label, value, toggleButton)

  def update( newValue: Boolean): Unit =
    if (!isReadOnly) {
      if (currentValue != newValue) {
        currentValue = newValue
        value.text = if (newValue) "1" else "0"
      }
    }

  override def getData(collector: ListBuffer[ConfigDatum]): Unit =
    logger.info("Collecting from RegisterBox")
    collector += ConfigDatum.apply(statusName, currentValue.toString)


  override def setData( provider: List[ConfigDatum]): Unit =
    logger.info("Providing to StatusFlag")
    update(
      getConfigValue(provider, statusName) match {
        case Some(b) => b.equals("true")
        case _ => false
      }
    )


