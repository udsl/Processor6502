package com.udsl.processor6502.ui:

  import com.typesafe.scalalogging.StrictLogging
  import com.udsl.processor6502.Utilities.*
  import com.udsl.processor6502.config.DataAgentRegistration.registerDataSource
  import com.udsl.processor6502.config.{ConfigDatum, DataConsumer, DataProvider}
  import com.udsl.processor6502.cpu.{Memory, Processor, VectorChangeListener}
  import com.udsl.processor6502.cpu.Processor.*
  import scalafx.application.Platform
  import scalafx.event.subscriptions.Subscription
  import scalafx.scene.layout.{HBox, StackPane, VBox}
  import com.udsl.processor6502.Dialogues.getNumberSettingDialogue
  import com.udsl.processor6502.cpu.Memory.{INTERRUPT_VECTOR, NMI_VECTOR, RESET_VECTOR}

  import scala.collection.IterableOnce.iterableOnceExtensionMethods
  import scala.collection.mutable.ListBuffer
  import scalafx.scene.control.{Button, Label, TextField, Tooltip}
  import scalafx.geometry.Insets

  class Vectors extends VBox, VectorChangeListener, StrictLogging {

    Memory.addVectorChangeListener(this)

    val nmi = new Vector("NMI", NMI_VECTOR)
    val reset = new Vector("RESET", RESET_VECTOR)
    val irq = new Vector("IRQ/BRK", INTERRUPT_VECTOR)

    def vectorChange(change: (String, Int)): Unit =
      change match
        case ("NMI", v) => nmi.updateDisplay(v)
        case ("RST", v) => reset.updateDisplay(v)
        case ("IRQ", v) => irq.updateDisplay(v)
        case _ => throw new Exception(s"Unexpected vector change event - $change")


    val display: StackPane = new StackPane {
      val titleBox: HBox = new HBox {
        val title: Label = new Label {

          text = "  Vectors  "
          style = "-fx-font-weight:bold; -fx-content-display: top; -fx-background-color: white; -fx-translate-y: -12; -fx-translate-x: 8;"
        }
        prefWidth = title.prefWidth.value
        children = List(title)
      }

      val theVectors: VBox = new VBox {
        style = "-fx-content-display: top; -fx-border-insets: -2 -2 -2 -2; -fx-background-color: white; -fx-border-color: grey; -fx-border-width: 2;"
      }

      val v: VBox = new VBox {
        padding = Insets(4, 4, 4, 4)
        children = List(nmi, reset, irq)
      }

      children = List(theVectors, titleBox, v)
    }
    
    padding = Insets(18, 18, 18, 0)
    children = List(display)
  }

  class Vector(val vectorName: String, val vectorAddress: Int, initialValue: Int = 0) extends HBox, DataProvider, DataConsumer, StrictLogging {
    override def agentFor: String = "Vectors"
    registerDataSource(this)

    logger.info(s"Creating vector '$vectorName' location '$vectorAddress")
    //    val changeHandler: (Int) => Unit = onChange
    val tooltip = new Tooltip(s"Location $vectorAddress (${vectorAddress.toHexString.toUpperCase})")
    val label: Label = new Label(vectorName) {
      prefWidth = 70
    }

    val value: TextField = new TextField {
      text = initialValue.toString
      disable = true
      prefWidth = 120
    }

    label.setTooltip(tooltip)

    val setButton: Button = new Button {
      text = "set"
      onAction = _ => {
        logger.info("Setting PC!")
        val dialog = getNumberSettingDialogue(s"New $vectorName Vector", currentValue)

        val result = dialog.showAndWait()
        result match
          case Some(value) =>
            updateDisplay(numericValue(value).get)
            memoryAccess.setMemoryToAddress(vectorAddress, numericValue(value).get)
          case None => logger.info("Dialog was canceled.")
      }
    }

    NumericFormatSelector.numericFormatProperty.onChange {
      (_, _, newValue) =>
        logger.info(s"Num format subscription fired: $newValue")
        value.text = numToString(currentValue)
    }

    var currentValue: Int = initialValue

    setButton.setTooltip(tooltip)

    children = List(label, value, setButton)

    def updateDisplay(change: Int): Unit =
      currentValue = change
      value.text = numToString(currentValue)

    override def supplyData(collector: ListBuffer[ConfigDatum]): Unit =
      logger.info(s"Collecting for vector $vectorName $currentValue")
      collector += ConfigDatum.apply(vectorName, currentValue.toString)

    override def receiveData(provider: List[ConfigDatum]): Unit =
      logger.info(s"Providing to Vector: $vectorName")
      getConfigValue(provider, vectorName) match
        case Some(value) => currentValue = numericValue(value).get
        case _ =>

  } 
