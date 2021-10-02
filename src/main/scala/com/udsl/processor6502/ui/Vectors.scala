package com.udsl.processor6502.ui:

  import com.udsl.processor6502.Utilities.*
  import com.udsl.processor6502.config.DataAgentRegistration.registerDataSource
  import com.udsl.processor6502.config.{ConfigDatum, DataConsumer, DataProvider}
  import com.udsl.processor6502.cpu.Processor
  import com.udsl.processor6502.cpu.Processor.*
  import scalafx.application.Platform
  import scalafx.event.subscriptions.Subscription
  import scalafx.geometry.Insets
  import scalafx.scene.control.*
  import scalafx.scene.layout.{HBox, StackPane, VBox}

  import scala.collection.IterableOnce.iterableOnceExtensionMethods
  import scala.collection.mutable.ListBuffer

  class Vectors extends VBox {
    val display = new StackPane {
      val titleBox = new HBox {
        val title: Label = new Label {
          text = "  Vectors  "
          style = "-fx-content-display: top; -fx-background-color: white; -fx-translate-y: -12; -fx-translate-x: 8;"
        }
        prefWidth = title.prefWidth.value
        children = List(title)
      }

      val theVectors = new VBox {
        style = "-fx-content-display: top; -fx-border-insets: -2 -2 -2 -2; -fx-background-color: white; -fx-border-color: grey; -fx-border-width: 2;"
      }

      val nmi = new Vector("NMI", 0xFFFA, nmiChanges)
      val reset = new Vector("RESET", 0xFFFC, resetChanges)
      val irq = new Vector("IRQ/BRK", 0xFFFE, irqChanges)

      val v = new VBox {
        padding = Insets(4, 4, 4, 4)
        children = List(nmi, reset, irq)
      }

      children = List(theVectors, titleBox, v)

      val rstSubscription: Subscription = Processor.resetVector._addr.onChange {
        (_, oldValue, newValue) => {
          println(s"rstSubscription fired: ${Processor.resetVector.toString} ${oldValue} ${newValue}")
          Platform.runLater(() -> {
            reset.value.setText(Processor.resetVector.toString)
          })
        }
      }

      val irqSubscription: Subscription = Processor.irqVector._addr.onChange {
        (_, oldValue, newValue) => {
          println(s"rstSubscription fired: ${Processor.irqVector.toString} ${oldValue} ${newValue}")
          Platform.runLater(() -> {
            irq.value.setText(Processor.irqVector.toString)
          })
        }
      }

      val nmiSubscription: Subscription = Processor.nmiVector._addr.onChange {
        (_, oldValue, newValue) => {
          println(s"rstSubscription fired: ${Processor.nmiVector.toString} ${oldValue} ${newValue}")
          Platform.runLater(() -> {
            nmi.value.setText(Processor.nmiVector.toString)
          })
        }
      }
    }

    def nmiChanges(newVectorDest: Int): Unit = {
      val lo: Int = newVectorDest % 256
      val hi: Int = (newVectorDest / 256) % 256
      println(s"NMI updated to $lo, $hi")
      Processor.nmiVector.addr = newVectorDest
      Processor.setMemoryByte(NMI_VECTOR_LO_ADDRESS_BYTE, lo)
      Processor.setMemoryByte(NMI_VECTOR_HI_ADDRESS_BYTE, hi)
    }

    def irqChanges(newVectorDest: Int): Unit = {
      val lo: Int = newVectorDest % 256
      val hi: Int = (newVectorDest / 256) % 256
      println(s"IRQ updated to $lo, $hi")
      Processor.irqVector.addr = newVectorDest
      Processor.setMemoryByte(IRQ_VECTOR_LO_ADDRESS_BYTE, lo)
      Processor.setMemoryByte(IRQ_VECTOR_HI_ADDRESS_BYTE, hi)
    }

    def resetChanges(newVectorDest: Int): Unit = {
      val lo: Int = newVectorDest % 256
      val hi: Int = (newVectorDest / 256) % 256
      println(s"RESET updated to $lo, $hi")
      Processor.resetVector.addr = newVectorDest
      Processor.setMemoryByte(RESET_VECTOR_LO_ADDRESS_BYTE, lo)
      Processor.setMemoryByte(RESET_VECTOR_HI_ADDRESS_BYTE, hi)
    }

    padding = Insets(18, 18, 18, 0)
    children = List(display)
  }

  class Vector(vectorName: String, vectorAddress: Int, onChange: (Int) => Unit, initalValue: Int = 0) extends HBox with DataProvider with DataConsumer {

    registerDataSource(this)

    println(s"Creating vector '$vectorName' location '$vectorAddress")
    val changeHandler: (Int) => Unit = onChange
    val tooltip = new Tooltip(s"Location $vectorAddress")
    val label: Label = new Label(vectorName) {
      prefWidth = 70
    }

    val value: TextField = new TextField {
      text = initalValue.toString
      disable = true
      prefWidth = 120
    }

    label.setTooltip(tooltip)

    val setButton: Button = new Button {
      text = "set"
      onAction = _ => {
        println("Setting PC!")
        val dialog: TextInputDialog = getAddressSettingDialogue(s"New ${vectorName} Vector", currentValue)

        val result = dialog.showAndWait()
        result match {
          case Some(value) => updated(value)
          case None => println("Dialog was canceled.")
        }
      }
    }

    val subscription: Subscription = NumericFormatSelector.numericFormatProperty.onChange {
      (_, oldValue, newValue) => {
        println(s"Num format subscription fired: $newValue")
        value.text = numToString(currentValue)
      }
    }
    var currentValue: Int = initalValue

    setButton.setTooltip(tooltip)

    children = List(label, value, setButton)

    override def getData(collector: ListBuffer[ConfigDatum]): Unit = {
      println(s"Collecting for vector $vectorName $currentValue")
      collector += ConfigDatum.apply(vectorName, currentValue.toString)
    }

    override def setData(provider: List[ConfigDatum]): Unit = {
      println(s"Providing to Vector: $vectorName")
      updated(getConfigValue(provider, vectorName, currentValue.toString))
    }

    def updated(str: String): Unit = {
      currentValue = Integer.parseInt(str)
      changeHandler(currentValue)
    }

  }