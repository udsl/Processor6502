package com.udsl.processor6502.UI

import com.udsl.processor6502.Utilities.getConfigValue
import com.udsl.processor6502.config.DataAgentRegistration.registerDataSource
import com.udsl.processor6502.config.{ConfigDatum, DataConsumer, DataProvider}
import com.udsl.processor6502.cpu.{Processor, StatusRegisterFlags}
import com.udsl.processor6502.cpu.StatusRegister.{BREAK_FLAG_MASK, CARRY_FLAG_MASK, DECIMAL_FLAG_MASK, INTERRUPT_FLAG_MASK, NEGATIVE_FLAG_MASK, OVERFLOW_FLAG_MASK, ZERO_FLAG_MASK}
import scalafx.event.subscriptions.Subscription
import scalafx.geometry.Insets
import scalafx.scene.control.Label
import scalafx.scene.layout.{HBox, StackPane, VBox}

import scala.collection.mutable.ListBuffer

class StatusRegisterView extends VBox {

    val srSubscription: Subscription = Processor.sr._ebr.onChange {
        (_, oldValue, newValue) => {
            println(s"Status register subscription fired - ${oldValue}, ${newValue}")
            updateDisplayedValues(newValue.intValue())
        }
    }

    def updateDisplayedValues(newValue: Int): Unit ={
        setFlag( StatusRegisterFlags.Negative, (NEGATIVE_FLAG_MASK & newValue) > 0)
        setFlag( StatusRegisterFlags.Overflow, (OVERFLOW_FLAG_MASK & newValue) > 0)
        setFlag( StatusRegisterFlags.Break, (BREAK_FLAG_MASK & newValue) > 0)
        setFlag( StatusRegisterFlags.Decimal, (DECIMAL_FLAG_MASK & newValue) > 0)
        setFlag( StatusRegisterFlags.Interrupt, (INTERRUPT_FLAG_MASK & newValue) > 0)
        setFlag( StatusRegisterFlags.Zero, (ZERO_FLAG_MASK & newValue) > 0)
        setFlag( StatusRegisterFlags.Carry, (CARRY_FLAG_MASK & newValue) > 0)
    }

    val display = new StackPane {
        val titleBox = new HBox {
            val title: Label = new Label {
                text = "  Status Flags  "
                style = "-fx-content-display: top; -fx-background-color: white; -fx-translate-y: -12; -fx-translate-x: 8;"
            }
            prefWidth = title.prefWidth.value
            children = List(title)
        }

        val statusBits = new VBox {
            val bits = new VBox {
                val neg = new StatusFlag("Negative")
                val ovr = new StatusFlag("Overflow")
                val u = new StatusFlag("Unused", true, true)
                val brk = new StatusFlag("Break")
                val dec = new StatusFlag("Decimal")
                val inter = new StatusFlag("Interrupt")
                val zero = new StatusFlag("Zero")
                val carry = new StatusFlag("Carry")
                padding = Insets(4, 4, 4, 4)
                children = List(neg, ovr, u, brk, dec, inter, zero, carry)
            }
            style = "-fx-content-display: top; -fx-border-insets: -2 -2 -2 -2; -fx-background-color: white; -fx-border-color: grey; -fx-border-width: 2;"
            children = List(bits)
        }

        children = List(statusBits, titleBox)
    }

    padding = Insets(18, 18, 18, 0)
    children = List(display)

    private def setFlag(flag: StatusRegisterFlags.Value, value: Boolean): Unit ={
        flag match{
            case StatusRegisterFlags.Negative => display.statusBits.bits.neg.update(value)
            case StatusRegisterFlags.Overflow => display.statusBits.bits.ovr.update(value)
            case StatusRegisterFlags.Break => display.statusBits.bits.brk.update(value)
            case StatusRegisterFlags.Decimal => display.statusBits.bits.dec.update(value)
            case StatusRegisterFlags.Interrupt => display.statusBits.bits.inter.update(value)
            case StatusRegisterFlags.Zero => display.statusBits.bits.zero.update(value)
            case StatusRegisterFlags.Carry => display.statusBits.bits.carry.update(value)
        }
    }
}

class StatusFlag( statusName: String, initalValue: Boolean = false, readOnly: Boolean = false) extends HBox with DataProvider with DataConsumer {

    registerDataSource( this)

    println(s"Creating StatusFlag '$statusName'")
    var currentValue: Boolean = initalValue
    val isReadOnly: Boolean = readOnly
    val label: Label = new Label(statusName){
        prefWidth = 70
    }
    val value = new Label(if (initalValue) "1" else "0")

    children = List(label, value)

    def update( newValue: Boolean): Unit ={
        if (!isReadOnly) {
            if (currentValue != newValue) {
                currentValue = newValue
                value.text = if (newValue) "1" else "0"
            }
        }
    }

    override def getData(collector: ListBuffer[ConfigDatum]): Unit = {
        println("Collecting from RegisterBox")
        collector += ConfigDatum.apply(statusName, currentValue.toString)
    }

    override def setData( provider: List[ConfigDatum]): Unit = {
        println("Providing to StatusFlag")
        update(
            getConfigValue(provider, statusName, currentValue.toString) match {
                case "true" => true
                case "false" => false
                case _ => false
            }
        )
    }

}

