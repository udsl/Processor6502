package com.udsl.processor6502.config

import scala.collection.mutable.ArrayBuffer

object DataAgentRegistration {
  val registeredProviders: ArrayBuffer[DataProvider] = ArrayBuffer[DataProvider]()
  val registeredConsumers: ArrayBuffer[DataConsumer] = ArrayBuffer[DataConsumer]()

  def registerDataSource( registrant: DataAgent): Unit = {
    registrant match {
      case c: DataAgent with DataProvider => registeredProviders += c
      case d: DataAgent with DataConsumer => registeredConsumers += d
      case _ => println("Just a DataAgent")
    }
  }

  def getRegisteredConsumers : Seq[DataConsumer] = {
    registeredConsumers.toSeq
  }

  def getRegisteredProviders : Seq[DataProvider] = {
    registeredProviders.toSeq
  }
}
