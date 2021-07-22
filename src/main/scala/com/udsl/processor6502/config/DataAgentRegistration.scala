package com.udsl.processor6502.config

import scala.collection.mutable.ArrayBuffer

object DataAgentRegistration {
  val registeredProviders: ArrayBuffer[DataProvider] = ArrayBuffer[DataProvider]()
  val registeredConsumers: ArrayBuffer[DataConsumer] = ArrayBuffer[DataConsumer]()

  def registerDataSource( registrant: DataAgent): Unit = {
    registrant match {
      case b: DataAgent with DataProvider with DataConsumer => {
        registeredProviders += b
        registeredConsumers += b
      }
      case c: DataAgent with DataProvider => registeredProviders += c
      case d: DataAgent with DataConsumer => registeredConsumers += d
      case _ => println("Not a DataProvider")
    }


//    registrant match {
//      case c: DataAgent with DataProvider => registeredProviders += c
//      case _ => println("Not a DataProvider")
//    }
//    registrant match {
//      case d: DataAgent with DataConsumer => registeredConsumers += d
//      case _ => println("Not a DataConsumer")
//    }
  }

  def getRegisteredConsumers : Seq[DataConsumer] = {
    registeredConsumers.toSeq
  }

  def getRegisteredProviders : Seq[DataProvider] = {
    registeredProviders.toSeq
  }
}
