package com.udsl.processor6502.config

import scala.collection.mutable.ListBuffer

trait DataAgent:
  def agentFor: String = "Unknown"

trait DataProvider extends DataAgent {
  def supplyData(collector: ListBuffer[ConfigDatum]): Unit

}

trait DataConsumer extends DataAgent{
  def receiveData(provider: List[ConfigDatum]):Unit
}
