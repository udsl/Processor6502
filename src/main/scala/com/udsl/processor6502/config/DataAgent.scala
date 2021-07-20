package com.udsl.processor6502.config

import scala.collection.mutable.{ListBuffer}

trait DataAgent

trait DataProvider extends DataAgent {
  def getData(collector: ListBuffer[ConfigDatum]): Unit

}

trait DataConsumer extends DataAgent{
  def setData( provider: List[ConfigDatum])
}
