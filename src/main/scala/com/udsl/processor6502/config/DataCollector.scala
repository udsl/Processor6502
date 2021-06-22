package com.udsl.processor6502.config

import scala.collection.mutable.ArrayBuffer

object DataCollector {
  val registeredSources = ArrayBuffer[DataSource]()

  def registerDataSource( registrant: DataSource): Unit = {
    registeredSources += registrant
  }

  def collectData(): ArrayBuffer[String] = {
    val data = ArrayBuffer[String]()
    for( source <- registeredSources){
      source.getData(data)
    }
    data
  }
}
