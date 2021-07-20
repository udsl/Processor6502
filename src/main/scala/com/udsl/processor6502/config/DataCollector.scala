package com.udsl.processor6502.config

import com.udsl.processor6502.config.DataAgentRegistration.getRegisteredProviders

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object DataCollector {

  def collectData(): List[ConfigDatum] = {
    val data: ListBuffer[ConfigDatum] = ListBuffer()
    for( source <- getRegisteredProviders){
      source.getData(data)
    }
    data.toList
  }
}
