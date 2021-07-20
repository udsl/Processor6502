package com.udsl.processor6502.config

import com.udsl.processor6502.config.DataAgentRegistration.registeredConsumers

import scala.collection.mutable.ArrayBuffer

object DataSupplier {
  def provideData(data: List[ConfigDatum] ): Unit = {
    for( source <- registeredConsumers){
      source.setData(data)
    }
  }
}
