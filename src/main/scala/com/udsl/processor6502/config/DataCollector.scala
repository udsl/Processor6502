package com.udsl.processor6502.config

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.config.DataAgentRegistration.getRegisteredProviders

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object DataCollector extends StrictLogging{

  def collectData(): List[ConfigDatum] = {
    val data: ListBuffer[ConfigDatum] = ListBuffer()
    for( source <- getRegisteredProviders){
      logger.info(s"Getting data from ${source.agentFor}")
      source.supplyData(data)
    }
    data.toList
  }
}
