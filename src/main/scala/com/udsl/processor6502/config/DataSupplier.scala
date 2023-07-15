package com.udsl.processor6502.config

import com.udsl.processor6502.FileIOUtilities.readConfigFile
import com.udsl.processor6502.Utilities.getConfigValue
import com.udsl.processor6502.config.DataAgentRegistration.registeredConsumers
import com.udsl.processor6502.ui.NumericFormatSelector.updateDisplay

import scala.collection.mutable.ArrayBuffer

object DataSupplier:
  def provideData(data: List[ConfigDatum] ): Unit = 
    for( source <- registeredConsumers)
      source.receiveData(data)
  
  def provideData(): Unit =
    val lines = readConfigFile
    if lines.nonEmpty then
      updateDisplay(
        getConfigValue(lines, "format") match
          case Some(value) => value
          case _ => "DEC"
      )
    provideData(lines)

