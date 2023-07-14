package com.udsl.processor6502.config

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Utilities.getConfigValue
import com.udsl.processor6502.config.DataAgentRegistration.registerDataSource
import scalafx.scene.layout.VBox

import scala.collection.mutable.ListBuffer

object AppOptions extends DataProvider , DataConsumer , StrictLogging{

  override def agentFor: String = "AppOptions"

  registerDataSource(this)

  private var _assmVersion: Int = 0
  def assmVersion: Int = _assmVersion
  def assmVersion_=(newVal: Int): Unit = _assmVersion = newVal

  override def supplyData(collector: ListBuffer[ConfigDatum]): Unit =
    logger.info("Collecting from AppOptions")
    collector += ConfigDatum.apply("app.option.assm.version", _assmVersion.toString)

  override def receiveData(provider: List[ConfigDatum]): Unit =
    logger.info("Providing to AppOptions")
    getConfigValue(provider, "app.option.assm.version") match {
      case Some(v) =>
        _assmVersion = v.toInt
      case _ =>
    }
}


