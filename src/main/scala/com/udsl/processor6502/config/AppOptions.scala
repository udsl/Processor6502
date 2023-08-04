package com.udsl.processor6502.config

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Utilities.getConfigValue
import com.udsl.processor6502.config.DataAgentRegistration.registerDataSource

import scala.collection.mutable.ListBuffer

object AppOptions extends DataProvider , DataConsumer , StrictLogging{

  override def agentFor: String = "AppOptions"

  registerDataSource(this)

  private var _assmVersion: Int = 1
  def assmVersion: Int = _assmVersion
  def assmVersion_=(newVal: Int): Unit = _assmVersion = newVal

  private var _userLoggingEnabled: Boolean = true
  def userLoggingEnabled: Boolean = _userLoggingEnabled
  def userLoggingEnabled_=(newVal: Boolean): Unit = _userLoggingEnabled = newVal

  private var _userLoggingVerbosity: Int = 2
  def userLoggingVerbosity: Int = _userLoggingVerbosity
  def userLoggingVerbosity_=(newVal: Int): Unit = _userLoggingVerbosity = newVal

  override def supplyData(collector: ListBuffer[ConfigDatum]): Unit =
    logger.info("Collecting from AppOptions")
    collector += ConfigDatum.apply("app.option.assm.version", _assmVersion.toString)
    collector += ConfigDatum.apply("app.option.userLoggingEnabled", _userLoggingEnabled.toString)
    collector += ConfigDatum.apply("app.option.userLoggingVerbosity", _userLoggingVerbosity.toString)
    
  override def receiveData(provider: List[ConfigDatum]): Unit =
    logger.info("Providing to AppOptions")
    getConfigValue(provider, "app.option.assm.version").map(f => f.toInt)
    getConfigValue(provider, "app.option.userLoggingEnabled").map(f => f.toBoolean)
    getConfigValue(provider, "app.option.userLoggingVerbosity").map(f => f.toInt)
}


