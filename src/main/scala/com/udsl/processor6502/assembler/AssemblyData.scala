package com.udsl.processor6502.assembler

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.AssembleLocation.currentLocation
import com.udsl.processor6502.assembler.AssemblyData.logger
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object AssemblyData extends StrictLogging:
  // labels defined in a base object so they common others
  // this enables multi file assemblyr
  
  val erorList: ListBuffer[ErrorRecord] = new ListBuffer[ErrorRecord]()
  var errorListeners: List[ErrorListener] = List()

  def addError(syn: ErrorRecord): Unit =
    for(listener: ErrorListener <- errorListeners)
      listener.doNotify(syn)
    erorList.appended(syn)

  def addErrorListener(listener: ErrorListener): Unit =
    if !errorListeners.contains(listener) then
      errorListeners = errorListeners.appended(listener)

  def isValid: (Boolean, List[String]) =
    val errors = ListBuffer[String]()
    (true, errors.toList)
    
  def printLabels(): Unit =
    logger.info(
      """
        |*****************
        |*               *
        |*     Labels    *
        |*               *
        |*****************
        |
        |""".stripMargin )
    if (LabelFactory.count == 0) logger.info("No labels defined") else for (label: Label<- LabelFactory.labels) {
      logger.info(s"\t$label.name value ${label.evaluate}")
    }

  def clear(): Unit =
    LabelFactory.clear()
    erorList.clear()