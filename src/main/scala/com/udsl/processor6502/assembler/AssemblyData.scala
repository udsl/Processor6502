package com.udsl.processor6502.assembler

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.AssembleLocation.currentLocation
import com.udsl.processor6502.assembler.AssemblyData.logger
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object AssemblyData extends StrictLogging:
  // labels defined in a base object so they common others
  // this enables multi file assembly
  
  val sytaxErrorList: ListBuffer[SyntaxErrorRecord] = new ListBuffer[SyntaxErrorRecord]()
  var syntaxErrorListeners: List[SyntaxErrorListener] = List()

  def addSyntaxError(syn: SyntaxErrorRecord): Unit =
    for(listener: SyntaxErrorListener <- syntaxErrorListeners)
      listener.doNotify(syn)
    sytaxErrorList.appended(syn)

  def addSyntaxErrorListener(listener: SyntaxErrorListener): Unit =
    if !syntaxErrorListeners.contains(listener) then
      syntaxErrorListeners = syntaxErrorListeners.appended(listener)

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
    sytaxErrorList.clear()