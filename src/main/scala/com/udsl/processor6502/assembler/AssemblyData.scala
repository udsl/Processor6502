package com.udsl.processor6502.assembler

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.AssembleLocation.currentLocation
import com.udsl.processor6502.assembler.AssemblyData.logger
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object AssemblyData extends StrictLogging:
  // labels defined in a base object so they common others
  // this enables multi file assembly
  val labels = new mutable.HashMap[String, (Int, Boolean)]()
  val sytaxErrorList: ListBuffer[SyntaxErrorRecord] = new ListBuffer[SyntaxErrorRecord]()

  def addSyntaxError(syn: SyntaxErrorRecord): Unit =
    sytaxErrorList.appended(syn)
    
  def clear(): Unit =
    labels.clear()
    sytaxErrorList.clear()

  def isValid: (Boolean, List[String]) =
    val errors = ListBuffer[String]()
    logger.debug(s"Have ${AssemblyData.labels.size} labels")
    (true, errors.toList)


  def addLabel(name: String): Unit =
    labels.get(name) match
      case Some((v, bool)) =>
        labels.addOne(name, (currentLocation, true))
      case None =>
        labels.addOne(name, (-1, false))

  def labelIsDefined(name: String): Boolean =
    labels.get(name) match
      case Some((_, _)) =>
        true
      case None =>
        false

  def labelIsValid(name: String): Boolean =
    labels.get(name) match
      case Some((_, valid)) =>
        valid
      case None =>
        false
        
  def labelValue(name: String): Option[Int] =
    labels.get(name) match
      case Some((v, valid)) =>
        if valid then Some(v) else None
      case None =>
        None

  def addLabel(name: String, value: Int): Unit =
    labels.get(name) match
      case Some((v, bool)) =>
        if bool && v != value then
          throw new Exception(s"Label '$name' already defined")
        else
          labels.addOne(name, (value, true))
      case None =>
        labels.addOne(name, (value, true))

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
    if (labels.isEmpty) logger.info("No labels defined") else for ((label, address) <- labels) {
      logger.info(s"\t$label address $address")
    }