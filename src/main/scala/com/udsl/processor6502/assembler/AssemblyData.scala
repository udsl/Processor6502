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
  var references = new ListBuffer[Reference]()

  def clear(): Unit =
    labels.clear()
    references.clear()

  def isValid: (Boolean, List[String]) =
    if AssemblyData.labels.isEmpty && AssemblyData.references.nonEmpty then
      val error = "No labels but have references!"
      logger.debug(error)
      return (false, List(error))
    var result = true
    val errors = ListBuffer[String]()
    for r <- AssemblyData.references do
      if !AssemblyData.labels.contains(r.name) then
        val error = s"Reference ${r.name} not found in labels."
        logger.debug(error)
        errors.addOne(error)
        result = false
    logger.debug(s"Have ${AssemblyData.labels.size} labels and ${AssemblyData.references.size} references")
    (result, errors.toList)

  def addReference(ref: String): Unit =
    logger.debug(s"addReference to '$ref' @ $currentLocation")
    references += Reference(ref);

  def addLabel(name: String): Unit =
    labels.get(name) match
      case Some((v, bool)) =>
        labels.addOne(name, (currentLocation, true))
      case None =>
        labels.addOne(name, (-1, false))

  def labelIsDefined(name: String): Boolean =
    labels.get(name) match
      case Some((v, bool)) =>
        bool
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

  def printRefs(): Unit =
    logger.info(
      """
        |*****************
        |*               *
        |*   References  *
        |*               *
        |*****************
        |
        |""".stripMargin )
    if references.isEmpty then
      logger.info("No references made")
    else
      for ref <- references do
        logger.info(s"Have reference to ${ref.name} value ${ref.value}")


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