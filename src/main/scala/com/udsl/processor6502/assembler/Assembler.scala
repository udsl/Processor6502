package com.udsl.processor6502.assembler

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.version1.Assemble6502.logger
import com.udsl.processor6502.assembler.version1.{SourceAsssemblierV1, TokenisedLine}
import com.udsl.processor6502.config.AppOptions.assmVersion
import com.udsl.processor6502.config.ConfigDatum

import reflect.Selectable.reflectiveSelectable
import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.language.reflectiveCalls
import scala.util.Try
import scala.util.Using

trait Assembler:
  def startAssembly(): Unit


class AssemblerDataStructure extends StrictLogging :
  val tokenisedList: ArrayBuffer[TokenisedLine] = ArrayBuffer.empty[TokenisedLine]
  var currentLine: Int = 1

object AssemblerDataStructure extends StrictLogging :
  def apply() : AssemblerDataStructure =
    new AssemblerDataStructure()


object Assembler extends StrictLogging :

  def apply(sourceFile: File): Assembler =
    logger.info("\n\n***** Starting File Assembly *****\n\n")
    assmVersion match
      case 1 => SourceAsssemblierV1.apply(sourceFile)
      case _ => throw new NotImplementedError("assemblier version2")
      
  def apply(source: String): Assembler =
    logger.info("\n\n***** Starting Source Assembly *****\n\n")
    assmVersion match
      case 1 => SourceAsssemblierV1.apply(source)
      case _ => throw new NotImplementedError("assemblier version2")

