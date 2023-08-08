package com.udsl.processor6502.assembler

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.version1.{SourceAsssemblierV1, TokenisedLineV1}
import com.udsl.processor6502.assembler.version2.{SourceAssemblerV2, TokenisedLineV2}
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
  def version: Int = -1

object Assembler extends StrictLogging :

  def apply(sourceFile: File): Assembler =
    logger.info("\n\n***** Starting File Assembly *****\n\n")
    assmVersion match
      case 1 => SourceAsssemblierV1.apply(sourceFile)
      case 2 => SourceAssemblerV2.apply(sourceFile)
      case _ => throw new NotImplementedError(s"Invadid assemblier version $assmVersion")

  private def sourceList(source: String): List[String] =
    Using(Source.fromString(source)) { reader =>
      reader.getLines.toList
    }.get
 

  def apply(source: String): Assembler =
    logger.info("\n\n***** Starting Source Assembly *****\n\n")
    assmVersion match
      case 1 => SourceAsssemblierV1.apply(source)
      case 2 => SourceAssemblerV2.apply(sourceList(source))
      case _ => throw new NotImplementedError(s"Invadid assemblier version $assmVersion")

