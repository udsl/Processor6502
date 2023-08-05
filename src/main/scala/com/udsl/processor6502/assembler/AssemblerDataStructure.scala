package com.udsl.processor6502.assembler

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.version1.TokenisedLineV1
import com.udsl.processor6502.assembler.version2.TokenisedLineV2

import scala.collection.mutable.ArrayBuffer

trait AssemblerDataStructure extends StrictLogging :
  var _currentLine: Int = 1

  def currentLine: Int =
    val cl = _currentLine
    _currentLine += 1
    cl
    
  def init(): Unit =
    _currentLine = 1

class AssemblerDataStructureV1 extends AssemblerDataStructure :
  val tokenisedList: ArrayBuffer[TokenisedLineV1] = ArrayBuffer.empty[TokenisedLineV1]

object AssemblerDataStructureV1 extends StrictLogging :
  def apply(): AssemblerDataStructureV1 =
    new AssemblerDataStructureV1()

class AssemblerDataStructureV2 extends AssemblerDataStructure :
  val tokenisedList: ArrayBuffer[TokenisedLineV2] = ArrayBuffer.empty[TokenisedLineV2]

object AssemblerDataStructureV2 extends StrictLogging :
  def apply(): AssemblerDataStructureV2 =
    new AssemblerDataStructureV2()