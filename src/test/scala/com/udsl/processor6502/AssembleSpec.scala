package com.udsl.processor6502

import com.udsl.processor6502.assembler.version1.SourceAsssemblierV1
import com.udsl.processor6502.assembler.{AssembleLocation, Assembler}
import com.udsl.processor6502.config.AppOptions
import com.udsl.processor6502.cpu.Memory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.io.File

class AssembleSpec extends AnyFlatSpec, should.Matchers {

  val fileToTestAgainst = "C:\\Users\\iango\\IdeaProjects\\Processor6502\\examples\\test\\unitquicktest.asm"

  "Given a configuration for V1 " should " start the V1 assembler" in {
    AppOptions.assmVersion = 1
    val assem = Assembler.apply(File(fileToTestAgainst))
    assert(assem.version == 1)
  }

  "Given a configuration for V2 " should " start the V2 assembler" in {
    AppOptions.assmVersion = 2
    val assem = Assembler.apply(File(fileToTestAgainst))
    assert(assem.version == 2)
  }

  "Assemling a file with V1 " should " result in objectcode output" in {
    AppOptions.assmVersion = 1
    val assem = Assembler.apply(File(fileToTestAgainst))
    assem.startAssembly()

    assert(SourceAsssemblierV1.assemblerData.tokenisedList.size == 11)
    val cells = Memory.getCells(2000, 5)
    assert(AssembleLocation.currentLocation == 2005)
  }
}
