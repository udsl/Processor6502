package com.udsl.processor6502

import com.udsl.processor6502.assembler.version1.SourceAsssemblierV1
import com.udsl.processor6502.assembler.{AssembleLocation, Assembler}
import com.udsl.processor6502.config.AppOptions
import com.udsl.processor6502.cpu.Memory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.io.File

class AssembleSpec extends AnyFlatSpec, should.Matchers {

  "Given a configuration for V1 " should " start the V1 assemblier" in {
    AppOptions.assmVersion = 1
    val assem = Assembler.apply(File("C:\\Users\\iango\\IdeaProjects\\Processor6502\\examples\\quicktest.asm"))
    assert(assem.version == 1)
  }

  "Given a configuration for V2 " should " start the V2 assemblier" in {
    AppOptions.assmVersion = 2
    val assem = Assembler.apply(File("C:\\Users\\iango\\IdeaProjects\\Processor6502\\examples\\quicktest.asm"))
    assert(assem.version == 2)
  }

  "Assemling a file with V1 " should " result in objectcode output" in {
    AppOptions.assmVersion = 1
    val assem = Assembler.apply(File("C:\\Users\\iango\\IdeaProjects\\Processor6502\\examples\\quicktest.asm"))
    assem.startAssembly()

    assert(SourceAsssemblierV1.assemblerData.tokenisedList.size == 15)
    val cells = Memory.getCells(2000, 8)
    assert(AssembleLocation.currentLocation == 2008)
  }
}
