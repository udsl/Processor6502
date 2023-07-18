package com.udsl.processor6502

import com.udsl.processor6502.assembler.Assembler
import com.udsl.processor6502.config.AppOptions
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
}
