package com.udsl.processor6502.assemble2.test

import com.udsl.processor6502.assembler.*
import com.udsl.processor6502.assembler.Assemble6502SecondPass.{assembleCommandToken, assembleInstructionToken}
import com.udsl.processor6502.assemblier2.Assemblier2
import com.udsl.processor6502.cpu.execution.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should


class Assemble2Spec extends AnyFlatSpec, should.Matchers :
  
  "Given a file name" should "get an assemblier with source lines"  in {
    val asm = Assemblier2.apply("examples/quicktest2.asm")
    assert(asm.souceLineCount == 12)
  }

  "Given a list of source lines" should 
    "get an assemblier with same source lines"  in {
      val asm = Assemblier2.apply(List(
        "clr; clear all lthe existing data",
        "LDX    #$08",
        "decrement:      DEX",
        "STX $0200",
        "CPX     #$03",
        "BNE decrement",
        "STX $0201",
        "BRK"))
      assert(asm.souceLineCount == 8)
    }



