package com.udsl.processor6502.assembleV2.tests

import TestUtilsV2.verifyTokens
import com.udsl.processor6502.assembler.version1.CommandToken
import com.udsl.processor6502.assembler.version2.{AssemblierV2, CommandTokenV2, InstructionTokenV2, LabelTokenV2, LineCommentTokenV2, TokenV2, TokenisedLineV2}
import com.udsl.processor6502.cpu.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}


class Assemble2Spec extends AnyFlatSpec with TableDrivenPropertyChecks with Matchers :

  val sourceFile = "examples/unitquicktest2.asm"
  val sourceLines: List[String] = List(
    "clr; clear all lthe existing data",
    "LDX    #$08",
    "decrement:      DEX",
    "STx $0200",
    "CpX     #$03",
    "bNE decrement",
    "STX $0201",
    "JMP  (jumpto)",
    "BRK",
    "jumpto: ADDR 512")

  val emptyStringArray = Array.empty[String]
  val expectedResults: TableFor2[Int, List[TokenV2]] = Table(
    ("lineNum", "tokens")
    , (1, List(CommandTokenV2.apply("CLR", Array()), LineCommentTokenV2.apply("clear all lthe existing data", emptyStringArray)))
    , (2, List(InstructionTokenV2.apply("LDX", Array("#$08"))))
    , (3, List(LabelTokenV2.apply("decrement", Array("DEX")), InstructionTokenV2.apply("DEX", emptyStringArray)))
    , (4, List(InstructionTokenV2.apply("STX", Array("$0200"))))
    , (5, List(InstructionTokenV2.apply("CPX", Array("#$03"))))
    , (6, List(InstructionTokenV2.apply("BNE", Array("decrement"))))
    , (7, List(InstructionTokenV2.apply("STX", Array("$0201"))))
    , (8, List(InstructionTokenV2.apply("JMP", Array("(jumpto)"))))
    , (9, List(InstructionTokenV2.apply("BRK", emptyStringArray)))
  )

  "Given a file name" should "get an assemblier with source lines"  in {
    val asm = AssemblierV2.apply(sourceFile)
    assert(asm.sourceLines.toList.length == 9)
  }

  "Given a list of source lines" should 
    "get an assemblier with same source lines"  in {
      val asm = AssemblierV2.apply(sourceLines)
      assert(asm.sourceLines.toList.length == 8)
    }

  "With those lines tokenisation" should
    "return a list of TokenisedLine" in {
      val asm = AssemblierV2.apply(sourceLines)
      val res = asm.tokenisation
      assert(res.length == 8)
      val it = res.iterator
      forAll(expectedResults) { (lineNum, tokens:List[TokenV2]) =>
        val r: TokenisedLineV2 = it.next()
        assert(r.source.lineNum == lineNum)
        assert(verifyTokens(tokens, r.tokens.toList))
      }
  }

  "With those lines read from file tokenisation" should
    "return a list of TokenisedLine" in {
    val asm = AssemblierV2.apply(sourceFile)
    val res = asm.tokenisation
    assert(res.length == 10)
    val it = res.iterator
    forAll(expectedResults) { (lineNum, tokens: List[TokenV2]) =>
      val r: TokenisedLineV2 = it.next()
      assert(r.source.lineNum == lineNum)
      assert(verifyTokens(tokens, r.tokens.toList))
    }
  }





