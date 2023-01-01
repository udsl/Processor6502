package com.udsl.processor6502.assemble2.test

import com.udsl.processor6502.assemblier2.{InstructionToken, *}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}


class Assemble2Spec extends AnyFlatSpec with TableDrivenPropertyChecks with Matchers :

  val sourceFile = "examples/quicktest2.asm"
  val sourceLines: List[String] = List(
    "clr; clear all lthe existing data",
    "LDX    #$08",
    "decrement:      DEX",
    "STx $0200",
    "CpX     #$03",
    "bNE decrement",
    "STX $0201",
    "BRK")

  val expectedResults: TableFor2[Int, List[Token]] = Table(
    ("lineNum", "tokens")
    , (1, List(CommandToken.apply("CLR", Array()), LineCommentToken.apply("clear all lthe existing data", Array())))
    , (2, List(InstructionToken.apply("LDX", Array("#$08"))))
    , (3, List(LabelToken.apply("decrement", Array("DEX")), InstructionToken.apply("DEX", Array())))
    , (4, List(InstructionToken.apply("STX", Array("$0200"))))
    , (5, List(InstructionToken.apply("CPX", Array("#$03"))))
    , (6, List(InstructionToken.apply("BNE", Array("decrement"))))
    , (7, List(InstructionToken.apply("STX", Array("$0201"))))
    , (8, List(InstructionToken.apply("BRK", Array())))
  )

  "Given a file name" should "get an assemblier with source lines"  in {
    val asm = Assemblier2.apply(sourceFile)
    assert(asm.souceLineCount == 8)
  }

  "Given a list of source lines" should 
    "get an assemblier with same source lines"  in {
      val asm = Assemblier2.apply(sourceLines)
      assert(asm.souceLineCount == 8)
    }

  "With those lines tokenisation" should
    "return a list of TokenisedLine" in {
      val asm = Assemblier2.apply(sourceLines)
      val res = asm.tokenisation
      assert(res.length == 8)
      val it = res.iterator
      forAll(expectedResults) { (lineNum, tokens:List[Token]) =>
        val r: TokenisedLine = it.next()
        assert(r.lineNumber == lineNum)
        assert(TestUtils.verifyTokens(tokens, r.tokens.toList))
      }
  }

  "With those lines read from file tokenisation" should
    "return a list of TokenisedLine" in {
    val asm = Assemblier2.apply(sourceFile)
    val res = asm.tokenisation
    assert(res.length == 8)
    val it = res.iterator
    forAll(expectedResults) { (lineNum, tokens: List[Token]) =>
      val r: TokenisedLine = it.next()
      assert(r.lineNumber == lineNum)
      assert(TestUtils.verifyTokens(tokens, r.tokens.toList))
    }
  }





