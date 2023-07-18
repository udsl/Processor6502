package com.udsl.processor6502.assemble2.test

import com.udsl.processor6502.assemble2.test.TestUtilsV2.validateTokens
import com.udsl.processor6502.assembler.AssemblyData
import com.udsl.processor6502.assembler.version2.{BlankLineTokenV2, CommandTokenV2, CommentLineTokenV2, InstructionTokenV2, LabelTokenV2, LineCommentTokenV2, TokenV2, TokenisedLineV2, TokeniserV2}
import com.udsl.processor6502.cpu.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class Tokenisation2Spec extends AnyFlatSpec with TableDrivenPropertyChecks with Matchers :

  "Given a source line" should "return a list of valid tokens" in {
    val expectedResults = Table(
      ("lineNum", "line", "token-count", "tokens")
      , (1, ";the quick brown fox jumps over the lazy dog", 1, List(CommentLineTokenV2.apply(Array("the quick brown fox jumps over the lazy dog"))))
      , (2, "", 1, List(BlankLineTokenV2.apply(Array(""))))
      , (3, "label: LDA #56 ; instruction with label", 3, List(
          LineCommentTokenV2.apply("instruction with label", Array()),
          LabelTokenV2.apply("label", Array("LDA #56")),
          InstructionTokenV2.apply("LDA", Array("#56"))))
      , (4, "orig $200", 1, List(CommandTokenV2.apply("ORIG", Array("$200"))))
    )
    forAll(expectedResults) { (lineNum, line, tokenCount, tokens) =>
      val res: TokenisedLineV2 = TokeniserV2.tockenise(line, lineNum)
      assert(res.tokens.size == tokenCount)
      assert(tokens == res.tokens)
      validateTokens(res.tokens, line)
      assert(res.sourceText == line)
      assert(res.lineNumber == lineNum)
    }
  }
    