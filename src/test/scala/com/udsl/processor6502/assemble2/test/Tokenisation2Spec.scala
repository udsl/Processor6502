package com.udsl.processor6502.assemble2.test

import com.udsl.processor6502.assemblier2.{BlankLineToken, CommandToken, CommentLineToken, InstructionToken, LabelToken, LineCommentToken, Tokeniser}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class Tokenisation2Spec extends AnyFlatSpec with TableDrivenPropertyChecks with Matchers :

  "Given a source line" should "return a list of valid tokens" in {
    val orders = Table(
      ("lineNum", "line", "token-count", "tokens", "tokenText")
      , (1, ";the quick brown fox jumps over the lazy dog", 1, List(CommentLineToken.apply(Array("the quick brown fox jumps over the lazy dog"))), "")
      , (2, "", 1, List(BlankLineToken.apply(Array(""))), "")
      , (3, "label: LDA #56 ; instruction with label", 3, List(
          LineCommentToken.apply("instruction with label", Array()),
          LabelToken.apply("label", Array("LDA #56")),
          InstructionToken.apply("LDA", Array("#56"))), "LDA")
      , (4, "orig $200", 1, List(CommandToken.apply("ORIG", Array("$200"))), "ORIG")
    )
    forAll(orders) { (lineNum, line, tokenCount, tokens, tokenText) =>
      val res = Tokeniser.tockenise(line, lineNum)
      assert(res.tokens.size == tokenCount)
      assert(tokens == res.tokens)
      assert(res.sourceLine == line)
      assert(res.lineNumber == lineNum)
    }
  }

