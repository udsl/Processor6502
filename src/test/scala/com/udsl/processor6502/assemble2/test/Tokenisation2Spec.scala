package com.udsl.processor6502.assemble2.test

import com.udsl.processor6502.assemblier2.{BlankLineToken, CommentLineToken, InstructionToken, Tokeniser}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class Tokenisation2Spec extends AnyFlatSpec with TableDrivenPropertyChecks with Matchers :

  "Given a source line" should "return a list of valid tokens" in {
    val orders = Table(
      ("line", "token-count", "tokens")
      , (";the quick brown fox jumps over the lazy dog", 1, List(CommentLineToken.apply(Array("the quick brown fox jumps over the lazy dog"))))
      , ("", 1, List(BlankLineToken.apply(Array(""))))
      , ("LDA #56", 1, List(InstructionToken.apply("LDA", Array("#56"))))
    )
    forAll(orders) { (line, tokenCount, tokens) =>
      val res = Tokeniser.tockenise(line)
      assert(res.size == tokenCount)
      assert(tokens.toSeq == res)
    }
  }

