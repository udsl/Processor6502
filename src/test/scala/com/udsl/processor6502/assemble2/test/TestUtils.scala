package com.udsl.processor6502.assemble2.test

import com.udsl.processor6502.assemblier2.Token

object TestUtils :

  /**
   * verify that the result list only contains the same tokens as the expected list
   * irrespective of order.
   */

  def verifyTokens(expected: List[Token], result: List[Token]): Boolean =
    val exp: List[Token] = expected.sortBy(f => f.name)
    val res: List[Token] = result.sortBy(f => f.name)
    expected.length == result.length && exp.equals(res)  //expected.sortBy(f => f.name) == result.sortBy(f => f.name)

