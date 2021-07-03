package com.udsl.processor6502.cpu

class IndexY(name: String) extends EightBitRegister(name: String) {

  def decrement(): Unit ={
    ebr -= 1
  }

  def increment(): Unit ={
    ebr += 1
  }
}

object IndexY {
  def apply(): IndexY ={
    val a_ = new IndexY("Index-Y")
    a_.ebr = 0
    a_
  }
}

