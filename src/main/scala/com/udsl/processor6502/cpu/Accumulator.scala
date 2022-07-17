package com.udsl.processor6502.cpu:

  class Accumulator(name: String) extends EightBitRegister(name: String) {

    def value: Int = _ebr.value
    
    def value_= (value: Int): Unit = {
      ebr = value
    }

    def decrement(): Unit ={
      ebr -= 1
    }
  
    def increment(): Unit ={
      ebr += 1
    }

  }
  
  object Accumulator {
    def apply(): Accumulator ={
      val a_ = new Accumulator("accumulator")
      a_.ebr = 0
      a_
    }
  }