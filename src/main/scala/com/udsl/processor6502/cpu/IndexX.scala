package com.udsl.processor6502.cpu

class IndexX(name: String) extends EightBitRegister(name: String) :

  def value: Int = _ebr.value

  def value_= (value: Int): Unit = 
    ebr = value
  
  def decrement(): Unit =
    ebr -= 1
  
  def increment(): Unit =
    ebr += 1
  


object IndexX: 
  def apply(): IndexX =
    val a_ = new IndexX("Index-X")
    a_.ebr = 0
    a_
  

