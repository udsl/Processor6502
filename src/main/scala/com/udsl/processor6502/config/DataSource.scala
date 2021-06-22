package com.udsl.processor6502.config

import scala.collection.mutable.ArrayBuffer

trait DataSource {
  def getData( collector: ArrayBuffer[String]) : Unit
}
