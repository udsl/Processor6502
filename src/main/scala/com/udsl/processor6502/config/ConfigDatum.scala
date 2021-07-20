package com.udsl.processor6502.config

class ConfigDatum( private val key: String, private val value: String){
  override def toString() : String = {
    s"$key:$value"
  }
}

object ConfigDatum{
  def apply(key: String, value: String) : ConfigDatum = {
    if (!key.matches("\\S+") || key.contains(":")) {
      throw new Exception(s"Invalid key: '$key'.")
    }
    new ConfigDatum(key, value)
  }
}