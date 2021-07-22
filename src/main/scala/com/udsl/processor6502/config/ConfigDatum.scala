package com.udsl.processor6502.config

class ConfigDatum( val key: String, val value: String){
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