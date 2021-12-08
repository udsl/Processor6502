package com.udsl.processor6502

import com.udsl.processor6502.*
import com.udsl.processor6502.assembler.TokenisedLine
import com.udsl.processor6502.cpu.*
import com.udsl.processor6502.ui.NumericFormatSelector.numericFormatProperty
import com.udsl.processor6502.config.ConfigDatum
import scalafx.application.Platform
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, ButtonType, TextInputDialog}
import scalafx.scene.input.{KeyEvent, MouseEvent}

import java.io.*
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.util.matching.Regex

object Utilities {
  var currentFormat: NumericFormatType = NumericFormatType.DEC

  def stringToNum(text: String): Int =
    currentFormat match {
      case NumericFormatType.HEX => Integer.parseInt(text, 16)
      case NumericFormatType.OCT => Integer.parseInt(text, 8)
      case NumericFormatType.BIN => Integer.parseInt(text, 2)
      case NumericFormatType.DEC => Integer.parseInt(text, 10)
      //        case _ => -1
    }

  def numToString(value: Int): String =
    numericFormatProperty.value match {
      case NumericFormatType.HEX => value.toHexString.toUpperCase
      case NumericFormatType.OCT => value.toOctalString
      case NumericFormatType.BIN => value.toBinaryString
      case NumericFormatType.DEC => value.toString
    }

  /**
   * Only positive HEX or DEC numbers accepted
   * @param str the string value
   */
  def isNumeric( str: String): Boolean =
    val numericPattern: Regex = "^[0-9]+$".r
    val hexPattern: Regex = "^\\$[0-9a-fA-F]+$".r
    numericPattern.findFirstMatchIn(str) match {
      case Some(_) =>
        true
      case None =>
        hexPattern.findFirstMatchIn(str) match {
          case Some(_) =>
            true
          case None =>
            false
      }
    }

  def isAlpha( str: String): Boolean =
    val alphaPattern: Regex = "^[A-Za-z]+$".r
    alphaPattern.findFirstMatchIn(str) match {
      case Some(_) => true
      case None => false
    }

  def numericValue(str: String): Int =
    if isNumeric(str) then
      if str.head == '$' then
        return Integer.parseInt(str, 16)
      else
        return Integer.parseInt(str)
    -1

  def isLable( str: String): Boolean =
    val alphaPattern: Regex = "^[A-Za-z][A-Za-z0-9]+$".r
    alphaPattern.findFirstMatchIn(str) match {
      case Some(_) => true
      case None => false
    }
  
  def getConfigValue(lines: List[ConfigDatum], configKey: String, default: String): String = {
    val datum = lines.find(_.key == configKey)

    datum match {
      case Some(datum) => datum.value
      case None => default
    }
  }
  
  
  def writeToFile(file: File, tokenisedList: List[TokenisedLine]): Unit = {
    val bw = new BufferedWriter(new FileWriter(file))
    for (token <- tokenisedList)
      bw.write(token.toString)
      bw.write("\n")
    bw.close()
  }
  
  def writeStringToFile(file: File, str: String): Unit = {
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(str)
    bw.close()
  }

  def writeStringToFile(file: File, str: List[String]): Unit = {
    val bw = new BufferedWriter(new FileWriter(file))
    for (s <- str)
      bw.write(s)
    bw.close()
  }

}