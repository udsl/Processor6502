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
    numToString(value, numericFormatProperty.value)

  def numToString(value: Int, format: NumericFormatType): String =
    format match {
      case NumericFormatType.HEX => s"$$${value.toHexString.toUpperCase}"
      case NumericFormatType.OCT => s"o${value.toOctalString}"
      case NumericFormatType.BIN => s"b${value.toBinaryString}"
      case NumericFormatType.DEC => s"${value.toString}"
    }

  def numToByteString(value: Int, format: NumericFormatType): String =
    format match {
      case NumericFormatType.HEX =>
        val v = value.toHexString.toUpperCase
        s"$$${if v.length == 1 then
          s"0$v"
        else
          v}"
      case NumericFormatType.OCT =>
        val v = value.toOctalString
        val str = if v.length < 3 then
          val s = s"000$v"
          s.substring(s.length - 3)
        else
          v
        s"o${str}"
      case NumericFormatType.BIN =>
        val v = value.toBinaryString
        val str = if v.length < 8 then
          val s = s"00000000$v"
          s.substring(s.length - 8)
        else
          v
        s"b${str}"

      case NumericFormatType.DEC =>
        val v = value.toString
        s"${if v.length == 1 then
          s"0$v"
        else
          v}"
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
      if str.charAt(0) == '$' then
        return Integer.parseInt(str.substring(1), 16)
      else
        return Integer.parseInt(str)
    -1

  def isLabel( str: String): Boolean =
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