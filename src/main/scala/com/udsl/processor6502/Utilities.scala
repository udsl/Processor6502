package com.udsl.processor6502

import com.udsl.processor6502.*
import com.udsl.processor6502.Dialogues.theStage
import com.udsl.processor6502.assembler.TokenisedLine
import com.udsl.processor6502.config.ConfigDatum
import com.udsl.processor6502.cpu.*
import com.udsl.processor6502.cpu.execution.{Accumulator, *}
import com.udsl.processor6502.ui.NumericFormatSelector.numericFormatProperty
import scalafx.application.Platform
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, ButtonType, TextInputDialog}
import scalafx.scene.input.{KeyEvent, MouseEvent}
import scalafx.stage.FileChooser
import com.udsl.processor6502.cpu.execution.Operand
import java.io.*
import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.util.matching.Regex

object Utilities:
  var currentFormat: NumericFormatType = NumericFormatType.DEC

  def verifyNumberEntry(text: String): (Boolean, String) =
    val pattern: Regex = currentFormat match {
      case NumericFormatType.HEX => "^\\$?[0-9a-fA-F]+$".r
      case NumericFormatType.OCT => "^[0-7]+$".r
      case NumericFormatType.BIN => "^[0-1]+$".r
      case NumericFormatType.DEC => "^[0-9]+$".r
    }
    pattern.findFirstMatchIn(text) match {
      case Some(_) =>
        (true, "")
      case None =>
        (false, s"Not a ${currentFormat.toString} number")
    }

  def stringToNum(text: String): Int =
    if verifyNumberEntry(text)._1 then
      currentFormat match {
        case NumericFormatType.HEX =>
          if text.startsWith("$") then Integer.parseInt(text.substring(1), 16) else Integer.parseInt(text, 16)
        case NumericFormatType.OCT => Integer.parseInt(text, 8)
        case NumericFormatType.BIN => Integer.parseInt(text, 2)
        case NumericFormatType.DEC => Integer.parseInt(text, 10)
      }
    else
      -1

  def numToString(value: Int, format: NumericFormatType = numericFormatProperty.value): String =
    format match {
      case NumericFormatType.HEX => s"$$${value.toHexString.toUpperCase}"
      case NumericFormatType.OCT => s"o${value.toOctalString}"
      case NumericFormatType.BIN => s"b${value.toBinaryString}"
      case NumericFormatType.DEC => s"${value.toString}"
    }

  def byteToHexString(value: Int): String =
    numToByteString(value, NumericFormatType.HEX)
    
  def numToByteString(value: Int, format: NumericFormatType): String =
    format match {
      case NumericFormatType.HEX =>
        val v = s"00${value.toHexString.toUpperCase}"
        s"$$${v.substring(v.length - 2)}"
      case NumericFormatType.OCT =>
        val v = s"000${value.toOctalString.toUpperCase}"
        s"o${v.substring(v.length - 3)}"
      case NumericFormatType.BIN =>
        val v = s"00000000${value.toBinaryString}"
        s"b${{v.substring(v.length - 8)}}"
      case NumericFormatType.DEC =>
        val v = s"000${value.toString}"
        s"${v.substring(v.length - 3)}"
    }

  def numToWordString(value: Operand, format: NumericFormatType): String =
    numToWordString(value.asAddressValue, format)
    
  def numToWordString(value: Int, format: NumericFormatType): String =
    format match {
      case NumericFormatType.HEX =>
        val v = s"0000${value.toHexString.toUpperCase}"
        s"$$${v.substring(v.length - 4)}"
      case NumericFormatType.OCT =>
        val v = s"000000${value.toOctalString.toUpperCase}"
        s"o${v.substring(v.length - 6)}"
      case NumericFormatType.BIN =>
        val v = s"0000000000000000${value.toBinaryString}"
        s"b${{v.substring(v.length - 16)}}"
      case NumericFormatType.DEC =>
        val v = s"00000${value.toString}"
        s"${v.substring(v.length - 5)}"
    }

  /**
   * A expression is a list of number and labels seperated by arithmetic operators +, -, / or *
   * The problem is spaces we could have 'a+1' which is the same value as 'a + 1' and even 'a+ 1' and 'a +1'
   * By deleting all the spaces we only have to deal with the first format.
   * We then split it up to its component parts by scanning the string
   * @param str the expression
   * @return true if a valid expression false otherwise
   */
  def isExpression( theExpession: String): Boolean =
    def getNumber(noWhiteSpaceExpession: String): Unit =
      var y = 0
      noWhiteSpaceExpession.charAt(0) match
        case '$' => { // is hex
          // find first +-*/ character

//          y = noWhiteSpaceExpession.

        }
        case x =>  if x >= '0' && x <= '9' then { // start of a decimal number
          y = noWhiteSpaceExpession.indexWhere(_.matches("\\W"))
        }
        case _ => y = 0
      val number = noWhiteSpaceExpession.substring(0, y)
      println(noWhiteSpaceExpession.substring(y))

    // The first item must be a number or a label, first try a number wiah must either terminate with an operator or the ned of the string
    getNumber(theExpession.filterNot(_.isWhitespace))

    println(s"isExpression($theExpession)")
    false

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

  def numericValue(str: String): Option[Int] =
    if isNumeric(str) then
      if str.charAt(0) == '$' then
        return Some(Integer.parseInt(str.substring(1), 16))
      else
        return Some(Integer.parseInt(str))
    None

  def isLabel( str: String): Boolean =
    val alphaPattern: Regex = "^[A-Za-z][A-Za-z0-9]+:?$".r
    alphaPattern.findFirstMatchIn(str) match {
      case Some(_) => true
      case None => false
    }

  def getConfigValue(lines: List[ConfigDatum], configKey: String): Option[String] = 
    val datum = lines.find(_.key == configKey)
    datum.flatMap(v => Option[String](v.value))
  
  def constructSourceLine(mnemonic: String, addrMode: AddressingMode, value: Int): String =
    val adr = addrMode match
      case Accumulator => "A"
      case Implied  => " "
      case Immediate => s"#${numToByteString(value, NumericFormatType.HEX) }"
      case ZeroPage
           | Relative => s"${numToByteString(value, NumericFormatType.HEX) }"
      case ZeroPageX => s"${numToByteString(value, NumericFormatType.HEX)}, X"
      case ZeroPageY => s"${numToByteString(value, NumericFormatType.HEX)}, Y"
      case IndirectX => s"(${numToByteString(value, NumericFormatType.HEX)}, X)"
      case IndirectY => s"(${numToByteString(value, NumericFormatType.HEX)}), Y)"
      case Absolute => s"${numToWordString(value, NumericFormatType.HEX)}"
      case Indirect => s"(${numToWordString(value, NumericFormatType.HEX)})"
      case AbsoluteX => s"${numToWordString(value, NumericFormatType.HEX)}, X"
      case AbsoluteY => s"${numToWordString(value, NumericFormatType.HEX)}, Y"
      case _ => ""
    if adr.nonEmpty then
      s"$mnemonic $adr"
    else
      adr


