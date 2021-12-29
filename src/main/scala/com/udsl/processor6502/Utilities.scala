package com.udsl.processor6502

import com.udsl.processor6502.*
import com.udsl.processor6502.assembler.TokenisedLine
import com.udsl.processor6502.cpu.*
import com.udsl.processor6502.ui.NumericFormatSelector.numericFormatProperty
import com.udsl.processor6502.config.ConfigDatum
import com.udsl.processor6502.cpu.execution.{Absolute, AbsoluteX, AbsoluteY, Accumulator, AddressingMode, Immediate, Implied, Indirect, IndirectX, IndirectY, Invalid, NotApplicable, Relative, Unknown, ZeroPage, ZeroPageX, ZeroPageY}
import scalafx.application.Platform
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, ButtonType, TextInputDialog}
import scalafx.scene.input.{KeyEvent, MouseEvent}
import scalafx.stage.FileChooser

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

  def numToWordString(value: (Int, Int), format: NumericFormatType): String =
    numToWordString(value._1 + (value._2 * 256), format)
    
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

  def getConfigValue(lines: List[ConfigDatum], configKey: String): Option[String] = {
    val datum = lines.find(_.key == configKey)

    datum match {
      case Some(datum) => Option[String](datum.value)
      case None => None
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

  def selectSourceFileToSave: File =
    getChosenSaveFile(getSourceFileChooser)

  def selectSourceFileToLoad: File =
    getChosenLoadFile(getSourceFileChooser)

  private def getSourceFileChooser: FileChooser =
    val chooser = new FileChooser
    val saveFilter = new FileChooser.ExtensionFilter("Code Save Files", "*.asm")
    chooser.getExtensionFilters.add(saveFilter)
    chooser

  def selectConfigFileToSave: File =
    getChosenSaveFile(getConfigFileChooser)

  def selectConfigFileToLoad: File =
    getChosenLoadFile(getConfigFileChooser)

  private def getConfigFileChooser: FileChooser =
    val chooser = new FileChooser
    val saveFilter = new FileChooser.ExtensionFilter("Config Save Files", "*.save")
    chooser.getExtensionFilters.add(saveFilter)
    chooser

  private def getChosenSaveFile(chooser: FileChooser): File =
    chooser.setInitialDirectory(new File("."));
    chooser.showSaveDialog(Main.stage)

  private def getChosenLoadFile(chooser: FileChooser): File =
    chooser.setInitialDirectory(new File("."));
    chooser.showOpenDialog(Main.stage)

  def constructSourceLine(mnemonic: String, addrMode: AddressingMode, value: (Int, Int)): String =
    val adr = addrMode match
      case Accumulator => "A"
      case Implied  => " "
      case Immediate => s"#${numToByteString(value._1, NumericFormatType.HEX) }"
      case ZeroPage
           | Relative => s"${numToByteString(value._1, NumericFormatType.HEX) }"
      case ZeroPageX => s"${numToByteString(value._1, NumericFormatType.HEX)}, X"
      case ZeroPageY => s"${numToByteString(value._1, NumericFormatType.HEX)}, Y"
      case IndirectX => s"(${numToByteString(value._1, NumericFormatType.HEX)}, X)"
      case IndirectY => s"(${numToByteString(value._1, NumericFormatType.HEX)}), Y)"
      case Absolute => s"${numToWordString(value, NumericFormatType.HEX)}"
      case Indirect => s"(${numToWordString(value, NumericFormatType.HEX)})"
      case AbsoluteX => s"${numToWordString(value, NumericFormatType.HEX)}, X"
      case AbsoluteY => s"${numToWordString(value, NumericFormatType.HEX)}, Y"
      case Invalid | Unknown | NotApplicable => ""
    if adr.nonEmpty then
      s" - ${mnemonic} $adr"
    else
      adr

}