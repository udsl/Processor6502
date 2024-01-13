package com.udsl.processor6502

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.*
import com.udsl.processor6502.Dialogues.theStage
import com.udsl.processor6502.assembler.{AssemblerException, LabelFactory, TokenisedLine}
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
import scala.::
import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.util.matching.Regex

object Utilities extends StrictLogging :
  var currentFormat: NumericFormatType = NumericFormatType.DEC
  val operators = Seq('+', '-', '*', '/')
  
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

  enum Operation {
    case +, -, *, /

    def valueOf(s: String): Option[Operation] = Operation.values.find(_.toString == s)
  }

  case class Operator(a: Int, b: Int, operation: Operation) {
    def compute: Option[Int] = operation match {
      case Operation.+ => Option(a + b)
      case Operation.- => Option(a - b)
      case Operation.* => Option(a * b)
      case Operation./ => Option(a / b)
    }
  }

  /**
   * A expression is a list of number and labels seperated by arithmetic operators +, -, / or *
   * The problem is spaces we could have 'a+1' which is the same value as 'a + 1' and even 'a+ 1' and 'a +1'
   * By deleting all the spaces we only have to deal with the first format.
   * We then split it up to its component parts by scanning the string
   * @param str the expression
   * @return true if a valid expression false otherwise
   */
  def getExpression( theExpession: String): List[String] = {
    var expressionParts: List[String] = List()

    val HEX_PATTERN = "\\$[0-9A-Fa-f]+\\s*[\\+\\-\\*/]".r
    val DECIMAL_PATTERN = "\\d+\\s*[\\+\\-\\*\\/]".r
    val LABEL_PATTERN = "[a-zA-Z]\\w*\\s*[\\+\\-\\*\\/]".r

    def parseForValue(noWhiteSpaceExpession: String): String = {

      def getValues(expression: String, regx: Regex): Option[Int] =
        regx.findFirstIn(noWhiteSpaceExpession) match
          case Some(a) => Some(a.length)
          case None => // No match so no operator must be th end of the expression so add this
            expressionParts = expressionParts.appended(expression)
            None

      def getExpressionParts(index: Int): String = {
        expressionParts = expressionParts.appended(noWhiteSpaceExpession.substring(0, index-1)).appended(noWhiteSpaceExpession.substring(index-1, index))
        noWhiteSpaceExpession.substring(index)
      }

      val parseResult: Option[Int] = noWhiteSpaceExpession.charAt(0) match {
        case '$' => // is hex
          getValues(noWhiteSpaceExpession, HEX_PATTERN)
        case x if x.isDigit => // start of a decimal number
          getValues(noWhiteSpaceExpession, DECIMAL_PATTERN)
        case x if x.isLetter => // start of a label
          getValues(noWhiteSpaceExpession, LABEL_PATTERN) 
        case _ => None
      }

      parseResult.map(getExpressionParts).getOrElse(noWhiteSpaceExpession)
    }

    // The first item must be a number or a label, first try a number wiah must either terminate with an operator or the ned of the string
    var expressionToParse = theExpession.filterNot(_.isWhitespace)
    var parsedExpression = ""
    
    while {
      parsedExpression = parseForValue(expressionToParse)
      !expressionToParse.equals(parsedExpression)
    } do {expressionToParse = parsedExpression}

    println(s"isExpression($theExpession)")
    expressionParts
  }
  
  def isExpression( theExpession: String): Boolean =
    operators.exists(theExpession.contains)

  def splitExpression(theExpession: String): List[String] =
    val operatorLength = 1
    
    def firstOcurance(exp: String) : Option[Int] =
      val indices = for {
        subStr <- operators
        idx = exp.indexOf(subStr) if idx != -1
      } yield idx
      if (indices.isEmpty) None else Some(indices.min)

    def loop(expression: String, results: List[String]): List[String] = {
      firstOcurance(expression).map { fo =>
        val firstPart = expression.substring(0, fo)
        val operator = expression.substring(fo, fo + operatorLength)
        val remainingExpression = expression.substring(fo + operatorLength)
        loop(remainingExpression, results ::: List(firstPart, operator))
      }.getOrElse(results :+ expression)
    }

    loop(theExpession, List.empty)
  
    
  def evaluateExpression(expression: List[String]): Either[String, Int]  =
    evaluateExpression(expression.toArray)

  def evaluateExpression(expression: Array[String]): Either[String, Int] =
    var currentIndex = 0
    var result: Option[Int] = None

    def getAndUpdate: Int =
      val ret = currentIndex
      currentIndex = currentIndex + 1
      ret

    // Extract the common code into a separate method
    def getExpressionElement: Option[String] =
      if expression != null && currentIndex < expression.length then
        Some(expression(getAndUpdate))
      else None

    // Use the new method to retrieve the element and apply the operations
    def getValue: Option[Int] =
      getExpressionElement.flatMap { str =>
        if str.length == 1 && operators.contains(str.charAt(0)) then // is an operator
          logger.info(s"Evaluating '$str' -> OPERATOR")
          None
        else if isLabel(str) then
          val labVal = LabelFactory.labelValue(str)
          logger.info(s"Evaluating label '$str' -> $labVal")
          labVal
        else
          val numericVal = numericValue(str)
          logger.info(s"Evaluating numeric value '$str' -> $numericVal")
          numericVal
      }

    def isSingleCharOperator(str: String): Boolean =
      str.length == 1 && operators.contains(str.charAt(0))

    def getOperation: Option[Operation] =
      getExpressionElement.flatMap { expressionElement =>
        if isSingleCharOperator(expressionElement) then
          Some(Operation.valueOf(expressionElement))
        else
          None
      }

    if expression == null || expression.isEmpty then
      return Left("Expression is emprty")

    result = getValue
    while {
      val op = getOperation
      if op.isEmpty then
        val x = result.getOrElse(-99)
        return Right(result.get)
      else
        val operand = getValue
        result = Operator(result.get, operand.get, op.get).compute
        true
    } do ()

    Right(result.get)

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


