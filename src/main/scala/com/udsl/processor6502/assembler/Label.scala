package com.udsl.processor6502.assembler

import com.udsl.processor6502.Utilities.{isLabel, numericValue, operators}

import scala.collection.mutable.ListBuffer
import scala.compiletime.asMatchable

enum Operation {
  case +, -, *, /
  def valueOf(s: String): Option[Operation] = Operation.values.find(_.toString == s)
}

case class Operator(a: Int, b: Int, operation: Operation) {
  def compute: Int = operation match {
    case Operation.+ => a + b
    case Operation.- => a - b
    case Operation.* => a * b
    case Operation./ => a / b
  }
}

trait Label(val name: String) :
  def evaluate: Int
  def hasValue: Boolean

/**
 * Provides the expression evaluator to labels
 * Expression is a list of values and labels seperated by operators
 */
trait LabelExpression(name: String) extends Label:
  protected def evaluateExpression(expression: Array[String]): Int =
    var currentIndex = 0
    var result: Int = 92

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
          None
        else if isLabel(str) then
          LabelFactory.labelValue(str)
        else
          numericValue(str)
      }

    if expression == null || expression.isEmpty then
      throw new AssemblerException(s"BAD Label $name", "expression not set")

    result = getValue.get
    while{
      val op = getOperation
      if op.isEmpty then
        return result
      else
        val operand = getValue
        result = Operator(result, operand.get, op.get).compute
        true
    }do()

    def isSingleCharOperator(str: String): Boolean =
      str.length == 1 && operators.contains(str.charAt(0))

    def getOperation: Option[Operation] =
      getExpressionElement.flatMap { expressionElement =>
        if isSingleCharOperator(expressionElement) then
          Some(Operation.valueOf(expressionElement))
        else
          None
      }

    result

/**
 * as the value of a label can be dependent on the position it may forward reference and not know on the first pass
 * so we may need to update the value on the second pass
  * @param name label name must be unique
 * @param value the value associated with this label
 */
class ValueLabel(name: String, var value: Int) extends Label(name):
  override def evaluate: Int =
    this.value

  override def hasValue = true
  
/**
 * An expresion lable is defined using an expression or a lable. The expression never changes
 * @param name the name of the label
 * @param expression the labels expression
 */
class ExpressionLabel(name: String, val expression: Array[String]) extends Label(name) with LabelExpression(name):
  override def evaluate: Int =
    evaluateExpression(expression)

  override def hasValue: Boolean = Option(expression) match
    case Some(_) => true
    case None => false

class ForwardReferenceLabel(name: String) extends Label(name), LabelExpression(name):
  var expression: Array[String] = _
  override def evaluate: Int =
    evaluateExpression(expression)

  override def hasValue: Boolean = Option(expression) match
    case Some(_) => true
    case None => false
    
object LabelFactory:
  val labels = new ListBuffer[Label]

//  def createLabel(name: String, value: Int): Label ???

  def clear(): Unit =
    labels.clear()

  def count: Int =
    labels.length


  def addLabel(name: String, value: Int): Boolean =
    if labelIsDefined(name) then
      return false
    labels.addOne(new ValueLabel(name, value))
    true

  def addLabel(name: String, expression: Array[String]): Boolean =
    if labelIsDefined(name) then
      return false
    labels.addOne(new ExpressionLabel(name, expression))
    true

  def addLabel(name: String): Boolean =
    if labelIsDefined(name) then
      return false
    labels.addOne(new ForwardReferenceLabel(name))
    true


  def labelIsDefined(name: String): Boolean =
    labels.exists(item => item.name.equals(name))

  def labelValue(name: String): Option[Int] =
    val fiteredList = labels.filter(_.name.equals(name))
    if fiteredList.nonEmpty then
      Some(fiteredList.last.evaluate)
    else
      None

