package com.udsl.processor6502.assembler

import scala.collection.mutable.ListBuffer

trait Label(val name: String) :
  def evaluate: Int

/**
 * as the value of a label can be dependent on the position it may forward reference and not know on the first pass
 * so we may need to update the value on the second pass
  * @param name label name must be unique
 * @param value the value associated with this label
 */
class ValueLabel(name: String, var value: Int) extends Label(name):
  override def evaluate: Int =
    this.value

/**
 * An expresion lable is defined using an expression or a lable. The expression never changes
 * @param name the name of the label
 * @param expression the labels expression
 */
class ExpressionLabel(name: String, val expression: String) extends Label(name):
  override def evaluate: Int =
    // TODO Evaluate the expression string
    0

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

  def addLabel(name: String, expression: String): Boolean =
    if labelIsDefined(name) then
      return false
    labels.addOne(new ExpressionLabel(name, expression))
    true

  def labelIsDefined(name: String): Boolean =
    labels.exists(item => item.name.equals(name))

  def labelValue(name: String): Option[Int] =
    labels.filter(_.name.equals(name)) match
      case label => Some(label.last.evaluate)
      case _ => None

