package com.udsl.processor6502.assemblier2

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.Utilities.{isLabel, numericValue}
import com.udsl.processor6502.cpu.execution.AddressingMode

import scala.collection.mutable.ListBuffer

trait Token (val fields: Array[String] ):
  val predictedAddressingModes: ListBuffer[AddressingMode] = ListBuffer[AddressingMode]()
  val name: String

  def display : String = s"${name} -> ${fields.mkString(", ")}"

  private def canEqual(other: Any) = other.isInstanceOf[Token]

  def fieldsAreEqual(thatFields:Array[String]): Boolean =
    fields.length == thatFields.length && fields.forall(p => {
      thatFields.contains(p)
    })

  override def equals(other: Any): Boolean = other match {
    case that: Token =>
      that.canEqual(this) &&
        this.name == that.name &&
        fieldsAreEqual(that.fields)
    case _ => false
  }


case class BlankLineToken(override val fields: Array[String] ) extends Token(fields: Array[String] ):
  override val name: String = "BlankLineToken"

case class CommentLineToken(override val fields: Array[String] ) extends Token(fields: Array[String] ):
  override val name: String = "CommentLineToken"

case class LineCommentToken (override val fields: Array[String]) extends Token(fields: Array[String] ):
  override val name: String = "LineCommentToken"

case class LabelToken (override val fields: Array[String]) extends Token(fields: Array[String] ):
  override val name: String = "LabelToken"

case class CommandToken (command: String, override val fields: Array[String]) extends Token(fields: Array[String] ):
  override val name: String = "CommandToken"

case class InstructionToken (mnemonic: String, override val fields: Array[String]) extends Token(fields: Array[String] ) with StrictLogging:
  override val name: String = "InstructionToken"
  override def toString = display



case class NoTokenToken (override val fields: Array[String]) extends Token(fields: Array[String] ):
  override val name: String = "BlankLineToken"

object Tokeniser :
  def tockenise(line: String) : List[Token] =
    var tokens = List[Token]()
    val toTokenise = line.trim
    // Blank line
    if toTokenise == "" then
      tokens ::= BlankLineToken.apply(Array(""))
    // line comment
    else if toTokenise.head == ';' then
      tokens ::= CommentLineToken.apply(Array(toTokenise.tail))
    else
      // Does this line have a comment
      val commentSplit = toTokenise.split(";")
      if commentSplit.length > 1 then
        tokens ::= LineCommentToken.apply(Array(toTokenise.tail.mkString))
      // is it a command
      val head = commentSplit.head.toUpperCase
      if !(head match {
        case "ADDR" | "BYT" | "WRD" | "ORIG" | "CLR" | "DEF" =>
          tokens ::= CommandToken.apply(head, Array(commentSplit.tail.mkString(" ")))
          true
        case _ => false
      }) then
        // Dose this line have a label
        val lableSplit = toTokenise.split(":")
        val ins: String = if isLabel(lableSplit.head) then
          tokens ::= LabelToken.apply(Array(head, commentSplit.tail.mkString(" ")))
          lableSplit.tail.mkString(" ")
        else
          toTokenise
        // is this line an instruction
        val fields = ins.split("\\s+")
        tokens ::= InstructionToken.apply(fields.head, fields.tail)
    tokens


