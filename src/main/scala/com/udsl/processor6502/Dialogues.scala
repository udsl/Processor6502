package com.udsl.processor6502

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.NumericFormatType.{BIN, DEC, HEX, OCT}
import com.udsl.processor6502.Utilities.{currentFormat, numToString, stringToNum, verifyNumberEntry}
import com.udsl.processor6502.config.ConfigDatum
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.application.Platform
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, ButtonType, TextInputDialog}
import scalafx.stage.FileChooser

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Dialogues extends StrictLogging:
  var theStage: PrimaryStage = null

  def errorAlert(header: String, content: String = "Please correct the issue and assemble again."): Unit =
    val alert = new Alert(AlertType.Error) {
      initOwner(theStage)
      title = "Error Alert"
      headerText = header
      contentText = content
    }

    alert.showAndWait()

  def getNumberSettingDialogue(dialogueTitle: String, currentValue: Int, addressNotByte: Boolean = true): TextInputDialog = {
    var lastChange = ""
    var lastFormat: NumericFormatType = currentFormat

    def validateCharacterInString(toValidate: String): Boolean =
      verifyNumberEntry(toValidate)._1


    new TextInputDialog(){
      initOwner(theStage)
      title = dialogueTitle
      headerText = s"Current format is: $currentFormat"
      contentText = "New value:"

      var lastKeyValid = false
      var lastKeyCode = 8
      var currentCaretPos: Int = editor.getCaretPosition

      editor.text = numToString(currentValue)

      editor.onKeyPressed = e =>
        val et = e.getEventType
        et.getName
        val k = e.getCode
        lastKeyCode = k.getCode
        lastKeyValid = (currentFormat match {
          case HEX => "0123456789ABCDEF".contains(k.getChar.toUpperCase)
          case OCT => "01234567".contains(k.getChar)
          case BIN => "10".contains(k.getChar)
          case DEC => "0123456789".contains(k.getChar)
        }) || (k.getCode == 8) || (k.getCode == 127) // TODO add other key codes for valid keys
        logger.info(s"${et.getName} occurred ${e.getCode} which is ${if (lastKeyValid) "VALID" else "INVALID"}")

      editor.text.onChange({
        (_, oldValue: String, newValue: String) =>
          logger.info(s"$oldValue => $newValue last keyCode: $lastKeyCode")

          if !oldValue.equals(newValue) then
            // If the last key press was not valid or the string contains a none valid character restore the old text.
            if !lastKeyValid || !validateCharacterInString(newValue.toUpperCase()) then
              Platform.runLater(new Runnable() {
                override def run(): Unit = {
                  //TODO deal with repositioning the cursor!
                  val caretPos: Int = editor.getCaretPosition
                  editor.text = lastChange
                  editor.positionCaret(caretPos - 1)
                }
              })
            else if lastChange.equals(newValue) then // Only the is a change in the text
              logger.info(s"$oldValue => $newValue")
              if (lastFormat.equals(currentFormat)) { // not due to changing format
                if (newValue.length > oldValue.length) { // if text is longer then user has typed a char
                  // That char could be at the end or inserted anywhere
                  val change = newValue.toUpperCase
                  val k = change diff oldValue
                  if (currentFormat match {
                    case HEX => "ABCDEF0123456789".contains(k) && change.length <= (if addressNotByte then 4 else 2)
                    case OCT => "01234567".contains(k) && change.length <= (if addressNotByte then 6 else 3)
                    case BIN => "10".contains(k) && change.length <= (if addressNotByte then 16 else 8)
                    case DEC => "0123456789".contains(k) && change.length <= (if addressNotByte then 5 else 3) && stringToNum(change) <= (if addressNotByte then 65535 else 255)
                  }) {
                    editor.text.value = change
                    lastChange = change
                  }
                  else {
                    editor.text = oldValue
                  }
                }
                else { // they have over typed or deleted one and that could be a case change
                  lastChange = newValue.toUpperCase
                  editor.text.value = lastChange // make sure the case is correct
                }
              }
              else { // format has changed
                lastFormat = currentFormat
                lastChange = newValue.toUpperCase
              }
      })
    }
  }

  def confirmation(header: String, content: String = "Are you ok with this?"): Boolean =
    val alert = new Alert(AlertType.Confirmation) {
      initOwner(theStage)
      title = "Confirmation Dialog"
      headerText = header
      contentText = content
    }

    val result = alert.showAndWait()

    result match
      case Some(ButtonType.OK) => logger.info("OK"); true
      case _ => logger.info("Cancel or closed"); false
