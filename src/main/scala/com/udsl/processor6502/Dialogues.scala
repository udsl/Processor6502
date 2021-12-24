package com.udsl.processor6502

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.NumericFormatType.{BIN, DEC, HEX, OCT}
import com.udsl.processor6502.Utilities.{currentFormat, numToString, stringToNum}
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

  def errorAlert(header: String, content: String = "Please correct the issue and assemble again."): Unit = {
    val alert = new Alert(AlertType.Error) {
      initOwner(theStage)
      title = "Error Alert"
      headerText = header
      contentText = content
    }

    alert.showAndWait()

    //    result match
    //      case Some(ButtonType.OK) => logger.info("OK"); true
    //      case _ => logger.info("Cancel or closed"); false
  }

  def getAddressSettingDialogue(dialogueTitle: String, currentValue: Int): TextInputDialog = {
    var lastChange = ""
    var lastFormat: NumericFormatType = currentFormat
    
    new TextInputDialog(defaultValue = "100"){ // todo correct this: String.valueOf(Main.pc.toString)) {
      initOwner(theStage)
      title = dialogueTitle
      headerText = s"Current format is: $currentFormat"
      contentText = "New value:"

      var lastKeyValid = false
      var lastKeyCode = 8
      editor.text = numToString(currentValue)

      editor.onKeyPressed = e => {
        val et = e.getEventType
        et.getName
        val k = e.getCode
        lastKeyCode = k.getCode
        lastKeyValid = (currentFormat match {
          case HEX => "0123456789ABCDEF".contains(k.getChar.toUpperCase)
          case OCT => "01234567".contains(k.getChar)
          case BIN => "10".contains(k.getChar)
          case DEC => "0123456789".contains(k.getChar)
          //            case _ => false
        }) || (k.getCode == 8) || (k.getCode == 127) // TODO add other key codes for valid keys
        logger.info(s"${et.getName} occurred ${e.getCode} which is ${if (lastKeyValid) "VALID" else "INVALID"}")
      }

      editor.text.onChange({
        (_, oldValue, newValue) =>
          logger.info(s"$oldValue => $newValue last keyCode: $lastKeyCode")
          if (!oldValue.equals(newValue)) {
            if (!lastKeyValid) {
              Platform.runLater(new Runnable() {
                override def run(): Unit = {
                  //TODO deal with repositioning the cursor!
                  editor.text = lastChange
                }
              })
            }
            else if (!lastChange.equals(newValue)) { // Only the is a change in the text
              logger.info(s"$oldValue => $newValue")
              if (lastFormat.equals(currentFormat)) { // not due to changing format
                if (newValue.length > oldValue.length) { // if text is longer then user has typed a char
                  // That char could be at the end or inserted anywhere
                  val change = newValue.toUpperCase
                  val k = change diff oldValue
                  if (currentFormat match {
                    case HEX => "ABCDEF0123456789".contains(k) && change.length <= 4
                    case OCT => "01234567".contains(k) && change.length <= 6
                    case BIN => "10".contains(k) && change.length <= 16
                    case DEC => "0123456789".contains(k) && change.length <= 5 && stringToNum(change) <= 65535
                    //                      case _ => false
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
            }
          }
      })
    }
  }

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
    chooser.showSaveDialog(theStage)

  private def getChosenLoadFile(chooser: FileChooser): File =
    chooser.setInitialDirectory(new File("."));
    chooser.showOpenDialog(theStage)

  def selectSourceFileToSave: File =
    getChosenSaveFile(getSourceFileChooser)

  def selectSourceFileToLoad: File =
    getChosenLoadFile(getSourceFileChooser)

  private def getSourceFileChooser: FileChooser =
    val chooser = new FileChooser
    val saveFilter = new FileChooser.ExtensionFilter("Code Save Files", "*.asm")
    chooser.getExtensionFilters.add(saveFilter)
    chooser

  /**
   * write a `Seq[String]` to the `filename` with a terminating CR
   */
  def writeConfigFile(data: List[ConfigDatum]): Unit = {
    val saveFile = selectConfigFileToSave
    if saveFile != null then
      val lines = data.map(f => f.toString())
      val bw = new BufferedWriter(new FileWriter(saveFile))
      for line <- lines do
        bw.write(line)
        bw.write("\n")
      bw.close()
  }

  /**
   * read the selected config file returning a seq of strings
   *
   */
  def readConfigFile: List[ConfigDatum] = {
    val r: ListBuffer[ConfigDatum] = ListBuffer[ConfigDatum]()
    val configFile = selectConfigFileToLoad
    if configFile != null then
      val bufferedSource = Source.fromFile(configFile)
      for (line <- bufferedSource.getLines) {
        val colonIndex = line.indexOf(':')
        val key = line.substring(0, colonIndex)
        val value = line.substring(colonIndex + 1)
        r += ConfigDatum.apply(key, value)
      }

      bufferedSource.close
    r.toList
  }

  def confirmation(header: String, content: String = "Are you ok with this?"): Boolean = {
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
  }



