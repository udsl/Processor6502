package com.udsl.processor6502:

  import com.udsl.processor6502.*
  import com.udsl.processor6502.cpu.*
  import com.udsl.processor6502.ui.NumericFormatSelector.numericFormatProperty
  import com.udsl.processor6502.config.ConfigDatum
  import scalafx.application.Platform
  import scalafx.scene.control.Alert.AlertType
  import scalafx.scene.control.{Alert, ButtonType, TextInputDialog}
  import scalafx.scene.input.{KeyEvent, MouseEvent}

  import java.io._
  import scala.collection.mutable.{ArrayBuffer, ListBuffer}
  import scala.io.Source

  object Utilities {
    var currentFormat: NumericFormatType = NumericFormatType.DEC

    def stringToNum(text: String): Int = {
      currentFormat match {
        case NumericFormatType.HEX => Integer.parseInt(text, 16)
        case NumericFormatType.OCT => Integer.parseInt(text, 8)
        case NumericFormatType.BIN => Integer.parseInt(text, 2)
        case NumericFormatType.DEC => Integer.parseInt(text, 10)
        case _ => -1
      }
    }

    def numToString( value: Int): String = {
      (numericFormatProperty.value) match {
        case NumericFormatType.HEX => value.toHexString.toUpperCase
        case NumericFormatType.OCT => value.toOctalString
        case NumericFormatType.BIN => value.toBinaryString
        case NumericFormatType.DEC => value.toString
      }
    }

    def getAddressSettingDialogue(dialogueTitle: String, currentValue: Int): TextInputDialog = {
      var lastChange = ""
      var lastFormat: NumericFormatType = currentFormat

      new TextInputDialog(defaultValue = s"${Processor.pc.toString}") {
        initOwner(Main.stage)
        title = dialogueTitle
        headerText = s"Current format is: ${currentFormat}"
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
            case NumericFormatType.HEX => "0123456789ABCDEF".contains(k.getChar.toUpperCase)
            case NumericFormatType.OCT => "01234567".contains(k.getChar)
            case NumericFormatType.BIN => "10".contains(k.getChar)
            case NumericFormatType.DEC => "0123456789".contains(k.getChar)
            case _ => false
          }) || (k.getCode == 8) || (k.getCode == 127) // TODO add other key codes for valid keys
          println(s"${et.getName} occurred ${e.getCode} which is ${if (lastKeyValid) "VALID" else "INVALID"}")
        }

        editor.text.onChange({
          (_, oldValue, newValue) =>
            println(s"${oldValue} => ${newValue} last keyCode: ${lastKeyCode}")
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
                println(s"$oldValue => $newValue")
                if (lastFormat.equals(currentFormat)) { // not due to changing format
                  if (newValue.length > oldValue.length) { // if text is longer then user has typed a char
                    // That char could be at the end or inserted anywhere
                    val change = newValue.toUpperCase
                    val k = change diff oldValue
                    if (currentFormat match {
                      case NumericFormatType.HEX => "ABCDEF0123456789".contains(k) && change.length <= 4
                      case NumericFormatType.OCT => "01234567".contains(k) && change.length <= 6
                      case NumericFormatType.BIN => "10".contains(k) && change.length <= 16
                      case NumericFormatType.DEC => "0123456789".contains(k) && change.length <= 5 && stringToNum(change) <= 65535
                      case _ => false
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

    /**
     * write a `Seq[String]` to the `filename` with a terminating CR
     */
    def writeConfigFile(data: List[ConfigDatum]): Unit = {
      val saveFile = Main.selectConfigFileToSave
      if (saveFile != null) {
        val lines = data.map(f => f.toString())
        val bw = new BufferedWriter(new FileWriter(saveFile))
        for (line <- lines) {
          bw.write(line)
          bw.write("\n")
        }
        bw.close()
      }
    }

    /**
     * read a file returning a seq of strings
     * @param filename the name of the file to read.
     */
    def readConfigFile: List[ConfigDatum] ={
      var r: ListBuffer[ConfigDatum] = ListBuffer[ConfigDatum]()
      val configFile = Main.selectConfigFileToLoad
      val bufferedSource = Source.fromFile(configFile)
      for (line <- bufferedSource.getLines) {
        val colonIndex = line.indexOf(':')
        val key = line.substring(0, colonIndex)
        val value = line.substring(colonIndex+1)
        r += ConfigDatum.apply(key, value)
      }

      bufferedSource.close
      r.toList
    }

    def getConfigValue(lines: List[ConfigDatum], configKey: String, default: String): String = {
      val datum = lines.find(_.key == configKey)

      datum match {
        case Some(datum) => datum.value
        case None  => default
      }
    }


    def confirmation(header: String, content: String = "Are you ok with this?"): Boolean = {
      val alert = new Alert(AlertType.Confirmation) {
        initOwner(Main.stage)
        title = "Confirmation Dialog"
        headerText = header
        contentText = content
      }

      val result = alert.showAndWait()

      result match {
        case Some(ButtonType.OK) => println("OK") ; true
        case _ => println("Cancel or closed") ; false
      }
    }
  }
