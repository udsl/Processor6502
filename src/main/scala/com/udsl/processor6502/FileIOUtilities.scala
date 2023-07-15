package com.udsl.processor6502

import com.udsl.processor6502.Dialogues.theStage
import com.udsl.processor6502.assembler.version1.TokenisedLine
import com.udsl.processor6502.config.ConfigDatum
import scalafx.stage.FileChooser

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable.ListBuffer
import scala.io.Source

object FileIOUtilities:
  def writeToFile(file: File, tokenisedList: List[TokenisedLine]): Unit =
    val bw = new BufferedWriter(new FileWriter(file))
    for (token <- tokenisedList)
      bw.write(token.toString)
      bw.write("\n")
    bw.close()

  def writeStringToFile(file: File, str: String): Unit =
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(str)
    bw.close()

  def writeStringToFile(file: File, str: List[String]): Unit =
    val bw = new BufferedWriter(new FileWriter(file))
    for (s <- str)
      bw.write(s)
    bw.close()

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
    chooser.setInitialDirectory(new File("."))
    chooser.showSaveDialog(theStage)

  private def getChosenLoadFile(chooser: FileChooser): File =
    chooser.setInitialDirectory(new File("."))
    chooser.showOpenDialog(theStage)

  def selectSourceFileToSave: Option[File] =
    Option(getChosenSaveFile(getSourceFileChooser))

  def selectMemoryImageFileToSave: Option[File] =
    Option(getChosenSaveFile(getDumpFileChooser))

  def selectMemoryImageFileToLoad: File =
    getChosenLoadFile(getDumpFileChooser)

  def selectSourceFileToLoad: Option[File] =
    Option(getChosenLoadFile(getSourceFileChooser))

  private def getSourceFileChooser: FileChooser =
    val chooser = new FileChooser
    val saveFilter = new FileChooser.ExtensionFilter("Code Save Files", "*.asm")
    chooser.getExtensionFilters.add(saveFilter)
    chooser

  private def getDumpFileChooser: FileChooser =
    val chooser = new FileChooser
    val saveFilter = new FileChooser.ExtensionFilter("Memory Image Files", "*.dmp")
    chooser.getExtensionFilters.add(saveFilter)
    chooser
  /**
   * write a `Seq[String]` to the `filename` with a terminating CR
   */
  def writeConfigFile(data: List[ConfigDatum]): Boolean =
    val saveFile = selectConfigFileToSave
    if saveFile != null then
      val lines = data.map(f => f.toString())
      val bw = new BufferedWriter(new FileWriter(saveFile))
      for line <- lines do
        bw.write(line)
        bw.write("\n")
      bw.close()
      true
    else
      false


  /**
   * read the selected config file returning a seq of strings
   *
   */
  def readConfigFile: List[ConfigDatum] =
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

