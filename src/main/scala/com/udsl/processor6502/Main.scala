package com.udsl.processor6502:

  import com.udsl.processor6502.Dialogues.theStage
  import com.udsl.processor6502.ui.popups.Executor
  import com.udsl.processor6502.ui.{FooterBox, MemoryBox, NumericFormatSelector, RegistersBox}
  import javafx.beans.value.ChangeListener
  import scalafx.application.JFXApp3
  import scalafx.beans.value.ObservableValue
  import scalafx.geometry.Insets
  import scalafx.scene.Scene
  import scalafx.scene.layout.*
  import scalafx.stage.FileChooser

  import java.io.File


  object Main extends JFXApp3 {

    def start(): Unit = {
      val registersBox = new RegistersBox()

      stage = new JFXApp3.PrimaryStage {

        title.value = "6502 Processor"
        width = 700
        height = 800
        resizable = false

        scene = new Scene {
          root =
            val memoryBox = new MemoryBox()

            val footer = new FooterBox()

            new BorderPane {
              maxWidth = 400
              maxHeight = 300
              padding = Insets(20)
              top = NumericFormatSelector.apply
              left = registersBox
              right = memoryBox
              bottom = footer
            }
        }


      }

      theStage = stage

      import scalafx.beans.binding.BindingIncludes.closure2ChangedListener
      import javafx.beans.value.ChangeListener

      val mainFocus = (o: javafx.beans.Observable, oldVal: java.lang.Boolean, newVal: java.lang.Boolean) => {
        def foo(o: javafx.beans.Observable, oldVal: java.lang.Boolean, newVal: java.lang.Boolean) =
          println(newVal)
          if newVal then
            Executor.toBack()
        foo(o, oldVal, newVal)
      }

      val x = stage.focusedProperty()
      x.addListener(mainFocus)
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

  }
