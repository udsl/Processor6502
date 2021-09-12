name := "Processor6502"

version := "0.1"

scalaVersion := "2.13.6"

// https://mvnrepository.com/artifact/org.scalafx/scalafx
libraryDependencies += "org.scalafx" %% "scalafx" % "15.0.1-R21"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"

// Add OS specific JavaFX dependencies
val javafxModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux") => "linux"
  case n if n.startsWith("Mac") => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}
libraryDependencies ++= javafxModules.map(m => "org.openjfx" % s"javafx-$m" % "15.0.1" classifier osName)

// Fork a new JVM for 'run' and 'test:run', to avoid JavaFX double initialization problems
fork := true

