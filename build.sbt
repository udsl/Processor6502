name := "Processor6502"

version := "0.1.1"

scalaVersion := "3.1.3"


// https://mvnrepository.com/artifact/org.scalafx/scalafx
libraryDependencies += "org.scalafx" %% "scalafx" % "18.0.1-R27"

// https://mvnrepository.com/artifact/org.scalacheck/scalacheck
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.16.0" % Test

// https://mvnrepository.com/artifact/org.scalactic/scalactic
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.12"
// https://mvnrepository.com/artifact/org.scalatest/scalatest
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % Test

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4"

libraryDependencies ++= {
  // Determine OS version of JavaFX binaries
  lazy val osName = System.getProperty("os.name") match {
    case n if n.startsWith("Linux") => "linux"
    case n if n.startsWith("Mac") => "mac"
    case n if n.startsWith("Windows") => "win"
    case _ => throw new Exception("Unknown platform!")
  }
  Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
    .map(m => "org.openjfx" % s"javafx-$m" % "16" classifier osName)
}

// Fork a new JVM for 'run' and 'test:run', to avoid JavaFX double initialization problems
fork := true

ThisBuild / assemblyMergeStrategy  := {
  case PathList("module-info.class") => MergeStrategy.discard
  case x if x.endsWith("/module-info.class") => MergeStrategy.discard
  case x =>
    val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
    oldStrategy(x)
}

