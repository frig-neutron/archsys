name := "archsys-sbt"

version := "1.0"

scalaVersion := "2.11.4"

libraryDependencies ++= {
  Seq(
    "ch.qos.logback" % "logback-classic" % "1.1.3" % "compile->default" // Logging
  )
}