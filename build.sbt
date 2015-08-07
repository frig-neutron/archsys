import com.typesafe.sbt.packager.linux.LinuxSymlink

name := "archsys"

organization := "com.proship"

version := "0.3.1"

scalaVersion := "2.11.4"

//for sbt-native-packager plugin

maintainer := "Proship IT Team <it@proship.com>"

packageSummary := "Server backup script"

//for sbt-assembly plugin

assemblyJarName in assembly := "archsys.jar"

mainClass in assembly := Some("com.proship.archsys.Runner")

test in assembly := {} //To skip the test during assembly

//for sbt-native-packager plugin

enablePlugins(JavaAppPackaging)

enablePlugins(UniversalPlugin)

enablePlugins(DebianPlugin)

debianPackageDependencies in Debian ++=
  Seq("xz-utils", "tar", "xinetd", "rsync", "mount", "file", "coreutils")

// removes all jar mappings in universal and appends the fat jar
mappings in Universal <<= (mappings in Universal, assembly in Compile) map { (mappings, fatJar) =>
  val filtered = mappings filter { case (file, name) =>  ! name.endsWith(".jar") }
  filtered :+ (fatJar -> ("lib/" + fatJar.getName))
}

// the bash scripts classpath only needs the fat jar
scriptClasspath := Seq( (assemblyJarName in assembly).value )

// We add "$lib_dir/conf/logback.xml" to the classpath and symlink it to /etc/archsys/logback.xml,
// since there is no way to add files outside of $lib_dir to the classpath
scriptClasspath += "conf/logback.xml"
linuxPackageSymlinks += LinuxSymlink("/usr/share/archsys/conf/logback.xml", "/etc/archsys/logback.xml")

//since we provide our own logback.xml to the classpath, we remove it from the archsys.jar
assemblyMergeStrategy in assembly := {
  case PathList("tarExcludes") => MergeStrategy.discard
  case PathList("logback.xml") => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

//lv* scripts are in /sbin, which is not included by default in the path
bashScriptExtraDefines += "export PATH=$PATH:/sbin"

// put in /etc/archsys
// this will mark the files as configuration files in the debian package
linuxPackageMappings ++=
  Seq(
    {
      val src = baseDirectory.value / "src" / "main" / "resources" / "tarExcludes"
      val pkgDst = "/etc/archsys/tarExcludes"
      src -> pkgDst
    },
    {
      val src = baseDirectory.value / "src" / "main" / "resources" / "logback.xml"
      val pkgDst = "/etc/archsys/logback.xml"
      src -> pkgDst
    }
  ) map ( packageMapping(_) withConfig() withPerms("0644") )

linuxPackageMappings ++=
  Seq(
    {
      val src = baseDirectory.value / "src" / "main" / "resources" / "doc"
      val pkgDst = "usr/share/doc/archsys"
      (src, pkgDst)
    }
  ) map (packageDirectoryAndContentsMapping(_))

linuxPackageMappings in Debian := linuxPackageMappings.value

libraryDependencies ++= {
  Seq(
    "ch.qos.logback" % "logback-classic" % "1.1.3" % "compile->default",// Logging
    "org.slf4j" % "slf4j-api" % "1.7.7" % "provided"
  )
}