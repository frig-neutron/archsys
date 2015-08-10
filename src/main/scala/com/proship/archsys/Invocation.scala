package com.proship.archsys

case class Invocation(args: Array[String]) {
  private def getOptParam(name: String) : Option[String] = {
    val prefix = "--"+name+"="
    args filter (_ startsWith prefix) match {
      case Array() => None
      case Array(x) => Some(x replace (prefix, ""))
      case _ => badUsage
    }
  }
  private def getParam(name: String) = {
    val params = getOptParam(name)
    params match {
      case None => badUsage
      case Some(x) => x
    }
  }
  private def badUsage = throw new IllegalArgumentException("usage: archsys.scala --volumes=/vol1:/vol1/sub2:... --howToRead=dd|rsync:/mnt/at|tar:/mnt/at [--debug=y]")
  private def getReaderTypeAndMountLocation(howToRead: String): (String, String) =
    howToRead.split(":") match {
      case Array("rsync", x) => ("rsync", x)
      case Array("tar", x) => ("tar", x)
      case Array("dd") => ("dd", null)
      case _ => badUsage
    }
  lazy val (readerType, mountAt) = getReaderTypeAndMountLocation(getParam("howToRead"))
  lazy val volumes = getParam("volumes").split(":").toList
  lazy val debug = getOptParam("debug") match {
    case None => false
    case Some(x) => x startsWith "y"
  }
}