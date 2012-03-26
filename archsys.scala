#!/bin/bash
  exec scala "$0" "$@"
!#
/**
 * Script to facilitate the archival of a complete or 
 * partial system using a variety of methods.
 *
 * Archival methods: 
 *  - rsync
 *  - 
 *  - compressed binary snapshot
 * 
 * Suppors archival of volumes using LVM (preferred) or
 * raw block devices (sometimes necessary).
 */

implicit def arrayToList[A](arr: Array[A]) = arr.toList

/**
 * Keep all the system-interface operations here
 */
object Sys {
  import scala.sys.process._
  import java.io.File

  implicit def stringToFile(path: String) : File = new File(path)
  implicit def stringBuilderToString(strb: StringBuilder) : String = strb.toString

  object DevNull extends ProcessLogger { 
    def buffer[T](f: => T): T = f
    def out(s: => String): Unit = ()
    def err(s: => String): Unit = ()
  }

  class StringProcessLogger extends ProcessLogger {
    private val outB = new StringBuilder
    private val errB = new StringBuilder

    private def appendLine(b: StringBuilder, s: String) = b append s append "\n"
    def buffer[T](f: => T): T = f
    def out(s: => String): Unit = appendLine(outB, s)
    def err(s: => String): Unit = appendLine(errB, s)

    def getOut : String = outB.trim
    def getErr : String = errB.trim

    private def linesOf(str: String) = str split "\n" map (_.trim)
    def getOutLines : List[String] = linesOf(getOut)
    def getErrLines : List[String] = linesOf(getErr)
  }

  def mounts = "mount" !!

  def isBlockDevice(dev: String) = {
    val log = new StringProcessLogger
    "file -b "+dev ! log // don't exception if retcode != 0
    log.getOut == "block special"
  }

  def isLvmManaged(dev: String) = 
    isBlockDevice(dev) && 
    ("lvdisplay "+dev ! DevNull) == 0 // don't exception if retcode != 0

  def getLvInfo(dev: String) = { 
    val log = new StringProcessLogger
    "lvdisplay -C "+dev ! log // don't exception if retcode != 0
    val bits = log.getOutLines(1).trim split " +" slice (0,2)
    new LvInfo(bits(1), bits(0))
  }

  def createLvSnap(src: LvInfo, dst: LvInfo) : Unit = {
    // _DO_ exception if retcode != 0
    println ("lvcreate -L 2G --snapshot --name "+dst.lv+" /dev/"+src.vg+"/"+src.lv)
  }

  // _DO_ exception if retcode != 0
  def mount(dev: String, path: String) = ()

  def destroyLv(lv: LvInfo) = { 
    // _DO_ exception if retcode != 0
    println ("deactivate")
    println ("kill!!!")
    ()
  }
}

class LvInfo(val vg: String, val lv: String)

object Volume {
  def apply(mountInfo: Tuple2[String,String]) : Volume = {
    val dev = mountInfo._1
    val path = mountInfo._2
    if (Sys.isLvmManaged(dev))
      LvmVolume(dev,path)
    else if (Sys.isBlockDevice(dev))
      RawVolume(dev, path)
    else 
      throw new IllegalArgumentException("not a real block device: "+dev)
  }

  case class LvmVolume(dev: String, path: String) extends Volume(dev, path) {

    val liveLv = Sys.getLvInfo(dev)
    val snapLv = new LvInfo(
      liveLv.vg,
      liveLv.lv+"-snap" )

    def acquire {
      println("mount " + this)
      Sys.createLvSnap(liveLv, snapLv)
    }
    def release = println ("unmount " + this)

    override def toString = super.toString+":vg="+liveLv.vg+":lv="+liveLv.lv
  }

  case class RawVolume(dev: String, path: String) extends Volume(dev, path) {
    def acquire = println("mount " + this)
    def release = println ("unmount " + this)
  }
}

/**
 * Basic archival volume abstraction.  
 * Can be acquired and released.
 */
abstract class Volume(dev: String, path: String) {
  def acquire : Unit
  def release : Unit

  override def toString = getClass+":"+dev+"@"+path
}


def inspectMountPoint (mountPath: String) : Volume = { 
  val mounts : Array[String] = Sys.mounts split "\n"

  val filtered = mounts filter (
    line => { 
      val fields = line split " +" map (_.trim)
      fields(2) == mountPath
    }) 

  val mountInfo : Array[Tuple2[String,String]] = filtered map (
    line => {
      val fields = line split " +" map (_.trim)
      (fields(0), fields(2))
    })

  if (mountInfo.isEmpty) 
    throw new IllegalArgumentException("nothing mounted at "+mountPath)

  def stringify(mounts: Array[Tuple2[String,String]]) = 
    mounts map (m => m._1+"@"+m._2) reduce (_+"\n"+_)

  if (mountInfo.size > 1)
    throw new IllegalStateException(
      "more than one thing mounted at "+mountPath+".\n"+
      "ambiguous host configuration.\n"+
      stringify(mountInfo))

  Volume(mountInfo(0))
}

case class Invocation(args: Array[String]) { 
  private def getParam(name: String) = {
    val prefix = "--"+name+"="
    val params = args filter (_ startsWith prefix)
    if (params.isEmpty || params.size > 1)
      throw new IllegalArgumentException("TODO: usage: ...blah..blah..blah...")   
    else
      params(0).replace(prefix, "")
  }
  lazy val readerType = getParam("readWith")
  lazy val volumes = getParam("volumes") split ":" toList
  lazy val mountAt = getParam("mountAt")
}
// --readWith=... --volumes=/:/usr:/var
object Invocation extends Invocation(args)

println (Invocation.readerType)
println (Invocation.mountAt)
println (Invocation.volumes)
System.exit(0)

val volumes : List[Volume] = Invoication.volumes map inspectMountPoint 

object VolumeReader {
    def apply(vols: List[Volume], howToRead: String) = { 
      RsyncReader(vols.head)
    }

    /**
     * RsyncReader requires an xinetd configuration to function.
     * The configuration should be the same as regular rsync, but with
     */
    case class RsyncReader(vol: Volume) extends VolumeReader(vol) {
      def read = ()
    }
    /**
     * since BinaryReader does not require (and in fact abhors filesystem
     * -level access, it only makes sense to use it if
     * 1. only one volume is selected for backup
     * 2. volume not mounted (or mounted r/o)
     */
    case class BinaryReader(vol: Volume) extends VolumeReader(vol) {
      def read = ()
    }
    case class TarballReader(vol: Volume) extends VolumeReader(vol) {
      def read = ()
    }
}
/**
 * if reader reads FSH, only root specified
 * if reader reads block devs, max_size(volumes) == 1
 */ 
abstract class VolumeReader(vol: Volume){ 
  def read : Unit // execs some system process on volume
}

val reader = VolumeReader(volumes, "thecmd")


def loan(volumes: List[Volume]) : Unit = 
  if (volumes.isEmpty)
    reader.read // payload
  else {
    val vol = volumes.head
    try {
      vol.acquire
      loan(volumes.tail)
    }
    finally {
      vol.release
    }
  }

loan(volumes)
