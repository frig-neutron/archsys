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

    private def linesOf(str: String) = str.trim split "\n" map (_.trim)
    def getOutLines : List[String] = linesOf(outB)
    def getErrLines : List[String] = linesOf(errB)
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
    (bits(1), bits(0))
  }

  def createLvSnap(vg: String, srcLv: String, dstLv: String) : Unit = {
    // _DO_ exception if retcode != 0
    println ("lvcreate -L 2G --snapshot --name "+dstLv+" /dev/"+vg+"/"+srcLv)
  }

  // _DO_ exception if retcode != 0
  def mount(dev: String, path: String) = ()

  def destroyLv(vg: String, lv: String) = { 
    // _DO_ exception if retcode != 0
    println ("deactivate")
    println ("kill!!!")
    ()
  }
}


object Volume {
  def apply(mountPath: String, reader: VolumeReader) : Volume = { 
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

    val (dev, path) = mountInfo(0)
    if (Sys.isLvmManaged(dev))
      new LvmVolume(dev, path, reader)
    else if (Sys.isBlockDevice(dev))
      new RawVolume(dev, path, reader)
    else 
      throw new IllegalArgumentException("not a real block device: "+dev)
  }

  class LvmVolume(dev: String, path: String, reader: VolumeReader) extends Volume(dev, path, reader) {
    val (vg, liveLv) = Sys.getLvInfo(dev)
    val snapLv = liveLv+"-snap"

    protected def doAcquire() = Sys.createLvSnap(vg, liveLv, snapLv)
    protected def doRelease() = println ("release " + this)
    protected def doMount() = println("mount " + this)
    protected def doUnmount() = println("unmount " + this)

    override def toString = super.toString+":vg="+vg+":lv="+liveLv
  }

  class RawVolume(dev: String, path: String, reader: VolumeReader) extends Volume(dev, path, reader) {
    protected def doAcquire() = println("acquire " + this)
    protected def doRelease() = println ("release " + this)
    protected def doMount() = println("mount " + this)
    protected def doUnmount() = println("unmount " + this)
  }
}

/**
 * Basic archival volume abstraction.  
 * Can be acquired and released.
 */
abstract class Volume(dev: String, path: String, reader: VolumeReader) {
  protected def doAcquire() : Unit
  protected def doRelease() : Unit
  protected def doMount() : Unit
  protected def doUnmount() : Unit

  def acquire() {
    doAcquire()
    if (reader.needsMounting) {
      doMount()
    }
  }
  def close () {
    if (reader.needsMounting) {
      doUnmount()
    }
    doRelease()
  }

  override def toString = getClass+":"+dev+"@"+path
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
  lazy val readerType = getParam("howToRead")
  lazy val volumes = getParam("volumes") split ":" toList
  lazy val mountAt = getParam("mountAt")
}
// --howToRead=... --volumes=/:/usr:/var
object Invocation extends Invocation(args)

println (Invocation.readerType)
println (Invocation.mountAt)
println (Invocation.volumes)

val volumes : List[Volume] = Invocation.volumes map (Volume(_)) 
val reader = VolumeReader(Invocation.readerType, volumes, Invocation.mountAt)

object VolumeReader {
    def apply(howToRead: String, volumes: List[Volume], mountAt: String): VolumeReader = { 
      howToRead match {
        case "rsync" => new RsyncReader(volumes, mountAt)
        case "binary" => new BinaryReader(volumes.head)
        case "tarball" => new TarballReader(volumes, mountAt)
        case _ => throw new IllegalArgumentException(howToRead+" is not a supported reader type.")
      }
    }

    /**
     * RsyncReader requires an xinetd configuration to function.
     * The configuration should be the same as regular rsync, but with
     */
    class RsyncReader(volumes: List[Volume], mountAt: String) extends VolumeReader(volumes, mountAt, true) {
      def read = ()
    }
    /**
     * since BinaryReader does not require (and in fact abhors filesystem
     * -level access, it only makes sense to use it if
     * 1. only one volume is selected for backup
     * 2. volume not mounted (or mounted r/o)
     */
    class BinaryReader(volume: Volume) extends VolumeReader(List(volume), null, false) {
      def read = ()
    }
    class TarballReader(volumes: List[Volume], mountAt: String) extends VolumeReader(volumes, mountAt, true) {
      def read = ()
    }
}
/**
 * if reader reads FSH, only root specified
 * if reader reads block devs, max_size(volumes) == 1
 */ 
abstract class VolumeReader(val volumes: List[Volume], val mountAt: String, val needsMounting: Boolean) { 
  def read : Unit // execs some system process on volume
}

/**
 * Loan pattern with duck typing for Closeable. Copied directly from 
 * AVBS project.
 */
def using[Closeable <: {def close(): Unit}, B](closeable: Closeable)(getB: Closeable => B) : B = 
  try {
    getB(closeable)
  } finally {
    closeable.close()
  }

def readVolumes(volumes: List[Volume]) {
  if (volumes.isEmpty)
    reader.read
  else { 
    using(volumes.head) { vol => {
        vol.acquire
        readVolumes(volumes.tail)
      }
    }
  }
}

readVolumes(reader.volumes)


/*
def loan(volumes: List[Volume]) : Unit = 
    try {
      if (volumes.isEmpty)
        reader.read // payload
      else {
        volumes.head.acquire
        loan(volumes.tail)
      }
    finally {
      vol.release
    }
  }
def readWithRsync = {
  getClientSocket 
  l2(clientSocket) { 
    startServer
    l2(serverProcess) { 
      openServerSocket
      l2(serverSocket) { 
        // rarara
      }
    }
  }
*/

