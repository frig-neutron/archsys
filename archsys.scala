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
    "file -bL "+dev ! log // don't exception if retcode != 0
    log.getOutLines(0) == "block special"
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

  def createLvSnap(vg: String, srcLv: String, dstLv: String) {
    // _DO_ exception if retcode != 0
    println("lvcreate -L 2G --snapshot --name "+dstLv+" /dev/"+vg+"/"+srcLv)
  }

  // _DO_ exception if retcode != 0
  def mount(dev: String, path: String) {
    println("mount -o ro "+dev+" "+path)
  }

  // _DO_ exception if retcode != 0
  def mountBind(srcPath: String, dstPath: String) {
    println("mount --bind "+srcPath+" "+dstPath)
    println("mount -o remount,ro "+dstPath)
  }

  // _DO_ exception if retcode != 0
  def unmount(path: String) {
    println("umount "+path)
  }

  def destroyLv(vg: String, lv: String) { 
    // _DO_ exception if retcode != 0
    println("lvremove -f "+vg+"/"+lv)
  }

  // _DO_ exception if retcode != 0
  def tar(path: String) {
    println("tar -cf - "+path)
  }

  // _DO_ exception if retcode != 0
  def dd(dev: String) {
    println("dd if="+dev)
  }
}


object Volume {
  def apply(mountPath: String, mountAt: String) : Volume = { 
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
      new LvmVolume(dev, path, mountAt)
    else if (Sys.isBlockDevice(dev))
      new RawVolume(dev, path, mountAt)
    else 
      throw new IllegalArgumentException("not a real block device: "+dev)
  }

  class LvmVolume(dev: String, path: String, mountAt: String) extends Volume(dev, path, mountAt) {
    val (vg, liveLv) = Sys.getLvInfo(dev)
    val snapLv = liveLv+"-snap"
    val snapDev = "/dev/"+vg+"/"+snapLv

    protected def doAcquire() = Sys.createLvSnap(vg, liveLv, snapLv)
    protected def doRelease() = Sys.destroyLv(vg, snapLv)
    protected def doMount() = Sys.mount(snapDev, mountAt+path)
    protected def doUnmount() = Sys.unmount(mountAt+path)

    override def getDevForReading = if (Sys.isLvmManaged(snapDev)) snapDev else super.getDevForReading

    override def toString = super.toString+":vg="+vg+":lv="+liveLv
  }

  class RawVolume(dev: String, path: String, mountAt: String) extends Volume(dev, path, mountAt) {
    protected def doAcquire() = println("acquire " + this)
    protected def doRelease() = println ("release " + this)
    protected def doMount() = Sys.mountBind(path, mountAt+path)
    protected def doUnmount() = Sys.unmount(mountAt+path)
  }
}

/**
 * Basic archival volume abstraction.  
 * Can be acquired and released.
 */
abstract class Volume(dev: String, path: String, mountAt: String) {
  private var acquired = false
  private var mounted = false

  protected def doAcquire() : Unit
  protected def doRelease() : Unit
  protected def doMount() : Unit
  protected def doUnmount() : Unit

  def getDevForReading = dev

  def getMountLocation = mountAt

  def acquire() {
    acquired = { doAcquire(); true }
    if (mountAt != null) {
      mounted = { doMount(); true }
    }
  }
  def close () {
    if (mounted) {
      mounted = { doUnmount(); false }
    }
    if (acquired) {
      acquired = { doRelease(); false }
    }
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
  private def getReaderTypeAndMountLocation(howToRead: Seq[Char]): Tuple2[String, String] = {
    howToRead match {
      case Seq('r','s','y','n','c',':',mountAt @ _*) if (! mountAt.isEmpty) => ("rsync", mountAt.mkString)
      case Seq('t','a','r',':',mountAt @ _*) if (! mountAt.isEmpty) => ("tar", mountAt.mkString)
      case Seq('d','d') => ("dd", null)
      case _ => throw new IllegalArgumentException("TODO: usage: ...blah..blah..blah...")
    }
  }
  lazy val (readerType, mountAt) = getReaderTypeAndMountLocation(getParam("howToRead"))
  lazy val volumes = getParam("volumes") split ":" toList
}
// --howToRead=... --volumes=/:/usr:/var
object Invocation extends Invocation(args)

println (Invocation.readerType)
println (Invocation.mountAt)
println (Invocation.volumes)

val volumes : List[Volume] = Invocation.volumes map (Volume(_, Invocation.mountAt)) 
val reader = VolumeReader(Invocation.readerType, volumes)

object VolumeReader {
    def apply(howToRead: String, volumes: List[Volume]): VolumeReader = { 
      howToRead match {
        case "rsync" => new RsyncReader(volumes)
        case "dd" => new BinaryReader(List(volumes.head))
        case "tar" => new TarballReader(volumes)
        case _ => throw new IllegalArgumentException(howToRead+" is not a supported reader type.")
      }
    }

    /**
     * RsyncReader requires an xinetd configuration to function.
     * The configuration should be the same as regular rsync, but with
     */
    class RsyncReader(volumes: List[Volume]) extends VolumeReader(volumes) {
      def read() = ()
    }
    /**
     * since BinaryReader does not require (and in fact abhors filesystem
     * -level access, it only makes sense to use it if
     * 1. only one volume is selected for backup
     * 2. volume not mounted (or mounted r/o)
     */
    class BinaryReader(volumes: List[Volume]) extends VolumeReader(volumes) {
      def read() = Sys.dd(volumes.head.getDevForReading)
    }
    class TarballReader(volumes: List[Volume]) extends VolumeReader(volumes) {
      def read() = Sys.tar(volumes.head.getMountLocation)
    }
}
/**
 * if reader reads FSH, only root specified
 * if reader reads block devs, max_size(volumes) == 1
 */ 
abstract class VolumeReader(val volumes: List[Volume]) { 
  def read() : Unit // execs some system process on volume
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

val first = volumes head

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


