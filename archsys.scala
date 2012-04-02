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
  import java.io.{File, Closeable, IOException}
  import java.net._

  implicit def stringToFile(path: String) : File = new File(path)
  implicit def stringBuilderToString(strb: StringBuilder) : String = strb.toString

  object DevNull extends ProcessLogger { 
    def buffer[T](f: => T): T = f
    def out(s: => String): Unit = ()
    def err(s: => String): Unit = ()
  }

  object ServerProcess extends Thread with Closeable {
    lazy val commPort = using(new ServerSocket(0)) {
      socket => socket.getLocalPort
    }
    var proc : Process = null
    override def run {
      proc = 
        "rsync -v --port="+commPort+
        " -4 --daemon --no-detach --log-file=/tmp/rsync.log"+
        " --log-file-format='%t: %f %n/%l xferred'" run DevNull
    }
    def close = proc.destroy
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

  class NonZeroExitCodeException extends Exception

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

  // _DO_ exception if retcode != 0
  def createLvSnap(vg: String, srcLv: String, dstLv: String) {
    val log = new StringProcessLogger
    val lvcreate = "lvcreate -L 2G --snapshot --name "+dstLv+" /dev/"+vg+"/"+srcLv
    execute(List(Process(lvcreate)), log)
  }

  /*
   *  It is dangerous to pipe from one Process to another with scala due to the inability to catch IOException caused by broken pipe.
   *  From scala source, ProcessImpl.PipeThread.runloop() will catch all IOException and just prints the stack trace.
   *  For example, we run the following command "tar -c <path> | xz". Killing "xz" process causes an IOException due to broken pipe which is caught and swallowed
   *  by scala and leave our script hanging.
   */
  private def execute(pbs: List[ProcessBuilder], logger: ProcessLogger, howToBuild: (ProcessBuilder, ProcessBuilder) => ProcessBuilder  = (_ ### _)) {
    var process: Process = null
    try {
      val pb = pbs.tail./:(pbs.head)(howToBuild)
      process = if (logger == null)
        pb run
      else
        pb run logger

      if (process.exitValue != 0)
        throw new NonZeroExitCodeException
    }
    finally {
      if (process != null) process.destroy
    }
  }

  // _DO_ exception if retcode != 0
  def mount(src: String, path: String) {
    val log = new StringProcessLogger
    if (isBlockDevice(src)) {
      val mount = "mount -o ro "+src+" "+path
      execute(List(Process(mount)), log)
    }
    else {
      val mountBind = "mount --bind "+src+" "+path
      val remount = "mount -o remount,ro "+path
      val processes = List(mountBind, remount) map (Process(_))
      execute(processes, log, (_ #&& _))
    }
  }

  // _DO_ exception if retcode != 0
  def unmount(path: String) {
    val log = new StringProcessLogger
    val umount = "umount "+path
    execute(List(Process(umount)), log)
  }

  // _DO_ exception if retcode != 0
  def destroyLv(vg: String, lv: String) { 
    val log = new StringProcessLogger
    val lvremove = "lvremove -f "+vg+"/"+lv
    execute(List(Process(lvremove)), log)
  }

  // _DO_ exception if retcode != 0
  def tar(path: String) {
    val tar = "tar -c "+path
    compress(Process(tar))
  }

  private def compress(process: ProcessBuilder) {
    val xz = "xz"
    execute(List(process, Process(xz)), null, (_ #| _))
  }

  // _DO_ exception if retcode != 0
  def dd(dev: String) {
    val dd = "dd if="+dev
    compress(Process(dd))
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
    protected def doAcquire() = ()
    protected def doRelease() = ()
    protected def doMount() = Sys.mount(path, mountAt+path)
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
      import java.nio.channels._
      import java.nio._
      import java.net._
      import java.io._

      object SocketJoin {

        val buf = ByteBuffer.allocate(16 * 1024 * 1024) // 16 meg buffer 
        private def sockCat(src: ReadableByteChannel, dst: WritableByteChannel) {
          val bytesRead = src read buf
          if (bytesRead == -1) 
            throw new ClosedChannelException
          buf.flip
          var bytesWritten = 0
          while (bytesWritten < bytesRead)
            bytesWritten += dst write buf
          buf.clear
        }

        def proxy(sock: (SocketChannel, SocketChannel)) = {
          sock._1.configureBlocking (false)
          sock._2.configureBlocking (false)
          while (true) { 
            sockCat(sock._1, sock._2)
            sockCat(sock._2, sock._1)
          }
        }
      }

      def read() = 
        using(System.inheritedChannel.asInstanceOf[SocketChannel]) {
          clientToLocal => {
            using(Sys.ServerProcess) {
              server => {
                using(SocketChannel.open) {
                  localToServer => {
                    server.run
                    /*
                    * attempt to connect until succeeds in 0.5 seconds interval 
                    * just in case the server port is not up when we invoke SocketChannel.connect().
                    * according to documentation on SocketChannel, the socket channel is by default in
                    * blocking mode and therefore will block until it is connected. IOException is thrown
                    * if there are any I/O errors (like the specified socket address does not exist (yet)).
                    * this is our cue to try again. 
                    */
                    lazy val connected = {
                      def connect(socketAddress: InetSocketAddress, retry: Int): Boolean = {
                        try {
                          Thread.sleep(500)
                          localToServer.connect(socketAddress)
                        } catch {
                          case e: IOException => if (retry <= 0) false else connect(socketAddress, retry-1)
                        }
                      }
                      connect(new InetSocketAddress("localhost", Sys.ServerProcess.commPort), 5)
                    }

                    if (connected) SocketJoin.proxy(clientToLocal, localToServer)
                  }
                }
              }
            }
          }
        }
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


