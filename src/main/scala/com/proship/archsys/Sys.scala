package com.proship.archsys

import java.io.{Closeable, File}
import java.net.ServerSocket

import org.slf4j.{Logger => SL4JLogger, LoggerFactory}

import scala.language.implicitConversions
import scala.sys.process._

object Sys {

  implicit def stringToFile(path: String) : File = new File(path)
  implicit def stringBuilderToString(strb: StringBuilder) : String = strb.toString()

  object DevNull extends ProcessLogger {
    def buffer[T](f: => T): T = f
    def out(s: => String): Unit = ()
    def err(s: => String): Unit = ()
  }

  object ServerProcess extends Closeable {
    lazy val commPort = using(new ServerSocket(0)) {
      socket => socket.getLocalPort
    }
    var proc : Process = null
    def run() {
      val rsyncCmd =
        "rsync --port="+commPort+" --config=/etc/rsyncd-archsys.conf"+
          " -4 --daemon --no-detach --log-file=/tmp/rsync.log"+
          " --log-file-format='%t: %f %n/%l xferred'"
      Sys.Logger.debug(rsyncCmd)
      proc = rsyncCmd run DevNull
    }
    def close() = try { proc.destroy() } finally { () }
  }

  class StringProcessLogger extends ProcessLogger {
    private val outB = new StringBuilder
    private val errB = new StringBuilder

    private def appendLine(b: StringBuilder, s: String) = b append s append "\n"
    def buffer[T](f: => T): T = f
    def out(s: => String): Unit = appendLine(outB, s)
    def err(s: => String): Unit = appendLine(errB, s)

    private def linesOf(str: String): List[String] = str.trim.split("\n").map(_.trim).toList
    def getOutLines : List[String] = linesOf(outB)
    def getErrLines : List[String] = linesOf(errB)
  }

  object Logger {
    private val log = LoggerFactory.getLogger(SL4JLogger.ROOT_LOGGER_NAME)

    def info(s: String) = log.info(s)
    def err(s: String) = log.error(s)
    def debug(s: String) = if (Runner.Invocation.debug) log.debug(s) else (s: String) => ()
  }

  class NonZeroExitCodeException(cmd: String, exitCode: Int) extends Exception(cmd+" failed with exit code "+exitCode)

  def mounts = "mount".!!

  def isBlockDevice(dev: String) = {
    val log = new StringProcessLogger
    "file -bL "+dev ! log // don't exception if retcode != 0
    log.getOutLines.head startsWith "block special"
  }

  def isLvmManaged(dev: String) =
    isBlockDevice(dev) && ("lvdisplay "+dev ! DevNull) == 0 // don't exception if retcode != 0

  def isBtrfsVolume(dev: String, mountType: String) = isBlockDevice(dev) && (mountType == "btrfs")

  def getLvInfo(dev: String) = {
    val log = new StringProcessLogger
    "lvdisplay -C "+dev ! log // don't exception if retcode != 0
    val bits = log.getOutLines(1).trim split " +" slice (0,4)
    (bits(1), bits(0), bits(3))
  }

  // _DO_ exception if retcode != 0
  def createLvSnap(vg: String, srcLv: String, dstLv: String, dstLvSize: String) {
    val lvcreate = "lvcreate -L " + dstLvSize + " --snapshot --name "+dstLv+" /dev/"+vg+"/"+srcLv
    val status = execute(List(Process(lvcreate)), DevNull)
    if (status != 0)
      throw new NonZeroExitCodeException(lvcreate, status)
    Sys.Logger.info("lvcreate "+vg+"/"+dstLv+" success")
  }

  def createBtrfsSnap(path: String, mountAt: String) {
    val snapCreate = "btrfs subvolume snapshot -r "+path+" "+mountAt
    val status = execute(List(Process(snapCreate)), DevNull)
    if (status != 0)
      throw new NonZeroExitCodeException(snapCreate, status)
    Sys.Logger.info("btrfs subvolume snapshot -r "+path+" "+mountAt+" success")
  }

  /*
   *  It is dangerous to pipe from one Process to another with scala due to the inability to catch IOException caused by broken pipe.
   *  From scala source, ProcessImpl.PipeThread.runloop() will catch all IOException and just prints the stack trace.
   *  For example, we run the following command "tar -c <path> | xz". Killing "xz" process causes an IOException due to broken pipe which is caught and swallowed
   *  by scala and leave our script hanging.
   */
  private def execute(pbs: List[ProcessBuilder], logger: ProcessLogger, howToBuild: (ProcessBuilder, ProcessBuilder) => ProcessBuilder  = _ ### _) = {
    val pb = pbs.tail./:(pbs.head)(howToBuild)
    if (logger == null)
      pb.!
    else
      pb ! logger
  }

  // _DO_ exception if retcode != 0
  def mount(src: String, path: String) {
    val cmds = if (isBlockDevice(src)) {
      val mount = "mount -o ro "+src+" "+path
      List(mount)
    }
    else {
      val mountBind = "mount --bind "+src+" "+path
      val remount = "mount -o remount,ro "+path
      List(mountBind, remount)
    }
    val pbs = cmds map (Process(_))
    val status = execute(pbs, DevNull, _ #&& _)
    if (status != 0)
      throw new NonZeroExitCodeException(cmds.mkString(" && "), status)
    Sys.Logger.info("mounting "+src+" to "+path+" success")
  }

  // _DO_ exception if retcode != 0
  def unmount(path: String) {
    val umount = "umount "+path
    val status = execute(List(Process(umount)), DevNull)
    if (status != 0)
      throw new NonZeroExitCodeException(umount, status)
    Sys.Logger.info("unmounting "+path+" success")
  }

  // _DO_ exception if retcode != 0
  def destroyLv(vg: String, lv: String) {
    val lvremove = "lvremove -f "+vg+"/"+lv
    val status = execute(List(Process(lvremove)), DevNull)
    if (status != 0)
      throw new NonZeroExitCodeException(lvremove, status)
    Sys.Logger.info("lvremove "+vg+"/"+lv+" success")
  }

  def destroyBtrfsSnap(mountAt: String) {
    val snapDelete = "btrfs subvolume delete "+mountAt
    val status = execute(List(Process(snapDelete)), DevNull)
    if (status != 0)
      throw new NonZeroExitCodeException(snapDelete, status)
    Sys.Logger.info("btrfs subvolume delete "+mountAt+" success")
  }

  // _DO_ exception if retcode != 0
  def tar(path: String) {
    //excludes file can contain exclude patterns that do not match on the system
    val excludeFile="/etc/archsys/tarExcludes"
    val excludes = getExcludes(excludeFile, path)
    val tar = "tar -c"+excludes+" "+path
    compress(tar)
    Sys.Logger.info("tar"+excludes+" "+path+" success")
  }

  private def compress(cmd: String) {
    val xz = "xz"
    val cmds = List(cmd, xz)
    val status = execute(cmds map (Process(_)), null, _ #| _)
    if (status != 0)
      throw new NonZeroExitCodeException(cmds.mkString(" | "), status)
  }

  // _DO_ exception if retcode != 0
  def dd(dev: String) {
    val dd = "dd if="+dev
    Sys.Logger.debug("invoke: "+dd)
    compress(dd)
    Sys.Logger.info("dd "+dev+" success")
  }

  private def getExcludes(file: String, path: String) = {
    if (new File(file).exists){
      val makeExcludes = scala.io.Source.fromFile(file).getLines()
      makeExcludes.map( " --exclude=" + path + _ ).mkString
    }
    else
      ""
  }
}
