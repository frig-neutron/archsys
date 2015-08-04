package com.proship.archsys

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
  private class RsyncReader(volumes: List[Volume]) extends VolumeReader(volumes) {
    import java.io._
    import java.net._
    import java.nio._
    import java.nio.channels._

    import scala.annotation.tailrec

    class SocketJoin(src: ReadableByteChannel, dst: WritableByteChannel) extends Thread {
      val pollDelay = 10
      val buf = ByteBuffer.allocateDirect(16 * 4 * 1024) // 64 KB buffer
      override def run() = {
        @tailrec def loop() {
          buf.clear
          val bytesRead = src read buf
          buf.flip

          while(buf.hasRemaining)
            if ((dst write buf) == 0)
              Thread sleep pollDelay

          Sys.Logger.debug("transferring "+bytesRead+ " bytes")

          if (bytesRead == 0)
            Thread sleep pollDelay

          if (bytesRead > -1)
            loop()
        }
        loop()
      }
    }

    protected def doRead() =
      using(System.inheritedChannel.asInstanceOf[SocketChannel]) { clientToLocal =>
        using(Sys.ServerProcess) { server =>
          using(SocketChannel.open) { localToServer =>
            server.run()
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
                  Thread.sleep(500) // TODO: investigate removal of sleep
                  localToServer.connect(socketAddress)
                } catch {
                  case e: IOException => if (retry <= 0) false else connect(socketAddress, retry-1)
                }
              }
              connect(new InetSocketAddress("localhost", Sys.ServerProcess.commPort), 5)
            }

            if (connected) {
              localToServer.configureBlocking(false)
              clientToLocal.configureBlocking(false)
              val loc2server = new SocketJoin(localToServer, clientToLocal)
              val loc2client = new SocketJoin(clientToLocal, localToServer)
              loc2server.start()
              loc2client.start()

              while (loc2server.isAlive || loc2client.isAlive) {
                loc2server.join(1000)
                loc2client.join(1000)
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
  private class BinaryReader(volumes: List[Volume]) extends VolumeReader(volumes) {
    protected def doRead() = Sys.dd(volumes.head.getDevForReading)
  }
  private class TarballReader(volumes: List[Volume]) extends VolumeReader(volumes) {
    protected def doRead() = Sys.tar(volumes.head.getMountLocation)
  }
}
/**
 * if reader reads FSH, only root specified
 * if reader reads block devs, max_size(volumes) == 1
 */
abstract class VolumeReader(volumes: List[Volume]) {
  protected def doRead() : Unit

  def read() = readVolumes(volumes)

  private def readVolumes(volumes: List[Volume]) {
    if (volumes.isEmpty)
      doRead()
    else {
      using(volumes.head) { vol =>
        Sys.Logger.debug("acquiring "+vol)
        vol.acquire()
        readVolumes(volumes.tail)
      }
    }
  }
}