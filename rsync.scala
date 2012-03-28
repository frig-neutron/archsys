#!/bin/bash
  exec scala "$0" "$@"
!#

import scala.sys.process._
import java.nio.channels._
import java.nio._
import java.net._
import java.io._

val clientToLocal = System.inheritedChannel.asInstanceOf[SocketChannel]
val localToServer = SocketChannel.open

object DevNull extends ProcessLogger { 
  def buffer[T](f: => T): T = f
  def out(s: => String): Unit = ()
  def err(s: => String): Unit = ()
}

object ServerProcess extends Thread with Closeable {
  lazy val commPort = {
    val serverSocket = new ServerSocket(0)
    val port = serverSocket.getLocalPort
    serverSocket.close
    port
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

try {
  ServerProcess.run
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
    connect(new InetSocketAddress("localhost", ServerProcess.commPort), 5)
  }

  if (connected) SocketJoin.proxy(clientToLocal, localToServer)

} finally { 
  ServerProcess.close
  if (clientToLocal != null) clientToLocal.close
  if (localToServer != null) localToServer.close
}

/*
Liveness problem:
  Revision f9d48cbef268a47706f9ef51450a35965bb06fbd has a problem  
  in that the server and client must talk in strictly alternating 
  turns.  If the server talks and then talks again, the client will
  not get the message because the main app thread will be blocked 
  listening to the client socket. The client may eventually get the 
  second message from the server, but only if it decides to say 
  something (unblocking the main thread in doing so).  Since the 
  client is not guaranteed to say something, we have a potential
  deadlock.  The problem is symmetrical from the perspective of the
  client.

Solutions: 
  Configure the NIO SocketChannels for non-blocking reads.
    - From SocketChannel.read javadoc: 
      "If another thread has already initiated a read operation 
       upon this channel, however, then an invocation of this method 
       will block until the first operation is complete."
    - From SelectableChannel description: 
      "In non-blocking mode an I/O operation will never block and 
       may transfer fewer bytes than were requested or possibly no 
       bytes at all. "
    - nothing left to do but try
    Whohoo! seems to work w/ live rsync.

  Use some sort of concurrency mechanism: 
    - Java Threads?
    - Scala Actors?
  SelectableChannel's non-blocking operation provides something simi-
    lar to an explicitly concurrent approach.

Rsync protocol corruption problem: 
  Using anything but a small buffer in SocketJoin.sockCat 
  introduces corruption into rsync protocol, breaking the data
  transfer before completion w/ rsync err 12.

  Workaround: use tiny buffer (1 byte works).
  Drawback: transfer rate drops to about 200 KB/s

  Cause: output buffer not completely flushed because socket config-
    ured for non-blocking operation.
  Solution: keep flushing output buffer until bufOut == bufIn
  Alt solution: switch output socket to synchoronous operation before 
    flushing.  Requires sockCat interface modification from 
    {Readable,Writable}ByteChannel to {R,W}BC + SelectableChannel.
    SelectableChannel provides blocking operation config interface.
  Implementing first solution, because it is a bit easier. Alternate
    solution may provide slightly better performance, though this 
    really needs to be tested with the hard stick of measurement.
*/
