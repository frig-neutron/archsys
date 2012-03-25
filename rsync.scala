#!/bin/bash
  exec scala "$0" "$@"
!#

import scala.sys.process._
import java.nio.channels._
import java.nio._
import java.net._
import java.io._

val commPort = 6666
val clientSocket = System.inheritedChannel.asInstanceOf[SocketChannel]
val serverSocket = SocketChannel.open

object DevNull extends ProcessLogger { 
  def buffer[T](f: => T): T = f
  def out(s: => String): Unit = ()
  def err(s: => String): Unit = ()
}

object Listener extends Thread with Closeable {
  var proc : Process = null
  override def run {
    proc = "rsync -v --port=6666 -4 --daemon --no-detach --log-file"+
      "=/tmp/rsync.log" run DevNull
  }
  def close = proc.destroy
}

val buf = ByteBuffer.allocate(16 * 1024 * 1024) // 16 meg buffer
def sockCat(src: ReadableByteChannel, dst: WritableByteChannel) {
  val bytesRead = src read buf
  if (bytesRead == -1) 
    throw new ClosedChannelException
  buf.flip
  dst write buf
  buf.clear
  Thread.sleep(100)
}

try {
  Listener.run
  Thread.sleep(500)

  serverSocket.connect(
    new InetSocketAddress(
      "localhost",
      commPort))

  clientSocket.configureBlocking (false)
  serverSocket.configureBlocking (false)

  while (true) { 
    sockCat(clientSocket, serverSocket)
    sockCat(serverSocket, clientSocket)
  }
} finally { 
  Listener.close
}
// "rsync -v --port=666 -4 --daemon --no-detach" !! 


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

*/
