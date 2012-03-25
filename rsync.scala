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

object Listener extends Thread {
  var proc : Process = null
  override def run : Unit = (proc = "nc -l 6666" #> (
    new File("/tmp/echo")) run)
  def kill = proc.destroy
}

// Listener.run
// Thread.sleep(500)
val buf = ByteBuffer.allocate(1024 * 1024) // 1 meg buffer

val serverSocket = SocketChannel.open(
  new InetSocketAddress(
    "localhost",
    commPort))

def sockCat(src: ReadableByteChannel, dst: WritableByteChannel) {
  val bytesRead = src read buf
  if (bytesRead == -1) 
    throw new ClosedChannelException
  buf.flip
  dst write buf
  buf.clear
}

while (true) { 
  println ("client -> server")
  sockCat(clientSocket, serverSocket)
  println ("server -> client")
  sockCat(serverSocket, clientSocket)
}
// Listener.kill
// "rsync -v --port=666 -4 --daemon --no-detach" !! 


/*
Liveness problem:

*/
