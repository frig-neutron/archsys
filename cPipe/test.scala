#!/bin/bash
  exec /home/groovur/local/bin/scala -classpath ./jna.jar "$0" "$@"
!#

import scala.sys.process._
import java.io.{File, Closeable, IOException}
import java.net._
import java.util.Arrays
import com.sun.jna.{Library, Native, Platform}
import scala.collection.JavaConversions._

trait CLibrary extends Library {
  def printf(s: String)
}

object CLibrary {
  def Instance = Native.loadLibrary(
    if (Platform.isWindows) "msvcrt" else "c", classOf[CLibrary]).asInstanceOf[CLibrary]
}

object HelloWorld {
  def main(args: Array[String]) {
    CLibrary.Instance.printf("Hello, World\n");
    for ((arg, i) <- args.zipWithIndex) {
      CLibrary.Instance.printf(
        "Argument %d: %s".format(i.asInstanceOf[AnyRef], arg))
    }
  }
}

trait POSIX extends Library {

  //Examples of naitive system calls
  def chmod (filename: String, mode: Int)
  def chown (filename: String, user: Int, group: Int)
  def rename (oldpath: String, newpath: String);
  def kill (pid: Int, signal: Int)
  def link (oldpath: String, newpath: String)
  def mkdir (path: String, mode: Int)
  def rmdir (path: String)
  def sync ()

  //Array of pointers MUST be terminated by a NULL pointer
  def execv(file: String, arg: Array[String]) : Int
  def fork() : Int
  def exit(status: Int)
  def _exit(status: Int)
}

object exampleOfPOSIX {
  def main(args: Array[String]) : Unit =
  {
    var posix = Native.loadLibrary("c", classOf[POSIX]).asInstanceOf[POSIX]
    
    //posix.mkdir("/tmp/newdir", 0777)
    //posix.rename("/tmp/newdir","/tmp/renamedir")
    
    var nullTerminator:String = null
    val pid = posix.fork()

    println(pid)  

    if (pid < 0)
    {
      //We are in the parent process
      //Fork Failed
      System.out.println("fork failed");
      posix.exit(99)
    }

    if (pid == 0)
    {
      //We are in the child process
      val params = Array("l", "-l", "666" , nullTerminator)
      val cmd = "/bin/nc"
      posix.execv(cmd, params)
      posix._exit(0)
    }
    else 
    {
      //We are in the parent process
      posix.exit(0)
    }
    System.out.println("fork exec done");   
  }
}

//HelloWorld.main(args)
exampleOfPOSIX.main(args)
