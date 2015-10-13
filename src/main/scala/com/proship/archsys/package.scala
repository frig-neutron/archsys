package com.proship

import scala.language.reflectiveCalls

package object archsys {
  /**
   * Loan pattern with duck typing for Closeable. Copied directly from
   * AVBS project.
   */
  def using[Closeable <: {def close(): Unit}, B](closeable: Closeable)(getB: Closeable => B) : B =
    try {
      Sys.Logger.debug("with "+closeable)
      getB(closeable)
    } finally {
      Thread.sleep(500) // TODO: wait for something explicit
      Sys.Logger.debug("closing "+closeable)

      try {
        closeable.close()
      }
      catch {
        case e: Exception => Sys.Logger.err("failed closing "+closeable, e)
      }
    }
}
