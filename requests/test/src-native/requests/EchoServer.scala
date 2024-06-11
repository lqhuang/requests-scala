package requests

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import java.io._
import java.net.InetSocketAddress
import java.util.zip.{GZIPInputStream, InflaterInputStream}
import requests.Compress._
import scala.annotation.tailrec
import scala.collection.mutable.StringBuilder
import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scala.scalanative.runtime.ffi._

private[requests] class EchoServer(implicit z: Zone) {

  var daemonPtr = alloc[Ptr[MicroHttpdDaemon]](1)

  MicroHttp.start(daemonPtr)

  def getPort(): Int = 
    MicroHttp.port(!daemonPtr)

  def stop(): Unit = {
    MicroHttp.stop(!daemonPtr)
    free(daemonPtr)
  }

}

private[requests] trait MicroHttpdDaemon {}

@extern
@link("microhttpd")
@define("REQUESTS_SCALA_POTATO")
object MicroHttp {
  @name("request_scala_start_server")
  def start(daemon: Ptr[Ptr[MicroHttpdDaemon]]): Int = extern
  
  @name("request_scala_port")
  def port(daemon: Ptr[MicroHttpdDaemon]): Int = extern

  @name("request_scala_stop_server")
  def stop(daemon: Ptr[MicroHttpdDaemon]): Unit = extern
}
