package requests

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import java.io._
import java.net.InetSocketAddress
import java.util.zip.{GZIPInputStream, InflaterInputStream}
import requests.Compress._
import scala.annotation.tailrec
import scala.collection.mutable.StringBuilder
import scala.scalanative.unsafe.extern
import scala.scalanative.unsafe.link

private[requests] class EchoServer {

  

  def getPort(): Int = server.getAddress.getPort
  def stop(): Unit = server.stop(0)

}

@extern @link("libmicrohttpd-dev")
private[requests] object MicroHttp {
  
}
