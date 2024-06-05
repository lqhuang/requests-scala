package requests

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import java.io._
import java.net.InetSocketAddress
import java.util.zip.{GZIPInputStream, InflaterInputStream}
import requests.Compress._
import scala.annotation.tailrec
import scala.collection.mutable.StringBuilder

private[requests] class EchoServer extends HttpHandler {
  private val server: HttpServer = HttpServer.create(new InetSocketAddress(0), 0)
  server.createContext("/echo", this)
  server.setExecutor(null); // default executor
  server.start()
  def getPort(): Int = server.getAddress.getPort
  def stop(): Unit = server.stop(0)
  override def handle(t: HttpExchange): Unit = {
    val h: java.util.List[String] =
      t.getRequestHeaders.get("Content-encoding")
    val c: Compress =
      if (h == null) None
      else if (h.contains("gzip")) Gzip
      else if (h.contains("deflate")) Deflate
      else None
    val msg = new ServerUtils.Plumper(c).decompress(t.getRequestBody)
    t.sendResponseHeaders(200, msg.length)
    t.getResponseBody.write(msg.getBytes())
  }
}

