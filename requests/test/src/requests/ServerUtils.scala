package requests

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import java.io._
import java.net.InetSocketAddress
import java.util.zip.{GZIPInputStream, InflaterInputStream}
import requests.Compress._
import scala.annotation.tailrec
import scala.collection.mutable.StringBuilder

object ServerUtils {
  def usingEchoServer(f: Int => Unit): Unit = {
    val server: EchoServer =  ???
      // new EchoServer
    try f(server.getPort())
    finally server.stop()
  }

  /** Stream uncompresser
    * @param c
    *   Compression mode
    */
  private[requests] class Plumper(c: Compress) {

    private def wrap(is: InputStream): InputStream =
      c match {
        case None    => is
        case Gzip    => new GZIPInputStream(is)
        case Deflate => new InflaterInputStream(is)
      }

    def decompress(compressed: InputStream): String = {
      val gis = wrap(compressed)
      val br = new BufferedReader(new InputStreamReader(gis, "UTF-8"))
      val sb = new StringBuilder()

      @tailrec
      def read(): Unit = {
        val line = br.readLine
        if (line != null) {
          sb.append(line)
          read()
        }
      }

      read()
      br.close()
      gis.close()
      compressed.close()
      sb.toString()
    }
  }

}
