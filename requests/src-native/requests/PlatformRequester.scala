
package requests

import javax.net.ssl.SSLContext
import java.net.HttpCookie
import java.io.InputStream
import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import java.io.ByteArrayOutputStream
import java.io.OutputStream

// TODO
// you need to install libcurl 
private[requests] object PlatformRequester {

  private val ccurl = libcurlPlatformCompat.instance

  private lazy val multi = {
    // This lives for the whole duration of the app, so 
    // we don't clean it up
    // TODO should it get cleaned up at some point?
    ccurl.multiInit()
  }

  def apply(verb: String, 
            sess: BaseSession,
             url: String,
             auth: RequestAuth,
             params: Iterable[(String, String)],
             blobHeaders: Iterable[(String, String)],
             headers: Iterable[(String, String)],
             data: RequestBlob,
             readTimeout: Int,
             connectTimeout: Int,
             proxy: (String, Int),
             cert: Cert,
             sslContext: SSLContext,
             cookies: Map[String, HttpCookie],
             cookieValues: Map[String, String],
             maxRedirects: Int,
             verifySslCerts: Boolean,
             autoDecompress: Boolean,
             compress: Compress,
             keepAlive: Boolean,
             check: Boolean,
             chunkedUpload: Boolean,
             redirectedFrom: Option[Response],
             onHeadersReceived: StreamHeaders => Unit): geny.Readable = new geny.Readable {

    private val upperCaseVerb = verb.toUpperCase()
    def readBytesThrough[T](f: InputStream => T): T = {
      Zone { implicit z =>
        val ccurl = libcurlPlatformCompat.instance
        val handle = ccurl.init

        if (handle == null) {
          // TODO not sure when the handle could be null
          throw new Exception("")
        }

        val curlu = ccurl.url()

        if (curlu == null) {
          // TODO not sure when the URL could be null
          throw new Exception("")
        }

        var headersSlist: Ptr[CurlSlist] = CurlSlist.empty

        // TODO check outcome of setting on all set callls

        try {
          curlu.set(CurlUrlPart.Url, url, CurlUrlFlag.Urlencode)

          params.foreach {
            case (k, v) => 
              curlu.set(CurlUrlPart.Query, s"$k=$v", CurlUrlFlag.Urlencode, CurlUrlFlag.Appendquery)
          }

          // As per curl docs:
          // > To specify port number in this string, append :[port] to the end of the 
          // > host name. The proxy's port number may optionally (but discouraged) 
          // > be specified with the separate option CURLOPT_PROXYPORT
          
          handle.setOpt(CurlOption.Proxy, s"${proxy._1}:${proxy._2}")

          // TODO cert
          // TODO ssl context?
          // TODO verify ssl

          //
          handle.setOpt(CurlOption.FollowLocation, true)

          upperCaseVerb match {
            case "GET" => handle.setOpt(CurlOption.HttpGet, true)
            case "HEAD" => handle.setOpt(CurlOption.Head, true)
            case "POST" => handle.setOpt(CurlOption.Post, true)
            case "PUT" => handle.setOpt(CurlOption.CustomRequest, "PUT")
            case "DELETE" => handle.setOpt(CurlOption.CustomRequest, "DELETE")
            case "OPTIONS" => handle.setOpt(CurlOption.RtspRequest, true)
            case "PATCH" => handle.setOpt(CurlOption.CustomRequest, "PATCH")
            case "CONNECT" => handle.setOpt(CurlOption.ConnectOnly, true)
            case "TRACE" => handle.setOpt(CurlOption.CustomRequest, "TRACE")
            case other => handle.setOpt(CurlOption.CustomRequest, other)
          }

          blobHeaders.foreach { case (k, v) => 
            headersSlist = headersSlist.append(s"$k:$v")
          }
          sess.headers.foreach { case (k, v) => 
            headersSlist = headersSlist.append(s"$k:$v")
          }
          headers.foreach { case (k, v) => 
            headersSlist = headersSlist.append(s"$k:$v")
          }
          compress.headers.foreach { case (k, v) => 
            headersSlist = headersSlist.append(s"$k:$v")
          }

          auth.header.foreach(v => 
            headersSlist = headersSlist.append(s"Authorization:$v")
          )

          handle.setOpt(CurlOption.TimeoutMs, readTimeout)

          handle.setOpt(CurlOption.ConnectTimeoutMs, connectTimeout)

          val sessionCookieValues = for{
            c <- (sess.cookies ++ cookies).valuesIterator
            if !c.hasExpired
            if c.getDomain == null || c.getDomain == curlu.get(CurlUrlPart.Host)._2
            if c.getPath == null || curlu.get(CurlUrlPart.Path)._2.startsWith(c.getPath)
          } yield (c.getName, c.getValue)

          val allCookies = sessionCookieValues ++ cookieValues
          if (allCookies.nonEmpty){
            val cookieValue = allCookies
                .map{case (k, v) => s"""$k="$v""""}
                .mkString("; ")            
                
              headersSlist = headersSlist.append(s"Cookie:${cookieValue}")
            
          }      
          
          handle.setOpt(CurlOption.HttpHeader, headersSlist)
          handle.setOpt(CurlOption.Url, curlu.get(CurlUrlPart.Url)._2) // TODO handle get fails

          // TODO setup upload

          // if (upperCaseVerb == "POST" || upperCaseVerb == "PUT" || upperCaseVerb == "PATCH" || upperCaseVerb == "DELETE") {
          //   if (!chunkedUpload) {
          //     val bytes = new ByteArrayOutputStream()
          //     usingOutputStream(compress.wrap(bytes)) { os => data.write(os) }
          //     val byteArray = bytes.toByteArray

          //     handle.setOpt(CurlOption.PostFieldSize, byteArray.length)

          //     // connection.setFixedLengthStreamingMode(byteArray.length)
          //     // usingOutputStream(connection.getOutputStream) { os => os.write(byteArray) }
          //   } else {
          //     // connection.setChunkedStreamingMode(0)
          //     // usingOutputStream(compress.wrap(connection.getOutputStream)) { os => data.write(os) }
          //   }
          // }

          // TODO setup header read
          // TODO set up body read

          val input = new InputStream {
            override def read(): Int =  {
              handle.pause(CurlPauseFlag.UnpauseAll)
              // Curl docs say that calling unpause 
              // might immediately trigger the callbacks 
              // due to buffered data before unpause returns
              // therefore we must check if we have new data before we perform

              // TODO check for new data
              // TODO set new data

              multi.perform
              handle.pause(CurlPauseFlag.PauseAll)
              // TODO handle callback bytes
              // TODO return

              ???

            }
             
          }

            
          f(input)
                  
        } finally {
          handle.cleanup
          curlu.cleanup
          headersSlist.free
        }
      }
    }
  }

  private def usingOutputStream[T](os: OutputStream)(fn: OutputStream => T): Unit = 
    try fn(os) finally os.close()
}

private[requests] class ByteReceiver(
  var headerBuffer: Array[Byte],
  var buffer: Array[Byte]
) {

  def receiveHeaders(): CInt = {
    
  }
  
}

private[requests] class PlatformRequester(
  receiver: ByteReceiver
  
)

object Main {

  val ccurl = libcurlPlatformCompat.instance

  lazy val multi = {
    ccurl.multiInit()
  }
  
  def main(args: Array[String]): Unit = {
    Zone { implicit z =>

      val handle = ccurl.init
      val url = ccurl.url()

      try {

        val urlStr = "http://localhost:8080/potato"

        if (url != null) {

          val c1 = url.set(CurlUrlPart.Url, urlStr, CurlUrlFlag.Urlencode, CurlUrlFlag.DisallowUser)

          handle.setOpt(CurlOption.Url, url.get(CurlUrlPart.Url)._2)
          multi.add(handle)
          var stillRunning = 1

          while (stillRunning > 0) {

            val (mc0, i) = multi.perform
            stillRunning = i
            var mc = mc0
            var numfds = 0

            if (mc == CurlMultiCode.Ok) {
              val (code, fds) = multi.wait_(1000)
              numfds = fds
              mc = code
             
            }

            if (mc != CurlMultiCode.Ok) {
              throw new Exception("POOP")
            }
            
          }


          
        }

      
      } finally {
        multi.cleanup
        handle.cleanup
        url.cleanup
      }

    }
  }


}