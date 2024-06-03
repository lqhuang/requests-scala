
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
import scala.scalanative.runtime.ffi

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
          
          if (proxy != null)
            handle.setOpt(CurlOption.Proxy, s"${proxy._1}:${proxy._2}")

          // TODO cert
          // TODO ssl context?
          // TODO verify ssl

          //
          handle.setOpt(CurlOption.FollowLocation)


          upperCaseVerb match {
            case "GET" => handle.setOpt(CurlOption.HttpGet)
            case "HEAD" => handle.setOpt(CurlOption.Head)
            case "POST" => handle.setOpt(CurlOption.Post)
            case "PUT" => handle.setOpt(CurlOption.CustomRequest, "PUT")
            case "DELETE" => handle.setOpt(CurlOption.CustomRequest, "DELETE")
            case "OPTIONS" => handle.setOpt(CurlOption.RtspRequest)
            case "PATCH" => handle.setOpt(CurlOption.CustomRequest, "PATCH")
            case "CONNECT" => handle.setOpt(CurlOption.ConnectOnly)
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

          val headerData = alloc[CStruct3[Ptr[Curl], Boolean, Array[Byte]]](1)
          headerData._1 = handle
          headerData._2 = false
          headerData._3 = new Array(0)

          val headerFunction = CFuncPtr.toPtr(CFuncPtr4.fromScalaFunction[Ptr[Byte], CSize, CSize, Ptr[CStruct3[Ptr[Curl], Boolean, Array[Byte]]], CSize]{(buffer, size, nitems, userdata) =>

            if (nitems * size == 2 && (!buffer) == '\r' && (!(buffer + 1)) == '\n') {
              // Curl passes headers a single line at a time to this callback, so getting an empty line as a buffer
              // means we reached the end of headers
              userdata._2 = true
              userdata._1.pause(CurlPauseFlag.PauseAll)
            } else {
              val arr = new Array[Byte]((nitems * size).toInt)
              ffi.memcpy(arr.at(0), buffer, nitems * size)
              userdata._3 = userdata._3 ++  arr
            }

            nitems * size

          })

          handle.setOpt(CurlOption.HeaderData, headerData)
          handle.setOpt(CurlOption.HeaderFunction, headerFunction)

          val writeData = alloc[Array[Byte]](1)
          !writeData = null

          val writeFunction = CFuncPtr.toPtr(CFuncPtr4.fromScalaFunction[Ptr[Byte], CSize, CSize, Ptr[Array[Byte]], CSize]{
            (buffer, size, nitems, userdata) =>
              if ((!userdata) != null) {
                // last data hasn't been handled yet, return pause
                Curl.WritefuncPause.toCSize
              } else {
                val arr = new Array[Byte]((nitems * size).toInt)
                ffi.memcpy(arr.at(0), buffer, nitems * size)
                !userdata = arr
                nitems * size
              }
          })

          handle.setOpt(CurlOption.WriteData, writeData)
          handle.setOpt(CurlOption.WriteFunction, writeFunction)

          multi.add(handle)

          // TODO look for current in list of active transfers
          // TODO check status

          var receivingHeaders = true
          var last = ""

          while (receivingHeaders) {

            val (code, inProgressCount) = multi.perform

            if (code != CurlMultiCode.Ok) {
              throw new Exception("Making the request went bad")
            }

            val hasFinishedHeaders =  headerData._2

            if (!hasFinishedHeaders) {
              multi.wait(1000)
            }

            receivingHeaders = !hasFinishedHeaders
            
          }

          val responseHeaders = new String(headerData._3)
            .split("\r\n")
            .tail // the first line is the status
            .foldLeft(List.empty[(String, String)]) { (acc, next) => 
              val colon = next.indexOf(':')
              if (colon == -1 && next.charAt(0).isWhitespace) {
                // must be a continuation 
                // TODO unsafe list matching
                val (k, v) :: t = acc 
                (k, v + next) :: acc
              } else {
                val (k, v) = next.splitAt(colon)
                (k, v.drop(1).trim()) :: acc
              }
            }.reverse.groupMap(_._1)(_._2)

          val status = handle.status
          // val responseHeaders = 
          //   handle.headers(CurlHeaderBitmask.Header)

          // TODO redirect handling

          onHeadersReceived(
            StreamHeaders(
              url,
              status,
              "Hek", // TODO
              responseHeaders,
              redirectedFrom
            )
          )

          val input = new InputStream {

            var current: Array[Byte] = null
            var at = 0
            var done = false

            override def read(): Int =  {

              if (done) {
                -1
              } else {

                if (current == null || at == current.size) {
                  // pull new
                  handle.unpause

                  while ((!writeData) == null && !done) {
                    val (a, num) = multi.perform

                    if (num == 0)
                      done = true
                    else {
                      // TODO check if the current handle is still in progress
                      if ((!writeData) == null) {
                        val (a, b) = multi.wait_(1000)
                      }
                    }

                  }
              
                  current = !writeData
                  !writeData = null
                  at = 0
                }

                if (current == null || current.size == 0)
                  read()
                else {
                  val res = current(at)
                  at += 1
                  res
                }

              }
            }
             
          }

            
          val res = f(input)
          res
                  
        } finally {
          multi.remove(handle)
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
  
}

private[requests] class PlatformRequester(
  receiver: ByteReceiver
  
)

object Main {

  def main(args: Array[String]): Unit = {
    val result = requests.get("http://localhost:8080")
    println(result.statusCode)
    println(result.headers.mkString("\n"))
    println(result.data)
  }


}
