
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
import CurlCode.CurlCode
import CurlUrlCode.CurlUrlCode
import CurlMultiCode.CurlMultiCode
import requests.RequestBlob.ByteSourceRequestBlob
import java.io.ByteArrayInputStream
import java.util.zip.GZIPInputStream
import java.util.zip.InflaterInputStream
import java.util.Arrays
import scala.util.Using

// TODO
// you need to install libcurl 
private[requests] object PlatformRequester {

  private val ccurl = libcurlPlatformCompat.instance

  private var multiPtr: Ptr[CurlMulti] = null
    
  private def multi = {
    if (multiPtr == null)
      multiPtr = ccurl.multiInit()

    multiPtr
  }

  // TODO clean up multi when no more transfers?

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
        handle.setOpt(CurlOption.Verbose)

        if (handle == null) {
          // As per curl docs:
          // > If this function returns NULL, something went wrong and you cannot use the other curl functions.
          throw new Exception("Could not initialise curl")
        }

        val curlu = ccurl.url()

        if (curlu == null) {
          throw new Exception("Could not allocate URL; insufficient memory")
        }

        var headersSlist: Ptr[CurlSlist] = CurlSlist.empty

        try {
          ensureUrlOk(curlu.set(CurlUrlPart.Url, url, CurlUrlFlag.Urlencode))

          params.foreach {
            case (k, v) => 
              // libcurl has a flag to urlencode, but it seemed to encode the values but not the keys
              // however, the appendquery flag is still useful, so we encode one by one
              ensureUrlOk(curlu.set(CurlUrlPart.Query, Util.urlEncodeSingle(k, v), CurlUrlFlag.Appendquery))
          }
          // As per curl docs:
          // > To specify port number in this string, append :[port] to the end of the 
          // > host name. The proxy's port number may optionally (but discouraged) 
          // > be specified with the separate option CURLOPT_PROXYPORT
          
          if (proxy != null)
            ensureOk(handle.setOpt(CurlOption.Proxy, s"${proxy._1}:${proxy._2}"), "Could not set proxy")

          // TODO cert
          // TODO ssl context?
          // TODO verify ssl

          //
          ensureOk(handle.setOpt(CurlOption.FollowLocation), "Could not set redirect follow")


          ensureOk(upperCaseVerb match {
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
          }, "Could not set method")


          val (contentLengthHeader, otherBlobHeaders) = blobHeaders.partition(_._1.equalsIgnoreCase("Content-Length"))

          otherBlobHeaders.foreach { case (k, v) => 
            headersSlist = appendHeader(headersSlist, k, v)
          }
          sess.headers.foreach { case (k, v) => 
            headersSlist = appendHeader(headersSlist, k, v)
          }
          headers.foreach { case (k, v) => 
            headersSlist = appendHeader(headersSlist, k, v)
          }
          compress.headers.foreach { case (k, v) => 
            headersSlist = appendHeader(headersSlist, k, v)
          }

          auth.header.foreach(v => 
            headersSlist = appendHeader(headersSlist, "Authorization", v)
          )

          // TODO double-check how to solve this
          // this disables Expect: 100-continue
          headersSlist = appendHeader(headersSlist, "Expect", "")

          ensureOk(handle.setOpt(CurlOption.TimeoutMs, readTimeout), "could not set timeout")

          ensureOk(handle.setOpt(CurlOption.ConnectTimeoutMs, connectTimeout), "could not set connect timeout")

          // TODO check cookies
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
                
              headersSlist = appendHeader(headersSlist, "Cookie", cookieValue)
            
          }      

          if (chunkedUpload) {
            headersSlist = appendHeader(headersSlist, "Transfer-Encoding", "chunked")
          }
          
          ensureOk(handle.setOpt(CurlOption.HttpHeader, headersSlist), "could not set headers")

          ensureOk(handle.setOpt(CurlOption.Url, curlu.get(CurlUrlPart.Url)._2), "could not set url")
          
          // TODO handle url get fails

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

            val res = nitems * size
            res

          })

          ensureOk(handle.setOpt(CurlOption.HeaderData, headerData), "could not set headerdata")
          ensureOk(handle.setOpt(CurlOption.HeaderFunction, headerFunction), "could not set headerfunction")

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
                val res = nitems * size
                res
              }
          })

          ensureOk(handle.setOpt(CurlOption.WriteData, writeData), "could not set writedata")
          ensureOk(handle.setOpt(CurlOption.WriteFunction, writeFunction), "could not set writefunction")

          val readdata: Ptr[Array[Byte]] = alloc(1)
          !readdata = null

          val readFunction = CFuncPtr.toPtr(CFuncPtr4.fromScalaFunction[Ptr[Byte], CSize, CSize, Ptr[Array[Byte]], CSize]{(buffer, size, nitems, userdata) =>
            if (!userdata == null) {
          //     println(s"${scala.io.AnsiColor.RED}R: Pause${scala.io.AnsiColor.RESET}")
              Curl.ReadfuncPause.toCSize
            } else if ((!userdata).size == 0) {
          //     println(s"${scala.io.AnsiColor.RED}R: Done${scala.io.AnsiColor.RESET}")
              0.toCSize
            } else {
              val dataSize = (!userdata).size
              val pre = new String(!userdata)
              
              val toPull = Math.min((nitems * size).toInt, dataSize)
              ffi.memcpy(
                buffer, (!userdata).at(0), toPull.toCSize)

              if (toPull < dataSize)
                 !userdata = (!userdata).slice(toPull, dataSize)
               else {
                 !userdata = null
               }

              val res = toPull.toCSize
          //     println(s"${scala.io.AnsiColor.RED}R: $res: ${new String(pre)}${scala.io.AnsiColor.RESET}")
              res
            }

          })

          ensureOk(handle.setOpt(CurlOption.ReadData, readdata), "Failed to set readdata")
          ensureOk(handle.setOpt(CurlOption.ReadFunction, readFunction), "Failed to set read function")

          var added = false
          
          if (upperCaseVerb == "POST" || upperCaseVerb == "PUT" || upperCaseVerb == "PATCH" || upperCaseVerb == "DELETE") {
            if (!chunkedUpload) {
              val bytes = new ByteArrayOutputStream()
              usingOutputStream(compress.wrap(bytes)) { os => data.write(os) }
              val byteArray = bytes.toByteArray
              if (byteArray != null && !byteArray.isEmpty)
                !readdata = byteArray

              // TODO Is this the right way to set the content length?
              ensureOk(handle.setOpt(CurlOption.PostFieldSize, byteArray.length), "Failed setting in file size")

              if (!added) {                
                ensureMultiOk(multi.add(handle), "Failed adding easy handle to global multi")
                added = true
              }              

              // TODO this loop probably should be made better
              while ((!readdata) != null) {
                val (code, stillRunning) = multi.perform
                ensureMultiOk(code, "Failed to perform the request")
                if (stillRunning > 0 && (!readdata) != null) {
                  val (code2, asdf) = multi.wait_(1000)
                  ensureMultiOk(code2, "Failed to perform the request")
                }
              }

              !readdata = new Array(0)
              // This is to force the read function to get invoked again to allow it to return
              // 0, indicating that the read is done
              val (code, stillRunning) = multi.perform
              ensureMultiOk(code, "Failed to perform the request")
              
            } else {
              usingOutputStream(compress.wrap(new OutputStream {

                // TODO buffer?

                override def write(b: Array[Byte]): Unit =
                  write(b, 0, b.length)

                override def write(b: Array[Byte], off: Int, len: Int): Unit = {
                  if (off > b.length || off < 0 || len < 0 || len > b.length - off)
                    throw new IndexOutOfBoundsException()
      
                  if (!added) {                
                    ensureMultiOk(multi.add(handle), "Failed adding easy handle to global multi")
                    added = true
                  }              

                  if ((!readdata) != null) {
                    ensureOk(handle.pause(CurlPauseFlag.PauseSend), "Could not pause")
                    if ((!readdata) != null) {
                      val newArray = Arrays.copyOf(!readdata, (!readdata).size + len)
                      System.arraycopy(b, off, newArray, newArray.size - len, len)
                      !readdata = newArray
                    } else {
                      !readdata = Arrays.copyOfRange(b, off, len)
                    }
                  } else {
                      !readdata = Arrays.copyOfRange(b, off, len)
                  }


                  ensureOk(handle.unpause, "Failed unpausing the easy handle")
                  val (code, still_running) = multi.perform
                  ensureMultiOk(code, "Failed performing multi transfer")

                }

                override def write(b: Int): Unit = {
                  val arr = Array[Byte](1)
                  arr(0) = b.toByte
                  write(arr)
                }

                override def close(): Unit =  {

                  if (!added) {                
                    ensureMultiOk(multi.add(handle), "Failed adding easy handle to global multi")
                    added = true
                  }              

                  // flush
                  if (!readdata == null) {
                    !readdata = Array()

                    ensureOk(handle.unpause, "Failed unpausing the easy handle")
                    val (code, still_running) = multi.perform

                    ensureMultiOk(code, "Failed performing multi transfer")

                  } else if ((!readdata).size > 0){
                    ensureOk(handle.unpause, "Failed unpausing the easy handle")
                    val (code, still_running) = multi.perform
                    ensureMultiOk(code, "Failed performing multi transfer")
                    val (code2, still_running2) = multi.wait_(1000)
                    ensureMultiOk(code2, "HEKEHK")

                    close()
                  }

                }

              })) { os => data.write(os) }
            }
          }

          if (!added) {                
            ensureMultiOk(multi.add(handle), "Failed adding easy handle to global multi")
            added = true
          }              

          // TODO look for current in list of active transfers
          // TODO check status

          var receivingHeaders = true

          while (receivingHeaders) {

            val (code, inProgressCount) = multi.perform

            ensureMultiOk(code, "Failed to perform network transfers")

            val hasFinishedHeaders =  headerData._2

            if (!hasFinishedHeaders) {
              // TODO 
              val (code, something) = multi.wait_(1000)
              ensureMultiOk(code, "Failed when waiting for socket activity")
            }

            receivingHeaders = !hasFinishedHeaders
            
          }

          // TODO would be nicer to not do our own header parsing
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
                (k.toLowerCase(), v.drop(1).trim()) :: acc
              }
            }.reverse.groupMap(_._1)(_._2)

          val status = handle.status
          // val responseHeaders = 
          //   handle.headers(CurlHeaderBitmask.Header)

          // TODO redirect handling

          val streamHeaders =  StreamHeaders(
              url,
              status,
              "Hek", // TODO
              responseHeaders,
              redirectedFrom
            )

          val deGzip = autoDecompress && responseHeaders.get("content-encoding").toSeq.flatten.exists(_.contains("gzip"))
          val deDeflate = autoDecompress && responseHeaders.get("content-encoding").toSeq.flatten.exists(_.contains("deflate"))

          onHeadersReceived(streamHeaders)

          val input = new InputStream {

            var current: Array[Byte] = null
            var at = 0
            var done = false

            override def read(): Int =  {

              val res = if (done) {
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

              res
            }
             
          }

            
          def processWrappedStream[V](f: java.io.InputStream => V): V = {
            // The HEAD method is identical to GET except that the server
            // MUST NOT return a message-body in the response.
            // https://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html section 9.4
            if (upperCaseVerb == "HEAD") f(new ByteArrayInputStream(Array()))
            else {
              try f(
                if (deGzip) new GZIPInputStream(input)
                else if (deDeflate) new InflaterInputStream(input)
                else input
              ) finally if (!keepAlive) ??? // TODO
            }
          }

          if (streamHeaders.is2xx || !check) processWrappedStream(f)
          else {
            val errorOutput = new ByteArrayOutputStream()
            processWrappedStream(geny.Internal.transfer(_, errorOutput))
            throw new RequestFailedException(
              Response(
                streamHeaders.url,
                streamHeaders.statusCode,
                streamHeaders.statusMessage,
                new geny.Bytes(errorOutput.toByteArray),
                streamHeaders.headers,
                streamHeaders.history
              )
            )
          }
                  
        } finally {
          multi.remove(handle)
          headersSlist.free
          curlu.cleanup
          handle.cleanup
        }
      }
    }
  }

  private def usingOutputStream[T](os: OutputStream)(fn: OutputStream => T): Unit = 
    try fn(os) finally os.close()

  private def ensureOk(code: CurlCode, message: => String): Unit = {
    if (code != CurlCode.Ok) {
      throw new Exception(s"Error: $message. Curl code: $code")
    }
  }

  private def ensureUrlOk(code: CurlUrlCode): Unit = {
    if (code != CurlUrlCode.Ok) {
      throw new Exception(s"Failed when building the url: $code")
    }
  }

  private def ensureMultiOk(code: CurlMultiCode, msg: => String): Unit = {
    if (code != CurlMultiCode.Ok) {
      throw new Exception(s"Error: $msg. Curl multi code: $code")
    }
  }

  private def appendHeader(slist: Ptr[CurlSlist], key: String, value: String): Ptr[CurlSlist] = {
    val res = slist.append(s"$key:$value")
    if (res == null) {
      throw new Exception(s"Failed to append header $key:$value")
    }
    res
  }
}

private[requests] class ByteReceiver(
  var headerBuffer: Array[Byte],
  var buffer: Array[Byte]
) {
  
}

private[requests] class PlatformRequester(
  receiver: ByteReceiver
  
)
