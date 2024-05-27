
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
    def readBytesThrough[T](f: java.io.InputStream => T): T = {
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

          if (upperCaseVerb == "POST" || upperCaseVerb == "PUT" || upperCaseVerb == "PATCH" || upperCaseVerb == "DELETE") {
            if (!chunkedUpload) {
              val bytes = new ByteArrayOutputStream()
              usingOutputStream(compress.wrap(bytes)) { os => data.write(os) }
              val byteArray = bytes.toByteArray

              handle.setOpt(CurlOption.PostFieldSize, byteArray.length)
              handle.setOpt(CurlOption.PostFields)
              

              connection.setFixedLengthStreamingMode(byteArray.length)
              usingOutputStream(connection.getOutputStream) { os => os.write(byteArray) }
            } else {
              connection.setChunkedStreamingMode(0)
              usingOutputStream(compress.wrap(connection.getOutputStream)) { os => data.write(os) }
            }
          }

          // TODO setup header read
          // TODO set up body read

          handle.perform

            
          ???
                  
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

object Main {
  def main(args: Array[String]): Unit = {
    Zone { implicit z =>

      
      val ccurl = libcurlPlatformCompat.instance
      val handle = ccurl.init
      val url = ccurl.url()

      try {

        val urlStr = "http://localhost:8080/potato"

        if (url != null) {

          val c1 = url.set(CurlUrlPart.Url, urlStr, CurlUrlFlag.Urlencode, CurlUrlFlag.DisallowUser)
          println(c1)
          val c0 = url.set(CurlUrlPart.Query, "hek=y a k", CurlUrlFlag.Urlencode, CurlUrlFlag.Appendquery)
          println(c0)
          val (c2, outStr) = url.get(CurlUrlPart.Url)
          println(c2)

          println(outStr)
          
        }

      
      } finally {
        handle.cleanup
        url.cleanup
      }

    }
  }


}
