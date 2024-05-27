
package requests

import javax.net.ssl.SSLContext
import java.net.HttpCookie
import java.io.InputStream
import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

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


        try {
          // curl.setoptPtr(handle, CurlOption.Url.id, toCString(url, StandardCharsets.UTF_8))
          // curl.setoptPtr(handle, CurlOption.HeaderFunction.id, ???)

          curlu.set(CurlUrlPart.Url, url)

          params.foreach {
            case (k, v) => curlu.set()
          }
            
                  
        } finally {
          handle.cleanup
          curlu.cleanup
        }
      }
    }
  }
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
