
package requests

import javax.net.ssl.SSLContext
import java.net.HttpCookie
import java.io.InputStream
import scala.scalanative.unsafe.Zone
import scala.scalanative.unsafe.Ptr
import scala.scalanative.unsafe.toCString
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
        val curl = libcurlPlatformCompat.instance
        val handle = curl.init

        if (handle == null) {
          ???
        }

        val curlurl = curl.url()

        if (url == null) {
          ???
        }

        curl.urlSet(curlurl, ???, toCString(url), )

        try {
          curl.setoptPtr(handle, CurlOption.Url.id, toCString(url, StandardCharsets.UTF_8))
          curl.setoptPtr(handle, CurlOption.HeaderFunction, ???)

        
        } finally {
          curl.cleanup(handle)
          curl.urlCleanup(curlurl)
        }
      }
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    Zone { implicit z =>

      
      val curl = libcurlPlatformCompat.instance

      val handle = curl.init
      try {

        val url = "http://localhost:8080/"

        if (curl != null) {
          curl.setoptInt(handle, CurlOption.Verbose.id, 1)
          curl.setoptPtr(handle, CurlOption.Url.id, toCString(url, StandardCharsets.UTF_8))
          val res = curl.perform(handle)
          println(res)
          
        }

      
      } finally {
        curl.cleanup(handle)
      }

    }
  }
}
