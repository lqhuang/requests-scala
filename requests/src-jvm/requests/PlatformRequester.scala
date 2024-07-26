package requests

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, OutputStream}
import java.net.{HttpCookie, HttpURLConnection, InetSocketAddress}
import java.util.zip.{GZIPInputStream, InflaterInputStream}
import javax.net.ssl._
import collection.JavaConverters._
import scala.collection.mutable
import java.net.http.HttpClient
import java.net.ProxySelector
import java.time.Duration
import java.io.PipedInputStream
import java.io.PipedOutputStream
import java.net.http.HttpRequest
import java.util.function.Supplier
import java.io.InputStream
import java.net.http.HttpResponse
import java.util.concurrent.ExecutionException
import java.net.http.HttpConnectTimeoutException
import java.net.http.HttpTimeoutException
import java.net.URL

private[requests] object PlatformRequester {

  private lazy val methodField: java.lang.reflect.Field = {
    val m = classOf[HttpURLConnection].getDeclaredField("method")

    m
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

    private val upperCaseVerb = verb.toUpperCase

    def readBytesThrough[T](f: java.io.InputStream => T): T = {

      val url0 = new java.net.URL(url)

      val url1 = if (params.nonEmpty) {
        val encodedParams = Util.urlEncode(params)
        val firstSep = if (url0.getQuery != null) "&" else "?"
        new java.net.URL(url + firstSep + encodedParams)
      } else url0

      var connection: HttpURLConnection = null

      try {

        val conn =
          if (proxy == null) url1.openConnection
          else {
            val (ip, port) = proxy
            val p = new java.net.Proxy(
              java.net.Proxy.Type.HTTP, new InetSocketAddress(ip, port)
            )
            url1.openConnection(p)
          }

        connection = conn match{
          case c: HttpsURLConnection =>
            if (cert != null) {
              c.setSSLSocketFactory(UtilJvm.clientCertSocketFactory(cert, verifySslCerts))
              if (!verifySslCerts) c.setHostnameVerifier(new HostnameVerifier { def verify(h: String, s: SSLSession) = true })
            } else if (sslContext != null) {
              c.setSSLSocketFactory(sslContext.getSocketFactory)
              if (!verifySslCerts) c.setHostnameVerifier(new HostnameVerifier { def verify(h: String, s: SSLSession) = true })
            } else if (!verifySslCerts) {
              c.setSSLSocketFactory(UtilJvm.noVerifySocketFactory)
              c.setHostnameVerifier(new HostnameVerifier { def verify(h: String, s: SSLSession) = true })
            }
            c
          case c: HttpURLConnection => c
        }

        connection.setInstanceFollowRedirects(false)
        if (Requester.officialHttpMethods.contains(upperCaseVerb)) {
          connection.setRequestMethod(upperCaseVerb)
        } else {
          // HttpURLConnection enforces a list of official http METHODs, but not everyone abides by the spec
          // this hack allows us set an unofficial http method
          connection match {
            case cs: HttpsURLConnection =>
              cs.getClass.getDeclaredFields.find(_.getName == "delegate").foreach{ del =>
                del.setAccessible(true)
                methodField.set(del.get(cs), upperCaseVerb)
              }
            case c =>
              methodField.set(c, upperCaseVerb)
          }
        }

        for((k, v) <- blobHeaders) connection.setRequestProperty(k, v)

        for((k, v) <- sess.headers) connection.setRequestProperty(k, v)

        for((k, v) <- headers) connection.setRequestProperty(k, v)

        for((k, v) <- compress.headers) connection.setRequestProperty(k, v)

        connection.setReadTimeout(readTimeout)
        auth.header.foreach(connection.setRequestProperty("Authorization", _))
        connection.setConnectTimeout(connectTimeout)
        connection.setUseCaches(false)
        connection.setDoOutput(true)

        val sessionCookieValues = for{
          c <- (sess.cookies ++ cookies).valuesIterator
          if !c.hasExpired
          if c.getDomain == null || c.getDomain == url1.getHost
          if c.getPath == null || url1.getPath.startsWith(c.getPath)
        } yield (c.getName, c.getValue)

        val allCookies = sessionCookieValues ++ cookieValues
        if (allCookies.nonEmpty){
          connection.setRequestProperty(
            "Cookie",
            allCookies
              .map{case (k, v) => s"""$k="$v""""}
              .mkString("; ")
          )
        }

        if (upperCaseVerb == "POST" || upperCaseVerb == "PUT" || upperCaseVerb == "PATCH" || upperCaseVerb == "DELETE") {
          if (!chunkedUpload) {
            val bytes = new ByteArrayOutputStream()
            usingOutputStream(compress.wrap(bytes)) { os => data.write(os) }
            val byteArray = bytes.toByteArray
            connection.setFixedLengthStreamingMode(byteArray.length)
            usingOutputStream(connection.getOutputStream) { os => os.write(byteArray) }
          } else {
            connection.setChunkedStreamingMode(0)
            usingOutputStream(compress.wrap(connection.getOutputStream)) { os => data.write(os) }
          }
        }

        val (responseCode, responseMsg, headerFields) = try {(
          connection.getResponseCode,
          connection.getResponseMessage,
          connection.getHeaderFields.asScala
            .filter(_._1 != null)
            .map{case (k, v) => (k.toLowerCase(), v.asScala.toSeq)}.toMap
        )} catch{
          case e: java.net.SocketTimeoutException =>
            throw new TimeoutException(url, readTimeout, connectTimeout)
          case e: java.net.UnknownHostException =>
            throw new UnknownHostException(url, e.getMessage)
          case e: javax.net.ssl.SSLHandshakeException =>
            throw new InvalidCertException(url, e)
        }

        val deGzip = autoDecompress && headerFields.get("content-encoding").toSeq.flatten.exists(_.contains("gzip"))
        val deDeflate = autoDecompress && headerFields.get("content-encoding").toSeq.flatten.exists(_.contains("deflate"))
        def persistCookies() = {
          if (sess.persistCookies) {
            headerFields
              .get("set-cookie")
              .iterator
              .flatten
              .flatMap(HttpCookie.parse(_).asScala)
              .foreach(c => sess.cookies(c.getName) = c)
          }
        }

        if (responseCode.toString.startsWith("3") && maxRedirects > 0){
          val out = new ByteArrayOutputStream()
          Util.transferTo(connection.getInputStream, out)
          val bytes = out.toByteArray

          val current = Response(
            url,
            responseCode,
            responseMsg,
            new geny.Bytes(bytes),
            headerFields,
            redirectedFrom
          )
          persistCookies()
          val newUrl = current.headers("location").head
          apply(
            verb, sess,
            new java.net.URL(url1, newUrl).toString, auth, params, blobHeaders,
            headers, data, readTimeout, connectTimeout, proxy, cert, sslContext, cookies,
            cookieValues, maxRedirects - 1, verifySslCerts, autoDecompress,
            compress, keepAlive, check, chunkedUpload, Some(current),
            onHeadersReceived
          ).readBytesThrough(f)
        }else{
          persistCookies()
          val streamHeaders = StreamHeaders(
            url,
            responseCode,
            responseMsg,
            headerFields,
            redirectedFrom
          )
          if (onHeadersReceived != null) onHeadersReceived(streamHeaders)

          val stream =
            if (connection.getResponseCode.toString.startsWith("2")) connection.getInputStream
            else connection.getErrorStream

          def processWrappedStream[V](f: java.io.InputStream => V): V = {
            // The HEAD method is identical to GET except that the server
            // MUST NOT return a message-body in the response.
            // https://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html section 9.4
            if (upperCaseVerb == "HEAD") f(new ByteArrayInputStream(Array()))
            else if (stream != null) {
              try f(
                if (deGzip) new GZIPInputStream(stream)
                else if (deDeflate) new InflaterInputStream(stream)
                else stream
              ) finally if (!keepAlive) stream.close()
            }else{
              f(new ByteArrayInputStream(Array()))
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
        }
      } finally if (!keepAlive && connection != null) {
        connection.disconnect()
      }
    }
  }

  private def usingOutputStream[T](os: OutputStream)(fn: OutputStream => T): Unit =
    try fn(os) finally os.close()

}
