package requests

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scala.scalanative.meta.LinktimeInfo.isWindows
import scala.scalanative.unsigned.UInt
import scala.collection.mutable.ArrayBuffer
import java.nio.charset.StandardCharsets

private[requests] trait CurlSlist { }

object CurlSlist {

  def empty: Ptr[CurlSlist] = null

  implicit def curlSlistSyntax(slist: Ptr[CurlSlist]): CurlSlistOps = 
    new CurlSlistOps(slist)
    
  final class CurlSlistOps(slist: Ptr[CurlSlist]) {

    private val ccurl = libcurlPlatformCompat.instance

    def append(s: String): Ptr[CurlSlist] = 
      Zone { implicit z =>
        ccurl.slistAppend(slist, toCString(s))
      }

    def free: Unit = 
      ccurl.slistFree(slist)
          
  }
}

private[requests] trait Curl {}

private[requests] trait CurlMulti {}

private[requests] object CurlMulti {
  implicit def curlMultiSyntax(curlMulti: Ptr[CurlMulti]): CurlMultiOps = 
    new CurlMultiOps(curlMulti)
    
    
  final class CurlMultiOps(multi: Ptr[CurlMulti]) {

    private val ccurl = libcurlPlatformCompat.instance
    
    def cleanup: Unit = ccurl.multiCleanup(multi)

    def add(handle: Ptr[Curl]): CurlMultiCode.CurlMultiCode = 
      CurlMultiCode(ccurl.multiAddHandle(multi, handle))

    def remove(handle: Ptr[Curl]): CurlMultiCode.CurlMultiCode = 
      CurlMultiCode(ccurl.multiRemoveHandle(multi, handle))

    def perform: (CurlMultiCode.CurlMultiCode, Int) = 
      Zone { implicit z =>
        val runningHandlesPtr = alloc[Int](1)

        val out = ccurl.multiPerform(multi, runningHandlesPtr)
        (CurlMultiCode(out), !runningHandlesPtr)
        
      }

    /**
    * returns the next message in the multi handle's 
    * internal message queue, or null on the left hand side,
    * and the number of remaining messages after this one
    * on the right hand side
    */
    def infoRead: (CurlMessage, Int) = 
      Zone { implicit z =>
        val messagesInQueue = alloc[Int](1)
        val msg = ccurl.multiInfoRead(multi, messagesInQueue)
        val res = if (msg != null)
          new CurlMessage(CurlMessageMessage(msg._1), msg._2, msg._3)
        else 
          null

        (res, !messagesInQueue)
      }

    def infoReadAll: Seq[CurlMessage] = 

      infoRead match {
        case (null, 0) => Seq.empty
        case (msg, i) => 
          val buffer = new ArrayBuffer[CurlMessage](i)
          buffer :+ msg

          var next = infoRead
          while (next._1 != null) {
            buffer :+ next._1
            next = infoRead
          }

          Seq.from(buffer)
          
      }

    def poll(timeoutMs: Int): (CurlMultiCode.CurlMultiCode, Int) = 
      Zone { implicit z =>

        val num = alloc[Int](1)
        
        val code = ccurl.multiPoll(
          multi,
          null,
          0.toUInt,
          timeoutMs,
          num
        )
        (CurlMultiCode(code), !num)
      }

    def wait_(timeoutMs: Int): (CurlMultiCode.CurlMultiCode, Int) = 
      Zone { implicit z =>

        val num = alloc[Int](1)
        
        val code = ccurl.multiPoll(
          multi,
          null,
          0.toUInt,
          timeoutMs,
          num
        )
        (CurlMultiCode(code), !num)
      }
      
    
  }
  
}

private[requests] class CurlMessage(message: CurlMessageMessage.CurlMessageMessage, handle: Ptr[Curl], data: Ptr[_]) {
  def check(compareTo: CurlCode.CurlCode): Boolean =
    data.asInstanceOf[CInt].toInt == compareTo.id
}

private[requests] object Curl {

  implicit def curlSyntax(curl: Ptr[Curl]): CurlOps = new CurlOps(curl)

  // TODO maybe this can go into FFI file, or be pulled out of defines?
  val WritefuncPause = 0x10000001
  val ReadfuncPause = 0x10000001

  final class CurlOps(curl: Ptr[Curl]) {

    private val ccurl = libcurlPlatformCompat.instance

    def cleanup: Unit = 
      ccurl.cleanup(curl)

    def perform: CurlCode.CurlCode = 
      CurlCode(ccurl.perform(curl))

    def setOpt(option: CurlOption.CurlOption, param: Int): CurlCode.CurlCode =
      CurlCode(ccurl.setoptInt(curl, option.id, param))
    
    def setOpt(option: CurlOption.CurlOption, param: Long): CurlCode.CurlCode =
      CurlCode(ccurl.setoptLong(curl, option.id, param))

    def setOpt(option: CurlOption.CurlOption, param: String)(implicit zone: Zone): CurlCode.CurlCode =
      CurlCode(ccurl.setoptPtr(curl, option.id, toCString(param)))

    def setOpt(option: CurlOption.CurlOption, param: Boolean): CurlCode.CurlCode =
      CurlCode(ccurl.setoptInt(curl, option.id, if (param) 1 else 0))

    def setOpt(option: CurlOption.CurlOption): CurlCode.CurlCode =
      CurlCode(ccurl.setoptInt(curl, option.id, 1))

    def setOpt(option: CurlOption.CurlOption, param: Ptr[_]): CurlCode.CurlCode =
      CurlCode(ccurl.setoptPtr(curl, option.id, param))

    def setOpt(option: CurlOption.CurlOption, param: CFuncPtr): CurlCode.CurlCode =
      setOpt(option, CFuncPtr.toPtr(param))

    def pause(flag: CurlPauseFlag.CurlPauseFlag): CurlCode.CurlCode = 
      CurlCode(ccurl.pause(curl, flag.id))

    def unpause: CurlCode.CurlCode = 
      pause(CurlPauseFlag.UnpauseAll)

    def info(curlInfo: CurlInfo.CurlInfo, parameter: Long)(implicit z: Zone): CurlCode.CurlCode = {
      val lPtr = alloc[Long](sizeof[Long])
      !lPtr = parameter
      CurlCode(ccurl.getInfo(curl, curlInfo.id, lPtr))
    }

    def info(curlInfo: CurlInfo.CurlInfo, parameter: String)(implicit z: Zone): CurlCode.CurlCode =
      CurlCode(ccurl.getInfo(curl, curlInfo.id, toCString(parameter, StandardCharsets.UTF_8)))

    def info(curlInfo: CurlInfo.CurlInfo, parameter: Ptr[_]): CurlCode.CurlCode =
      CurlCode(ccurl.getInfo(curl, curlInfo.id, parameter))

    def status: Int = Zone { implicit z =>
      val status = alloc[Long](1)
      val res = info(CurlInfo.ResponseCode, status)
      if (res == CurlCode.Ok) {
        (!status).toInt
      } else {
        throw new Exception(s"Cannot obtain status code: ${res}")
      }
    }

    def headers(flags: CurlHeaderBitmask.CurlHeaderBitmask*): Seq[(String, String)] = {

      val flagsMask = flags.map(_.id).sum

      val first = ccurl.nextHeader(curl, flagsMask.toUInt, -1, null)

      if (first == null) {
        Nil
      } else {

        val res = ArrayBuffer[(String, String)]()

        var next = first
        while (next != null) {

          val scalaHeader = 
            fromCString(first._1) ->  
              fromCString(first._2)

          res.append(scalaHeader)

          next = ccurl.nextHeader(curl, flagsMask.toUInt, -1, next)
          
        }

        res.toSeq
          
      }
    }
  }
  
}

private[requests] object CurlHeaderBitmask extends Enumeration {
  type CurlHeaderBitmask = Value

  val Header = Value(1,"CURLH_HEADER")
  val Trailer = Value(2,"CURLH_TRAILER")
  val Connect = Value(4,"CURLH_CONNECT")
  val `1xx` = Value(8,"CURLH_1XX")
  val Pseudo = Value(16,"CURLH_PSEUDO")
}

private[requests] trait Mime {}

private[requests] trait MimePart {}

private[requests] trait CurlUrl {}

private[requests] object CurlUrl {

  implicit def curlUrlSyntax(curlUrl: Ptr[CurlUrl]): CurlUrlOps = new CurlUrlOps(curlUrl)

  final class CurlUrlOps(url: Ptr[CurlUrl]) {

    private val ccurl = libcurlPlatformCompat.instance

    def cleanup: Unit =
      ccurl.urlCleanup(url)
      
    def set(part: CurlUrlPart.CurlUrlPart, content: String, flags: CurlUrlFlag.CurlUrlFlag*)(implicit zone: Zone): CurlUrlCode.CurlUrlCode = 
      CurlUrlCode(ccurl.urlSet(url, part.id, toCString(content), flags.foldLeft(0)(_ + _.id).toUInt))

    def get(part: CurlUrlPart.CurlUrlPart, flags: CurlUrlFlag.CurlUrlFlag*): (CurlUrlCode.CurlUrlCode, String) = {
      Zone { implicit z => 
        val out: Ptr[Ptr[Byte]] = alloc(1)
        val status = CurlUrlCode(ccurl.urlGet(url, part.id, out, flags.foldLeft(0)(_ + _.id).toUInt))

        val outStr = if (!out == null) "" else fromCString(!out)

        (status, outStr)
      }
    }
    
  }
  
}

private[requests] object libcurlPlatformCompat {
  @extern @link("libcurl") @link("crypt32") @define("REQUESTS_SCALA_CURL_FFI")
  private object libcurlWin64 extends CCurl

  @extern @link("curl") @define("REQUESTS_SCALA_CURL_FFI")  
  private object libcurlDefault extends CCurl

  val instance: CCurl =
    if (isWindows) libcurlWin64
    else libcurlDefault
}

@extern
private[requests] trait CCurl {

 // struct CURLMsg {
 //   CURLMSG msg;       /* what this message means */
 //   CURL *easy_handle; /* the handle it concerns */
 //   union {
 //     void *whatever;    /* message-specific data */
 //     CURLcode result;   /* return code for transfer */
 //   } data;
 // };
  type CurlMsg = CStruct3[
    CInt,
    Ptr[Curl],
    Ptr[_]
  ]

  type CurlHeader = CStruct6[
    Ptr[Byte],
    Ptr[Byte],
    CSize,
    CSize,
    UInt,
    Ptr[_]
  ]

  @name("requests_scala_curl_setopt_int")
  def setoptInt(handle: Ptr[Curl], option: CInt, parameter: Int): CInt = extern

  @name("requests_scala_curl_setopt_long")
  def setoptLong(handle: Ptr[Curl], option: CInt, parameter: Long): CInt = extern

  @name("requests_scala_curl_setopt_pointer")
  def setoptPtr(handle: Ptr[Curl], option: CInt, parameter: Ptr[_]): CInt = extern

  @name("requests_scala_curl_getinfo_pointer")
  def getInfo(handle: Ptr[Curl], info: CInt, parameter: Ptr[_]): CInt = extern

  @name("curl_easy_init")
  def init: Ptr[Curl] = extern

  @name("curl_easy_cleanup")
  def cleanup(handle: Ptr[Curl]): Unit = extern

  @name("curl_easy_perform")
  def perform(easy_handle: Ptr[Curl]): CInt = extern

  @name("curl_easy_pause")
  def pause(easy_handle: Ptr[Curl], bitmask: CInt): CInt = extern

  @name("curl_easy_header")
  def header(easy_handle: Ptr[Curl], name: Ptr[Byte], index: CSize, origin: UInt, request: Int, hout: Ptr[Ptr[CurlHeader]]): CInt = extern

  @name("curl_easy_nextheader")
  def nextHeader(easy_handle: Ptr[Curl], origin: UInt, request: Int, prev: Ptr[CurlHeader]): Ptr[CurlHeader] = extern

  @name("curl_url")
  def url(): Ptr[CurlUrl] = extern

  @name("curl_url_cleanup")
  def urlCleanup(url: Ptr[CurlUrl]): Unit = extern

  @name("curl_url_set")
  def urlSet(url: Ptr[CurlUrl], part: CInt, content: Ptr[_], flags: UInt): CInt = extern 

  @name("curl_url_get")
  def urlGet(url: Ptr[CurlUrl], part: CInt, content: Ptr[Ptr[Byte]], flags: UInt): CInt = extern

  @name("curl_slist_append")
  def slistAppend(list: Ptr[CurlSlist], string: CString): Ptr[CurlSlist] = extern

  @name("curl_slist_free_all")
  def slistFree(list: Ptr[CurlSlist]): Unit = extern

  @name("curl_multi_init")
  def multiInit(): Ptr[CurlMulti] = extern
  
  @name("curl_multi_cleanup")
  def multiCleanup(multi: Ptr[CurlMulti]): Unit = extern

  @name("curl_multi_add_handle")
  def multiAddHandle(multi: Ptr[CurlMulti], easy: Ptr[Curl]): CInt = extern

  @name("curl_multi_add_handle")
  def multiRemoveHandle(multi: Ptr[CurlMulti], easy: Ptr[Curl]): CInt = extern

  @name("curl_multi_perform")
  def multiPerform(multi: Ptr[CurlMulti], runningHandles: Ptr[Int]): CInt = extern

  @name("curl_multi_info_read")
  def multiInfoRead(multi: Ptr[CurlMulti], msgsInQueue: Ptr[Int]): Ptr[CurlMsg] = extern

  @name("curl_multi_poll")
  def multiPoll(multi: Ptr[CurlMulti], extra_fds: Ptr[_], extra_nfds: UInt, timeoutMs: Int, numfds: Ptr[Int]): CInt = extern

  @name("curl_multi_wait")
  def multiWait(multi: Ptr[CurlMulti], extra_fds: Ptr[_], extra_nfds: UInt, timeoutMs: Int, numfds: Ptr[Int]): CInt = extern



// CURLMcode curl_multi_wait(CURLM *multi_handle,
//                           struct curl_waitfd extra_fds[],
//                           unsigned int extra_nfds,
//                           int timeout_ms,
//                           int *numfds);
                            // CURLMcode curl_multi_poll(CURLM *multi_handle,
  //                         struct curl_waitfd extra_fds[],
  //                         unsigned int extra_nfds,
  //                         int timeout_ms,
  //                         int *numfds);
}

private[requests] object CurlMessageMessage extends Enumeration {
  type CurlMessageMessage = Value
  val None = Value(0, "CURLMSG_NONE")
  val Done = Value(0, "CURLMSG_DONE")
  val Last = Value(0, "CURLMSG_LAST")
  
}

private[requests] object CurlHeaderCode extends Enumeration {
  type CurlHeaderCode = Value

  val Ok = Value(0, "CURLHE_OK")
  val Badindex = Value(0, "CURLHE_BADINDEX")
  val Missing = Value(0, "CURLHE_MISSING")
  val Noheaders = Value(0, "CURLHE_NOHEADERS")
  val Norequest = Value(0, "CURLHE_NOREQUEST")
  val OutOfMemory = Value(0, "CURLHE_OUT_OF_MEMORY")
  val BadArgument = Value(0, "CURLHE_BAD_ARGUMENT")
  val NotBuiltIn = Value(0, "CURLHE_NOT_BUILT_IN")
}

private[requests] object CurlMultiCode extends Enumeration {
  type CurlMultiCode = Value

  val CallMultiPerform = Value(-1, "CALL_MULTI_PERFORM")
  val Ok = Value(0, "OK")
  val BadHandle = Value(1, "BAD_HANDLE")
  val BadEasyHandle = Value(2, "BAD_EASY_HANDLE")
  val OutOfMemory = Value(3, "OUT_OF_MEMORY")
  val InternalError = Value(4, "INTERNAL_ERROR")
  val BadSocket = Value(5, "BAD_SOCKET")
  val UnknownOption = Value(6, "UNKNOWN_OPTION")
  val AddedAlready = Value(7, "ADDED_ALREADY")
  val RecursiveApiCall = Value(8, "RECURSIVE_API_CALL")
  val WakeupFailure = Value(9, "WAKEUP_FAILURE")
  val BadFunctionArgument = Value(10, "BAD_FUNCTION_ARGUMENT")
  val AbortedByCallback = Value(11, "ABORTED_BY_CALLBACK")
  val UnrecoverablePoll = Value(12, "UNRECOVERABLE_POLL")
  val Last = Value(13, "LAST")
  
}

private[requests] object CurlCode extends Enumeration {
  type CurlCode = Value
  val Ok = Value(0, "OK")
  val UnsupportedProtocol = Value(1, "UNSUPPORTED_PROTOCOL")
  val FailedInit = Value(2, "FAILED_INIT")
  val UrlMalformat = Value(3, "URL_MALFORMAT")
  val NotBuiltIn = Value(4, "NOT_BUILT_IN")
  val CouldntResolveProxy = Value(5, "COULDNT_RESOLVE_PROXY")
  val CouldntResolveHost = Value(6, "COULDNT_RESOLVE_HOST")
  val CouldntConnect = Value(7, "COULDNT_CONNECT")
  val WeirdServerReply = Value(8, "WEIRD_SERVER_REPLY")
  val RemoteAccessDenied = Value(9, "REMOTE_ACCESS_DENIED")
  val FtpAccessFailed = Value(10, "FTP_ACCEPT_FAILED")
  val FtpWeirdPassReply = Value(11, "FTP_WEIRD_PASS_REPLY")
  val FtpAccessTimeout = Value(12, "FTP_ACCEPT_TIMEOUT")
  val FtpWeirdPasvReply = Value(13, "FTP_WEIRD_PASV_REPLY")
  val FtpWeird227Format = Value(14, "FTP_WEIRD_227_FORMAT")
  val FtpCantGetHost = Value(15, "FTP_CANT_GET_HOST")
  val Http2 = Value(16, "HTTP2")
  val FtpCouldntSetType = Value(17, "FTP_COULDNT_SET_TYPE")
  val PartialFile = Value(18, "PARTIAL_FILE")
  val FtpCouldntRetrFile = Value(19, "FTP_COULDNT_RETR_FILE")
  val Obsolete20 = Value(20, "OBSOLETE20")
  val QuoteError = Value(21, "QUOTE_ERROR")
  val HttpReturnedError = Value(22, "HTTP_RETURNED_ERROR")
  val WriteError = Value(23, "WRITE_ERROR")
  val Obsolete24 = Value(24, "OBSOLETE24")
  val UploadFailed = Value(25, "UPLOAD_FAILED")
  val ReadError = Value(26, "READ_ERROR")
  val OutOfMemory = Value(27, "OUT_OF_MEMORY")
  val OperationTimedOut = Value(28, "OPERATION_TIMEDOUT")
  val Obsolete29 = Value(29, "OBSOLETE29")
  val FtpPortFailed = Value(30, "FTP_PORT_FAILED")
  val FtpCouldntUseRest = Value(31, "FTP_COULDNT_USE_REST")
  val Obsolete32 = Value(32, "OBSOLETE32")
  val RangeError = Value(33, "RANGE_ERROR")
  val HttpPortError = Value(34, "HTTP_POST_ERROR")
  val SslConnectError = Value(35, "SSL_CONNECT_ERROR")
  val BadDownloadResume = Value(36, "BAD_DOWNLOAD_RESUME")
  val FileCouldntReadFile = Value(37, "FILE_COULDNT_READ_FILE")
  val LdapCannotBind = Value(38, "LDAP_CANNOT_BIND")
  val LdapSearchFailed = Value(39, "LDAP_SEARCH_FAILED")
  val Obsolete40 = Value(40, "OBSOLETE40")
  val FunctionNotFound = Value(41, "FUNCTION_NOT_FOUND")
  val AbortedByCallback = Value(42, "ABORTED_BY_CALLBACK")
  val BadFunctionArgument = Value(43, "BAD_FUNCTION_ARGUMENT")
  val Obsolete44 = Value(44, "OBSOLETE44")
  val InterfaceFailed = Value(45, "INTERFACE_FAILED")
  val Obsolete46 = Value(46, "OBSOLETE46")
  val TooManyRedirects = Value(47, "TOO_MANY_REDIRECTS")
  val UnknownOption = Value(48, "UNKNOWN_OPTION")
  val TelnetOptionSyntax = Value(49, "TELNET_OPTION_SYNTAX")
  val Obsolete50 = Value(50, "OBSOLETE50")
  val PeerFailedVerification = Value(51, "PEER_FAILED_VERIFICATION")
  val GotNothing = Value(52, "GOT_NOTHING")
  val SslEngineNotFound = Value(53, "SSL_ENGINE_NOTFOUND")
  val SslEngineSetFailed = Value(54, "SSL_ENGINE_SETFAILED")
  val SendError = Value(55, "SEND_ERROR")
  val RecvError = Value(56, "RECV_ERROR")
  val Obsolete57 = Value(57, "OBSOLETE57")
  val SslCertProblem = Value(58, "SSL_CERTPROBLEM")
  val SslCipher = Value(59, "SSL_CIPHER")
  val SslCacert = Value(60, "SSL_CACERT")
  val BadContentEncoding = Value(61, "BAD_CONTENT_ENCODING")
  val LdapInvalidUrl = Value(62, "LDAP_INVALID_URL")
  val FileSizeExceeded = Value(63, "FILESIZE_EXCEEDED")
  val UseSslFailed = Value(64, "USE_SSL_FAILED")
  val SendFailRewind = Value(65, "SEND_FAIL_REWIND")
  val SslEngineInitFailed = Value(66, "SSL_ENGINE_INITFAILED")
  val LoginDenied = Value(67, "LOGIN_DENIED")
  val TftpNotFound = Value(68, "TFTP_NOTFOUND")
  val TftpPerm = Value(69, "TFTP_PERM")
  val RemoteDiskFull = Value(70, "REMOTE_DISK_FULL")
  val TftpIllegal = Value(71, "TFTP_ILLEGAL")
  val TftpUnknownId = Value(72, "TFTP_UNKNOWNID")
  val RemoteFileExists = Value(73, "REMOTE_FILE_EXISTS")
  val TftpNoSuchUser = Value(74, "TFTP_NOSUCHUSER")
  val ConvFailed = Value(75, "CONV_FAILED")
  val ConvReqd = Value(76, "CONV_REQD")
  val SslCacertBadfile = Value(77, "SSL_CACERT_BADFILE")
  val RemoteFileNotFound = Value(78, "REMOTE_FILE_NOT_FOUND")
  val Ssh = Value(79, "SSH")
  val SslShutdownFailed = Value(80, "SSL_SHUTDOWN_FAILED")
  val Again = Value(81, "AGAIN")
  val SslCrlBadFile = Value(82, "SSL_CRL_BADFILE")
  val SslIssuerError = Value(83, "SSL_ISSUER_ERROR")
  val FtpPretFailed = Value(84, "FTP_PRET_FAILED")
  val RtspCseqError = Value(85, "RTSP_CSEQ_ERROR")
  val RtspSessionError = Value(86, "RTSP_SESSION_ERROR")
  val FtpBadFileList = Value(87, "FTP_BAD_FILE_LIST")
  val ChunkFailed = Value(88, "CHUNK_FAILED")
  val NoConnectionAvailable = Value(89, "NO_CONNECTION_AVAILABLE")
  val SslPinnedPubKeyNoMatch = Value(90, "SSL_PINNEDPUBKEYNOTMATCH")
  val SslInvalidCertStatus = Value(91, "SSL_INVALIDCERTSTATUS")
  val Http2Stream = Value(92, "HTTP2_STREAM")
}

private[requests] object CurlOption extends Enumeration {
  type CurlOption = Value

  /** * This is the FILE * or void * the regular output should be written to.
    */
  val WriteData = Value(10000 + 1)

  /** The full URL to get/put */
  val Url = Value(10000 + 2)

  /** Port number to connect to, if other than default. */
  val Port = Value(3)

  /** Name of proxy to use. */
  val Proxy = Value(10000 + 4)

  /** "user:password;options" to use when fetching. */
  val UserPassword = Value(10000 + 5)

  /** "user:password" to use with proxy. */
  val ProxyUserPassword = Value(10000 + 6)

  /** Range to get, specified as an ASCII string. */
  val Range = Value(10000 + 7)

  /** Specified file stream to upload from (use as input): */
  val ReadData = Value(10000 + 9)

  /** Buffer to receive error messages in, must be at least CURL_ERROR_SIZE bytes big.
    */
  val ErrorBuffer = Value(10000 + 10)

  /** Function that will be called to store the output (instead of fwrite). The parameters will use fwrite() syntax,
    * make sure to follow them.
    */
  val WriteFunction = Value(20000 + 11)

  /** Function that will be called to read the input (instead of fread). The parameters will use fread() syntax, make
    * sure to follow them.
    */
  val ReadFunction = Value(20000 + 12)

  /** Time-out the read operation after this amount of seconds */
  val Timeout = Value(13)

  /** If the CURLOPT_INFILE is used, this can be used to inform libcurl about how large the file being sent really is.
    * That allows better error checking and better verifies that the upload was successful. -1 means unknown size.
    *
    * For large file support, there is also a _LARGE version of the key which takes an off_t type, allowing platforms
    * with larger off_t sizes to handle larger files. See below for INFILESIZE_LARGE.
    */
  val InFileSize = Value(14)

  /** POST static input fields. */
  val PostFields = Value(10000 + 15)

  /** Set the referrer page (needed by some CGIs) */
  val Referer = Value(10000 + 16)

  /** Set the FTP PORT string (interface name, named or numerical IP address) Use i.e '-' to use default address.
    */
  val FtpPort = Value(10000 + 17)

  /** Set the User-Agent string (examined by some CGIs) */
  val UserAgent = Value(10000 + 18)

  /** Set the "low speed limit" */
  val LowSpeedLimit = Value(19)

  /** Set the "low speed time" */
  val LowSpeedTime = Value(20)

  /** Set the continuation offset.
    *
    * Note there is also a _LARGE version of this key which uses off_t types, allowing for large file offsets on
    * platforms which use larger-than-32-bit off_t's. Look below for RESUME_FROM_LARGE.
    */
  val ResumeFrom = Value(21)

  /** Set cookie in request: */
  val Cookie = Value(10000 + 22)

  /** This points to a linked list of headers, struct curl_slist kind. This list is also used for RTSP (in spite of its
    * name)
    */
  val HttpHeader = Value(10000 + 23)

  /** This points to a linked list of post entries, struct curl_httppost */
  val HttpPost = Value(10000 + 24)

  /** name of the file keeping your private SSL-certificate */
  val SslCertificate = Value(10000 + 25)

  /** password for the SSL or SSH private key */
  val KeyPassword = Value(10000 + 26)

  /** send TYPE parameter? */
  val CrLf = Value(27)

  /** send linked-list of QUOTE commands */
  val Quote = Value(10000 + 28)

  /** send FILE * or void * to store headers to, if you use a callback it is simply passed to the callback unmodified
    */
  val HeaderData = Value(10000 + 29)

  /** point to a file to read the initial cookies from, also enables "cookie awareness"
    */
  val CookieFile = Value(10000 + 31)

  /** What version to specifically try to use. See CURL_SSLVERSION defines below.
    */
  val SslVersion = Value(32)

  /** Custom request, for customizing the get command like HTTP: DELETE, TRACE and others FTP: to use a different list
    * command
    */
  val CustomRequest = Value(10000 + 36)

  /** FILE handle to use instead of stderr */
  val StdErr = Value(10000 + 37)

  /** send linked-list of post-transfer QUOTE commands */
  val PostQuote = Value(10000 + 39)

  /** talk a lot */
  val Verbose = Value(41)

  /** throw the header out too */
  val Header = Value(42)

  /** shut off the progress meter */
  val NoProgress = Value(43)

  /** use HEAD to get http document */
  val Head = Value(44)

  /** no output on http error codes >= 400 */
  val FailOnError = Value(45)

  /** this is an upload */
  val Upload = Value(46)

  /** HTTP POST method */
  val Post = Value(47)

  /** bare names when listing directories */
  val DirListOnly = Value(48)

  /** Append instead of overwrite on upload! */
  val Append = Value(50)

  /** Specify whether to read the user+password from the .netrc or the URL. This must be one of the CURL_NETRC_* enums
    * below.
    */
  val Netrc = Value(51)

  /** use Location: Luke! */
  val FollowLocation = Value(52)

  /** transfer data in text/ASCII format */
  val TransferText = Value(53)

  /** HTTP PUT */
  val Put = Value(54)

  /** Data passed to the CURLOPT_PROGRESSFUNCTION and CURLOPT_XFERINFOFUNCTION callbacks
    */
  val ProgressData = Value(10000 + 57)

  /** We want the referrer field set automatically when following locations */
  val AutoReferer = Value(58)

  /** Port of the proxy, can be set in the proxy string as well with: "[host]:[port]"
    */
  val ProxyPort = Value(59)

  /** size of the POST input data, if strlen() is not good to use */
  val PostFieldSize = Value(60)

  /** tunnel non-http operations through a HTTP proxy */
  val HttpProxyTunnel = Value(61)

  /** Set the interface string to use as outgoing network interface */
  val Interface = Value(10000 + 62)

  /** Set the krb4/5 security level, this also enables krb4/5 awareness. This is a string, 'clear', 'safe',
    * 'confidential' or 'private'. If the string is set but doesn't match one of these, 'private' will be used.
    */
  val KrbLevel = Value(10000 + 63)

  /** Set if we should verify the peer in ssl handshake, set 1 to verify. */
  val SslVerifyPeer = Value(64)

  /** The CApath or CAfile used to validate the peer certificate this option is used only if SSL_VERIFYPEER is true
    */
  val CaInfo = Value(10000 + 65)

  /** Maximum number of http redirects to follow */
  val MaxRedirects = Value(68)

  /** Pass a long set to 1 to get the date of the requested document (if possible)! Pass a zero to shut it off.
    */
  val FileTime = Value(69)

  /** This points to a linked list of telnet options */
  val TelnetOptions = Value(10000 + 70)

  /** Max amount of cached alive connections */
  val MaxConnects = Value(71)

  /** Set to explicitly use a new connection for the upcoming transfer. Do not use this unless you're absolutely sure of
    * this, as it makes the operation slower and is less friendly for the network.
    */
  val FreshConnect = Value(74)

  /** Set to explicitly forbid the upcoming transfer's connection to be re-used when done. Do not use this unless you're
    * absolutely sure of this, as it makes the operation slower and is less friendly for the network.
    */
  val ForbidReuse = Value(75)

  /** Set to a file name that contains random data for libcurl to use to seed the random engine when doing SSL connects.
    */
  val RandomFile = Value(10000 + 76)

  /** Set to the Entropy Gathering Daemon socket pathname */
  val EntropyGatheringDaemonSocket = Value(10000 + 77)

  /** Time-out connect operations after this amount of seconds, if connects are OK within this time, then fine... This
    * only aborts the connect phase.
    */
  val ConnectTimeout = Value(78)

  /** Function that will be called to store headers (instead of fwrite). The parameters will use fwrite() syntax, make
    * sure to follow them.
    */
  val HeaderFunction = Value(20000 + 79)

  /** Set this to force the HTTP request to get back to GET. Only really usable if POST, PUT or a custom request have
    * been used first.
    */
  val HttpGet = Value(80)

  /** Set if we should verify the Common name from the peer certificate in ssl handshake, set 1 to check existence, 2 to
    * ensure that it matches the provided hostname.
    */
  val SslVerifyHost = Value(81)

  /** Specify which file name to write all known cookies in after completed operation. Set file name to "-" (dash) to
    * make it go to stdout.
    */
  val CookieJar = Value(10000 + 82)

  /** Specify which SSL ciphers to use */
  val SslCipherList = Value(10000 + 83)

  /** Specify which HTTP version to use! This must be set to one of the CURL_HTTP_VERSION* enums set below.
    */
  val HttpVersion = Value(84)

  /** Specifically switch on or off the FTP engine's use of the EPSV command. By default, that one will always be
    * attempted before the more traditional PASV command.
    */
  val FtpUseEpsv = Value(85)

  /** type of the file keeping your SSL-certificate ("DER", "PEM", "ENG") */
  val Sslcerttype = Value(10000 + 86)

  /** name of the file keeping your private SSL-key */
  val Sslkey = Value(10000 + 87)

  /** type of the file keeping your private SSL-key ("DER", "PEM", "ENG") */
  val Sslkeytype = Value(10000 + 88)

  /** crypto engine for the SSL-sub system */
  val Sslengine = Value(10000 + 89)

  /** set the crypto engine for the SSL-sub system as default the param has no meaning...
    */
  val SslengineDefault = Value(90)

  /** DNS cache timeout */
  val DnsCacheTimeout = Value(92)

  /** send linked-list of pre-transfer QUOTE commands */
  val Prequote = Value(10000 + 93)

  /** set the debug function */
  val Debugfunction = Value(20000 + 94)

  /** set the data for the debug function */
  val Debugdata = Value(10000 + 95)

  /** mark this as start of a cookie session */
  val Cookiesession = Value(96)

  /** The CApath directory used to validate the peer certificate this option is used only if SSL_VERIFYPEER is true
    */
  val Capath = Value(10000 + 97)

  /** Instruct libcurl to use a smaller receive buffer */
  val Buffersize = Value(98)

  /** Instruct libcurl to not use any signal/alarm handlers, even when using timeouts. This option is useful for
    * multi-threaded applications. See libcurl-the-guide for more background information.
    */
  val Nosignal = Value(99)

  /** Provide a CURLShare for mutexing non-ts data */
  val Share = Value(10000 + 100)

  /** indicates type of proxy. accepted values are CURLPROXY_HTTP (default), CURLPROXY_HTTPS, CURLPROXY_SOCKS4,
    * CURLPROXY_SOCKS4A and CURLPROXY_SOCKS5.
    */
  val Proxytype = Value(101)

  /** Set the Accept-Encoding string. Use this to tell a server you would like the response to be compressed. Before
    * 7.21.6, this was known as CURLOPT_ENCODING
    */
  val AcceptEncoding = Value(10000 + 102)

  /** Set pointer to private data */
  val Private = Value(10000 + 103)

  /** Set aliases for HTTP 200 in the HTTP Response header */
  val Http200aliases = Value(10000 + 104)

  /** Continue to send authentication (user+password) when following locations, even when hostname changed. This can
    * potentially send off the name and password to whatever host the server decides.
    */
  val UnrestrictedAuth = Value(105)

  /** Specifically switch on or off the FTP engine's use of the EPRT command ( it also disables the LPRT attempt). By
    * default, those ones will always be attempted before the good old traditional PORT command.
    */
  val FtpUseEprt = Value(106)

  /** Set this to a bitmask value to enable the particular authentications methods you like. Use this in combination
    * with CURLOPT_USERPWD. Note that setting multiple bits may cause extra network round-trips.
    */
  val Httpauth = Value(107)

  /** Set the ssl context callback function, currently only for OpenSSL ssl_ctx in second argument. The function must be
    * matching the curl_ssl_ctx_callback proto.
    */
  val SslCtxFunction = Value(20000 + 108)

  /** Set the userdata for the ssl context callback function's third argument
    */
  val SslCtxData = Value(10000 + 109)

  /** FTP Option that causes missing dirs to be created on the remote server. In 7.19.4 we introduced the convenience
    * enums for this option using the CURLFTP_CREATE_DIR prefix.
    */
  val FtpCreateMissingDirs = Value(110)

  /** Set this to a bitmask value to enable the particular authentications methods you like. Use this in combination
    * with CURLOPT_PROXYUSERPWD. Note that setting multiple bits may cause extra network round-trips.
    */
  val Proxyauth = Value(111)

  /** FTP option that changes the timeout, in seconds, associated with getting a response. This is different from
    * transfer timeout time and essentially places a demand on the FTP server to acknowledge commands in a timely
    * manner.
    */
  val FtpResponseTimeout = Value(112)

  /** Set this option to one of the CURL_IPRESOLVE_* defines (see below) to tell libcurl to resolve names to those IP
    * versions only. This only has affect on systems with support for more than one, i.e IPv4 _and_ IPv6.
    */
  val Ipresolve = Value(113)

  /** Set this option to limit the size of a file that will be downloaded from an HTTP or FTP server. Note there is also
    * _LARGE version which adds large file support for platforms which have larger off_t sizes. See MAXFILESIZE_LARGE
    * below.
    */
  val Maxfilesize = Value(114)

  /** See the comment for INFILESIZE above, but in short, specifies the size of the file being uploaded. -1 means
    * unknown.
    */
  val InfilesizeLarge = Value(30000 + 115)

  /** Sets the continuation offset. There is also a LONG version of this; look above for RESUME_FROM.
    */
  val ResumeFromLarge = Value(30000 + 116)

  /** Sets the maximum size of data that will be downloaded from an HTTP or FTP server. See MAXFILESIZE above for the
    * LONG version.
    */
  val MaxfilesizeLarge = Value(30000 + 117)

  /** Set this option to the file name of your .netrc file you want libcurl to parse (using the CURLOPT_NETRC option).
    * If not set, libcurl will do a poor attempt to find the user's home directory and check for a .netrc file in there.
    */
  val NetrcFile = Value(10000 + 118)

  /** Enable SSL/TLS for FTP, pick one of: CURLUSESSL_TRY - try using SSL, proceed anyway otherwise CURLUSESSL_CONTROL -
    * SSL for the control connection or fail CURLUSESSL_ALL - SSL for all communication or fail
    */
  val UseSsl = Value(119)

  /** The _LARGE version of the standard POSTFIELDSIZE option */
  val PostfieldsizeLarge = Value(30000 + 120)

  /** Enable/disable the TCP Nagle algorithm */
  val TcpNoDelay = Value(121)

  /** When FTP over SSL/TLS is selected (with CURLOPT_USE_SSL), this option can be used to change libcurl's default
    * action which is to first try "AUTH SSL" and then "AUTH TLS" in this order, and proceed when a OK response has been
    * received. Available parameters are: CURLFTPAUTH_DEFAULT - let libcurl decide CURLFTPAUTH_SSL - try "AUTH SSL"
    * first, then TLS CURLFTPAUTH_TLS - try "AUTH TLS" first, then SSL
    */
  val Ftpsslauth = Value(129)

  val Ioctlfunction = Value(20000 + 130)
  val Ioctldata = Value(10000 + 131)

  /** zero terminated string for pass on to the FTP server when asked for "account" info
    */
  val FtpAccount = Value(10000 + 134)

  /** feed cookie into cookie engine */
  val Cookielist = Value(10000 + 135)

  /** ignore Content-Length */
  val IgnoreContentLength = Value(136)

  /** Set to non-zero to skip the IP address received in a 227 PASV FTP server response. Typically used for FTP-SSL
    * purposes but is not restricted to that. libcurl will then instead use the same IP address it used for the control
    * connection.
    */
  val FtpSkipPasvIp = Value(137)

  /** Select "file method" to use when doing FTP, see the curl_ftpmethod above.
    */
  val FtpFilemethod = Value(138)

  /** Local port number to bind the socket to */
  val LocalPort = Value(139)

  /** Number of ports to try, including the first one set with LOCALPORT. Thus, setting it to 1 will make no additional
    * attempts but the first.
    */
  val Localportrange = Value(140)

  /** no transfer, set up connection and let application use the socket by extracting it with CURLINFO_LASTSOCKET
    */
  val ConnectOnly = Value(141)

  /** Function that will be called to convert from the network encoding (instead of using the iconv calls in libcurl)
    */
  val ConvFromNetworkFunction = Value(20000 + 142)

  /** Function that will be called to convert to the network encoding (instead of using the iconv calls in libcurl)
    */
  val ConvToNetworkFunction = Value(20000 + 143)

  /** Function that will be called to convert from UTF8 (instead of using the iconv calls in libcurl) Note that this is
    * used only for SSL certificate processing
    */
  val ConvFromUtf8Function = Value(20000 + 144)

  /** if the connection proceeds too quickly then need to slow it down */
  /** limit-rate: maximum number of bytes per second to send or receive */
  val MaxSendSpeedLarge = Value(30000 + 145)
  val MaxRecvSpeedLarge = Value(30000 + 146)

  /** Pointer to command string to send if USER/PASS fails. */
  val FtpAlternativeToUser = Value(10000 + 147)

  /** callback function for setting socket options */
  val Sockoptfunction = Value(20000 + 148)
  val Sockoptdata = Value(10000 + 149)

  /** set to 0 to disable session ID re-use for this transfer, default is enabled (== 1)
    */
  val SslSessionidCache = Value(150)

  /** allowed SSH authentication methods */
  val SshAuthTypes = Value(151)

  /** Used by scp/sftp to do public/private key authentication */
  val SshPublicKeyfile = Value(10000 + 152)
  val SshPrivateKeyfile = Value(10000 + 153)

  /** Send CCC (Clear Command Channel) after authentication */
  val FtpSslCcc = Value(154)

  /** Same as TIMEOUT and CONNECTTIMEOUT, but with ms resolution */
  val TimeoutMs = Value(155)
  val ConnectTimeoutMs = Value(156)

  /** set to zero to disable the libcurl's decoding and thus pass the raw body data to the application even when it is
    * encoded/compressed
    */
  val HttpTransferDecoding = Value(157)
  val HttpContentDecoding = Value(158)

  /** Permission used when creating new files and directories on the remote server for protocols that support it,
    * SFTP/SCP/FILE
    */
  val NewFilePerms = Value(159)
  val NewDirectoryPerms = Value(160)

  /** Set the behaviour of POST when redirecting. Values must be set to one of CURL_REDIR* defines below. This used to
    * be called CURLOPT_POST301
    */
  val Postredir = Value(161)

  /** used by scp/sftp to verify the host's public key */
  val SshHostPublicKeyMd5 = Value(10000 + 162)

  /** Callback function for opening socket (instead of socket(2)). Optionally, callback is able change the address or
    * refuse to connect returning CURL_SOCKET_BAD. The callback should have type curl_opensocket_callback
    */
  val Opensocketfunction = Value(20000 + 163)
  val Opensocketdata = Value(10000 + 164)

  /** POST volatile input fields. */
  val Copypostfields = Value(10000 + 165)

  /** set transfer mode (;type=<a|i>) when doing FTP via an HTTP proxy */
  val ProxyTransferMode = Value(166)

  /** Callback function for seeking in the input stream */
  val Seekfunction = Value(20000 + 167)
  val Seekdata = Value(10000 + 168)

  /** CRL file */
  val Crlfile = Value(10000 + 169)

  /** Issuer certificate */
  val Issuercert = Value(10000 + 170)

  /** (IPv6) Address scope */
  val AddressScope = Value(171)

  /** Collect certificate chain info and allow it to get retrievable with CURLINFO_CERTINFO after the transfer is
    * complete.
    */
  val Certinfo = Value(172)

  /** "name" and "pwd" to use when fetching. */
  val Username = Value(10000 + 173)
  val Password = Value(10000 + 174)

  /** "name" and "pwd" to use with Proxy when fetching. */
  val Proxyusername = Value(10000 + 175)
  val Proxypassword = Value(10000 + 176)

  /** Comma separated list of hostnames defining no-proxy zones. These should match both hostnames directly, and
    * hostnames within a domain. For example, local.com will match local.com and www.local.com, but NOT notlocal.com or
    * www.notlocal.com. For compatibility with other implementations of this, .local.com will be considered to be the
    * same as local.com. A single * is the only valid wildcard, and effectively disables the use of proxy.
    */
  val Noproxy = Value(10000 + 177)

  /** block size for TFTP transfers */
  val TftpBlksize = Value(178)

  /** Socks Service */
  val Socks5GssapiNec = Value(180)

  /** set the bitmask for the protocols that are allowed to be used for the transfer, which thus helps the app which
    * takes URLs from users or other external inputs and want to restrict what protocol(s) to deal with. Defaults to
    * CURLPROTO_ALL.
    */
  val Protocols = Value(181)

  /** set the bitmask for the protocols that libcurl is allowed to follow to, as a subset of the CURLOPT_PROTOCOLS ones.
    * That means the protocol needs to be set in both bitmasks to be allowed to get redirected to. Defaults to all
    * protocols except FILE and SCP.
    */
  val RedirProtocols = Value(182)

  /** set the SSH knownhost file name to use */
  val SshKnownhosts = Value(10000 + 183)

  /** set the SSH host key callback, must point to a curl_sshkeycallback function
    */
  val SshKeyfunction = Value(20000 + 184)

  /** set the SSH host key callback custom pointer */
  val SshKeydata = Value(10000 + 185)

  /** set the SMTP mail originator */
  val MailFrom = Value(10000 + 186)

  /** set the list of SMTP mail receiver(s) */
  val MailRcpt = Value(10000 + 187)

  /** FTP: send PRET before PASV */
  val FtpUsePret = Value(188)

  /** RTSP request method (OPTIONS, SETUP, PLAY, etc...) */
  val RtspRequest = Value(189)

  /** The RTSP session identifier */
  val RtspSessionId = Value(10000 + 190)

  /** The RTSP stream URI */
  val RtspStreamUri = Value(10000 + 191)

  /** The Transport: header to use in RTSP requests */
  val RtspTransport = Value(10000 + 192)

  /** Manually initialize the client RTSP CSeq for this handle */
  val RtspClientCseq = Value(193)

  /** Manually initialize the server RTSP CSeq for this handle */
  val RtspServerCseq = Value(194)

  /** The stream to pass to INTERLEAVEFUNCTION. */
  val Interleavedata = Value(10000 + 195)

  /** Let the application define a custom write method for RTP data */
  val Interleavefunction = Value(20000 + 196)

  /** Turn on wildcard matching */
  val Wildcardmatch = Value(197)

  /** Directory matching callback called before downloading of an individual file (chunk) started
    */
  val ChunkBgnFunction = Value(20000 + 198)

  /** Directory matching callback called after the file (chunk) was downloaded, or skipped
    */
  val ChunkEndFunction = Value(20000 + 199)

  /** Change match (fnmatch-like) callback for wildcard matching */
  val FnmatchFunction = Value(20000 + 200)

  /** Let the application define custom chunk data pointer */
  val ChunkData = Value(10000 + 201)

  /** FNMATCH_FUNCTION user pointer */
  val FnmatchData = Value(10000 + 202)

  /** send linked-list of name:port:address sets */
  val Resolve = Value(10000 + 203)

  /** Set a username for authenticated TLS */
  val TlsauthUsername = Value(10000 + 204)

  /** Set a password for authenticated TLS */
  val TlsauthPassword = Value(10000 + 205)

  /** Set authentication type for authenticated TLS */
  val TlsauthType = Value(10000 + 206)

  /** Set to 1 to enable the "TE:" header in HTTP requests to ask for compressed transfer-encoded responses. Set to 0 to
    * disable the use of TE: in outgoing requests. The current default is 0, but it might change in a future libcurl
    * release. libcurl will ask for the compressed methods it knows of, and if that isn't any, it will not ask for
    * transfer-encoding at all even if this option is set to 1.
    */
  val TransferEncoding = Value(207)

  /** Callback function for closing socket (instead of close(2)). The callback should have type
    * curl_closesocket_callback
    */
  val Closesocketfunction = Value(20000 + 208)
  val Closesocketdata = Value(10000 + 209)

  /** allow GSSAPI credential delegation */
  val GssapiDelegation = Value(210)

  /** Set the name servers to use for DNS resolution */
  val DnsServers = Value(10000 + 211)

  /** Time-out accept operations (currently for FTP only) after this amount of milliseconds.
    */
  val AccepttimeoutMs = Value(212)

  /** Set TCP keepalive */
  val TcpKeepalive = Value(213)

  /** non-universal keepalive knobs (Linux, AIX, HP-UX, more) */
  val TcpKeepidle = Value(214)
  val TcpKeepintvl = Value(215)

  /** Enable/disable specific SSL features with a bitmask, see CURLSSLOPT_* */
  val SslOptions = Value(216)

  /** Set the SMTP auth originator */
  val MailAuth = Value(10000 + 217)

  /** Enable/disable SASL initial response */
  val SaslIr = Value(218)

  /** Function that will be called instead of the internal progress display function. This function should be defined as
    * the curl_xferinfo_callback prototype defines. (Deprecates CURLOPT_PROGRESSFUNCTION)
    */
  val Xferinfofunction = Value(20000 + 219)

  /** The XOAUTH2 bearer token */
  val Xoauth2Bearer = Value(10000 + 220)

  /** Set the interface string to use as outgoing network interface for DNS requests. Only supported by the c-ares DNS
    * backend
    */
  val DnsInterface = Value(10000 + 221)

  /** Set the local IPv4 address to use for outgoing DNS requests. Only supported by the c-ares DNS backend
    */
  val DnsLocalIp4 = Value(10000 + 222)

  /** Set the local IPv6 address to use for outgoing DNS requests. Only supported by the c-ares DNS backend
    */
  val DnsLocalIp6 = Value(10000 + 223)

  /** Set authentication options directly */
  val LoginOptions = Value(10000 + 224)

  /** Enable/disable TLS NPN extension (http2 over ssl might fail without) */
  val SslEnableNpn = Value(225)

  /** Enable/disable TLS ALPN extension (http2 over ssl might fail without) */
  val SslEnableAlpn = Value(226)

  /** Time to wait for a response to a HTTP request containing an Expect: 100-continue header before sending the data
    * anyway.
    */
  val Expect_100TimeoutMs = Value(227)

  /** This points to a linked list of headers used for proxy requests only, struct curl_slist kind
    */
  val Proxyheader = Value(10000 + 228)

  /** Pass in a bitmask of "header options" */
  val Headeropt = Value(229)

  /** The public key in DER form used to validate the peer public key this option is used only if SSL_VERIFYPEER is true
    */
  val Pinnedpublickey = Value(10000 + 230)

  /** Path to Unix domain socket */
  val UnixSocketPath = Value(10000 + 231)

  /** Set if we should verify the certificate status. */
  val SslVerifystatus = Value(232)

  /** Set if we should enable TLS false start. */
  val SslFalsestart = Value(233)

  /** Do not squash dot-dot sequences */
  val PathAsIs = Value(234)

  /** Proxy Service Name */
  val ProxyServiceName = Value(10000 + 235)

  /** Service Name */
  val ServiceName = Value(10000 + 236)

  /** Wait/don't wait for pipe/mutex to clarify */
  val Pipewait = Value(237)

  /** Set the protocol used when curl is given a URL without a protocol */
  val DefaultProtocol = Value(10000 + 238)

  /** Set stream weight, 1 - 256 (default is 16) */
  val StreamWeight = Value(239)

  /** Set stream dependency on another CURL handle */
  val StreamDepends = Value(10000 + 240)

  /** Set E-xclusive stream dependency on another CURL handle */
  val StreamDependsE = Value(10000 + 241)

  /** Do not send any tftp option requests to the server */
  val TftpNoOptions = Value(242)

  /** Linked-list of host:port:connect-to-host:connect-to-port, overrides the URL's host:port (only for the network
    * layer)
    */
  val ConnectTo = Value(10000 + 243)

  /** Set TCP Fast Open */
  val TcpFastopen = Value(244)

  /** Continue to send data if the server responds early with an HTTP status code >= 300
    */
  val KeepSendingOnError = Value(245)

  /** The CApath or CAfile used to validate the proxy certificate this option is used only if PROXY_SSL_VERIFYPEER is
    * true
    */
  val ProxyCainfo = Value(10000 + 246)

  /** The CApath directory used to validate the proxy certificate this option is used only if PROXY_SSL_VERIFYPEER is
    * true
    */
  val ProxyCapath = Value(10000 + 247)

  /** Set if we should verify the proxy in ssl handshake, set 1 to verify.
    */
  val ProxySslVerifypeer = Value(248)

  /** Set if we should verify the Common name from the proxy certificate in ssl handshake, set 1 to check existence, 2
    * to ensure that it matches the provided hostname.
    */
  val ProxySslVerifyhost = Value(249)

  /** What version to specifically try to use for proxy. See CURL_SSLVERSION defines below.
    */
  val ProxySslversion = Value(250)

  /** Set a username for authenticated TLS for proxy */
  val ProxyTlsauthUsername = Value(10000 + 251)

  /** Set a password for authenticated TLS for proxy */
  val ProxyTlsauthPassword = Value(10000 + 252)

  /** Set authentication type for authenticated TLS for proxy */
  val ProxyTlsauthType = Value(10000 + 253)

  /** name of the file keeping your private SSL-certificate for proxy */
  val ProxySslcert = Value(10000 + 254)

  /** type of the file keeping your SSL-certificate ("DER", "PEM", "ENG") for proxy
    */
  val ProxySslcerttype = Value(10000 + 255)

  /** name of the file keeping your private SSL-key for proxy */
  val ProxySslkey = Value(10000 + 256)

  /** type of the file keeping your private SSL-key ("DER", "PEM", "ENG") for proxy
    */
  val ProxySslkeytype = Value(10000 + 257)

  /** password for the SSL private key for proxy */
  val ProxyKeypasswd = Value(10000 + 258)

  /** Specify which SSL ciphers to use for proxy */
  val ProxySslCipherList = Value(10000 + 259)

  /** CRL file for proxy */
  val ProxyCrlfile = Value(10000 + 260)

  /** Enable/disable specific SSL features with a bitmask for proxy, see CURLSSLOPT_*
    */
  val ProxySslOptions = Value(261)

  /** Name of pre proxy to use. */
  val PreProxy = Value(10000 + 262)

  /** The public key in DER form used to validate the proxy public key this option is used only if PROXY_SSL_VERIFYPEER
    * is true
    */
  val ProxyPinnedpublickey = Value(10000 + 263)

  /** Path to an abstract Unix domain socket */
  val AbstractUnixSocket = Value(10000 + 264)

  /** Suppress proxy CONNECT response headers from user callbacks */
  val SuppressConnectHeaders = Value(265)

  /** The request target, instead of extracted from the URL */
  val RequestTarget = Value(10000 + 266)

  /** bitmask of allowed auth methods for connections to SOCKS5 proxies */
  val Socks5Auth = Value(267)

  /** Enable/disable SSH compression */
  val SshCompression = Value(268)

  /** Post MIME data. */
  val Mimepost = Value(10000 + 269)

  /** Time to use with the CURLOPT_TIMECONDITION. Specified in number of seconds since 1 Jan 1970.
    */
  val TimevalueLarge = Value(30000 + 270)

  /** Head start in milliseconds to give happy eyeballs. */
  val HappyEyeballsTimeoutMs = Value(271)

  /** Function that will be called before a resolver request is made */
  val ResolverStartFunction = Value(20000 + 272)

  /** User data to pass to the resolver start callback. */
  val ResolverStartData = Value(10000 + 273)

  /** send HAProxy PROXY protocol header? */
  val Haproxyprotocol = Value(274)

  /** shuffle addresses before use when DNS returns multiple */
  val DnsShuffleAddresses = Value(275)

  /** Specify which TLS 1.3 ciphers suites to use */
  val Tls13Ciphers = Value(10000 + 276)

  val ProxyTls13Ciphers = Value(10000 + 277)
}

private[requests] object CurlInfo extends Enumeration {
  type CurlInfo = Value

  private val String: Int = 0x100000
  private val Long: Int = 0x200000
  private val Double: Int = 0x300000
  private val Slist: Int = 0x400000
  private val Ptr: Int = 0x400000
  private val Socket: Int = 0x500000
  private val OffT: Int = 0x600000
  private val Mask: Int = 0x0fffff
  private val Typemask: Int = 0xf00000

  val EffectiveUrl = Value(String + 1)
  val ResponseCode = Value(Long + 2)
  val TotalTime = Value(Double + 3)
  val NamelookupTime = Value(Double + 4)
  val ConnectTime = Value(Double + 5)
  val PretransferTime = Value(Double + 6)
  val SizeUpload = Value(Double + 7)
  val SizeUploadT = Value(OffT + 7)
  val SizeDownload = Value(Double + 8)
  val SizeDownloadT = Value(OffT + 8)
  val SpeedDownload = Value(Double + 9)
  val SpeedDownloadT = Value(OffT + 9)
  val SpeedUpload = Value(Double + 10)
  val SpeedUploadT = Value(OffT + 10)
  val HeaderSize = Value(Long + 11)
  val RequestSize = Value(Long + 12)
  val SslVerifyresult = Value(Long + 13)
  val Filetime = Value(Long + 14)
  val FiletimeT = Value(OffT + 14)
  val ContentLengthDownload = Value(Double + 15)
  val ContentLengthDownloadT = Value(OffT + 15)
  val ContentLengthUpload = Value(Double + 16)
  val ContentLengthUploadT = Value(OffT + 16)
  val StarttransferTime = Value(Double + 17)
  val ContentType = Value(String + 18)
  val RedirectTime = Value(Double + 19)
  val RedirectCount = Value(Long + 20)
  val Private = Value(String + 21)
  val HttpConnectcode = Value(Long + 22)
  val HttpauthAvail = Value(Long + 23)
  val ProxyauthAvail = Value(Long + 24)
  val OsErrno = Value(Long + 25)
  val NumConnects = Value(Long + 26)
  val SslEngines = Value(Slist + 27)
  val Cookielist = Value(Slist + 28)
  val Lastsocket = Value(Long + 29)
  val FtpEntryPath = Value(String + 30)
  val RedirectUrl = Value(String + 31)
  val PrimaryIp = Value(String + 32)
  val AppconnectTime = Value(Double + 33)
  val Certinfo = Value(Ptr + 34)
  val ConditionUnmet = Value(Long + 35)
  val RtspSessionId = Value(String + 36)
  val RtspClientCseq = Value(Long + 37)
  val RtspServerCseq = Value(Long + 38)
  val RtspCseqRecv = Value(Long + 39)
  val PrimaryPort = Value(Long + 40)
  val LocalIp = Value(String + 41)
  val LocalPort = Value(Long + 42)
  val TlsSession = Value(Ptr + 43)
  val Activesocket = Value(Socket + 44)
  val TlsSslPtr = Value(Ptr + 45)
  val HttpVersion = Value(Long + 46)
  val ProxySslVerifyresult = Value(Long + 47)
  val Protocol = Value(Long + 48)
  val Scheme = Value(String + 49)
  val TotalTimeT = Value(OffT + 50)
  val NamelookupTimeT = Value(OffT + 51)
  val ConnectTimeT = Value(OffT + 52)
  val PretransferTimeT = Value(OffT + 53)
  val StarttransferTimeT = Value(OffT + 54)
  val RedirectTimeT = Value(OffT + 55)
  val AppconnectTimeT = Value(OffT + 56)
}

/* the error codes for the URL API */
object CurlUrlCode extends Enumeration {
  type CurlUrlCode = Value
  val Ok = Value(0, "CURLUE_OK")
  val BadHandle = Value(1, "CURLUE_BAD_HANDLE")
  val BadPartpointer = Value(2, "CURLUE_BAD_PARTPOINTER")
  val MalformedInput = Value(3, "CURLUE_MALFORMED_INPUT")
  val BadPortNumber = Value(4, "CURLUE_BAD_PORT_NUMBER")
  val UnsupportedScheme = Value(5, "CURLUE_UNSUPPORTED_SCHEME")
  val Urldecode = Value(6, "CURLUE_URLDECODE")
  val OutOfMemory = Value(7, "CURLUE_OUT_OF_MEMORY")
  val UserNotAllowed = Value(8, "CURLUE_USER_NOT_ALLOWED")
  val UnknownPart = Value(9, "CURLUE_UNKNOWN_PART")
  val NoScheme = Value(10, "CURLUE_NO_SCHEME")
  val NoUser = Value(20, "CURLUE_NO_USER")
  val NoPassword = Value(21, "CURLUE_NO_PASSWORD")
  val NoOptions = Value(22, "CURLUE_NO_OPTIONS")
  val NoHost = Value(23, "CURLUE_NO_HOST")
  val NoPort = Value(24, "CURLUE_NO_PORT")
  val NoQuery = Value(25, "CURLUE_NO_QUERY")
  val NoFragment = Value(26, "CURLUE_NO_FRAGMENT")
  val NoZoneid = Value(27, "CURLUE_NO_ZONEID")
  val BadFileUrl = Value(28, "CURLUE_BAD_FILE_URL")
  val BadFragment = Value(29, "CURLUE_BAD_FRAGMENT")
  val BadHostname = Value(30, "CURLUE_BAD_HOSTNAME")
  val BadIpv6 = Value(31, "CURLUE_BAD_IPV6")
  val BadLogin = Value(32, "CURLUE_BAD_LOGIN")
  val BadPassword = Value(33, "CURLUE_BAD_PASSWORD")
  val BadPath = Value(34, "CURLUE_BAD_PATH")
  val BadQuery = Value(35, "CURLUE_BAD_QUERY")
  val BadScheme = Value(36, "CURLUE_BAD_SCHEME")
  val BadSlashes = Value(37, "CURLUE_BAD_SLASHES")
  val BadUser = Value(38, "CURLUE_BAD_USER")
  val LacksIdn = Value(39, "CURLUE_LACKS_IDN")
  val TooLarge = Value(40, "CURLUE_TOO_LARGE")
  val Last = Value(41, "CURLUE_LAST")
}

object CurlUrlPart extends Enumeration {
  type CurlUrlPart = Value
  val Url = Value(0, "CURLUPART_URL")
  val Scheme = Value(1, "CURLUPART_SCHEME")
  val User = Value(2, "CURLUPART_USER")
  val Password = Value(3, "CURLUPART_PASSWORD")
  val Options = Value(4, "CURLUPART_OPTIONS")
  val Host = Value(5, "CURLUPART_HOST")
  val Port = Value(6, "CURLUPART_PORT")
  val Path = Value(7, "CURLUPART_PATH")
  val Query = Value(8, "CURLUPART_QUERY")
  val Fragment = Value(9, "CURLUPART_FRAGMENT")
  val Zoneid = Value(10, "CURLUPART_ZONEID")
}

object CurlUrlFlag extends Enumeration {
  type CurlUrlFlag = Value
  val DefaultPort = Value(1, "CURLU_DEFAULT_PORT")       
  val NoDefaultPort = Value(2, "CURLU_NO_DEFAULT_PORT")    
  val DefaultScheme = Value(4, "CURLU_DEFAULT_SCHEME")     
  val NonSupportScheme = Value(8, "CURLU_NON_SUPPORT_SCHEME") 
  val PathAsIs = Value(16, "CURLU_PATH_AS_IS")         
  val DisallowUser = Value(32, "CURLU_DISALLOW_USER")      
  val Urldecode = Value(64, "CURLU_URLDECODE")          
  val Urlencode = Value(128, "CURLU_URLENCODE")          
  val Appendquery = Value(256, "CURLU_APPENDQUERY")        
  val GuessScheme = Value(512, "CURLU_GUESS_SCHEME")       
  val NoAuthority = Value(1024, "CURLU_NO_AUTHORITY")      
  val AllowSpace = Value(2048, "CURLU_ALLOW_SPACE")       
  val Punycode = Value(4096, "CURLU_PUNYCODE")          
  val Puny2idn = Value(8192, "CURLU_PUNY2IDN")          
  val GetEmpty = Value(16384, "CURLU_GET_EMPTY")         
}

object CurlPauseFlag extends Enumeration {
  type CurlPauseFlag = Value

  val PauseRecv = Value(1, "CURLPAUSE_RECV")
  val PauseSend = Value(2, "CURLPAUSE_SEND")
  val PauseAll = Value(3, "CURLPAUSE_ALL")
  val UnpauseAll = Value(0, "CURLPAUSE_CONT")
  
}
