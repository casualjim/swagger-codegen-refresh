package com.wordnik.client

import com.ning.http._
import client._
import client.{ Cookie => AhcCookie }
import collection.JavaConverters._
import java.util.Locale
import java.util.concurrent.ConcurrentHashMap
import io.{Codec, Source}
import java.nio.charset.Charset
import com.wordnik.client.SwaggerConfig.DataFormat
import java.io.{File, Closeable}
import java.net.URI
import rl.MapQueryString
import akka.dispatch.{Promise, ExecutionContext, Future}
import com.wordnik.model._
import org.json4s.jackson.JsonMethods
import org.json4s._


object Client {
  val DefaultUserAgent = "SwaggerClient/1.0"

  val DefaultConfig = (
    new AsyncHttpClientConfig.Builder()
      setUserAgent DefaultUserAgent
      setCompressionEnabled true      // enable content-compression
      setAllowPoolingConnection true  // enable http keep-alive
      setFollowRedirects false).build()

  private implicit def stringWithExt(s: String) = new {
    def isBlank = s == null || s.trim.isEmpty
    def nonBlank = !isBlank


    def blankOption = if (isBlank) None else Option(s)
  }

  case class CookieOptions(
          domain  : String  = "",
          path    : String  = "",
          maxAge  : Int     = -1,
          secure  : Boolean = false,
          comment : String  = "",
          httpOnly: Boolean = false,
          encoding: String  = "UTF-8")

  trait HttpCookie {
    implicit def cookieOptions: CookieOptions
    def name: String
    def value: String

  }

  case class RequestCookie(name: String, value: String, cookieOptions: CookieOptions = CookieOptions()) extends HttpCookie
  case class Cookie(name: String, value: String)(implicit val cookieOptions: CookieOptions = CookieOptions()) extends HttpCookie {

    private def ensureDotDomain = if (!cookieOptions.domain.startsWith("."))
      "." + cookieOptions.domain
    else
      cookieOptions.domain

    def toCookieString = {
      val sb = new StringBuffer
      sb append name append "="
      sb append value

      if(cookieOptions.domain.nonBlank)
        sb.append("; Domain=").append(ensureDotDomain.toLowerCase(Locale.ENGLISH))

      val pth = cookieOptions.path
      if(pth.nonBlank) sb append "; Path=" append (if(!pth.startsWith("/")) {
        "/" + pth
      } else { pth })

      if(cookieOptions.comment.nonBlank) sb append ("; Comment=") append cookieOptions.comment

      if(cookieOptions.maxAge > -1) sb append "; Max-Age=" append cookieOptions.maxAge

      if (cookieOptions.secure) sb append "; Secure"
      if (cookieOptions.httpOnly) sb append "; HttpOnly"
      sb.toString
    }

  }


  class CookieJar(private val reqCookies: Map[String, RequestCookie])  {
    private val cookies = new ConcurrentHashMap[String, HttpCookie].asScala ++ reqCookies

    def get(key: String) = cookies.get(key) filter (_.cookieOptions.maxAge != 0) map (_.value)

    def apply(key: String) = get(key) getOrElse (throw new Exception("No cookie could be found for the specified key [%s]" format key))

    def update(name: String, value: String)(implicit cookieOptions: CookieOptions=CookieOptions()) = {
      cookies += name -> Cookie(name, value)(cookieOptions)
    }

    def set(name: String, value: String)(implicit cookieOptions: CookieOptions=CookieOptions()) = {
      this.update(name, value)(cookieOptions)
    }

    def delete(name: String)(implicit cookieOptions: CookieOptions = CookieOptions(maxAge = 0)) {
      this.update(name, "")(cookieOptions.copy(maxAge = 0))
    }

    def +=(keyValuePair: (String, String))(implicit cookieOptions: CookieOptions = CookieOptions()) = {
      this.update(keyValuePair._1, keyValuePair._2)(cookieOptions)
    }

    def -=(key: String)(implicit cookieOptions: CookieOptions = CookieOptions(maxAge = 0)) {
      delete(key)(cookieOptions)
    }

    def size =  cookies.size

    def foreach[U](fn: (HttpCookie) => U) = cookies foreach { case (_, v) => fn(v) }

    private[client] def responseCookies = cookies.values collect { case c: Cookie => c }

    override def toString: String = cookies.toString()
  }

  class AhcClientResponse(response: Response)  {
    val cookies = (response.getCookies.asScala map { cookie =>
      val cko = CookieOptions(cookie.getDomain, cookie.getPath, cookie.getMaxAge)
      cookie.getName -> Cookie(cookie.getName, cookie.getValue)(cko)
    }).toMap

    val headers = (response.getHeaders.keySet().asScala map { k => k -> response.getHeaders(k).asScala.mkString("; ")}).toMap

    val status = ResponseStatus(response.getStatusCode, response.getStatusText)

    val contentType = response.getContentType

    val inputStream = response.getResponseBodyAsStream

    val uri = response.getUri

    private[this] var _body: String = null

    def statusCode = status.code
    def statusText = status.line
    def body = {
      if (_body == null) _body = Source.fromInputStream(inputStream).mkString
      _body
    }

    private[this] def nioCharset = charset map Charset.forName getOrElse Codec.UTF8
    def mediaType: Option[String] = headers.get("Content-Type") flatMap { _.split(";").headOption }

    def charset: Option[String] =
      for {
        ct <- headers.get("Content-Type")
        charset <- ct.split(";").drop(1).headOption
      } yield charset.toUpperCase.replace("CHARSET=", "").trim
  }

  private object StringHttpMethod {
    val GET = "GET"
    val POST = "POST"
    val DELETE = "DELETE"
    val PUT = "PUT"
    val CONNECT = "CONNECT"
    val HEAD = "HEAD"
    val OPTIONS = "OPTIONS"
    val PATCH = "PATCH"
    val TRACE = "TRACE"
  }
}
object SwaggerConfig {
  sealed trait DataFormat {
    def name: String
    def contentType: String
  }
  object DataFormat {
    case object Json extends DataFormat {
      val contentType: String = "application/json;charset=utf-8"

      val name: String = "json"
    }
    case object XML extends DataFormat {
      val contentType: String = "application/xml"

      val name: String = "xml"
    }
  }
}

class Client(baseUrl: String, clientConfig: AsyncHttpClientConfig) {

  import Client._
  import Client.StringHttpMethod._
  private implicit val execContext = ExecutionContext.fromExecutorService(clientConfig.executorService())

  private val mimes = new Mimes {
    protected def warn(message: String) = System.err.println("[WARN] " + message)
  }

  private val cookies = new CookieJar(Map.empty)

  private[this] val underlying = new AsyncHttpClient(clientConfig) {
    def preparePatch(uri: String): AsyncHttpClient#BoundRequestBuilder = requestBuilder(PATCH, uri)
    def prepareTrace(uri: String): AsyncHttpClient#BoundRequestBuilder = requestBuilder(TRACE, uri)
  }

  private def requestFactory(method: String): String ⇒ AsyncHttpClient#BoundRequestBuilder = {
    method.toUpperCase(Locale.ENGLISH) match {
      case `GET`     ⇒ underlying.prepareGet _
      case `POST`    ⇒ underlying.preparePost _
      case `PUT`     ⇒ underlying.preparePut _
      case `DELETE`  ⇒ underlying.prepareDelete _
      case `HEAD`    ⇒ underlying.prepareHead _
      case `OPTIONS` ⇒ underlying.prepareOptions _
      case `CONNECT` ⇒ underlying.prepareConnect _
      case `PATCH`   ⇒ underlying.preparePatch _
      case `TRACE`   ⇒ underlying.prepareTrace _
    }
  }

  private def addParameters(method: String, params: Iterable[(String, String)], isMultipart: Boolean = false, charset: Charset = Codec.UTF8)(req: AsyncHttpClient#BoundRequestBuilder) = {
    method.toUpperCase(Locale.ENGLISH) match {
      case `GET` | `DELETE` | `HEAD` | `OPTIONS` ⇒ params foreach { case (k, v) ⇒ req addQueryParameter (k, v) }
      case `PUT` | `POST`   | `PATCH`            ⇒ {
        if (!isMultipart)
          params foreach { case (k, v) ⇒ req addParameter (k, v) }
        else {
          params foreach { case (k, v) => req addBodyPart new StringPart(k, v, charset.name)}
        }
      }
      case _                                     ⇒ // we don't care, carry on
    }
    req
  }

  private def addHeaders(headers: Iterable[(String, String)])(req: AsyncHttpClient#BoundRequestBuilder) = {
    headers foreach { case (k, v) => req.addHeader(k, v) }
    req
  }

  private val allowsBody = Vector(PUT, POST, PATCH)

  def get(uri: String, params: Iterable[(String, Any)], headers: Iterable[(String, String)], body: String) =
    submit(GET, uri, params, headers, body)

  def post(uri: String, params: Iterable[(String, Any)], headers: Iterable[(String, String)], body: String) =
    submit(POST, uri, params, headers, body)

  def put(uri: String, params: Iterable[(String, Any)], headers: Iterable[(String, String)], body: String) =
    submit(PUT, uri, params, headers, body)

  def patch(uri: String, params: Iterable[(String, Any)], headers: Iterable[(String, String)], body: String) =
    submit(PATCH, uri, params, headers, body)

  def delete(uri: String, params: Iterable[(String, Any)], headers: Iterable[(String, String)], body: String) =
    submit(DELETE, uri, params, headers, body)

  def connect(uri: String, params: Iterable[(String, Any)], headers: Iterable[(String, String)], body: String) =
    submit(CONNECT, uri, params, headers, body)

  def options(uri: String, params: Iterable[(String, Any)], headers: Iterable[(String, String)], body: String) =
    submit(OPTIONS, uri, params, headers, body)

  def head(uri: String, params: Iterable[(String, Any)], headers: Iterable[(String, String)], body: String) =
    submit(HEAD, uri, params, headers, body)

  def trace(uri: String, params: Iterable[(String, Any)], headers: Iterable[(String, String)], body: String) =
    submit(TRACE, uri, params, headers, body)


  def submit(method: String, uri: String, params: Iterable[(String, Any)], headers: Iterable[(String, String)], body: String): Future[AhcClientResponse] = {
    val base = URI.create(baseUrl).normalize()
    val u = URI.create(uri).normalize()
    val files = params collect {
      case (k, v: File) => k -> v
    }
    val realParams = params collect {
      case (k, v: String) => k -> v
      case (k, null) => k -> ""
      case (k, v) => k -> v.toString
    }
    val isMultipart = {
      allowsBody.contains(method.toUpperCase(Locale.ENGLISH)) && {
        val ct = (defaultWriteContentType(files) ++ headers)("Content-Type")
        ct.toLowerCase(Locale.ENGLISH).startsWith("multipart/form-data")
      }
    }

    val reqUri = if (u.isAbsolute) u else {
      // There is no constructor on java.net.URI that will not encode the path
      // except for the one where you pass in a uri as string so we're concatenating ourselves
//      val uu = new URI(base.getScheme, base.getUserInfo, base.getHost, base.getPort, base.getRawPath + u.getRawPath, u.getRawQuery, u.getRawFragment)
      val b = "%s://%s:%d".format(base.getScheme, base.getHost, base.getPort)
      val p = base.getRawPath + u.getRawPath.blankOption.getOrElse("/")
      val q = u.getRawQuery.blankOption.map("?"+_).getOrElse("")
      val f = u.getRawFragment.blankOption.map("#"+_).getOrElse("")
      URI.create(b+p+q+f)
    }
    val req = (requestFactory(method)
      andThen (addHeaders(headers) _)
      andThen (addParameters(method, realParams, isMultipart) _))(reqUri.toASCIIString)
    if (isMultipart) {
      files foreach { case (nm, file) =>
        req.addBodyPart(new FilePart(nm, file, mimes(file), FileCharset(file).name))
      }
    }
    if (cookies.size > 0) {
      cookies foreach { cookie =>
        val ahcCookie = new AhcCookie(
          cookie.cookieOptions.domain,
          cookie.name,
          cookie.value,
          cookie.cookieOptions.path,
          cookie.cookieOptions.maxAge,
          cookie.cookieOptions.secure)
        req.addCookie(ahcCookie)
      }
    }
    u.getQuery.blankOption foreach { uu =>
      MapQueryString.parseString(uu) foreach { case (k, v) => v foreach { req.addQueryParameter(k, _) } }
    }
    if (allowsBody.contains(method.toUpperCase(Locale.ENGLISH)) && body.nonBlank) req.setBody(body)

    val promise = Promise[AhcClientResponse]()
    req.execute(async(promise))
    promise
  }

  private[this] def defaultWriteContentType(files: Iterable[(String, File)]) = {
    val value = if (files.nonEmpty) "multipart/form-data" else "application/x-www-form-urlencoded; charset=utf-8"
    Map("Content-Type" -> value)
  }

  private def async(promise: Promise[AhcClientResponse]) = new AsyncCompletionHandler[Promise[AhcClientResponse]] {


    override def onThrowable(t: Throwable) {
      promise.complete(Left(t))
    }

    def onCompleted(response: Response) = {
      promise.complete(Right(new AhcClientResponse(response)))
    }
  }


  def close() = underlying.close()
}
case class SwaggerConfig(
  baseUrl: String,
  jsonFormats: Formats = DefaultFormats,
  dataFormat: SwaggerConfig.DataFormat = DataFormat.Json,
  httpClientConfig: AsyncHttpClientConfig = Client.DefaultConfig)

class SwaggerApiClient(config: SwaggerConfig) extends Closeable {

  private[this] implicit val jsonFormats = config.jsonFormats
  val baseUrl = config.baseUrl
  val dataFormat = config.dataFormat
  private[this] val client = new Client(config.baseUrl, config.httpClientConfig)

  val pets = new PetsApiClient(client, config)

  val store = new StoreApiClient(client, config)

  def close() {
    client.close()
  }
}

class PetsApiClient(client: Client, config: SwaggerConfig) extends JsonMethods {

  private[this] implicit val formats = config.jsonFormats

  def getPetById(id: Long): Future[Pet] = {
    client.get("/pet.json/"+id.toString, Map.empty, Map.empty, "") map { res =>
      parse(res.body).extract[Pet]
    }
  }

  def addPet(pet: Pet): Future[Pet] = {
    client.post("/pet.json", Map.empty, Map.empty, compact(render(Extraction.decompose(pet)))) map { res =>
      parse(res.body).extract[Pet]
    }
  }

  def updatePet(pet: Pet): Future[Pet] = {
    client.put("/pet.json", Map.empty, Map.empty, compact(render(Extraction.decompose(pet)))) map { res =>
      parse(res.body).extract[Pet]
    }
  }

  def findPetsByStatus(status: List[String]): Future[List[Pet]] = {
    client.get("/pet.json/findPetsByStatus", Map("status" -> status.mkString(",")), Nil, "") map { res =>
      parse(res.body) match {
        case JArray(jvs) => jvs map (_.extract[Pet])
        case jv => List(jv.extract[Pet])
      }
    }
  }

  def findPetsByTags(tags: Iterable[Tag]): Future[List[Pet]] = {
    client.get("/pet.json/findByTags", Map("tags" -> tags.map(_.name).mkString(",")), Nil, "") map { res =>
      parse(res.body) match {
        case JArray(jvs) => jvs map (_.extract[Pet])
        case jv => List(jv.extract[Pet])
      }
    }
  }
}

class StoreApiClient(client: Client, config: SwaggerConfig) extends JsonMethods {
  private[this] implicit val formats = config.jsonFormats

  def getOrderById(id: Long): Future[Order] = {
    client.get("/store.json/order/" + id.toString, Map.empty, Map.empty, null) map { res =>
      parse(res.body).extract[Order]
    }
  }

  def deleteOrder(id: Long): Future[Unit] = {
    client.delete("/store.json/order/" + id.toString, Map.empty, Map.empty, null) map { _ => ()}
  }

  def placeOrder(order: Order): Future[Unit] = {
    client.post("/store.json/order", Map.empty, Map.empty, null) map { _ => () }
  }
}

class UserApiClient(client: Client, config: SwaggerConfig) extends JsonMethods {
  private[this] implicit val formats = config.jsonFormats

  def createUsersWithArrayInput(users: Array[User]): Future[Array[User]] = {
    client.post("/user.json/createWithList", Map.empty, Map.empty, compact(render(Extraction.decompose(users.toList)))) map { res =>
      parse(res.body).extract[List[User]].toArray
    }
  }

  def createUser(user: User): Future[User] = {
    client.post("/user.json", Map.empty, Map.empty, compact(render(Extraction.decompose(user)))) map { res =>
      parse(res.body).extract[User]
    }
  }

  def createUsersWithListInput(users: List[User]): Future[List[User]] = {
    client.post("/user.json/createWithList", Map.empty, Map.empty, compact(render(Extraction.decompose(users)))) map { res =>
      parse(res.body).extract[List[User]]
    }
  }

  def updateUser(username: String, user: User): Future[User] = {
    client.put("/user.json/" + username, Map.empty, Map.empty, compact(render(Extraction.decompose(user)))) map { res =>
      parse(res.body).extract[User]
    }
  }

  def getUserByName(username: String): Future[User] = {
    client.get("/user.json/" + username, Map.empty, Map.empty, "") map { res => parse(res.body).extract[User] }
  }


  def deleteUser(username: String): Future[Unit] = {
    client.delete("/user.json/" + username, Map.empty, Map.empty, "") map { _ => () }
  }

  def loginUser(username: String, password: String): Future[User] = {
    client.get("/user.json/login", Map("username" -> username, "password" -> password), Map.empty, "") map { res =>
      parse(res.body).extract[User]
    }
  }

  def logoutUser(): Future[Unit] = {
    client.get("/user.json/logout", Map.empty, Map.empty, "") map { _ => ()}
  }
}