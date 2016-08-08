package zw.co.base2theory

case class Url(val url: String ){
    def get = url
}

case class Cookie( val name: String , val value: String ){
    override def toString = s"${name}=${value};"
}

case class Header(val header: String , val value: String ){
    override def toString = s"${header} : ${value}"
}

class UserAgent(val u_agent: String , val browser: Option[String] = None ){
    override def toString = s"""UA: ${browser.getOrElse("Default")}"""
}
object UserAgent{
    case object Firefox45 extends UserAgent("Mozilla/5.0 (X11; Linux x86_64; rv:45.0) Gecko/20100101 Firefox/45.0" , Some("Firefox 45/0") )
    val Default = Firefox45
}

class Verb(val v: String){
    override def toString = v
}
object Verb{
    case object GET extends Verb("GET")
    case object POST extends Verb("POST")
    case object DELETE extends Verb("DELETE")
    case object PUT extends Verb("PUT")
}

class PostData( val data: Option[Map[String,String]] = None ){
    def get = data.getOrElse( Map[String,String]() )
}

class Request(val url: Url , val verb: Verb = Verb.GET , val headers: Option[Set[Header]] = None , val cookies: Option[Set[Cookie]] = None , val postData: Option[PostData] = None, val userAgent: UserAgent = UserAgent.Default  ){
    def httpResponse: Int = 200
    def data : String = ""
    
    override def toString = data
}


object Request{
    import java.io.OutputStreamWriter
    import java.net.{URLConnection, URL}
    
    /**
     * Run the actual request and return a Request
     */ 
    def apply(url: Url ,verb: Verb = Verb.GET ,_headers: Option[Set[Header]] = None ,_cookies: Option[Set[Cookie]] = None ,postData: Option[PostData] = None, userAgent: UserAgent = UserAgent.Default ): Request = {
        val httpClient = new Http( UserAgent.Default , "utf-8" ){
            override val headers = _headers.getOrElse(Set[Header]() )
        }
        httpClient.cookies = _cookies.getOrElse( Set[Cookie]() )
        val resp_data = verb match{
            case Verb.GET => httpClient.Get(url.get)
            case Verb.POST => httpClient.Post(url.get , postData.getOrElse(new PostData()).get )
        }
        new Request(url , verb , _headers ,Some(httpClient.cookies) ,postData,userAgent){
            override def data : String = resp_data 
        }
    }
    
    
    //http://stackoverflow.com/questions/5564074/scala-http-operations
    //Customized a bit
    class Http(userAgent: UserAgent,
               encoding: String,
               HttpRequestTimeout: Int = 15000) {
    
      import collection.JavaConversions._
      //import Implicits.wrapInputStream
      import java.net.URLEncoder.encode
    
      var cookies = Set[Cookie]()
      val headers = Set[Header]()
    
      private def loadCookies(conn: URLConnection) {
        for (Cookie(name, value) <- cookies) conn.setRequestProperty("Cookie", name + "=" + value)
      }
      
      private def loadHeaders(conn: URLConnection ){
          for( Header(name , value) <- headers ) conn.setRequestProperty( name , value  )
      }
    
      private def saveCookies(conn: URLConnection) {
        conn.getHeaderFields.lift("Set-Cookie") match {
          case Some(cList) => cList foreach { c =>
            val (name,value) = c span { _ != '=' }
            cookies += Cookie( name , (value drop 1))
          }
          case None =>
        }
      }
    
      private def encodePostData(data: Map[String, String]) =
        (for ((name, value) <- data) yield encode(name, encoding) + "=" + encode(value, encoding)).mkString("&")
    
      def Get(url: String) = {
        val u = new URL(url)
        val conn = u.openConnection()
        conn.setRequestProperty("User-Agent", userAgent.u_agent)
        conn.setConnectTimeout(HttpRequestTimeout)
    
        loadCookies(conn)
        loadHeaders(conn)
        conn.connect
    
        saveCookies(conn)
    
        scala.io.Source.fromInputStream(conn.getInputStream).mkString
      }
    
      def Post(url: String, data: Map[String, String]) = {
        val u = new URL(url)
        val conn = u.openConnection
    
        conn.setRequestProperty("User-Agent", userAgent.u_agent)
        conn.setConnectTimeout(HttpRequestTimeout)
    
        loadCookies(conn)
        loadHeaders(conn)
    
        conn.setDoOutput(true)
        conn.connect
    
       val wr = new OutputStreamWriter(conn.getOutputStream())
        wr.write(encodePostData(data))
        wr.flush
        wr.close
    
    
        saveCookies(conn)
        scala.io.Source.fromInputStream(conn.getInputStream).mkString
      }
    }
}
