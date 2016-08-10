package zw.co.base2theory

trait Resource{
    def data: String
    def url:  String
    def mime: String
}

abstract class HtmlPage(override val url: String , override val data : String ) extends Resource{
    def code = data
    def mime = "text/html"
    def request: Request
}

object HtmlPage{
    def apply( r: Request ): HtmlPage = new HtmlPage(  r.url.get , r.data ){
        def request: Request = r
    }
}