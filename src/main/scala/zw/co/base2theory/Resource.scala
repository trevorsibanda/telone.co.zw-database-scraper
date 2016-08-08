package zw.co.base2theory

trait Resource{
    def data: String
    def url:  String
    def mime: String
}

case class HtmlPage(override val url: String , override val data : String ) extends Resource{
    def code = data
    def mime = "text/html"
}