package zw.co.base2theory.config

object Config{
    import zw.co.base2theory.{Url,Cookie,Header,UserAgent}
    object Scraper{
        //start page
        val startPageUrl = Url("http://www.telone.co.zw/listings/search")
        //search start page
        val searchStartPageUrl = Url( startPageUrl.url + "?pg=0" )
        //maximum number of search pages
        val maxSearchPages: Int = 99999
        //Initial cookies
        val initCookies = Some( Set[Cookie](  Cookie("device","web") , Cookie("tds","%%%%%|") ) )
        //initial headers
        val initHeaders = Some( Set[Header]( Header("Accept","text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"),
                                Header("Accept-Language", "en-US,en;q=0.5"),
                                Header("Host" , "www.telone.co.zw")) )
        //user agent
        val userAgent  = UserAgent.Default
    }
    object Database{
        //application.conf config entry
        val configEntry: String = "scraper"
    }
}
    