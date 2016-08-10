package zw.co.base2theory

trait ScrapedPage{
    val currentPage: Int 
    def pageResults: Int = 0
    val entries: Option[Seq[Entry]] 
    val cities:  Option[Seq[String]]
}

case class ScrapedStartPage(val page: HtmlPage ) extends ScrapedPage{
    val currentPage: Int = 0
    val entries: Option[Seq[Entry]] = None
    val cities: Option[Seq[String]] = try{
        Some( (for( x <-  ScrapedPage.SelectOptionPattern.findAllMatchIn(page.code) ) yield x.group(1) )
                .toList.filter( e => !(Seq("City","All") contains e))
                .toSeq 
            )
    }catch{
        case e: scala.MatchError => None 
    } 
    
    override def toString = s"ScrapedStartPage( ${cities} )"
}

case class ScrapedSearchPage( val page: HtmlPage ) extends ScrapedPage{
    val currentPage : Int = try{
        val ScrapedPage.UrlPageNumberPattern(pg)= page.url
        pg.toInt
    }catch{
        case e: scala.MatchError => 0 //@todo log error message
    }
    val entries: Option[Seq[Entry]] = {
        val entries = (for(i <- ScrapedPage.EntryPattern.findAllMatchIn(page.code)) 
                        //Entry( val name: String, val fq_telnumber: Int , val city_code: Int , val local_number: Int ,  val city: String , val address: String, val raw: Option[String] = None)
                        yield Entry(i.group(1),i.group(2),i.group(3).toInt,i.group(4).toInt,ScrapedPage.extractCity(i.group(5)),i.group(5),Some(i.group(0))) 
                        ).toList.toSeq
        Some(entries)                
    }
    val cities: Option[Seq[String]] = None
    val entriesPerPage: Int = 19
    override def pageResults: Int = try{
        ScrapedPage.ResultCountPattern.findAllMatchIn(page.code).next.group(1).toInt
    }catch{
        case _: Throwable => 0
    }
    def maxPages: Int = pageResults/entriesPerPage
    def hasNextPage: Boolean = currentPage*entriesPerPage <= pageResults
}

object ScrapedPage{
    import zw.co.base2theory.config._
    final val SelectOptionPattern= """<option value="(?:.*)">(.*)<\/option>""".r
    final val EntryPattern = """[\s\t\n\r]+<div class="list">[\r\n\s]+<div class="description">[\t\r\n\s]+<h3>([a-zA-Z0-9.,\- ]+)<\/h3>[\r\t\n\s]+<span>((\d+) (\d+))<\/span>[\t\r\n\s]+<\/div>[\t\r\n\s]+<p class="address">[\r\n\t\s]+([a-zA-Z0-9\-,\/; ]+)[\r\n\s]+<!--<br \/>[\r\n\s]+Business-->[\r\n\s]+<\/p>[\r\n\s]+<\/div>""".r
    final val UrlPageNumberPattern = """^(?:.*)?pg=(\d+)""".r
    final val ResultCountPattern = """<p><big>Your search for <strong>(?:.*)</strong> returned (\d+) results in the directory.</big></p>""".r
    
    def extractCity( address: String  ): String = try{
        address.splitAt( address.lastIndexOf(" ") )._2.stripPrefix(" ")
    }catch{
        case _: Throwable => ""
    }
    
    def pageUrl( page_num: Int ): Url = try{
        new Url( Config.Scraper.startPageUrl.get + "?pg=" + page_num )
    }catch {
        case _ : Throwable => Config.Scraper.startPageUrl
    }
    
    
}

object Scraper {
    import zw.co.base2theory.config._
    def main(args: Array[String]): Unit = {
        println("[+] Accessing start page: \t " + Config.Scraper.startPageUrl.get )
        val startPage =  HtmlPage( Request( Url(Config.Scraper.startPageUrl.get) ) )
        println("[-] Scraping start page")
        val scrapedStart = new ScrapedStartPage(startPage)
        println(s"[+] Found  ${scrapedStart.cities.getOrElse(Seq()).length} cities in this database ")
        
        val start = scrape( Config.Scraper.searchStartPageUrl , Config.Scraper.initHeaders , Config.Scraper.initCookies , Config.Scraper.userAgent  )
        //loop through entire database and run
        val entries = Range(1,start.maxPages+1).map{ i => {
                val scraped = scrape( ScrapedPage.pageUrl(i) , Config.Scraper.initHeaders , Config.Scraper.initCookies , Config.Scraper.userAgent  )
                scraped.entries.get.map{
                    entry => println(s"${entry.name}\t ${entry.city}\t ${entry.fq_telnumber}")
                }
                scraped.entries.get
            }
        }
        /*
        val initPage = new ScrapedSearchPage( initHtmlPage )
        
        */
    }
    
    
    def scrape( url: Url , headers: Option[Set[Header]] , cookies: Option[Set[Cookie]] , userAgent: UserAgent ): ScrapedSearchPage = {
        val request = Request( url , Verb.GET, headers , cookies , None , userAgent )
        val page = HtmlPage( request )
        val scraped = new ScrapedSearchPage(  page )
        println(s"[*] Page ${scraped.currentPage} => ${scraped.page.url} ")
        scraped.entries match{
            case None => { 
                println("[!] No entries found for this particular search. Check cookies, user agent and try again") 
            }
            case l  => {
                println(s"[=] Query produces ${scraped.pageResults} total results ")
                println(s"[=] Maximum number of pages = ${scraped.maxPages} ")
                println(s"[=] Found ${l.get.length} search entry results ")
                //println(scraped.page.code)
            }
        }
        scraped
    }
}
