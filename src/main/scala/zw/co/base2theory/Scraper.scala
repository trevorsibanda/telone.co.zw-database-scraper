package zw.co.base2theory

object Scraper {
  def main(args: Array[String]): Unit = {
    println( Request( Url("http://www.telone.co.zw/listings/search") ) )
  }
}
