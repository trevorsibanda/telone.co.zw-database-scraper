package zw.co.base2theory
import zw.co.base2theory.config.Config.Database.{profile => profile}
import profile.api._

case class Entry( val name: String, val fq_telnumber: String , val city_code: Int , val local_number: Int ,  val city: String , val address: String, val raw: Option[String] = None){
    override def toString = s"${name}|${city}|${fq_telnumber}"
}

class EntryTable(_tag: Tag) extends Table[Entry](_tag , "entry"){
    def name            = column[String]("name")
    def fq_telnumber    = column[String]("telnumber" , O.PrimaryKey)
    def city_code       = column[Int]("city_code" )
    def local_number    = column[Int]("local_telnumber")
    def city            = column[String]("city")
    def address         = column[String]("address")
    def raw             = column[Option[String]]("raw_html")
    
    def * = (name , fq_telnumber , city_code , local_number , city , address , raw)<>(Entry.tupled, Entry.unapply)
}

