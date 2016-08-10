package zw.co.base2theory

case class Entry( val name: String, val fq_telnumber: String , val city_code: Int , val local_number: Int ,  val city: String , val address: String, val raw: Option[String] = None){
    override def toString = s"${name}|${city}|${fq_telnumber}"
}

