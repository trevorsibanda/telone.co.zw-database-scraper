package zw.co.base2theory
import zw.co.base2theory.config.Config.Database.{profile => profile}
import profile.api._

case class City(val name: String , val code: Option[String]= None )

class CityTable(_tag: Tag ) extends Table[City](_tag , "city"){
    def name = column[String]("name" , O.PrimaryKey)
    def code = column[Option[String]]("code" )
    
    def * = (name , code) <> (City.tupled, City.unapply)
}