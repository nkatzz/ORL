package oled.datahandling

import com.mongodb.casbah.Imports.{BasicDBList, BasicDBObject, DBObject}
import com.mongodb.casbah.Imports._
import com.mongodb.casbah.commons.MongoDBObject

/**
 * Created by nkatz at 4/12/19
 */

object Example {

  def apply(queryAtoms: List[String], observations: List[String], time: String) = {
    val exmpl = new Example
    exmpl.queryAtoms = queryAtoms
    exmpl.observations = observations
    exmpl.time = time
    exmpl
  }

  def apply(o: DBObject) = {
    val queryAtoms = o.asInstanceOf[BasicDBObject].get("annotation").asInstanceOf[BasicDBList].toList.map(_.toString)//.toSet
    val observations = o.asInstanceOf[BasicDBObject].get("narrative").asInstanceOf[BasicDBList].toList.map(_.toString)//.toSet
    val time = o.asInstanceOf[BasicDBObject].get("time").toString
    val exmpl = new Example
    exmpl.queryAtoms = queryAtoms
    exmpl.observations = observations
    exmpl.time = time
    exmpl
  }

  def apply(): Example = {
    new Example()
  }

}

class Example() {

  var queryAtoms: List[String] = Nil
  var observations: List[String] = Nil
  var time = "0"

  val isEmpty: Boolean = queryAtoms.isEmpty && observations.isEmpty

  def tostring: String = (queryAtoms ++ observations).mkString("\n")

  def toMongoEntryObject: DBObject = {
    val entry = MongoDBObject("time" -> time) ++ ("annotation" -> queryAtoms) ++ ("narrative" -> observations)
    entry
  }

  def toASP() = {
    val q = queryAtoms.map(x => s"example($x).")
    val o = observations.map(x => s"$x.")
    q ++ o
  }

}
