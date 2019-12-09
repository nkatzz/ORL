package oled.datahandling

import com.mongodb.casbah.Imports.{BasicDBList, BasicDBObject, DBObject}
import com.mongodb.casbah.Imports._
import com.mongodb.casbah.commons.MongoDBObject

/**
 * Created by nkatz at 4/12/19
 */

object Example {

  def apply(queryAtoms: Set[String], observations: Set[String], time: String) = {
    val exmpl = new Example
    exmpl.queryAtoms = queryAtoms
    exmpl.observations = observations
    exmpl.time = time
    exmpl
  }

  def apply(o: DBObject) = {
    val queryAtoms = o.asInstanceOf[BasicDBObject].get("annotation").asInstanceOf[BasicDBList].toList.map(_.toString).toSet
    val observations = o.asInstanceOf[BasicDBObject].get("narrative").asInstanceOf[BasicDBList].toList.map(_.toString).toSet
    val time = o.asInstanceOf[BasicDBObject].get("time").toString
    val e = new Example
    e.queryAtoms = queryAtoms
    e.observations = observations
    e.time = time
  }

}

class Example {

  var queryAtoms: Set[String] = Set.empty[String]
  var observations: Set[String] = Set.empty[String]
  var time = "0"

  def tostring = (queryAtoms ++ observations).mkString("\n")

  def toMongoEntryObject = {
    val entry = MongoDBObject("time" -> time) ++ ("annotation" -> queryAtoms) ++ ("narrative" -> observations)
    entry
  }

}
