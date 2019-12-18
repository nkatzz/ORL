/*
 * Copyright (C) 2016  Nikos Katzouris
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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
    val queryAtoms = o.asInstanceOf[BasicDBObject].get("annotation").asInstanceOf[BasicDBList].toList.map(_.toString) //.toSet
    val observations = o.asInstanceOf[BasicDBObject].get("narrative").asInstanceOf[BasicDBList].toList.map(_.toString) //.toSet
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
