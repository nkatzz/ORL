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

package orl.datahandling

import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.{MongoClient, MongoCollection}

import scala.io.Source

/**
  * Created by nkatz at 13/12/19
  */

object InputHandling {

  // TODO
  // Currently used by the maritime runner
  trait InputSource

  trait MongoSource extends InputSource {

    def createIndex(collection: MongoCollection, sort: String = "ascending", sortKey: String = "None"): Unit = {
      sortKey match {
        case "None" =>
        case _ =>
          val i = if (sort == "ascending") 1 else -1
          collection.createIndex(MongoDBObject(sortKey -> i))
      }
    }

    def allData(collection: MongoCollection, sort: String = "ascending", sortKey: String = "None"): collection.CursorType = {
      sortKey match {
        case "None" => collection.find()
        case _ =>
          val i = if (sort == "ascending") 1 else -1
          collection.find().sort(MongoDBObject(sortKey -> i))
      }
    }
  }

  // TODO
  trait FileSource

  case class FileDataOptions(
    file: String,
    chunkSize: Int = 100,
    targetConcept: String = "None",
    sort: String = "ascending",
    what: String = "training",
    sortByFunction: String => Int)

  def getFileData(opts: FileDataOptions): Iterator[Example] = {
    val source = Source.fromFile(opts.file)
    val grouped = source.getLines.toList.groupBy(line => opts.sortByFunction(line)).toList
    val sorted = if (opts.sort == "ascending") grouped.sortBy(_._1) else grouped.sortWith(_._1 < _._1)
    val examples = sorted.map(_._2).grouped(opts.chunkSize).map { list =>
      val time = opts.sortByFunction(list.head.head)
      val (queryAtoms, evidenceAtoms) = list.flatten.partition(x => x.contains(opts.targetConcept))
      Example(queryAtoms, evidenceAtoms, time.toString)
    }
    source.close
    examples
  }

  /*
  def getData[T <: Source](opts: T, dataFunc: (T) => Iterator[Example]) = {
    dataFunc(opts)
  }
  def run[T <: Source](opts: T, dataFunc: (T) => Iterator[Example]): Iterator[Example] = {
    dataFunc(opts)
  }
  //getData(new DefaultMongoDataOptions(""), getMongoData)
  */

  class MongoDataOptions(val dbNames: Vector[String], val chunkSize: Int = 1,
      val limit: Double = Double.PositiveInfinity.toInt,
      val targetConcept: String = "None", val sortDbByField: String = "time",
      val sort: String = "ascending", val what: String = "training") extends MongoSource

  /* "what" is either training or testing */
  def getMongoData(opts: MongoDataOptions): Iterator[Example] = {
    val mc = MongoClient()

    val exmplIters = opts.dbNames map { dbName =>

      val collection: MongoCollection = mc(dbName)("examples")

      val data = opts.allData(collection, opts.sort, opts.sortDbByField) map { x =>
        val e = Example(x)

        opts.targetConcept match {
          case "None" => Example(e.queryAtoms, e.observations, e.time)
          case _ => Example(e.queryAtoms filter (_.contains(opts.targetConcept)), e.observations, e.time)
        }
      }

      if (opts.what == "training") {
        opts.chunkSize > 1 match {
          case false => data
          case _ =>
            data.grouped(opts.chunkSize).map { x =>
              //data.sliding(opts.chunkSize).map { x =>
              x.foldLeft(Example()) { (z, y) => Example(z.queryAtoms ++ y.queryAtoms, z.observations ++ y.observations, x.head.time) }
            }
        }
      } else { // no chunking for testing data
        ///*
        val firstDataPoint = data.next()
        val annotation = firstDataPoint.queryAtoms
        val narrative = firstDataPoint.observations
        val time = firstDataPoint.time
        val merged = data.foldLeft(annotation, narrative) { (accum, ex) =>
          (accum._1 ++ ex.queryAtoms, accum._2 ++ ex.observations)
        }
        val e = Example(merged._1, merged._2, time)
        Iterator(e)
        //*/

        // comment the above and uncomment this to have chunked data
        /*
        data.grouped(opts.chunkSize).map { x =>
          //data.sliding(opts.chunkSize).map { x =>
          x.foldLeft(Example()) { (z, y) =>
            new Example(annot = z.annotation ++ y.annotation, nar = z.narrative ++ y.narrative, _time = x.head.time)
          }
        }
        */
      }
    }
    exmplIters.foldLeft(Iterator[Example]())(_ ++ _)
    //Random.shuffle(exmplIters.foldLeft(Iterator[Example]())(_ ++ _))
  }

}
