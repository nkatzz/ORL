package oled.app.runutils

import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.{MongoClient, MongoCollection}
import oled.datahandling.Example

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
  }


}
