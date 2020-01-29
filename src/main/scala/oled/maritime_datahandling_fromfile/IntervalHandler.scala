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

package oled.maritime_datahandling_fromfile

/**
  * Created by nkatz on 18/12/19.
  */

import java.io.{BufferedWriter, File, FileWriter}

import intervalTree.IntervalTree
import data._
import oled.maritime_datahandling_fromfile.data._

import scala.collection.mutable.ListBuffer
import scala.io.Source
import oled.datahandling.Example
import java.time._

import oled.app.runutils.InputHandling.InputSource
import com.vividsolutions.jts.geom
import com.vividsolutions.jts.geom.{Coordinate, Geometry, GeometryFactory, Point}
import com.vividsolutions.jts.io
import com.vividsolutions.jts.io.WKTReader
import com.vividsolutions.jts.operation.distance.DistanceOp
import oled.app.runutils.RunningOptions

import scala.collection.{SortedSet, mutable}

// Reads data in this format:
/*change_in_heading|1443694493|1443694493|245257000
change_in_speed_start|1443890320|1443890320|259019000
change_in_speed_end|1443916603|1443916603|311018500
coord|1443686670|1443686670|228041600|-4.47298500000000043286|48.38163999999999731472
entersArea|1443875806|1443875806|228017700|18515
leavesArea|1443887789|1443887789|235113366|21501
gap_start|1444316024|1444316024|246043000
gap_end|1445063602|1445063602|269103970
proximity|1445357304|1445354425|1445357304|true|227519920|227521960
velocity|1443665064|1443665064|228374000|19.30468943370810563920|31.39999999999999857891|35.54927442329611153582
slow_motion_start|1444262324|1444262324|228167900
slow_motion_end|1444281397|1444281397|258088080
stop_start|1444031990|1444031990|227705102
stop_end|1444187303|1444187303|259019000*/
/*
class MongoDataOptions(val dbNames: Vector[String], val chunkSize: Int = 1,
                       val limit: Double = Double.PositiveInfinity.toInt,
                       val targetConcept: String = "None", val sortDbByField: String = "time",
                       val sort: String = "ascending", val what: String = "training") extends MongoSource
*/

class FileDataOptions(val HLE_Files_Dir: String, val LLE_File: String,
                      val allHLEs: List[String], val allLLEs: List[String],val runOpts: RunningOptions) extends InputSource

object IntervalHandler {

  // FOR NOW WILL NOT TAKE ARGUMENTS
  def readInputFromFile(opts: FileDataOptions): Iterator[Example] = {
    // The path to the folder with RTEC results with HLE intervals. The generateIntervalTree
    // methods reads from those files (see the method).
    val pathToCoastFile = "/home/manosl/Desktop/BSc_Thesis/Datasets/European Coastline/Europe_Coastline_converted_WKT_Format.txt"

    val wkt_of_map = Source.fromFile(pathToCoastFile).getLines().next()

    val WKT_Reader: WKTReader = new WKTReader()
    val map_polygons: Geometry = WKT_Reader.read(wkt_of_map)

    val geom_factory: GeometryFactory = new GeometryFactory()

    // Right way to get distance
    val n_points = DistanceOp.nearestPoints(map_polygons, geom_factory.createPoint(new Coordinate(-4.3472633,48.118046)))

    print(Havershine.haversine(n_points(0).y,n_points(0).x,n_points(1).y,n_points(1).x) * 1000) // in meters

    print("\n")

    val pathToHLEIntervals = opts.HLE_Files_Dir

    // The path to the critical points (LLEs)
    val pathToLLEs = opts.LLE_File

    println("Generating intervals tree...")

    val intervalTree =
      generateIntervalTree(
        pathToHLEIntervals,
        opts.allHLEs ++ opts.allLLEs // Because there are some HLEs in allLLEs
      )


      // mode is either "asp" or "mln"
      /**
        * Parses the input data into logical syntax and generates data mini-batches for training.
        *
        * A data batch is a chunk of input data of given size. Size is measured by temporal duration,
        * so given batchSize = n, a mini-batch consists of input data with time stamps t to t+n.
        *
        */
      class ExampleIterator(inputSource: Iterator[String], batchSize: Int, targetEvent: String, mode: String)
              extends Iterator[Example]
      {
        var prev_batch_timestamp: Long = 0
        var batchCount = 0

        def hasNext = inputSource.hasNext

        def next() = {
          var currentBatch = new ListBuffer[String]
          var timesAccum = scala.collection.mutable.SortedSet[Long]()
          var llesAccum = scala.collection.mutable.SortedSet[String]()

          val INF_TS = 2000000000

          //var curr_id = 0

          while ((timesAccum.size <= batchSize) && (inputSource.hasNext)) {
            val newLine = inputSource.next()
            //println(newLine)
            val split = newLine.split("\\|")
            println(split.mkString(" "))

            val time = split(1)
            val lle = split(0)

            if (!timesAccum.contains(time.toLong)) timesAccum += time.toLong

            if (!llesAccum.contains(lle)) llesAccum += lle


            currentBatch += generateLLEInstances(newLine, mode)
          }

          timesAccum += prev_batch_timestamp

          val json_time = prev_batch_timestamp

          //currentBatch += generateLLEInstances(newLine, mode)
          batchCount += 1

          //what is the use of this line?
          val nexts = timesAccum.sliding(2).map(x => if (mode == "asp") s"next(${x.last},${x.head})" else s"next(${x.last},${x.head})")

          var nextsHashMap = new mutable.HashMap[Long,Long]()
          var slideIterator = timesAccum.sliding(2)

          while(slideIterator.hasNext){
            val currSortedSet = slideIterator.next()
            val currKey:Long = currSortedSet.head
            val currVal:Long = currSortedSet.last

            nextsHashMap += (currKey -> currVal)
          }

          val intervals = if (inputSource.hasNext) intervalTree.range(prev_batch_timestamp, timesAccum.last) else intervalTree.range(prev_batch_timestamp, INF_TS)

          if (!inputSource.hasNext) timesAccum += INF_TS

          prev_batch_timestamp = timesAccum.last

          var extras: List[String] = (timesAccum).flatMap { timeStamp =>
            val containedIn = intervals.filter(interval => ((opts.allHLEs.contains(interval._3.hle) && timeStamp != timesAccum.last && interval._3.stime < nextsHashMap(timeStamp) && nextsHashMap(timeStamp) < interval._3.etime) ||
                                  (opts.allLLEs.contains(interval._3.hle) && interval._3.stime < timeStamp && timeStamp < interval._3.etime)))
            if(timeStamp != timesAccum.last) {
              containedIn.flatMap(x => HLEIntervalToAtom(x._3, timeStamp.toString, nextsHashMap(timeStamp).toString, opts.allHLEs))
            }
            else{
              containedIn.flatMap(x => HLEIntervalToAtom(x._3, timeStamp.toString, "None", opts.allHLEs))
            }
          } toList

          extras = extras ++ intervals.flatMap((interval) => if (interval._3.stime >= timesAccum.head) HLEIntervalToAtom(interval._3, interval._3.stime.toString, "None", opts.allHLEs) else List("None")).asInstanceOf[List[String]]
          extras = extras ++ intervals.flatMap((interval) => if (interval._3.etime <= timesAccum.last) HLEIntervalToAtom(interval._3, interval._3.etime.toString, "None", opts.allHLEs) else List("None")).asInstanceOf[List[String]]

          // Why this line is used?
          if (extras.nonEmpty) {
            val stop = "stop"
          }

          for (x <- extras) currentBatch += x
          for (x <- nexts) currentBatch += x


          val all_events = currentBatch.filter(x => x != "None")

          var temp = all_events.clone()
          var annotation = ListBuffer[String]()
          var narrative = ListBuffer[String]()

          val hleIter: Iterator[String] = opts.allHLEs.iterator
          val lleIter: Iterator[String] = opts.allLLEs.iterator


          while(lleIter.hasNext) {
            var currEvent = lleIter.next()
            annotation = annotation ++ temp.filter(x => x.startsWith("happensAt(" + currEvent + "("))
            temp = all_events.clone()
          }

          while(hleIter.hasNext) {
            var currEvent = hleIter.next()
            narrative = narrative ++ temp.filter(x => x.startsWith("holdsAt(" + currEvent + "("))
            temp = all_events.clone()
          }

          val curr_exmpl = Example(annotation.toList,narrative.toList,json_time.toString)

          curr_exmpl
        }
      }

    /*
      def readDataIntoMiniBatches(dataPath: String, batchSize: Int, targetEvent: String, LLE_bias: List[String], mode: String): Iterator[Example] = {

        var currentBatch = new ListBuffer[String]
        var timesAccum = scala.collection.mutable.SortedSet[Long]()
        var llesAccum = scala.collection.mutable.SortedSet[String]()
        var batchCount = 0

        var prev_batch_timestamp: Long = 0
        val INF_TS = 2000000000

        //var curr_id = 0

        var iterators_list = new ListBuffer[Iterator[Example]]()

        while (data.hasNext) {
          val newLine = data.next()
          //println(newLine)
          val split = newLine.split("\\|")
          println(split.mkString(" "))

          val time = split(1)
          val lle = split(0)

          if (!timesAccum.contains(time.toLong)) timesAccum += time.toLong

          if (!llesAccum.contains(lle)) llesAccum += lle

          if ((timesAccum.size <= batchSize) && (data.hasNext)) {
            currentBatch += generateLLEInstances(newLine, mode)
          } else {
            val json_time = prev_batch_timestamp

            currentBatch += generateLLEInstances(newLine, mode)
            batchCount += 1

            //what is the use of this line?
            val nexts = timesAccum.sliding(2).map(x => if (mode == "asp") s"next(${x.last},${x.head})" else s"next(${x.last},${x.head})")
            val intervals = if (data.hasNext) intervalTree.range(prev_batch_timestamp, timesAccum.last) else intervalTree.range(prev_batch_timestamp, INF_TS)

            timesAccum += prev_batch_timestamp

            if (!data.hasNext) timesAccum += INF_TS

            prev_batch_timestamp = timesAccum.last

            var extras = timesAccum.flatMap{ timeStamp =>
              val containedIn = intervals.filter(interval => (interval._3.stime < timeStamp && timeStamp < interval._3.etime))
              containedIn.map(x => HLEIntervalToAtom(x._3, timeStamp.toString, targetEvent))
            }

            extras = extras ++ intervals.map((interval) => if (interval._3.stime >= timesAccum.head) HLEIntervalToAtom(interval._3, interval._3.stime.toString, targetEvent) else "None")
            extras = extras ++ intervals.map((interval) => if (interval._3.etime <= timesAccum.last) HLEIntervalToAtom(interval._3, interval._3.etime.toString, targetEvent) else "None")

            // Why this line is used?
            if (extras.nonEmpty) {
              val stop = "stop"
            }

            for (x <- extras) currentBatch += x
            for (x <- nexts) currentBatch += x


            val all_events = currentBatch.filter(x => x != "None")
            val annotation = all_events.filter(x => x.startsWith("holdsAt(" + targetEvent))
            val narrative = all_events.filter(x => !x.startsWith("holdsAt(" + targetEvent))

            val curr_exmpl = Example(annotation.toList,narrative.toList,json_time.toString)
            /*
            val json_obj = JSONFileObj(curr_id, json_time, annotation.toList, narrative.toList)

            curr_id += 1
            */

            println(batchCount)
            iterators_list.append(Iterator(curr_exmpl))
            currentBatch = new ListBuffer[String]()
            timesAccum.clear()
          }
        }

        println(s"All batches: $batchCount")
        println(s"LLEs: $llesAccum")

        iterators_list.foldLeft(Iterator[Example]())(_ ++ _)
      }*/

      val data = Source.fromFile(pathToLLEs).getLines.filter(x =>
        opts.allLLEs.contains(x.split("\\|")(0))
        //!x.startsWith("coord") && !x.startsWith("velocity") && !x.startsWith("entersArea") && !x.startsWith("leavesArea")
      ).toIterator

      val it: Iterator[Example] = new ExampleIterator(data,opts.runOpts.chunkSize,opts.runOpts.targetHLE, mode="asp")

      /*while (it.hasNext) {
        println(it.next)
      }*/

      it
  }

  // mode is either "asp" or "mln"
  def generateLLEInstances(line: String, mode: String) = {
    // These have a different schema
    // proximity is HLE
    val abnormalLLEs = Set[String]("coord", "entersArea", "leavesArea", "velocity")
    val split = line.split("\\|")
    if (!abnormalLLEs.contains(split(0))) {

      // These have a common simple schema:
      // change_in_heading, change_in_speed_start, change_in_speed_end,
      // gap_start, gap_end, slow_motion_start, slow_motion_end, stop_start, stop_end
      val lle = split(0)
      val time = split(1)
      val vessel = split(3)
      if (mode == "asp") s"happensAt($lle($vessel),$time)" else s"HappensAt(${lle.capitalize}_$vessel),$time)"
    } else {

      if (split(0) == "coord") {
        //coord|1443686670|1443686670|228041600|-4.47298500000000043286|48.38163999999999731472
        /*
        val lle = split(0)
        val time = split(1)
        val vessel = split(3)
        val lon = split(4)
        val lat = split(5)
        // Don't return nothing in the MLN case (can't handle the coords syntax)
        if ("mode" == "asp") s"happensAt($lle($vessel,$lon,$lat),$time)" else ""
        */
        // do nothing (we won't use coord).
        "None"
      } else if (split(0) == "entersArea" || split(0) == "leavesArea") {
        //entersArea|1443875806|1443875806|228017700|18515
        // For me enters and leaves are has the form
        //['Fluent','Timestamp','Area_ID','Vessel_ID']

        val lle = split(0)
        val time = split(1)
        val vessel = split(3)
        val area = split(2)
        if (mode == "asp") s"happensAt($lle($vessel,$area),$time)"
        else s"HappensAt(${lle.capitalize}_${vessel}_$area,$time)"

      } else if (split(0) == "velocity") {

        // do nothing (we won't use velocity)
        "None"

      } else {
        throw new RuntimeException(s"Unexpected event: $line")
      }
    }
  }

  /* Generate an HLE logical atom. The i var carries all the info, the t var is the particular
   * time point of the generated atom. "target" is the name of the target complex event. The
   * target event is turned into a holdsAt predicate, while all others are turned into happensAt predicates. */
  def HLEIntervalToAtom(i: HLEInterval, t: String, next_t: String, considered_HLEs: List[String]/*target: String*/): List[String] = {

    val functor = if (considered_HLEs.contains(i.hle)) "holdsAt" else "happensAt"

    val fluentTerm =
      if (i.value != "true") s"${i.hle}(${(i.vessels :+ i.value).mkString(",")})"
      else s"${i.hle}(${i.vessels.mkString(",")})"

    val timestamp = {
      if(next_t == "None" || !considered_HLEs.contains(i.hle)){
        t
      }
      else {
        next_t
      }
    }

    val fluentTerm2 = {if (i.hle == "rendezVous" || i.hle == "proximity"
                        || i.hle == "pilotBoarding" || i.hle == "tugging")
                      {
                        if (i.value != "true") s"${i.hle}(${(i.vessels.reverse :+ i.value).mkString(",")})"
                        else s"${i.hle}(${i.vessels.reverse.mkString(",")})"
                      } else "None"}

    if(fluentTerm2 != "None"){
      List[String](s"$functor($fluentTerm,$timestamp)",s"$functor($fluentTerm2,$timestamp)")
    }
    else {
      List[String](s"$functor($fluentTerm,$timestamp)")
    }
  }

  def generateIntervalTree(pathToHLEs: String, interestedIn: List[String]) = {

      def getListOfFiles(dir: String): List[File] = {
        val d = new File(dir)
        if (d.exists && d.isDirectory) {
          println(d.listFiles.toList)
          d.listFiles.filter(f => f.isFile).toList
          // This line does not suits me
          // d.listFiles.filter(f => f.isFile && interestedIn.exists(eventName => f.getName.contains(eventName))).toList
        } else {
          List[File]()
        }
      }

    val files = getListOfFiles(pathToHLEs).map(_.getCanonicalPath)

    val intervalTree = new IntervalTree[HLEInterval]()

    var counter = 0
    files foreach { file =>
      println(s"Updating interval tree from $file")

      val data = Source.fromFile(file).getLines.filter(line => interestedIn.contains(line.split("\\|")(0)))

      while (data.hasNext) {
        val newLine = data.next()
        val i = HLEInterval(newLine)

        counter += 1

        //println(newLine)
        intervalTree.addInterval(i.stime, i.etime, i)
      }
    }

    intervalTree
  }

}
