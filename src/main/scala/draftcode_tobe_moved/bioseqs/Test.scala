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

package draftcode_tobe_moved.bioseqs

import scala.collection.mutable.ListBuffer

/**
  * Created by nkatz at 8/1/20
  */
object Test extends App {

  type MinBatch = Vector[Int]

  val (miniBatchesWrong, time1) = time { wrong(1 to 1000000 toIterator, 10) }

  println(time1)

  val stop = ""
  miniBatchesWrong foreach println

  /*def right(data: Iterator[Int], batchSize: Int) = {

    val t = data.foldLeft(0, List.empty[Int], Vector.empty[MinBatch]){ (x, y) =>
      0
    }

  }*/

  def wrong(data: Iterator[Int], batchSize: Int) = {

    var buffer = new ListBuffer[Int]()
    var batchCounterSize = 0
    var batches = new ListBuffer[MinBatch]()

    while (data.hasNext) {
      while (batchCounterSize < batchSize) {
        buffer.append(data.next())
        batchCounterSize += 1
      }
      batches.append(buffer.toVector)
      batchCounterSize = 0
      buffer = new ListBuffer[Int]()
    }
    batches.toIterator
  }

  // Utility function to time the execution of stuff.
  def time[R](codeBlock: => R): (R, Double) = {
    val t0 = System.nanoTime()
    val result = codeBlock
    val t1 = System.nanoTime()
    val totalTime = (t1 - t0) / 1000000000.0
    (result, totalTime)
  }

}
