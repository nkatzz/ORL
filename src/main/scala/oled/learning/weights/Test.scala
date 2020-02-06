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

package oled.learning.weights

import scala.util.Random

/**
  * Created by nkatz on 7/10/19.
  */
object Test extends App {

  val r = new Random()

  def rand() = {
    val rangeMin = 0.0
    val rangeMax = 0.1
    rangeMin + (rangeMax - rangeMin) * r.nextDouble()
  }

  val realWeights = (1 to 10) map (_ => rand())
  val minDiff = realWeights.toSet.subsets(2).map(pair => math.abs(pair.head - pair.tail.head)).toVector.sorted.head

  val scaleFactor = 200.0 / minDiff

  val intWeights = realWeights.map(x => (x -> math.round(x * scaleFactor)))

  println(intWeights)

  //println(minDiff)

}
