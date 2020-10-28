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

package orl.learning.hillclimbing

import lomrf.logic.Clause

/**
  * Created by nkatz at 17/4/20
  */

object Test extends App {

  type Triplet = (Int, Int, Int)

  def tps(x: Triplet) = x._1
  def fps(x: Triplet) = x._2
  def fns(x: Triplet) = x._3

  def foilGain(parentCounts: Triplet, childCounts: Triplet) = {

    val precision = (x: Triplet) => tps(x).toDouble / (tps(x).toDouble + fps(x).toDouble)

    val parentCoverage = precision(parentCounts)
    val childCoverage = precision(childCounts)

    val gain = {
      val x = tps(childCounts) * (Math.log(childCoverage) - Math.log(parentCoverage))
      if (x <= 0.0) 0.0 else x
    }

    val max = tps(parentCounts).toDouble * (-Math.log(parentCoverage))
    println(s"max: $max")
    val normalizedGain = gain / max

    println(gain)
    println(normalizedGain)
  }

  def entropyReduction(theory: Vector[Clause], rule: Clause) = {

    /*rule.weight match {

    }*/

  }

  val parentCounts = (946, 2, 0)
  val childCounts = (755, 0, 0)
  foilGain(parentCounts, childCounts)
}

object Test2 extends App {

  util.Random.setSeed(100)
  val x = util.Random.shuffle(Seq(1, 2, 3, 4))

  println(x)
}
