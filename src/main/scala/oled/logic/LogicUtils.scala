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

package oled.logic

import scala.collection.mutable.ListBuffer

/**
  * Created by nkatz at 4/12/19
  */

object LogicUtils {

  def compressTheory(theory: Iterable[Clause]): List[Clause] = {
    val compressed = new ListBuffer[Clause]
    val included = (c: Clause) => compressed.toList.exists(x => x.thetaSubsumes(c) && c.thetaSubsumes(x))
    for (c <- theory) {
      if (!included(c)) compressed += c
    }
    compressed.toList
  }

  def showTheoryWithStats(clauses: Iterable[Clause], scoreFun: String, showWeights: Boolean = true) = {
    clauses.map(x => x.showWithStats(scoreFun, showWeights)).mkString("\n")
  }

}
