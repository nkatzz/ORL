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

package logic

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.funsuite.AnyFunSuite
import trail.logic.{Clause, Literal}

class AtomSubsumptionTest extends AnyFunSuite with LazyLogging {

  logger.info("Executing Atom subsumption tests...")

  def subsumes(x: (Literal, Literal)) = x._1.thetaSubsumes(x._2)
  def parse(pair: (String, String)) = (Literal.parseWPB2(pair._1), Literal.parseWPB2(pair._2))

  test("Theta subsumption between atoms") {
    // true
    val pair1 = ("holdsAt(inroom(X,Y),T)", "holdsAt(inroom(p1,rm2),8)")
    assert(subsumes(parse(pair1)))

    // false
    val pair2 = ("holdsAt(inroom(X,Y),T)", "happensAt(inroom(p1,rm2),8)")
    assert(!subsumes(parse(pair2)))
  }

}
