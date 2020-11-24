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
import trail.logic.parsers.{ModesParser, PB2LogicParser}
import trail.logic.{Literal, ModeAtom}
import utils.ModesHandler.getModes

import scala.io.Source
import scala.util.matching.Regex

class InputDataParsingTest extends AnyFunSuite with LazyLogging {

  test("Parsing input data & splitting into observation & query atoms (permissions dataset).") {

    val data =
      """
        |holdsAt(perm(enter(p1,rm1)),0)
        |holdsAt(perm(enter(p2,rm1)),0)
        |happensAt(enter(p1,rm1),0)
        |
        |holdsAt(inroom(p1,rm1),1)
        |holdsAt(perm(enter(p2,rm1)),1)
        |holdsAt(perm(leave(p1,rm1)),1)
        |happensAt(enter(p2,rm1),1)
        |
        |holdsAt(inroom(p1,rm1),2)
        |holdsAt(inroom(p2,rm1),2)
        |holdsAt(perm(leave(p1,rm1)),2)
        |holdsAt(perm(leave(p2,rm1)),2)
        |happensAt(enter(p1,rm2),2)
        |
        |holdsAt(inroom(p1,rm1),3)
        |holdsAt(inroom(p2,rm1),3)
        |holdsAt(perm(leave(p1,rm1)),3)
        |holdsAt(perm(leave(p2,rm1)),3)
        |happensAt(enter(p2,rm2),3)
        |
        |holdsAt(inroom(p1,rm1),4)
        |holdsAt(inroom(p2,rm1),4)
        |holdsAt(perm(leave(p1,rm1)),4)
        |holdsAt(perm(leave(p2,rm1)),4)
        |happensAt(leave(p1,rm1),4)
        |
        |holdsAt(inroom(p2,rm1),5)
        |holdsAt(perm(enter(p1,rm1)),5)
        |holdsAt(perm(leave(p2,rm1)),5)
        |happensAt(leave(p2,rm1),5)
        |
        |holdsAt(perm(enter(p1,rm1)),6)
        |holdsAt(perm(enter(p2,rm1)),6)
        |holdsAt(perm(enter(p1,rm2)),6)
        |holdsAt(perm(enter(p2,rm2)),6)
        |happensAt(enter(p1,rm2),6)
        |
        |holdsAt(inroom(p1,rm2),7)
        |holdsAt(perm(enter(p1,rm1)),7)
        |holdsAt(perm(enter(p2,rm1)),7)
        |holdsAt(perm(enter(p2,rm2)),7)
        |holdsAt(perm(leave(p1,rm2)),7)
        |happensAt(enter(p2,rm2),7)
        |
        |holdsAt(inroom(p1,rm2),8)
        |holdsAt(inroom(p2,rm2),8)
        |holdsAt(perm(enter(p1,rm1)),8)
        |holdsAt(perm(enter(p2,rm1)),8)
        |holdsAt(perm(leave(p1,rm2)),8)
        |holdsAt(perm(leave(p2,rm2)),8)
        |happensAt(leave(p1,rm2),8)
        |
        |holdsAt(inroom(p2,rm2),9)
        |holdsAt(perm(enter(p1,rm1)),9)
        |holdsAt(perm(enter(p2,rm1)),9)
        |holdsAt(perm(enter(p1,rm2)),9)
        |holdsAt(perm(leave(p2,rm2)),9)
        |happensAt(leave(p2,rm2),9)
        |
        |""".stripMargin

    val modeDecls_1 =
      """
        |head(initiatedAt(inroom(+person,+room),+time))
        |head(terminatedAt(inroom(+person,+room),+time))
        |
        |examplePattern(holdsAt(inroom(+person,+room),+time))
        |
        |body(happensAt(enter(+person,+room),+time))
        |body(happensAt(leave(+person,+room),+time))
        |body(holdsAt(perm(enter(+person,+room)),+time))
        |body(holdsAt(perm(leave(+person,+room)),+time))
        |body(holdsAt(in_someroom(+person),+time))
        |body(not holdsAt(in_someroom(+person),+time))
        |
        |inputPredicate(happensAt(enter(+person,+room),+time))
        |inputPredicate(happensAt(leave(+person,+room),+time))
        |inputPredicate(holdsAt(perm(enter(+person,+room)),+time))
        |inputPredicate(holdsAt(perm(leave(+person,+room)),+time))
        |inputPredicate(holdsAt(in_someroom(+person),+time))
        |
        |""".stripMargin

    val modeDecls_2 =
      """
        |head(initiatedAt(inroom(+person,+room),+time))
        |head(terminatedAt(inroom(+person,+room),+time))
        |
        |examplePattern(holdsAt(inroom(+person,+room),+time))
        |examplePattern(holdsAt(perm(enter(+person,+room)),+time))
        |examplePattern(holdsAt(perm(leave(+person,+room)),+time))
        |
        |body(happensAt(enter(+person,+room),+time))
        |body(happensAt(leave(+person,+room),+time))
        |body(holdsAt(perm(enter(+person,+room)),+time))
        |body(holdsAt(perm(leave(+person,+room)),+time))
        |body(holdsAt(in_someroom(+person),+time))
        |body(not holdsAt(in_someroom(+person),+time))
        |
        |inputPredicate(happensAt(enter(+person,+room),+time))
        |inputPredicate(happensAt(leave(+person,+room),+time))
        |inputPredicate(holdsAt(perm(enter(+person,+room)),+time))
        |inputPredicate(holdsAt(perm(leave(+person,+room)),+time))
        |inputPredicate(holdsAt(in_someroom(+person),+time))
        |
        |""".stripMargin

    val inputAtoms = PB2LogicParser.parseInputData(data)
    val (_, _, exmplPatterns1, _) = getModes(modeDecls_1)
    val (queryAtoms1, observationAtoms1) = inputAtoms.partition(x => x.matchingMode(exmplPatterns1) != ModeAtom("", List()))

    assert(queryAtoms1.size == 12)
    assert(observationAtoms1.size == 38)

    val (_, _, exmplPatterns2, _) = getModes(modeDecls_2)
    val (queryAtoms2, observationAtoms2) = inputAtoms.partition(x => x.matchingMode(exmplPatterns2) != ModeAtom("", List()))

    assert(queryAtoms2.size == 40)
    assert(observationAtoms2.size == 10)
  }

  test("Parsing input data & splitting into observation & query atoms (yale shooting example).") {

    val data =
      """
        |% Observations:           % Query atoms:
        |%---------------           ---------------
        |                          holdsAt(alive,0).
        |                          holdsAt(alive,1).
        |happensAt(shoot,2).       holdsAt(alive,2).
        |happensAt(load,3).        holdsAt(alive,3).
        |                          holdsAt(alive,4).  holdsAt(loaded,4).
        |happensAt(shoot,5).       holdsAt(loaded,5). holdsAt(alive,5).
        |                          holdsAt(dead,6).
        |                          holdsAt(dead,7).
        |happensAt(shoot,8).       holdsAt(dead,8).
        |happensAt(load,9).        holdsAt(dead,9).
        |                          holdsAt(loaded,10). holdsAt(dead,10).
        |happensAt(shoot,11).      holdsAt(loaded,11). holdsAt(dead,11).
        |
        |""".stripMargin

    val modeDecls =
      """
        |examplePattern(holdsAt(#fluent,+time))
        |
        |modeh(initiatedAt(#fluent,+time))
        |modeh(terminatedAt(#fluent,+time))
        |
        |modeb(holdsAt(#fluent,+time))
        |modeb(happensAt(#event,+time))
        |
        |inputPredicate(happensAt(#event,+time))
        |inputPredicate(holdsAt(#fluent,+time))
        |
        |""".stripMargin

    val inputAtoms = PB2LogicParser.parseInputData(data)
    val (_, _, exmplPatterns1, _) = getModes(modeDecls)
    val (queryAtoms, observationAtoms) = inputAtoms.partition(x => x.matchingMode(exmplPatterns1) != ModeAtom("", List()))

    assert(queryAtoms.size == 16)
    assert(observationAtoms.size == 6)

  }

}
