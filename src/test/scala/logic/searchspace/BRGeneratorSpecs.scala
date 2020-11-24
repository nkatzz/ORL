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

package logic.searchspace

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.funsuite.AnyFunSuite
import trail.learning.utils.searchspace.BRGenerator
import trail.logic.ModeAtom
import utils.ModesHandler.getModes

import scala.util.Success

class BRGeneratorSpecs extends AnyFunSuite with LazyLogging {

  def brgen(modehs: List[ModeAtom], modebs: List[ModeAtom],
      domainConstants: Map[String, Array[String]]) = {
    val bottomTheory = BRGenerator.generateBRs(modehs, modebs, domainConstants)
    println(bottomTheory.map(x => x.tostring).mkString("\n"))
    bottomTheory
  }

  test("Dummy 'protein' example for bottom theory generation ") {

    val modeDecls =
      """
        |modeh(initiatedAt(blocks(#protein,#protein,+scenario),+time))
        |modeh(initiatedAt(blocks(#protein,#protein,+scenario),+time))
        |
        |examplePattern(holdsAt(blocks(#protein,#protein,+scenario),+time))
        |
        |modeb(exists(#protein,+scenario,+time))
        |modeb(not exists(#protein,+scenario,+time))
        |modeb(affects(#protein,-protein,+scenario,+time))
        |modeb(affects(#protein,+protein,+scenario,+time))
        |
        |""".stripMargin

    val domainConstants = Map("protein" -> Array("p1", "p2", "p3"), "scenario" -> Array("s1", "s2"))
    val (modehs, modebs, _, _) = getModes(modeDecls)

    val result = "initiatedAt(blocks(p2,p1,X1),X2) :- exists(p1,X1,X2),not exists(p2,X1,X2),exists(p3,X1,X2),not exists(p1,X1,X2),affects(p2,X3,X1,X2),affects(p3,X3,X1,X2),exists(p2,X1,X2),affects(p1,X3,X1,X2),not exists(p3,X1,X2).\ninitiatedAt(blocks(p1,p2,X1),X2) :- exists(p1,X1,X2),not exists(p2,X1,X2),exists(p3,X1,X2),not exists(p1,X1,X2),affects(p2,X3,X1,X2),affects(p3,X3,X1,X2),exists(p2,X1,X2),affects(p1,X3,X1,X2),not exists(p3,X1,X2).\ninitiatedAt(blocks(p3,p1,X1),X2) :- exists(p1,X1,X2),not exists(p2,X1,X2),exists(p3,X1,X2),not exists(p1,X1,X2),affects(p2,X3,X1,X2),affects(p3,X3,X1,X2),exists(p2,X1,X2),affects(p1,X3,X1,X2),not exists(p3,X1,X2).\ninitiatedAt(blocks(p1,p3,X1),X2) :- exists(p1,X1,X2),not exists(p2,X1,X2),exists(p3,X1,X2),not exists(p1,X1,X2),affects(p2,X3,X1,X2),affects(p3,X3,X1,X2),exists(p2,X1,X2),affects(p1,X3,X1,X2),not exists(p3,X1,X2).\ninitiatedAt(blocks(p3,p2,X1),X2) :- exists(p1,X1,X2),not exists(p2,X1,X2),exists(p3,X1,X2),not exists(p1,X1,X2),affects(p2,X3,X1,X2),affects(p3,X3,X1,X2),exists(p2,X1,X2),affects(p1,X3,X1,X2),not exists(p3,X1,X2).\ninitiatedAt(blocks(p2,p3,X1),X2) :- exists(p1,X1,X2),not exists(p2,X1,X2),exists(p3,X1,X2),not exists(p1,X1,X2),affects(p2,X3,X1,X2),affects(p3,X3,X1,X2),exists(p2,X1,X2),affects(p1,X3,X1,X2),not exists(p3,X1,X2).\ninitiatedAt(blocks(p2,p1,X1),X2) :- exists(p1,X1,X2),not exists(p2,X1,X2),exists(p3,X1,X2),not exists(p1,X1,X2),affects(p2,X3,X1,X2),affects(p3,X3,X1,X2),exists(p2,X1,X2),affects(p1,X3,X1,X2),not exists(p3,X1,X2).\ninitiatedAt(blocks(p1,p2,X1),X2) :- exists(p1,X1,X2),not exists(p2,X1,X2),exists(p3,X1,X2),not exists(p1,X1,X2),affects(p2,X3,X1,X2),affects(p3,X3,X1,X2),exists(p2,X1,X2),affects(p1,X3,X1,X2),not exists(p3,X1,X2).\ninitiatedAt(blocks(p3,p1,X1),X2) :- exists(p1,X1,X2),not exists(p2,X1,X2),exists(p3,X1,X2),not exists(p1,X1,X2),affects(p2,X3,X1,X2),affects(p3,X3,X1,X2),exists(p2,X1,X2),affects(p1,X3,X1,X2),not exists(p3,X1,X2).\ninitiatedAt(blocks(p1,p3,X1),X2) :- exists(p1,X1,X2),not exists(p2,X1,X2),exists(p3,X1,X2),not exists(p1,X1,X2),affects(p2,X3,X1,X2),affects(p3,X3,X1,X2),exists(p2,X1,X2),affects(p1,X3,X1,X2),not exists(p3,X1,X2).\ninitiatedAt(blocks(p3,p2,X1),X2) :- exists(p1,X1,X2),not exists(p2,X1,X2),exists(p3,X1,X2),not exists(p1,X1,X2),affects(p2,X3,X1,X2),affects(p3,X3,X1,X2),exists(p2,X1,X2),affects(p1,X3,X1,X2),not exists(p3,X1,X2).\ninitiatedAt(blocks(p2,p3,X1),X2) :- exists(p1,X1,X2),not exists(p2,X1,X2),exists(p3,X1,X2),not exists(p1,X1,X2),affects(p2,X3,X1,X2),affects(p3,X3,X1,X2),exists(p2,X1,X2),affects(p1,X3,X1,X2),not exists(p3,X1,X2)."
    val bt = brgen(modehs, modebs, domainConstants)

    assert(bt.map(x => x.tostring).mkString("\n") == result)
  }

  test("Yale shooting example for bottom theory generation") {

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

    val result = "initiatedAt(loaded,X1) :- holdsAt(alive,X1),happensAt(load,X1),holdsAt(dead,X1),happensAt(shoot,X1),holdsAt(loaded,X1).\ninitiatedAt(dead,X1) :- holdsAt(alive,X1),happensAt(load,X1),holdsAt(dead,X1),happensAt(shoot,X1),holdsAt(loaded,X1).\ninitiatedAt(alive,X1) :- holdsAt(alive,X1),happensAt(load,X1),holdsAt(dead,X1),happensAt(shoot,X1),holdsAt(loaded,X1).\nterminatedAt(loaded,X1) :- holdsAt(alive,X1),happensAt(load,X1),holdsAt(dead,X1),happensAt(shoot,X1),holdsAt(loaded,X1).\nterminatedAt(dead,X1) :- holdsAt(alive,X1),happensAt(load,X1),holdsAt(dead,X1),happensAt(shoot,X1),holdsAt(loaded,X1).\nterminatedAt(alive,X1) :- holdsAt(alive,X1),happensAt(load,X1),holdsAt(dead,X1),happensAt(shoot,X1),holdsAt(loaded,X1)."
    val domainConstants = Map("fluent" -> Array("loaded", "dead", "alive"), "event" -> Array("shoot", "load"))
    val (modehs, modebs, _, _) = getModes(modeDecls)
    val bt = brgen(modehs, modebs, domainConstants)
    assert(bt.map(x => x.tostring).mkString("\n") == result)
  }

  test("CAVIAR example for bottom theory generation") {

    val modeDecls =
      """
        |examplePattern(holdsAt(move(+person,+person),+time))
        |
        |modeh(initiatedAt(move(+person,+person),+time))
        |modeh(terminatedAt(move(+person,+person),+time))
        |
        |modeb(holdsAt(move(+person,+person),+time))
        |modeb(not holdsAt(move(+person,+person),+time))
        |
        |modeb(happensAt(enter(+person),+time))
        |modeb(happensAt(exit(+person),+time))
        |modeb(happensAt(walking(+person),+time))
        |modeb(happensAt(active(+person),+time))
        |modeb(happensAt(inactive(+person),+time))
        |modeb(happensAt(running(+person),+time))
        |modeb(happensAt(close(+person,+person,#threshold_value),+time))
        |modeb(close(+person,+person,#threshold_value,+time))
        |modeb(far(+person,+person,#threshold_value,+time))
        |modeb(orientationMove(+person,+person,+time))
        |modeb(orientationFar(+person,+person,+time))
        |
        |
        |modeb(not happensAt(exit(+person),+time))
        |modeb(not happensAt(walking(+person),+time))
        |modeb(not happensAt(active(+person),+time))
        |modeb(not happensAt(inactive(+person),+time))
        |
        |inputPredicate(happensAt(walking(+person),+time))
        |inputPredicate(happensAt(active(+person),+time))
        |inputPredicate(happensAt(inactive(+person),+time))
        |inputPredicate(happensAt(running(+person),+time))
        |inputPredicate(happensAt(enter(+person),+time))
        |inputPredicate(happensAt(exit(+person),+time))
        |inputPredicate(close(+person,+person,#threshold_value,+time))
        |inputPredicate(orientationMove(+person,+person,+time))
        |
        |comparisonPredicate(close(+person,+person,#threshold_value,+time), lessThan, comparison_term_position(3))
        |comparisonPredicate(far(+person,+person,#threshold_value,+time), greaterThan, comparison_term_position(3))
        |
        |""".stripMargin

    val domainConstants = Map("threshold_value" -> Array("24", "25", "34", "40"))

    val result = "initiatedAt(move(X1,X2),X3) :- happensAt(close(X1,X2,24),X3),happensAt(active(X1),X3),happensAt(walking(X2),X3),orientationFar(X1,X2,X3),close(X1,X2,25,X3),not holdsAt(move(X1,X2),X3),close(X2,X1,40,X3),far(X2,X1,25,X3),happensAt(exit(X2),X3),happensAt(exit(X1),X3),not happensAt(walking(X2),X3),happensAt(inactive(X1),X3),far(X1,X2,25,X3),orientationFar(X2,X1,X3),close(X1,X2,24,X3),happensAt(close(X1,X2,40),X3),far(X2,X1,40,X3),orientationMove(X1,X2,X3),not happensAt(inactive(X2),X3),not happensAt(exit(X1),X3),happensAt(running(X2),X3),happensAt(close(X2,X1,40),X3),happensAt(running(X1),X3),far(X2,X1,24,X3),happensAt(close(X1,X2,25),X3),not happensAt(active(X1),X3),happensAt(active(X2),X3),not happensAt(inactive(X1),X3),happensAt(inactive(X2),X3),close(X1,X2,34,X3),not happensAt(walking(X1),X3),holdsAt(move(X1,X2),X3),not happensAt(active(X2),X3),happensAt(walking(X1),X3),happensAt(close(X2,X1,25),X3),happensAt(enter(X2),X3),holdsAt(move(X2,X1),X3),not happensAt(exit(X2),X3),close(X2,X1,34,X3),close(X2,X1,24,X3),happensAt(close(X2,X1,24),X3),happensAt(enter(X1),X3),happensAt(close(X1,X2,34),X3),far(X2,X1,34,X3),orientationMove(X2,X1,X3),far(X1,X2,40,X3),happensAt(close(X2,X1,34),X3),close(X1,X2,40,X3),not holdsAt(move(X2,X1),X3),far(X1,X2,24,X3),close(X2,X1,25,X3),far(X1,X2,34,X3).\nterminatedAt(move(X1,X2),X3) :- happensAt(close(X1,X2,24),X3),happensAt(active(X1),X3),happensAt(walking(X2),X3),orientationFar(X1,X2,X3),close(X1,X2,25,X3),not holdsAt(move(X1,X2),X3),close(X2,X1,40,X3),far(X2,X1,25,X3),happensAt(exit(X2),X3),happensAt(exit(X1),X3),not happensAt(walking(X2),X3),happensAt(inactive(X1),X3),far(X1,X2,25,X3),orientationFar(X2,X1,X3),close(X1,X2,24,X3),happensAt(close(X1,X2,40),X3),far(X2,X1,40,X3),orientationMove(X1,X2,X3),not happensAt(inactive(X2),X3),not happensAt(exit(X1),X3),happensAt(running(X2),X3),happensAt(close(X2,X1,40),X3),happensAt(running(X1),X3),far(X2,X1,24,X3),happensAt(close(X1,X2,25),X3),not happensAt(active(X1),X3),happensAt(active(X2),X3),not happensAt(inactive(X1),X3),happensAt(inactive(X2),X3),close(X1,X2,34,X3),not happensAt(walking(X1),X3),holdsAt(move(X1,X2),X3),not happensAt(active(X2),X3),happensAt(walking(X1),X3),happensAt(close(X2,X1,25),X3),happensAt(enter(X2),X3),holdsAt(move(X2,X1),X3),not happensAt(exit(X2),X3),close(X2,X1,34,X3),close(X2,X1,24,X3),happensAt(close(X2,X1,24),X3),happensAt(enter(X1),X3),happensAt(close(X1,X2,34),X3),far(X2,X1,34,X3),orientationMove(X2,X1,X3),far(X1,X2,40,X3),happensAt(close(X2,X1,34),X3),close(X1,X2,40,X3),not holdsAt(move(X2,X1),X3),far(X1,X2,24,X3),close(X2,X1,25,X3),far(X1,X2,34,X3)."

    val (modehs, modebs, _, _) = getModes(modeDecls)
    val bt = brgen(modehs, modebs, domainConstants)
    assert(bt.map(x => x.tostring).mkString("\n") == result)
  }

  test("CAVIAR with no Event Calculus and output variables test") {

    val modeDecls =
      """
        |examplePattern(move(+person,+person,+time))
        |
        |modeh(move(+person,+person,+time))
        |
        |modeb(happensAt(enter(+person),+time))
        |modeb(happensAt(exit(+person),+time))
        |%modeb(happensAt(walking(+person),+time))
        |%modeb(happensAt(active(+person),+time))
        |%modeb(happensAt(inactive(+person),+time))
        |%modeb(happensAt(running(+person),+time))
        |%modeb(happensAt(close(+person,+person,#threshold_value),+time))
        |%modeb(close(+person,+person,#threshold_value,+time))
        |%modeb(far(+person,+person,#threshold_value,+time))
        |%modeb(orientationMove(+person,+person,+time))
        |%modeb(orientationFar(+person,+person,+time))
        |
        |modeb(before(+time,-time))
        |%modeb(before(-time,+time))
        |
        |
        |%modeb(not happensAt(exit(+person),+time))
        |%modeb(not happensAt(walking(+person),+time))
        |%modeb(not happensAt(active(+person),+time))
        |%modeb(not happensAt(inactive(+person),+time))
        |
        |
        |""".stripMargin

    val domainConstants = Map("threshold_value" -> Array("24", "25", "34", "40"))
    val (modehs, modebs, _, _) = getModes(modeDecls)
    val bt = brgen(modehs, modebs, domainConstants)

  }

  test("RendezVous example for bottom theory generation") {
    val modeDecls =
      """
        |examplePattern(holdsAt(rendezVous(+vessel,+vessel),+time))
        |
        |modeh(initiatedAt(rendezVous(+vessel,+vessel),+time))
        |modeh(terminatedAt(rendezVous(+vessel,+vessel),+time))
        |
        |modeh(holdsAt(rendezVous(+vessel,+vessel),+time))
        |
        |modeb(happensAt(gap(+vessel,#location),+time))
        |modeb(happensAt(changingSpeed(+vessel),+time))
        |modeb(happensAt(lowSpeed(+vessel),+time))
        |modeb(happensAt(stopped(+vessel,#location),+time))
        |modeb(happensAt(withinArea(+vessel,#area),+time))
        |modeb(proximity(+vessel,+vessel,+time))
        |modeb(far(+vessel,+vessel,+time))
        |
        |inputPredicate(happensAt(gap(+vessel,#location),+time))
        |inputPredicate(happensAt(changingSpeed(+vessel),+time))
        |inputPredicate(happensAt(lowSpeed(+vessel),+time))
        |inputPredicate(happensAt(stopped(+vessel,#location),+time))
        |inputPredicate(happensAt(withinArea(+vessel,#area),+time))
        |inputPredicate(proximity(+vessel,+vessel,+time))
        |inputPredicate(far(+vessel,+vessel,+time))
        |
        |""".stripMargin

    val domainConstants = Map("location" -> Array("nearPorts", "farFromPorts"), "area" -> Array("natura", "fishing", "nearCoast"))
    val result = "initiatedAt(rendezVous(X1,X2),X3) :- happensAt(withinArea(X1,nearCoast),X3),happensAt(withinArea(X1,natura),X3),happensAt(withinArea(X2,natura),X3),happensAt(stopped(X2,farFromPorts),X3),happensAt(stopped(X1,farFromPorts),X3),happensAt(changingSpeed(X1),X3),happensAt(changingSpeed(X2),X3),happensAt(withinArea(X1,fishing),X3),proximity(X2,X1,X3),happensAt(gap(X2,nearPorts),X3),happensAt(stopped(X2,nearPorts),X3),far(X1,X2,X3),happensAt(gap(X1,farFromPorts),X3),happensAt(stopped(X1,nearPorts),X3),happensAt(lowSpeed(X1),X3),happensAt(withinArea(X2,fishing),X3),happensAt(withinArea(X2,nearCoast),X3),proximity(X1,X2,X3),happensAt(lowSpeed(X2),X3),far(X2,X1,X3),happensAt(gap(X2,farFromPorts),X3),happensAt(gap(X1,nearPorts),X3).\nterminatedAt(rendezVous(X1,X2),X3) :- happensAt(withinArea(X1,nearCoast),X3),happensAt(withinArea(X1,natura),X3),happensAt(withinArea(X2,natura),X3),happensAt(stopped(X2,farFromPorts),X3),happensAt(stopped(X1,farFromPorts),X3),happensAt(changingSpeed(X1),X3),happensAt(changingSpeed(X2),X3),happensAt(withinArea(X1,fishing),X3),proximity(X2,X1,X3),happensAt(gap(X2,nearPorts),X3),happensAt(stopped(X2,nearPorts),X3),far(X1,X2,X3),happensAt(gap(X1,farFromPorts),X3),happensAt(stopped(X1,nearPorts),X3),happensAt(lowSpeed(X1),X3),happensAt(withinArea(X2,fishing),X3),happensAt(withinArea(X2,nearCoast),X3),proximity(X1,X2,X3),happensAt(lowSpeed(X2),X3),far(X2,X1,X3),happensAt(gap(X2,farFromPorts),X3),happensAt(gap(X1,nearPorts),X3).\nholdsAt(rendezVous(X1,X2),X3) :- happensAt(withinArea(X1,nearCoast),X3),happensAt(withinArea(X1,natura),X3),happensAt(withinArea(X2,natura),X3),happensAt(stopped(X2,farFromPorts),X3),happensAt(stopped(X1,farFromPorts),X3),happensAt(changingSpeed(X1),X3),happensAt(changingSpeed(X2),X3),happensAt(withinArea(X1,fishing),X3),proximity(X2,X1,X3),happensAt(gap(X2,nearPorts),X3),happensAt(stopped(X2,nearPorts),X3),far(X1,X2,X3),happensAt(gap(X1,farFromPorts),X3),happensAt(stopped(X1,nearPorts),X3),happensAt(lowSpeed(X1),X3),happensAt(withinArea(X2,fishing),X3),happensAt(withinArea(X2,nearCoast),X3),proximity(X1,X2,X3),happensAt(lowSpeed(X2),X3),far(X2,X1,X3),happensAt(gap(X2,farFromPorts),X3),happensAt(gap(X1,nearPorts),X3)."
    val (modehs, modebs, _, _) = getModes(modeDecls)
    val bt = brgen(modehs, modebs, domainConstants)
    assert(bt.map(x => x.tostring).mkString("\n") == result)
  }

  test("Dummy paths example, no const placemarkers") {
    val modeDecls =
      """
        |examplePattern(knows(+person,+person))
        |
        |modeh(knows(+person,+person))
        |
        |modeb(friends(+person,+person))
        |modeb(friends(+person,-person))
        |modeb(knows(+person,-person))
        |%modeb(friends(-person,+person))
        |
        |
        |
        |""".stripMargin

    val domainConstants = Map("person" -> Array.empty[String])
    val (modehs, modebs, _, _) = getModes(modeDecls)
    val bt = brgen(modehs, modebs, domainConstants)

  }
}
