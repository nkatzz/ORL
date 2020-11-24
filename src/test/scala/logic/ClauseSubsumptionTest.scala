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
import trail.logic.Clause

class ClauseSubsumptionTest extends AnyFunSuite with LazyLogging {

  logger.info("Executing Clause subsumption tests...")

  def subsumes(x: (Clause, Clause)) = x._1.thetaSubsumes(x._2)
  def parse(pair: (String, String)) = (Clause.parseWPB2(pair._1), Clause.parseWPB2(pair._2))

  test("Theta-subsumption between clauses"){
    // Same clauses (true)
    val pair1 = ("p(X,Y) :- q(Z,r(1,2,T),T),r(A,B,C,2,3,a,e,A,B)", "p(X,Y) :- q(Z,r(1,2,T),T),r(A,B,C,2,3,a,e,A,B)")

    // subsumer is "shorter" (true)
    val pair2 = ("p(X,Y) :- r(A,B,C,2,3,a,e,A,B)", "p(X,Y) :- q(Z,r(1,2,T),T),r(A,B,C,2,3,a,e,A,B)")

    // clauses differ in constants (false)
    val pair3 = ("p(X,Y) :- q(Z,r(1,2,T),T),r(A,B,C,1,3,a,e,A,B)", "p(X,Y) :- q(Z,r(1,2,T),T),r(A,B,C,2,3,a,e,A,B)")

    // "Longer" subsumes "shorter" (true)
    val pair4 = ("q(X):-p(X,Y),p(Y,X)", "q(A):-p(A,A)")

    // subsumer is superset (false)
    val pair5 = ("p(X,Y) :- q(Z,r(1,2,T),T),r(A,B,C,2,3,a,e,A,B)", "p(X,Y) :- r(A,B,C,2,3,a,e,A,B)")

    // clauses are irrelevant (false)
    val pair6 = ("terminatedAt(moving(X0,X1),X2) :- happensAt(walking(X0),X2),happensAt(walking(X1),X2),distLessThan(X0,X1,40,X2),distLessThan(X0,X1,34,X2),distLessThan(X0,X1,27,X2),distLessThan(X0,X1,24,X2),distLessThan(X0,X1,25,X2),distLessThan(X1,X0,40,X2),distLessThan(X1,X0,34,X2),distLessThan(X1,X0,27,X2),distLessThan(X1,X0,24,X2),distLessThan(X1,X0,25,X2),orientFar(X0,X1,45,X2),orientFar(X1,X0,45,X2),holdsAt(visible(X0),X2),holdsAt(visible(X1),X2)",
      "initiatedAt(meeting(X0,X1),X2) :- happensAt(walking(X1),X2),happensAt(walking(X0),X2),distLessThan(X1,X0,40,X2),distLessThan(X1,X0,34,X2),distLessThan(X1,X0,27,X2),distLessThan(X1,X0,24,X2),distLessThan(X1,X0,25,X2),distLessThan(X0,X1,40,X2),distLessThan(X0,X1,34,X2),distLessThan(X0,X1,27,X2),distLessThan(X0,X1,24,X2),distLessThan(X0,X1,25,X2),orientFar(X1,X0,45,X2),orientFar(X0,X1,45,X2),holdsAt(visible(X1),X2),holdsAt(visible(X0),X2)")

    // first clause misses some body atoms (true)
    val pair7 = ("terminatedAt(moving(X0,X1),X2) :- happensAt(walking(X0),X2),happensAt(walking(X1),X2),distLessThan(X0,X1,34,X2),distLessThan(X0,X1,27,X2),distLessThan(X0,X1,24,X2),distLessThan(X0,X1,25,X2),distLessThan(X1,X0,40,X2),distLessThan(X1,X0,27,X2),distLessThan(X1,X0,24,X2),orientFar(X0,X1,45,X2),orientFar(X1,X0,45,X2),holdsAt(visible(X0),X2),holdsAt(visible(X1),X2)",
      "terminatedAt(moving(X0,X1),X2) :- happensAt(walking(X0),X2),happensAt(walking(X1),X2),distLessThan(X0,X1,40,X2),distLessThan(X0,X1,34,X2),distLessThan(X0,X1,27,X2),distLessThan(X0,X1,24,X2),distLessThan(X0,X1,25,X2),distLessThan(X1,X0,40,X2),distLessThan(X1,X0,34,X2),distLessThan(X1,X0,27,X2),distLessThan(X1,X0,24,X2),distLessThan(X1,X0,25,X2),orientFar(X0,X1,45,X2),orientFar(X1,X0,45,X2),holdsAt(visible(X0),X2),holdsAt(visible(X1),X2)")

    // variable renaming (X0 -> A in the 2nd clause) (true)
    val pair8 = ("initiatedAt(meeting(X0,X1),X2) :- happensAt(walking(X1),X2),happensAt(walking(X0),X2),distLessThan(X1,X0,40,X2),distLessThan(X1,X0,34,X2),distLessThan(X1,X0,27,X2),distLessThan(X1,X0,24,X2),distLessThan(X1,X0,25,X2),distLessThan(X0,X1,40,X2),distLessThan(X0,X1,34,X2),distLessThan(X0,X1,27,X2),distLessThan(X0,X1,24,X2),distLessThan(X0,X1,25,X2),orientFar(X1,X0,45,X2),orientFar(X0,X1,45,X2),holdsAt(visible(X1),X2),holdsAt(visible(X0),X2)",
      "initiatedAt(meeting(A,X1),X2) :- happensAt(walking(X1),X2),happensAt(walking(A),X2),distLessThan(X1,A,40,X2),distLessThan(X1,A,34,X2),distLessThan(X1,A,27,X2),distLessThan(X1,A,24,X2),distLessThan(X1,A,25,X2),distLessThan(A,X1,40,X2),distLessThan(A,X1,34,X2),distLessThan(A,X1,27,X2),distLessThan(A,X1,24,X2),distLessThan(A,X1,25,X2),orientFar(X1,A,45,X2),orientFar(A,X1,45,X2),holdsAt(visible(X1),X2),holdsAt(visible(A),X2)")

    val pair9 = ("p(X,Y,Z,W,S,R,T) :- a(1,2,4,X,S),b(1),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y)",
      "p(X,Y,Z,W,S,R,T) :- a(1,2,4,X,S),b(1),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y)")

    // true
    val pair10 = ("p(X,Y) :- q1(X,Z),q2(Z,Y)", "p(1,3) :- q1(1,2),q2(2,3)")

    // false
    val pair11 = ("p(X,Y) :- q1(X,Y),q2(Y,Y)", "p(1,3) :- q1(1,2),q2(2,3)")
    assert(subsumes(parse(pair1)))
    assert(subsumes(parse(pair2)))
    assert(!subsumes(parse(pair3)))
    assert(subsumes(parse(pair4)))
    assert(!subsumes(parse(pair5)))
    assert(!subsumes(parse(pair6)))
    assert(subsumes(parse(pair7)))
    assert(subsumes(parse(pair8)))
    assert(subsumes(parse(pair9)))
    assert(subsumes(parse(pair10)))
    assert(!subsumes(parse(pair11)))
  }

}
