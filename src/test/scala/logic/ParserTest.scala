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
import trail.logic.Literal
import trail.logic.parsers.PB2LogicParser

class ParserTest extends AnyFunSuite with LazyLogging {

  logger.info("Executing Parser's tests...")

  test("Parser test (using the parboiled2 parser)") {
    val t = PB2LogicParser.parseAtom("fluentGrnd(holdsAt(reFuel(\"7330_124060\"),1498863690))", debug = true)
    // - in
    PB2LogicParser.parseAtom("use(-34534534,6)", debug = true)
    PB2LogicParser.parseAtom("initiatedAt(meeting(X0,X1,45),1,Z,Petryb,a(p(2,3,z(23,g,f,ert(sdjfskj,Xkjsh))),1),oo(12,23,E))", debug = true)
    // with a final "."
    PB2LogicParser.parseAtom("initiatedAt(meeting(X0,X1,45),1,Z,Petryb,a(p(2,3,z(23,g,f,ert(sdjfskj,Xkjsh))),1),oo(12,23,E)).", debug = true)
    // The Atom parser succeeds in the first match, e.g here it parses the (garbage) expression since it matches the head.
    // I'll have to do something with EOI to disallow that
    PB2LogicParser.parseAtom("initiatedAt(meeting(X0,X1,45),1,Z,Petryb,a(p(2,3,z(23,g,f,ert(sdjfskj,Xkjsh))),1),oo(12,23,E)). :- sdfsdfsfsdfsdf", debug = true)
    // negation
    PB2LogicParser.parseAtom("not initiatedAt(meeting(X0,X1,45),1,Z,Petryb,a(p(2,3,z(23,g,f,ert(sdjfskj,Xkjsh))),1),oo(12,23,E))", debug = true)
    // negation with final "."
    PB2LogicParser.parseAtom("not initiatedAt(meeting(X0,X1,45),1,Z,Petryb,a(p(2,3,z(23,g,f,ert(sdjfskj,Xkjsh))),1),oo(12,23,E)).", debug = true)
    // clause
    PB2LogicParser.parseClause("initiatedAt(meeting(X0,X1,45),1,Z,Petryb,a(p(2,3,z(23,g,f,ert(sdjfskj,Xkjsh))),1),oo(12,23,E)) :- happensAt(walking(X,34,p(a(s,2),Z)),T,Or),close(1,2,3,4,Yt)", debug = true)
    // clause with final "."
    PB2LogicParser.parseClause("initiatedAt(meeting(X0,X1,45),1,Z,Petryb,a(p(2,3,z(23,g,f,ert(sdjfskj,Xkjsh))),1),oo(12,23,E)) :- happensAt(walking(X,34,p(a(s,2),Z)),T,Or),close(1,2,3,4,Yt)", debug = true)
    // clause with NAF in the body
    PB2LogicParser.parseClause("initiatedAt(meeting(X0,X1,45),1,Z,Petryb,a(p(2,3,z(23,g,f,ert(sdjfskj,Xkjsh))),1),oo(12,23,E)) :- not happensAt(walking(X,34,p(a(s,2),Z)),T,Or),close(1,2,3,4,Yt),not happens(a(X,R,T,z(a,23,4)))", debug = true)

    val mlnTest = PB2LogicParser.parseAtom("initiatedAt(meeting(a(k,l,m),b,45),c,a,d)", debug = true)
    val a = Literal.toMLNFlat(mlnTest.asInstanceOf[Literal])
    println(a.tostring)

    // This does not work for variabilized literals (throws an exception).
    val mlnTest1 = PB2LogicParser.parseAtom("initiatedAt(c,A,d)", debug = true)
    //val b = Literal.toMLNFlat(mlnTest1.asInstanceOf[Literal])
    //println(b.tostring)
  }
}
