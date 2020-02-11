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

import oled.logic.parsers.ModesParser

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

  /**
    * This excludes from the top theory that is used for prediction rules that subsume others.
    * For instance if we have in the top theory
    *
    * r1: initiatedAt(meet(X0,X1),X2) :- happensAt(active(X0),X2)
    * r2: initiatedAt(meet(X0,X1),X2) :- happensAt(active(X0),X2), close(X0,X1,24,X2)
    *
    * then only r2 is used for prediction.
    *
    * The rational is that since r2 has been generated, this means that it is of higher quality than r1
    * (since r2 was r1 at some point, but it got specialized).
    *
    * r1 has not been specialized, since no literal of sufficient gain exists in its bottom clause,
    * or it is too "young". In the first case it will be pruned away at some point, while in the second
    * case it will be specialized (into a version that differs from r2 itself).
    */
  def compressTheoryKeepMoreSpecific(_initialTheory: Iterable[Clause]) = {

    var compressed = List[Clause]()

    // First compress the initialTheory in the regular way, to remove duplicates.
    // Then (and only then) the following works.
    val initialTheory = compressTheory(_initialTheory)

    var _compressed = initialTheory.foldLeft(List[Clause]()) { (compressedTheory, clause) =>
      // we also need to remove the clause itself from the initial list, otherwise the test
      // below will always fail (as a clause always subsumes itself).
      val initWithoutClause = initialTheory.filter(x => x != clause)
      if (!initWithoutClause.exists(otherClause => clause.thetaSubsumes(otherClause))) compressedTheory :+ clause else compressedTheory
    }

    // If _compressed is empty, then initialTheory consists of multiple copies of the same rule, so just keep the first one.
    if (_compressed.isEmpty && initialTheory.nonEmpty) compressed = List(initialTheory.head)
    else compressed = _compressed

    compressed
  }

  def showTheoryWithStats(clauses: Iterable[Clause], scoreFun: String, showWeights: Boolean = true) = {
    clauses.map(x => x.showWithStatsFormal(scoreFun, showWeights)).mkString("\n")
  }

  def main(args: Array[String]) = {

    val p = new ModesParser
    //val e = "modeb(close(+person,+person,24,+time))"
    val e = "modeh(initiatedAt(meeting(+person,+person),+time))"
    val t = p.getParseResult(p.parseModes(p.modeAtom, e))
    val args = t.args
    println(t.tostring)
    println(t.varbed.tostring)
  }

  /**
    * Generates a bottom clause directly from the mode declarations.
    * This is for the case where we don't use a data-driven BC, to cope with the noise in the data.
    *
    */
  def bottomClauseFromModes(headDecl: ModeAtom, bodyDecls: List[ModeAtom]) = {

    val p = new ModesParser
    //def parse(expression: String) = p.getParseResult(p.parseModes(p.mode, expression))

    var variables = ListBuffer.empty[Variable]
    var constants = ListBuffer.empty[Constant]
    var inputVariables = ListBuffer.empty[Variable]
    var outputVariables = ListBuffer.empty[Variable] /*TODO*/

      /**
        * Generates atoms for the BC from one mode declaration
        */
      def generateHeadBCAtom() = {
        val predSymbol = headDecl.predSymbol
        //headDecl.
      }

  }

}
