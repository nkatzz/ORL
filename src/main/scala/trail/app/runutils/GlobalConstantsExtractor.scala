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

package trail.app.runutils

import trail.inference.ASPSolver
import trail.logic.{Constant, Literal, ModeAtom}

/**
  * Exract global domain constants from the BK to use for constructing bottom theories to
  * structure the search space in a data-independent fashion.
  * The global domain constants replace ground placemarkers in mode declarations. For instance, consider the
  * mode atom specification:
  * body(close(+person,+person,#dist,+time))
  * To generate a rule with an atom with this signature in its body we need constants values for #dist, which are
  * extracted from the BK (for instance, in CAVIAR, these are the values of distance_threshold/1).
  * *
  * Similarly, given the mode atom specification modeh(initiatedAt(#fluent,+time)) (in Yale shooting), we need
  * global constants to replace the #fluent argument (these are loaded, dead, alive).
  * *
  * TODO: In addition to the domain constants found in the BK, also gradually augment this set with additional constants
  *   extracted from the data over time (which may not be known beforehand).
  *
  */

class GlobalConstantsExtractor(mip: ModesInfoParser, BK: String) {

  private val pospmk = """\+[a-z][a-zA-Z0-9_]*""".r
  private val negpmk = """\-[a-z][a-zA-Z0-9_]*""".r
  private val constpmk = """\#[a-z][a-zA-Z0-9_]*""".r

  private val types = (mip.headModes ++ mip.bodyModes).foldLeft(Set.empty[String]) { (x, modeAtom) =>
    val str = modeAtom.totext
    val placemarkers = (pospmk.findAllIn(str).toSet ++ negpmk.findAllIn(str).toSet ++ constpmk.findAllIn(str).toSet).map(_.drop(1))
    x ++ placemarkers
  }

  private val shows = types.map(x => s"#show $x/1.")
  private val program = s"$BK\n\n#show.\n${shows.mkString("\n")}"
  private val result = ASPSolver.solve(program)

  val domainConstants: Map[String, Array[String]] =
    result.map(atom => Literal.parseWPB2(atom)).map { atom =>
      (atom.predSymbol, atom.terms.head.asInstanceOf[Constant].name)
    }.groupBy(x => x._1).map { case (x, y) => x -> y.map(z => z._2).toArray }

}

