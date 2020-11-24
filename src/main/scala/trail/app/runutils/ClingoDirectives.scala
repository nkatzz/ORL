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
import scala.collection.immutable

class ClingoDirectives(modesInfo: ModesInfoParser) {

  private val exmplPatterns = modesInfo.exmplPatterns.map(x => x.varbed)

  private val tps_fps_fns_tns = (x: String, what: String) => {
    what match {
      case "tps" => s"tps($x) :- $x, example($x), target($x)."
      case "fps" => s"fps($x) :- $x, not example($x), target($x)."
      case "fns" => s"fns($x) :- example($x), not $x, target($x)."
      case "tns" => s"tns($x) :- not example($x), not $x, target($x)."
    }
  }

  private val hardCoverageConstraints = (atom: String, what: String) => {
    what match {
      case "pos" => s":- example($atom), not $atom, target($atom)." // cover all positives constraint
      case "neg" => s":- $atom, not example($atom), target($atom)." // exclude all negatives constraint
      case "" => ""
    }
  }

  private val minimize = (atom: String, vars: Vector[String], what: String) => {
    what match {
      case "fps" => s"#minimize{1@1,${vars.mkString(",")} : fps($atom)}."
      case "fns" => s"#minimize{1@1,${vars.mkString(",")} : fns($atom)}."
      case _ => ""
    }
  }

  val tps_fps_fns_tns_defs: immutable.Seq[String] = List("tps", "fps", "fns", "tns").flatMap { what =>
    exmplPatterns.map { atom => tps_fps_fns_tns(atom.tostring, what) }
  }

  val hardCoverageConstrs: immutable.Seq[String] = List("pos", "neg").flatMap { what =>
    exmplPatterns.map { atom => hardCoverageConstraints(atom.tostring, what) }
  }

  val minimizeStatements: immutable.Seq[String] = List("fps", "fns").flatMap { what =>
    exmplPatterns.map { x =>
      val vars = x.getVars.map(_.name).toVector
      minimize(x.tostring, vars, what)
    }
  }

}
