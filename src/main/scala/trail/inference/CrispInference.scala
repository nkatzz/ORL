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

package trail.inference

import trail.app.runutils.RunningOptions
import trail.datahandling.Example
import trail.logic.Clause

/**
  * Created by nkatz at 27/9/20
  */

class CrispInference(val data: Example, val theory: List[Clause],
    val inps: RunningOptions, val bk: String = "") {

  val input = data.toASP()
  val target = inps.globals.eps1.map(x => x.varbed)
  val program =
    s"""
      |${input.mkString(" ")}
      |
      |""".stripMargin

}
