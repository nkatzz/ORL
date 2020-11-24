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

import trail.logic.parsers.PB2LogicParser

/**
  * Parses data in a training set (for the one-shot learning case), or in a mini-batch
  * (for the incremental/online case) and discriminates between evidence and query atoms.
  * It also generates and Example object from the input (mini-) batch.
  */

object InputDataParser {

  def parseData(input: String, inps: RunningOptions) = {
    val inputAtoms = PB2LogicParser.parseInputData(input)
    val targetAtoms = inps.globals.exmplPatternsVarbed
    val (queryAtoms, observationAtoms) = inputAtoms.partition(x => targetAtoms.exists(y => y.thetaSubsumes(x)))
    (queryAtoms, observationAtoms)
  }

}
