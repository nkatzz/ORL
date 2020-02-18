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

package orl.learning

import com.typesafe.scalalogging.LazyLogging
import orl.app.runutils.RunningOptions
import orl.logic.Clause

/**
  * Created by nkatz at 31/1/20
  */

class PruningSpecs(val minPrecision: Double, val bodyLength: Int, val oldness: Int) {}

class Pruning(specs: PruningSpecs, inps: RunningOptions) extends LazyLogging {

  private val weightLearning = inps.weightLean
  private val acceptablePrecision = specs.minPrecision
  private val maxBodyLength = specs.bodyLength
  private val oldness = specs.oldness

  /**
    * A rule is hopeless if its precision...
    */
  def isHopeless(c: Clause) = {

  }

  def showPruned(c: Clause) = {
    // Note that the number of examples a rule has been evaluated on is the number of examples
    // it fires on, NOT the number of examples seen so far in the stream. Therefore, we're pruning
    // if the rule is of low quality after TPs+FPs examples.
    val msg =
      s"\n===========================================================\n" +
        s"\nPruned clause (Precision: ${c.precision} | TPs: ${c.tps} FPs: ${c.fps} FNs: ${c.fns} | Weight: ${c.weight})\n\n${c.tostring}\n\n" +
        s"After ${c.seenExmplsNum} examples." +
        s"\n===========================================================\n"
    logger.info(msg)
  }

}
