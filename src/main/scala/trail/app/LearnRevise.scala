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

package trail.app

import com.typesafe.scalalogging.LazyLogging
import trail.app.runutils.CMDArgs
import trail.learning.batch.LearnRevise

object LearnRevise extends App with LazyLogging {

  val inps = CMDArgs.parseInputOptions(args)
  if (inps.test != "None") {
    // Testing an existing theory
    val learner = new LearnRevise(inps)
    val (tps, fps, fns) = learner.evaluateTheory(learner.existingTheory, learner.exmpl)
    logger.info(s"\nPerformance on test set: TPs: ${tps.size}, FPs: ${fps.size}, FNs: ${fns.size}")

  } else {
    // Learning.
    val learner = new LearnRevise(inps)
    val t = trail.app.utils.Utils.time{ learner.learnRevise() }
    //logger.info(s"Total time: ${t._2}")
    //logger.info(s"Total time(from the LearnRevise class): ${learner.totalInductionTime}")
    //val (inducedRules, refinedRules, retainedRules, removedRules) = learner.learnRevise()
  }

}
