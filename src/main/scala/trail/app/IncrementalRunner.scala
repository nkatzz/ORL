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

import akka.actor.{ActorSystem, Props}
import trail.app.runutils.{CMDArgs, InputHandling}
import InputHandling.FileDataOptions
import trail.learning.incremental.HillClimbingTheoryLearner
import trail.learning.online.Types.Run

class IncrementalRunner extends App {

  val inps = CMDArgs.parseInputOptions(args)

  val trainingDataOptions = FileDataOptions(
    filepath       = inps.train,
    chunkSize      = inps.chunkSize,
    targetConcepts = inps.targetConcepts,
    sortByFunction = (x: String) => x.split(',').last.replaceAll("\\)", "").trim.toInt
  )

  val testingDataOptions = FileDataOptions(
    filepath       = inps.test,
    chunkSize      = inps.chunkSize,
    targetConcepts = inps.targetConcepts,
    setting        = "testing",
    sortByFunction = (x: String) => x.split(',').last.replaceAll("\\)", "").trim.toInt
  )

  val system = ActorSystem("hill-climbing-learner")
  val startMsg = new Run

  val learner = system.actorOf(Props(
    new HillClimbingTheoryLearner(
      inps,
      trainingDataOptions,
      testingDataOptions,
      InputHandling.getFileData,
      InputHandling.getFileData
    )), name = "LocalCoordinator")

  learner ! startMsg

}
