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

package trail.learning.online

import akka.actor.{ActorSystem, Props}
import com.typesafe.scalalogging.LazyLogging
import trail.app.runutils.CMDArgs
import trail.datahandling.InputHandling
import trail.datahandling.InputHandling.FileDataOptions
import trail.learning.online.Types.RunSingleCore

object OnlineFilePartitionerRunner extends App with LazyLogging {

  /**
    * Run the application with data fetched from a file.
    * The data are partitioned dynamically (therefore, the mini-batch size is
    * adjustable and the --chunksize aption applies here).
    * This is the main difference of this runner from the FileRunner class, where
    * the data are passed from a file, but they are already partitioned into mini-batches.
    *
    */

  val (argsOK, msg) = CMDArgs.argsOk(args)

  if (argsOK) {

    val runningOptions = CMDArgs.getOLEDInputArgs(args)

    val trainingDataOptions = FileDataOptions(
      filepath       = runningOptions.train,
      chunkSize      = runningOptions.chunkSize,
      targetConcepts = runningOptions.targetConcepts,
      sortByFunction = (x: String) => x.split(',').last.replaceAll("\\)", "").trim.toInt
    )

    val testingDataOptions = FileDataOptions(
      filepath       = runningOptions.test,
      chunkSize      = runningOptions.chunkSize,
      targetConcepts = runningOptions.targetConcepts,
      setting        = "testing",
      sortByFunction = (x: String) => x.split(',').last.replaceAll("\\)", "").trim.toInt
    )

    val system = ActorSystem("LocalLearningSystem")
    val startMsg = new RunSingleCore

    val coordinator = system.actorOf(Props(
      new LocalCoordinator(
        runningOptions,
        trainingDataOptions,
        testingDataOptions,
        InputHandling.getFileData,
        InputHandling.getFileData
      )), name = "LocalCoordinator")

    coordinator ! startMsg

  } else {
    logger.error(msg)
    sys.exit(1)
  }
}
