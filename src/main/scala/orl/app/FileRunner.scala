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

package orl.app

import akka.actor.{ActorSystem, Props}
import com.typesafe.scalalogging.LazyLogging
import orl.app.runutils.CMDArgs
import orl.datahandling.InputHandling
import orl.datahandling.InputHandling.FileDataOptions
import orl.learning.Types.RunSingleCore
import orl.learning.LocalCoordinator
import orl.logic.Clause

import scala.util.control.Breaks

object FileRunner extends App with LazyLogging {

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
