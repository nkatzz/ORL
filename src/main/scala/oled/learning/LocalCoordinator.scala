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

package oled.learning

import akka.actor.{Actor, Props}
import oled.app.runutils.InputHandling.InputSource
import oled.app.runutils.RunningOptions
import oled.datahandling.Example
import oled.learning.Types.{LocalLearnerFinished, Run, RunSingleCore}

/**
  * Created by nkatz at 13/12/19
  */

class LocalCoordinator[T <: InputSource](inps: RunningOptions, trainingDataOptions: T,
    testingDataOptions: T, trainingDataFunction: T => Iterator[Example],
    testingDataFunction: T => Iterator[Example]) extends Actor {

  def receive = {
    case msg: RunSingleCore =>
      val worker = context.actorOf(Props(new Learner(inps, trainingDataOptions,
                                                     testingDataOptions, trainingDataFunction, testingDataFunction)), name = s"worker-${this.##}")

      worker ! new Run

    case _: LocalLearnerFinished => context.system.terminate()
  }

}
