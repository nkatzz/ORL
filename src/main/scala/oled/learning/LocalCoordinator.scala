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
    case msg : RunSingleCore =>
      val worker = context.actorOf(Props(new Learner(inps, trainingDataOptions,
            testingDataOptions, trainingDataFunction, testingDataFunction)), name = s"worker-${this.##}")

      worker ! new Run

    case _ : LocalLearnerFinished => context.system.terminate()
  }


}
