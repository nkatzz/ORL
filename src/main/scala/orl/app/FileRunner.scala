package orl.app

import akka.actor.{ActorSystem, Props}
import com.typesafe.scalalogging.LazyLogging
import orl.app.runutils.CMDArgs
import orl.datahandling.InputHandling
import orl.datahandling.InputHandling.FileDataOptions
import orl.learning.LocalCoordinator
import orl.learning.Types.RunSingleCore

object FileRunner extends App with LazyLogging {

  val (argsOK, msg) = CMDArgs.argsOk(args)

  if (argsOK) {

    val runningOptions = CMDArgs.getOLEDInputArgs(args)

    val trainingDataOptions = FileDataOptions(
      file          = runningOptions.train,
      chunkSize     = runningOptions.chunkSize,
      targetConcept = runningOptions.targetHLE,
      sortByFunction = (x: String) => x.split(',').last.replaceAll("\\)", "").trim.toInt
    )

    val testingDataOptions = FileDataOptions(
      file          = runningOptions.test,
      chunkSize     = runningOptions.chunkSize,
      targetConcept = runningOptions.targetHLE,
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
