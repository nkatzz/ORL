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

import java.text.DecimalFormat

import akka.actor.{Actor, PoisonPill}
import org.slf4j.{Logger, LoggerFactory}
import orl.app.runutils.RunningOptions
import orl.datahandling.Example
import orl.datahandling.InputHandling.InputSource
import orl.learning.Types.{FinishedBatch, LocalLearnerFinished, Run, StartOver}
import orl.logic.{Clause, Literal, LogicUtils}
import orl.utils.Utils.{underline, underlineStars}

/**
  * Created by nkatz at 13/12/19
  *
  * Defines the basics logic for processing data and the abstract methods process and generateNewRules,
  * which is implemented by inheriting classes, that use either LoMRF for MAP inference, or Clingo.
  *
  */

abstract class Learner[T <: InputSource](inps: RunningOptions, trainingDataOptions: T,
    testingDataOptions: T, trainingDataFunction: T => Iterator[Example],
    testingDataFunction: T => Iterator[Example]) extends Actor {

  import context.become

  val startTime: Long = System.nanoTime()
  val logger: Logger = LoggerFactory.getLogger(self.path.name)
  var repeatFor: Int = inps.repeatFor
  var data: Iterator[Example] = Iterator[Example]()
  var state = new State(inps)

  var inertiaAtoms = Set.empty[Literal]
  var batchCount = 1
  var withHandCrafted = false

  var avgNumberOfMistakesSoFar = 0.0

  var trainingTime = 0.0

  /**
    * Maximum number of seed atoms (generated from mistakes) from which bottom clauses
    * will be generated. Set to Double.PositiveInfinity to lift the restriction on the
    * max seeds and used all atoms corresponding to mistakes.
    */
  val maxNumberOfSeedAtoms: Double = 100.0 //Double.PositiveInfinity // 20.0

  /**
    * This is used with the "subsets" specialization strategy, where all subsets of a bottom clause
    * up to maxClauseLength are generated.
    */
  val maxClauseLength: Int = 4

  //private var previousPredicateCompletion = Set[WeightedFormula]()
  //private var previousCNF = Vector[lomrf.logic.Clause]()

  // Use a hand-crafted theory for debugging
  /*def matches(p: Regex, str: String) = p.pattern.matcher(str).matches
  val source = Source.fromFile("/home/nkatz/dev/BKExamples/BK-various-taks/WeightLearning/Caviar/fragment/meeting/ASP/asp-rules-test-moving")
  val list = source.getLines.filter(line => !matches("""""".r, line) && !line.startsWith("%"))
  val rulesList = list.map(x => Clause.parse(x)).toList
  source.close
  state.updateRules(rulesList, "add", inps)
  withHandCrafted = true*/

  private def getTrainingData: Iterator[Example] = trainingDataFunction(trainingDataOptions)
  private def getNextBatch: Example = if (data.isEmpty) Example() else data.next()

  /**
    * Abstract method, to be implemented by specific learners.
    *
    */
  def process(exmpl: Example): Unit

  /**
    * Abstract method, to be implemented by specific learners.
    *
    */
  def generateNewRules(existingTheory: List[Clause], exmpl: Example, inps: RunningOptions): List[Clause]

  /**
    * Abstract method, to be implemented by specific learners.
    * It displays statistics from the learning process, performs cross-validation
    * (if testing set is provided) etc.
    *
    */
  def wrapUp()

  /**
    * The logic for data processing follows.
    *
    * When this class receives a Run message, it calls the start() method, which fetches the data
    * and sends the first batch (an Example instance) to self.
    *
    * This class has two states: controlState & processingState. The latter is the state of the class when
    * processing an example/batch (doing all the learning work), while the former is the state in between (e.g. finished
    * processing a batch, what should I do)? The distinction is useful because when in control state the learner may
    * do various house-keeping taks, including e.g. communicating with other learners to exchange info, in a
    * distributed learning setting.
    *
    * Initially the learner is in control state. Upon receiving a message of type Example, the learner enters in a
    * processingState and does all the work related to processing the current data. When finished, it sends a FinishedBatch
    * message to self. Upon receiving such a message, the learner either sends the next batch for processing, wraps things
    * up (prints informative stats messages) and terminates, in case the data is finished or, keeps re-iterating over the data
    * (it re-fetches the data and starts over, this is specified by the repeatFor parameter).
    *
    * This design simulates a setting when the data constantly arrive from an external source. In between processing two
    * data batches, the learner is in control state. Communication protocols with other learners should be implemented
    * when the learner is in control state. For instance, it could be specified that if the learner has just finished
    * its k-th data batch, it should broadcast its current theory to a global coordinator.
    */

  def receive: PartialFunction[Any, Unit] = {
    case _: Run =>
      become(controlState)
      start()
  }

  def start(): Unit = {
    this.repeatFor -= 1
    //data = scala.util.Random.shuffle(getTrainingData)
    data = getTrainingData
    if (data.isEmpty) { logger.error(s"No data received."); System.exit(-1) }
    val nextBatch = getNextBatch
    self ! nextBatch
  }

  def controlState: Receive = {
    case exmpl: Example =>
      if (exmpl.isEmpty) {
        wrapUp()
      } else {
        become(processingState)
        self ! exmpl
      }

    case _: FinishedBatch =>
      // This is the place to do any control checks that
      // may require communication with the coordinator (or other actors).
      // This is why we separate between control and processing state, so that we
      // may do any necessary checks right after a data chunk has been processed.
      // For now, just get the next data chunk.
      self ! getNextBatch

    case _: StartOver =>
      logger.info(underline(s"Starting a new training iteration (${this.repeatFor} iterations remaining.)"))
      state.finishedIterationInfo(logger)
      become(controlState)
      start()
  }

  def processingState: Receive = {
    case exmpl: Example =>
      process(exmpl)
      batchCount += 1
      become(controlState)
      self ! new FinishedBatch
  }

  def shutDown() = {
    self ! PoisonPill
    context.parent ! new LocalLearnerFinished
  }

  def showStats(theory: List[Clause]) = {

      // used for printing out the average loss vector
      def avgLoss(in: Vector[Int]) = {
        in.foldLeft(0, 0, Vector.empty[Double]) { (x, y) =>
          val (count, prevSum, avgVector) = (x._1, x._2, x._3)
          val (newCount, newSum) = (count + 1, prevSum + y)
          (newCount, newSum, avgVector :+ newSum.toDouble / newCount)
        }
      }

    val endTime = System.nanoTime()
    val totalTime = (endTime - startTime) / 1000000000.0

    trainingTime = totalTime

    logger.info(s"\nTheory:\n${LogicUtils.showTheoryWithStats(theory, inps.scoringFun, inps.weightLean)}\nTraining time: $totalTime")
    logger.info(s"Mistakes per batch:\n${state.perBatchError}")
    logger.info(s"Accumulated mistakes per batch:\n${state.perBatchError.scanLeft(0.0)(_ + _).tail}")
    logger.info(s"Average loss vector:\n${avgLoss(state.perBatchError)}")
    //logger.info(s"Sending the theory to the parent actor")
    state.finalInfo(logger)

  }

  def format(x: Double) = {
    val defaultNumFormat = new DecimalFormat("0.###")
    defaultNumFormat.format(x)
  }

  def batchInfoMsg(theoryForPrediction: List[Clause], tpCounts: Int, fpCounts: Int,
      fnCounts: Int, inferenceTime: Double, scoringTime: Double) = {

    val batchMsg = underlineStars(s"*** BATCH $batchCount ***")
    val theoryMsg = underline(s"TPs: $tpCounts, FPs: $fpCounts, FNs: $fnCounts. Inference time: $inferenceTime, scoring time: $scoringTime. Theory used for prediction:")
    val theory = {
      if (inps.weightLean) {
        theoryForPrediction.map(x => s"${x.tostring} | W: ${format(x.weight)} | Precision: ${format(x.precision)} | (TPs,FPs): (${x.tps}, ${x.fps}) ").mkString("\n")
      } else {
        theoryForPrediction.map(x => s"${x.tostring} | Precision: ${format(x.precision)} | (TPs,FPs): (${x.tps}, ${x.fps}) ").mkString("\n")

      }
    }
    if (theoryForPrediction.nonEmpty) s"\n$batchMsg\n$theoryMsg\n$theory" else s"*** BATCH $batchCount ***"
  }

  def updateStats(tpCounts: Int, fpCounts: Int, fnCounts: Int, totalGroundings: Int) = {
    state.totalTPs += tpCounts
    state.totalFPs += fpCounts
    state.totalFNs += fnCounts
    state.perBatchError = state.perBatchError :+ (fpCounts + fnCounts)
    state.totalGroundings += totalGroundings
    state.updateRulesGroundingsCounts(totalGroundings)
  }

}
