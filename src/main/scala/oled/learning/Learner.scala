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

import java.text.DecimalFormat

import akka.actor.{Actor, PoisonPill}
import oled.app.runutils.InputHandling.InputSource
import oled.app.runutils.RunningOptions
import oled.datahandling.Example
import oled.inference.{ASPSolver, MAPSolver}
import oled.learning.Types.{FinishedBatch, LocalLearnerFinished, Run, StartOver}
import oled.learning.structure.{OldStructureLearningFunctions, RuleExpansion}
import oled.logic.{Clause, Literal, LogicUtils}
import oled.utils.Utils.{underline, underlineStars}
import org.slf4j.LoggerFactory

/**
  * Created by nkatz at 13/12/19
  */

class Learner[T <: InputSource](
    inps: RunningOptions,
    trainingDataOptions: T,
    testingDataOptions: T,
    trainingDataFunction: T => Iterator[Example],
    testingDataFunction: T => Iterator[Example]) extends Actor {

  import context.become

  private val startTime = System.nanoTime()
  private val logger = LoggerFactory.getLogger(self.path.name)
  private var repeatFor = inps.repeatFor
  private var data = Iterator[Example]()
  private var state = new State(inps)

  private var inertiaAtoms = Set.empty[Literal]
  private var batchCount = 0
  private var withHandCrafted = false

  var avgNumberOfMistakesSoFar = 0.0

  //private var previousPredicateCompletion = Set[WeightedFormula]()
  //private var previousCNF = Vector[lomrf.logic.Clause]()

  private def getTrainingData = trainingDataFunction(trainingDataOptions)
  private def getNextBatch = if (data.isEmpty) Example() else data.next()

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

  def process(exmpl: Example): Unit = {

    //logger.info("\n"+underline(s"*** BATCH $batchCount *** "))

    val e = LearningUtils.dataToMLNFormat(exmpl, inps)

    var rules = List.empty[Clause]
    var inferredState = Map.empty[String, Boolean]
    var tpCounts = 0
    var fpCounts = 0
    var fnCounts = 0
    var totalGroundings = 0
    var rulesCompressed = List.empty[Clause]

    if (inps.weightLean) {

      //rules = state.getAllRules(inps, "all").filter(x => x.body.nonEmpty)

      rules = state.getAllRules(inps, "top")

      //rules = state.getBestRules(inps.globals)

      rulesCompressed = LogicUtils.compressTheory(rules)
      //rulesCompressed = LogicUtils.compressTheoryKeepMoreSpecific(rules)

      //println("MAP inference...")
      val mapInfResult = MAPSolver.solve(rulesCompressed, e, this.inertiaAtoms, inps)

      inferredState = mapInfResult._1

      // Doing this in parallel is trivial (to speed things up in case of many rules/large batches).
      // Simply split the rules to multiple workers, the grounding/counting tasks executed are completely rule-independent.
      //println("      Scoring...")
      val (_tpCounts, _fpCounts, _fnCounts, _totalGroundings, _inertiaAtoms) =
        LearningUtils.scoreAndUpdateWeights(e, inferredState, state.getAllRules(inps, "all").toVector, inps, logger, batchCount = batchCount)

      tpCounts = _tpCounts
      fpCounts = _fpCounts
      fnCounts = _fnCounts
      totalGroundings = _totalGroundings
      inertiaAtoms = _inertiaAtoms.toSet

      /*=============== OLED ================*/
    } else {
      rulesCompressed = state.getBestRules(inps.globals, "score") //.filter(x => x.score(inps.scoringFun) >= 0.9)
      val inferredState = ASPSolver.crispLogicInference(rulesCompressed, e, inps.globals)

      val (_tpCounts, _fpCounts, _fnCounts, _totalGroundings, _inertiaAtoms) =
        LearningUtils.scoreAndUpdateWeights(e, inferredState, state.getAllRules(inps, "all").toVector, inps, logger)

      tpCounts = _tpCounts
      fpCounts = _fpCounts
      fnCounts = _fnCounts
      totalGroundings = _totalGroundings
      inertiaAtoms = _inertiaAtoms.toSet
    }

    updateStats(tpCounts, fpCounts, fnCounts)

    this.inertiaAtoms = inertiaAtoms
    this.inertiaAtoms = Set.empty[Literal] // Use this to difuse inertia

    state.perBatchError = state.perBatchError :+ (fpCounts + fnCounts)

    logger.info(batchInfoMsg(rulesCompressed, tpCounts, fpCounts, fnCounts))

    //logger.info(s"\n${state.perBatchError}")
    //logger.info(s"\nFPs: $fpCounts, FNs: $fnCounts")

    if (!withHandCrafted) {
      state.totalGroundings += totalGroundings
      state.updateGroundingsCounts(totalGroundings)

      var newInit = List.empty[Clause]
      var newTerm = List.empty[Clause]

      if (fpCounts != 0 || fnCounts != 0) {
        /*val topInit = state.initiationRules.filter(_.body.nonEmpty)
        val topTerm = state.terminationRules.filter(_.body.nonEmpty)
        val growNewInit = growNewRuleTest(topInit, e, inps.globals, "initiatedAt")
        val growNewTerm = growNewRuleTest(topTerm, e, inps.globals, "terminatedAt")
        newInit = if (growNewInit) generateNewRules(topInit, e, "initiatedAt", inps.globals) else Nil
        newTerm = if (growNewTerm) generateNewRules(topTerm, e, "terminatedAt", inps.globals) else Nil
        //newInit = generateNewRules(topInit, e, "initiatedAt", inps.globals) //if (growNewInit) generateNewRules(topInit, e, "initiatedAt", inps.globals) else Nil
        //newTerm = generateNewRules(topTerm, e, "terminatedAt", inps.globals) //if (growNewTerm) generateNewRules(topTerm, e, "terminatedAt", inps.globals) else Nil
        */

        val theory = rulesCompressed

        val newRules = OldStructureLearningFunctions.generateNewRules(theory, e, inps)

        val (init, term) = newRules.partition(x => x.head.predSymbol == "initiatedAt")

        newInit = init //.filter(p => !state.isBlackListed(p))
        newTerm = term //.filter(p => !state.isBlackListed(p))

        val allNew = newInit ++ newTerm
        if (allNew.nonEmpty) LearningUtils.showNewRulesMsg(fpCounts, fnCounts, allNew, logger)
        state.updateRules(newInit ++ newTerm, "add", inps)
      }

      val newRules = newInit ++ newTerm

      // score the new rules and update their weights
      LearningUtils.scoreAndUpdateWeights(e, inferredState, newRules.toVector, inps, logger, newRules = true)

      /* Rules' expansion. */
      // We only need the top rules for expansion here.
      val init = state.initiationRules
      val term = state.terminationRules
      val expandedTheory = RuleExpansion.expandRules(init ++ term, inps, logger)

      state.updateRules(expandedTheory._1, "replace", inps)

      val pruningSpecs = new PruningSpecs(0.8, 2, 15000)

      val pruned = state.pruneRules(pruningSpecs, inps, logger)
    }
  }

  def wrapUp() = {
    logger.info(s"\nFinished the data")
    if (repeatFor > 0) {
      self ! new StartOver
    } else if (repeatFor == 0) {
      val theory = state.getAllRules(inps, "top")

      showStats(theory)

      if (trainingDataOptions != testingDataOptions) { // test set given, eval on that
        val testData = testingDataFunction(testingDataOptions)
        LearningUtils.evalOnTestSet(testData, theory, inps)
      }

      shutDown()

    } else { // Just to be on the safe side...
      throw new RuntimeException("This should never have happened (repeatFor is negative).")
    }
  }

  def shutDown() = {
    self ! PoisonPill
    context.parent ! new LocalLearnerFinished
  }

  def showStats(theory: List[Clause]) = {

      // used for printing out the avegare loss vector
      def avgLoss(in: Vector[Int]) = {
        in.foldLeft(0, 0, Vector.empty[Double]) { (x, y) =>
          val (count, prevSum, avgVector) = (x._1, x._2, x._3)
          val (newCount, newSum) = (count + 1, prevSum + y)
          (newCount, newSum, avgVector :+ newSum.toDouble / newCount)
        }
      }

    val endTime = System.nanoTime()
    val totalTime = (endTime - startTime) / 1000000000.0

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

  def batchInfoMsg(theoryForPrediction: List[Clause], tpCounts: Int, fpCounts: Int, fnCounts: Int) = {
    val batchMsg = underlineStars(s"*** BATCH $batchCount ***")
    val theoryMsg = underline(s"TPs: $tpCounts, FPs: $fpCounts, FNs: $fnCounts. Theory used for prediction:")
    val theory = {
      if (inps.weightLean) {
        theoryForPrediction.map(x => s"${x.tostring} | W: ${format(x.weight)} | Precision: ${format(x.precision)} | (TPs,FPs): (${x.tps}, ${x.fps}) ").mkString("\n")
      } else {
        theoryForPrediction.map(x => s"${x.tostring} | Precision: ${format(x.precision)} | (TPs,FPs): (${x.tps}, ${x.fps}) ").mkString("\n")

      }
    }
    if (theoryForPrediction.nonEmpty) s"\n$batchMsg\n$theoryMsg\n$theory" else s"*** BATCH $batchCount ***"
  }

  def updateStats(tpCounts: Int, fpCounts: Int, fnCounts: Int) = {
    state.totalTPs += tpCounts
    state.totalFPs += fpCounts
    state.totalFNs += fnCounts
  }

}
