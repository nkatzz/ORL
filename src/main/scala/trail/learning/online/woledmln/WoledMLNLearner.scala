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

package trail.learning.online.woledmln

import java.text.DecimalFormat

import trail.app.runutils.InputHandling.InputSource
import trail.app.runutils.{Example, RunningOptions}
import trail.learning.online.OnlineLearner
import trail.learning.online.Types.StartOver
import trail.learning.structure.{OldStructureLearningFunctions, RuleExpansion}
import trail.logic.{Clause, Literal, LogicUtils}

/**
  * Created by nkatz at 12/2/20
  *
  * Implements the abstract methods of the parent abstract class, and uses LoMRF for MAP inference.
  * This is an implementation of the original version of the WOLED algorithm.
  *
  */

object WoledMLNLearner {
  /*var averageGroundingTime = 0.0
  var averagePredCompletionTime = 0.0
  var averageSolvingTime = 0.0*/

  var averageGroundingTime = Vector.empty[Double]
  var averagePredCompletionTime = Vector.empty[Double]
  var averageSolvingTime = Vector.empty[Double]
}

class WoledMLNLearner[T <: InputSource](inps: RunningOptions, trainingDataOptions: T,
    testingDataOptions: T, trainingDataFunction: T => Iterator[Example],
    testingDataFunction: T => Iterator[Example]) extends OnlineLearner(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction) {

  def process(exmpl: Example): Unit = {

    var e = Example()
    var rules = List.empty[Clause]
    var inferredState = Map.empty[String, Boolean]
    var rulesCompressed = List.empty[Clause]
    var inferenceTime = 0.0
    var scoringTime = 0.0
    var newRules = List.empty[Clause]

    e = WoledMLNLearnerUtils.dataToMLNFormat(exmpl, inps)

    //rules = state.getAllRules(inps, "all").filter(x => x.body.nonEmpty)
    rules = state.getAllRules("top")

    rulesCompressed = LogicUtils.compressTheory(rules)
    //rulesCompressed = LogicUtils.compressTheoryKeepMoreSpecific(rules)

    println("MAP Inference...")

    val map = trail.app.utils.Utils.time(MAPInference.solve(rulesCompressed, e, this.inertiaAtoms, inps, batchCount = batchCount))
    val mapInfResult = map._1
    inferenceTime = map._2

    inferredState = mapInfResult._1

    println("Finished MAP")

    //val m = new WeightedSATInference(rulesCompressed, e, inps)
    //val res = trail.app.utils.Utils.time(m.solve)
    //logger.info("\n" + underline(s"ASPWeightedSat (${res._2} sec): TPs: ${m.TPs}, FPs: ${m.FPs}, FNs: ${m.FNs}"))

    // Doing this in parallel is trivial (to speed things up in case of many rules/large batches).
    // Simply split the rules to multiple workers, the grounding/counting tasks executed are completely rule-independent.
    //println("      Scoring...")

    val scoring = trail.app.utils.Utils.time {
      WoledMLNLearnerUtils.
        scoreAndUpdateWeights(e, inferredState,
                              state.getAllRules("all").toVector, inps, logger, batchCount = batchCount)
    }

    val (tpCounts, fpCounts, fnCounts, totalGroundings, _inertiaAtoms) = scoring._1
    scoringTime = scoring._2

    inertiaAtoms = _inertiaAtoms.toSet

    updateStats(tpCounts, fpCounts, fnCounts, totalGroundings)

    this.inertiaAtoms = inertiaAtoms
    this.inertiaAtoms = Set.empty[Literal] // Use this to difuse inertia

    logger.info(batchInfoMsg(rulesCompressed, tpCounts, fpCounts, fnCounts, inferenceTime, scoringTime))

    //logger.info(s"\n${state.perBatchError}")
    //logger.info(s"\nFPs: $fpCounts, FNs: $fnCounts")

    if (!withHandCrafted) {
      if (fpCounts > 0 || fnCounts > 0) {
        newRules = generateNewRulesConservative(rulesCompressed, e, inps) //.filter(p => !state.isBlackListed(p))
        //newRules = generateNewRulesEager(rulesCompressed, e, inps)//.filter(p => !state.isBlackListed(p))
        if (newRules.nonEmpty) {
          WoledMLNLearnerUtils.showNewRulesMsg(fpCounts, fnCounts, newRules, logger)
          state.updateRules(newRules, "add")
        }
      }

      // score the new rules and update their weights
      val newRulesWithRefs = newRules.flatMap(x => x.refinements :+ x).toVector
      WoledMLNLearnerUtils.scoreAndUpdateWeights(e, inferredState, newRulesWithRefs, inps, logger, batchCount = batchCount, newRules = true)

      /* Rules' expansion. */
      // We only need the top rules for expansion here.
      val init = state.initiationRules
      val term = state.terminationRules
      val expandedTheory = RuleExpansion.expandRules(init ++ term, inps, logger)

      state.updateRules(expandedTheory._1, "replace")

      //val pruningSpecs = new PruningSpecs(0.8, 2, 100)
      //val pruned = state.pruneRules(pruningSpecs, inps, logger)
    }
  }

  def generateNewRules(existingTheory: List[Clause], ex: Example, in: RunningOptions) = {
    generateNewRulesConservative(existingTheory, ex, inps)
    //generateNewRulesEager(existingTheory, ex, inps)
  }

  /**
    * Generates new rules by (minimally) abducing new rule heads from the data, using the
    * existing rules in the theory to avoid abducing redundant atoms.
    */
  def generateNewRulesConservative(existingTheory: List[Clause], ex: Example, in: RunningOptions) = {
    OldStructureLearningFunctions.generateNewRules(existingTheory, ex, inps)
  }

  /**
    * Generates new rules directly from the commited mistakes.
    * This method does not actually use the existing theory.
    */
  def generateNewRulesEager(existingTheory: List[Clause], ex: Example, in: RunningOptions) = {
    val topInit = state.initiationRules.filter(_.body.nonEmpty)
    val topTerm = state.terminationRules.filter(_.body.nonEmpty)
    //val growNewInit = OldStructureLearningFunctions.growNewRuleTest(topInit, ex, inps.globals, "initiatedAt")
    //val growNewTerm = OldStructureLearningFunctions.growNewRuleTest(topTerm, ex, inps.globals, "terminatedAt")
    //newInit = if (growNewInit) OldStructureLearningFunctions.generateNewRulesOLED(topInit, e, "initiatedAt", inps.globals) else Nil
    //newTerm = if (growNewTerm) OldStructureLearningFunctions.generateNewRulesOLED(topTerm, e, "terminatedAt", inps.globals) else Nil
    val newInit = OldStructureLearningFunctions.generateNewRulesOLED(topInit, ex, "initiatedAt", inps.globals) //if (growNewInit) generateNewRules(topInit, e, "initiatedAt", inps.globals) else Nil
    val newTerm = OldStructureLearningFunctions.generateNewRulesOLED(topTerm, ex, "terminatedAt", inps.globals) //if (growNewTerm) generateNewRules(topTerm, e, "terminatedAt", inps.globals) else Nil
    newInit ++ newTerm
  }

  /**
    * Prints statistics & evaluates on test set (if one provided)
    */
  def wrapUp() = {
    logger.info(s"\nFinished the data")
    if (repeatFor > 0) {
      self ! new StartOver
    } else if (repeatFor == 0) {
      val theory = state.getAllRules("top")

      showStats(theory)

        def format(x: Double) = {
          val defaultNumFormat = new DecimalFormat("0.######")
          defaultNumFormat.format(x)
        }

      /*logger.info(s"\nAverage predicate completion time: " +
        s"${format(WoledMLNLearner.averagePredCompletionTime)}\nAverage grounding time: " +
        s"${format(WoledMLNLearner.averageGroundingTime)}\nAverage solving time: ${format(WoledMLNLearner.averageSolvingTime)}")*/

      logger.info(s"\nAverage predicate completion time: " +
        s"${WoledMLNLearner.averagePredCompletionTime.sum / WoledMLNLearner.averagePredCompletionTime.length.toDouble}\nAverage grounding time: " +
        s"${WoledMLNLearner.averageGroundingTime.sum / WoledMLNLearner.averageGroundingTime.length.toDouble}\nAverage solving time: " +
        s"${WoledMLNLearner.averageSolvingTime.sum / WoledMLNLearner.averageGroundingTime.length.toDouble}")

      if (trainingDataOptions != testingDataOptions) { // test set given, eval on that
        val testData = testingDataFunction(testingDataOptions)
        WoledMLNLearnerUtils.evalOnTestSet(testData, theory, inps)
      }

      shutDown()

    } else { // Just to be on the safe side...
      throw new RuntimeException("This should never have happened (repeatFor is negative).")
    }
  }

}
