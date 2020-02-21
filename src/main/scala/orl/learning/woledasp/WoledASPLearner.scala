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

package orl.learning.woledasp

import java.text.DecimalFormat

import orl.app.runutils.RunningOptions
import orl.datahandling.Example
import orl.datahandling.InputHandling.InputSource
import orl.learning.{Learner, PruningSpecs}
import orl.learning.Types.StartOver
import orl.learning.structure.{OldStructureLearningFunctions, RuleExpansion}
import orl.learning.woledmln.WoledMLNLearnerUtils
import orl.logic.Clause
import orl.utils.Utils.{underline, underlineStars}

/**
  * Created by nkatz at 12/2/20
  *
  * Implements the abstract methods of the parent abstract class and uses Clingo for MAP-like,
  * WeightedSAT inference. The inferred state is used to learn weights for the rules.
  *
  * This is a version of the WOLED algorithm that relies exclusively on ASP to learn structure
  * and weights for rules.
  */
class WoledASPLearner[T <: InputSource](inps: RunningOptions, trainingDataOptions: T,
    testingDataOptions: T, trainingDataFunction: T => Iterator[Example],
    testingDataFunction: T => Iterator[Example])
  extends Learner(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction) {

  val showTheory: Boolean = true // show the theory used for inference at each step.

  /**
    * To make predictions at each batch we select rules (top theory) as follows:
    * If a a rule is too young and its body is empty, we add to the top theory
    * all its level-1 specializations (those with one literal in their body).
    * Otherwise, if a top rule that has resulted from a Hoeffding test has at least one
    * literal to its body we use that rule.
    *
    */
  def getRulesForPrediction() = {
    val topRules = state.getTopTheory()
    topRules flatMap { rule =>
      // If the body is empty then use all specializations for prediction.
      // These are 'level-1' specializations (a single literal in the body).
      if (rule.body.isEmpty) rule.refinements //:+ rule
      else List(rule)
    }
  }

  val inferenceMetaProgram = "SAT"

  def process(_exmpl: Example) = {

    /**
      * TODO
      *
      * I'm doing this here (though we're not using LoMRF) to avoid the
      * holdsAt(visible) predicate in the input, that messes everything up.
      * I need to fix this whole thing with counting and problems with holdsAt/2, target predicates etc.
      * This is related to the meta-rules used for scoring the actual rules.
      */
    val exmpl = WoledMLNLearnerUtils.dataToMLNFormat(_exmpl, inps)

    //val rules = state.getAllRules(inps, "all").filter(x => x.body.nonEmpty)
    //val rules = state.getAllRules(inps, "top")

    val rulesCompressed = getRulesForPrediction()

    if (batchCount == 92) {
      val stop = "stop"
    }

    //val rulesCompressed = LogicUtils.compressTheory(rules)
    //val rulesCompressed = LogicUtils.compressTheoryKeepMoreSpecific(rules)

    /** Get the inferred state. */
    val inference = new ASPWeightedInference(rulesCompressed, exmpl, inps)
    val res = orl.utils.Utils.time{ inference.performInference(inferenceMetaProgram) }

    val inferenceTime = res._2
    val tpCounts = inference.TPs.size
    val fpCounts = inference.FPs.size
    val fnCounts = inference.FNs.size

    var newRules = List.empty[Clause]

    /** Generate new rules from mistakes */
    if (!withHandCrafted) {
      if (fpCounts > 0 || fnCounts > 0) {

        val atomsFromFPMistakes = inference.FPs.map(x => x.replaceAll("holdsAt", "terminatedAt"))
        val atomsFromFNMistakes = inference.FPs.map(x => x.replaceAll("holdsAt", "initiatedAt"))

        newRules = generateNewRules(rulesCompressed, exmpl, inps, atomsFromFPMistakes ++ atomsFromFNMistakes )

        //newRules = generateNewRules(rulesCompressed, exmpl, inps)

        if (newRules.nonEmpty) state.updateRules(newRules, "add", inps)
      }
    }

    var totalGroundings = 0
    var scoringTime = 0.0
    var secondInferenceTime = 0.0

    /** Update weights and coverage counts for existing and new rules. */

    if (newRules.isEmpty) {
      // In this case the theory is the same, we only need to count groundings in the inferred state.
      val ((_totalGroundings, _inertiaAtoms), _scoringTime) = orl.utils.Utils.time(inference.updateWeightsAndScore(batchCount))
      totalGroundings = _totalGroundings
      this.inertiaAtoms = _inertiaAtoms.toSet
      scoringTime = _scoringTime
    } else {

      // In this case we need to preform inference again, this time with the augmented theory.
      val newRulesSpecializationsOnly = newRules.flatMap(_.refinements)
      val allRules = rulesCompressed ++ newRulesSpecializationsOnly // newRules

      val inferenceNew = new ASPWeightedInference(allRules, exmpl, inps)
      val res = orl.utils.Utils.time{ inferenceNew.performInference(inferenceMetaProgram) }
      secondInferenceTime = res._2

      val ((_totalGroundings, _inertiaAtoms), _scoringTime) =
        orl.utils.Utils.time(inferenceNew.updateWeightsAndScore(batchCount))

      totalGroundings = _totalGroundings
      this.inertiaAtoms = _inertiaAtoms.toSet
      scoringTime = _scoringTime
    }

    updateStats(tpCounts, fpCounts, fnCounts, totalGroundings) // Per batch error is updated here.

    logger.info(batchInfoMsg(rulesCompressed, newRules, tpCounts, fpCounts, fnCounts, inferenceTime + secondInferenceTime, scoringTime))

    if (!withHandCrafted) {
      /** Attempt to expand existing rules. */
      val init = state.initiationRules
      val term = state.terminationRules
      val expandedTheory = RuleExpansion.expandRules(init ++ term, inps, logger)

      state.updateRules(expandedTheory._1, "replace", inps)

      val pruningSpecs = new PruningSpecs(0.3, 2, 15000)
      state.lowQualityBasedPruning(pruningSpecs, inps, logger)
      //state.subsumptionBasedPruning() // This has never worked...
    }
  }

  /**
    * This class needs to implement the abstract method.
    * */
  def generateNewRules(existingTheory: List[Clause], ex: Example, in: RunningOptions) = {
    generateNewRules(existingTheory, ex, inps, Set())
  }

  def generateNewRules(existingTheory: List[Clause], ex: Example, in: RunningOptions, mistakes: Set[String] = Set()) = {
    generateNewRulesConservative(existingTheory, ex, inps, mistakes)
    //generateNewRulesEager(existingTheory, ex, inps)
  }

  /**
    * Generates new rules by abducing new rule heads from the data, using the
    * existing rules in the theory to avoid abducing redundant atoms.
    *
    * Mistakes are head atoms generated from actual prediction mistakes. If non-empty
    * there is no abduction and these atoms are used instead.
    */
  def generateNewRulesConservative(existingTheory: List[Clause], ex: Example, in: RunningOptions, mistakes: Set[String] = Set()) = {
    OldStructureLearningFunctions.generateNewRules(existingTheory, ex, inps, mistakes)
    //val abd = new ASPWeightedInference(existingTheory, ex, inps)
    //abd.abduction()
  }

  /**
    * Generates new rules directly from the committed mistakes.
    * This method does not actually use the existing theory.
    *
    * Mistakes are head atoms generated from actual prediction mistakes. If non-empty
    * * there is no abduction and these atoms are used instead.
    */
  def generateNewRulesEager(existingTheory: List[Clause], ex: Example, in: RunningOptions) = {
    val topInit = state.initiationRules.filter(_.body.nonEmpty)
    val topTerm = state.terminationRules.filter(_.body.nonEmpty)
    //val growNewInit = OldStructureLearningFunctions.growNewRuleTest(topInit, ex, inps.globals, "initiatedAt")
    //val growNewTerm = OldStructureLearningFunctions.growNewRuleTest(topTerm, ex, inps.globals, "terminatedAt")
    //val newInit = if (growNewInit) OldStructureLearningFunctions.generateNewRulesOLED(topInit, ex, "initiatedAt", inps.globals) else Nil
    //val newTerm = if (growNewTerm) OldStructureLearningFunctions.generateNewRulesOLED(topTerm, ex, "terminatedAt", inps.globals) else Nil
    val newInit = OldStructureLearningFunctions.generateNewRulesOLED(topInit, ex, "initiatedAt", inps.globals) //if (growNewInit) generateNewRules(topInit, e, "initiatedAt", inps.globals) else Nil
    val newTerm = OldStructureLearningFunctions.generateNewRulesOLED(topTerm, ex, "terminatedAt", inps.globals) //if (growNewTerm) generateNewRules(topTerm, e, "terminatedAt", inps.globals) else Nil
    newInit ++ newTerm
  }

  def batchInfoMsg(theoryForPrediction: List[Clause], newRules: List[Clause],
      tpCounts: Int, fpCounts: Int, fnCounts: Int, inferenceTime: Double, scoringTime: Double) = {

      def format(x: Double) = {
        val defaultNumFormat = new DecimalFormat("0.######")
        //val defaultNumFormat = new DecimalFormat("0.###############")

        defaultNumFormat.format(x)
      }

    val batchMsg = underlineStars(s"*** BATCH $batchCount ***")
    val theoryMsg = {
      if (showTheory) {
        underline(s"TPs: $tpCounts, FPs: $fpCounts, FNs: $fnCounts. Inference time: " +
          s"$inferenceTime, scoring time: $scoringTime. Theory used for prediction:")
      } else underline(s"TPs: $tpCounts, FPs: $fpCounts, FNs: $fnCounts. Inference time: $inferenceTime, scoring time: $scoringTime.")
    }

    /*val theory = {
      if (inps.weightLean) {
        theoryForPrediction.map(x => s"${x.tostring} | W: ${format(x.weight)} | Precision: ${format(x.precision)} | (TPs,FPs): (${x.tps}, ${x.fps}) ").mkString("\n")
      } else {
        theoryForPrediction.map(x => s"${x.tostring} | Precision: ${format(x.precision)} | (TPs,FPs): (${x.tps}, ${x.fps}) ").mkString("\n")
      }
    }*/

    val theory = theoryForPrediction.filter(_.weight != 0.0).map(x => s"${format(x.weight)} ${x.tostring} (TPs: ${x.tps}, FPs: ${x.fps})").mkString("\n")

    val inferenceMsg = {
      if (theoryForPrediction.nonEmpty) {
        s"\n$batchMsg\n$theoryMsg\n$theory"
      } else {
        if (newRules.nonEmpty) {
          s"\n$batchMsg"
        } else {
          s"*** BATCH $batchCount ***"
        }
      }
    }

    val newRulesMessage = if (newRules.nonEmpty) newRulesMsg(fpCounts, fnCounts, newRules) else ""

    val message = {
      if (!showTheory) {
        s"\n$batchMsg\n$theoryMsg"
      } else {
        if (newRulesMessage == "") inferenceMsg else s"$inferenceMsg\n$newRulesMessage"
      }
    }

    message
  }

  /* Used for debugging */
  /*def batchInfoMsg(theoryForPrediction: List[Clause], newRules: List[Clause],
                   tpCounts: Int, fpCounts: Int, fnCounts: Int, inferenceTime: Double, scoringTime: Double) = {

    val batchMsg = underlineStars(s"*** BATCH $batchCount ***")

    val allRules = state.initiationRules ++ state.terminationRules

    val (topRules,refs) = allRules.partition(_.isTopRule)

    def showRule(x: Clause) = s"${x.tostring} | W: ${format(x.weight)} | Precision: ${format(x.precision)} | (TPs,FPs): (${x.tps}, ${x.fps}) | InfoGain: ${x.foilGain("precision")}"

    val rulesMsg = allRules.map { rule =>
      val one = underline(s"Top rule:\n")+showRule(rule)
      val two = s"${underline("Specializations:")}\n${rule.refinements.map(showRule).mkString("\n")}"
      s"\n$one\n$two"
    }

    val (regural, emptyBodied) = allRules.partition(_.body.nonEmpty)

    val m = regural.map(x => s"${x.tostring}, ${x.tps}, ${x.fps}, ${x.weight}").mkString("\n")

    val m1 = emptyBodied.map(x => (List(x)++x.refinements).map(x => s"${x.tostring}, ${x.tps}, ${x.fps}, ${x.weight}").mkString("\n")).mkString("\n")

    //s"\n$batchMsg\n$rulesMsg"
    s"\n$batchMsg\n$m\n\n$m1"
  }*/

  def newRulesMsg(fps: Int, fns: Int, newRules: List[Clause]) = {

      def showBCs(bottomClause: Clause) = {
        bottomClause.toStrList match {
          case Nil => throw new RuntimeException("Cannot generate a Clause object for the empty clause")
          case h :: ts =>
            ts.length match {
              case 0 => s"$h."
              case 1 => s"$h :- ${ts.head}."
              case _ => s"""$h :- ${(for (x <- ts) yield if (ts.indexOf(x) == ts.length - 1) x + "." else x + ",").mkString(" ")}"""
            }
        }
      }

    val msg = s"Mistakes: FPs: $fps, FNs: $fns. Growing new rules from the following BCs:"
    val umsg = "\n" + underline(msg)
    val bcs = newRules.map(x => showBCs(x.supportSet.head)).mkString("\n")
    val message = s"$umsg\n$bcs"
    message
  }

  /**
    * Prints statistics & evaluates on test set (if one provided)
    */
  def wrapUp() = {

      def iterationWrapUp() = {
        val theory = getRulesForPrediction()
        showStats(theory)
        if (trainingDataOptions != testingDataOptions) { // test set given, eval on that
          val testData = testingDataFunction(testingDataOptions)
          evalOnTestSet(testData, theory, inps)
        }
      }
    logger.info(s"\nFinished the data")
    if (repeatFor > 0) {
      iterationWrapUp()
      self ! new StartOver
    } else if (repeatFor == 0) {
      iterationWrapUp()
      shutDown()
    } else { // Just to be on the safe side...
      throw new RuntimeException("This should never have happened (repeatFor is negative).")
    }
  }

  def evalOnTestSet(testData: Iterator[Example], rules: List[Clause], inps: RunningOptions) = {

    logger.info("\nEvaluating on the test set...")

    var totalTPs = 0
    var totalFPs = 0
    var totalFNs = 0

    testData foreach { _batch =>

      /**
        * TODO
        *
        * I'm doing this here (though we're not using LoMRF) to avoid the
        * holdsAt(visible) predicate in the input, that messes everything up.
        * I need to fix this whole thing with counting and problems with holdsAt/2, target predicates etc.
        * This is related to the meta-rules used for scoring the actual rules.
        */
      val batch = WoledMLNLearnerUtils.dataToMLNFormat(_batch, inps)

      val inference = new ASPWeightedInference(rules, batch, inps)
      inference.performInference()
      totalTPs += inference.TPs.size
      totalFPs += inference.FPs.size
      totalFNs += inference.FNs.size
    }

    val precision = totalTPs.toDouble / (totalTPs + totalFPs)
    val recall = totalTPs.toDouble / (totalTPs + totalFNs)
    val f1 = 2 * (precision * recall) / (precision + recall)
    val theory = rules.map(x => s"${format(x.weight)} ${x.tostring}").mkString("\n")
    val msg = s"\nTheory:\n$theory\nF1-score on test set: $f1\nTPs: $totalTPs, FPs: $totalFPs, FNs: $totalFNs"
    logger.info(msg)
    orl.utils.Utils.dumpToFile(msg, "/home/nkatz/Desktop/kernel", "append")
  }

}
