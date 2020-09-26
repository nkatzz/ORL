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
import orl.learning.LearnUtils.setTypePredicates
import orl.learning.{Learner, PruningSpecs, TheoryRevision}
import orl.learning.Types.StartOver
import orl.learning.structure.{OldStructureLearningFunctions, RuleExpansion}
import orl.learning.woledmln.WoledMLNLearnerUtils
import orl.logic.{Clause, Literal}
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
class WoledASPLearner[T <: InputSource](
    inps: RunningOptions,
    trainingDataOptions: T,
    testingDataOptions: T,
    trainingDataFunction: T => Iterator[Example],
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
    /*topRules flatMap { rule =>
      if (rule.body.isEmpty) rule.refinements
      else List(rule)
    }*/
    topRules.filter(x => x.body.nonEmpty)
  }

  def process(_exmpl: Example) = {
    if (inps.ruleLearningStrategy == "hoeffding") {
      processDataHoeffding(_exmpl)
    } else if (inps.ruleLearningStrategy == "subsets") {
      processDataSubsets(_exmpl)
    } else if (inps.ruleLearningStrategy == "tr") {
      processTR(_exmpl)
    } else {
      logger.error("Specify a rule generation strategy")
      System.exit(-1)
    }
  }

  /**
    * Learns rules via non-monotonic theory revision techniques.
    *
    */
  def processTR(_exmpl: Example) = {

    var exmpl = _exmpl

    if (inps.withInertia) {
      exmpl = Example(exmpl.queryAtoms, exmpl.observations ++ this.inertiaAtoms.map(_.tostring), exmpl.time)
    }

    var currentTheory = state.getTopTheory() //.filter(_.body.nonEmpty)
    val initialTheory = currentTheory

    val inference = new ASPWeightedInference(currentTheory, exmpl, inps)
    val res = orl.utils.Utils.time{ inference.performInference() }
    val inferenceTime = res._2
    state.totalInferenceTime += inferenceTime
    state.inferenceTime = state.inferenceTime :+ inferenceTime
    var tpCounts = inference.TPs.size
    var fpCounts = inference.FPs.size
    var fnCounts = inference.FNs.size
    val (tps, fps, fns) = (tpCounts, fpCounts, fnCounts)

    val revise = fnCounts > 0 //|| inference.orphanFPs.nonEmpty

    inference.updateWeightsAndScore(batchCount)

    if (!withHandCrafted && revise) {
      /* The following code is for learning via generating BCs from mistakes */
      //val atomsFromFPMistakes = inference.orphanFPs.map(x => x.replaceAll("holdsAt", "terminatedAt"))
      /*val atomsFromFPMistakes = sampleSeeds(inference.FPs.toVector).
        map(x => x.replaceAll("holdsAt", "terminatedAt")).toSet

      val atomsFromFNMistakes = sampleSeeds(inference.FNs.toVector).
        map(x => x.replaceAll("holdsAt", "initiatedAt")).toSet

      val mistakes = atomsFromFPMistakes ++ atomsFromFNMistakes
      logger.info(s"Generating bottom clauses from ${mistakes.size} seed atoms...")
      val _topRules = generateNewRulesConservative(currentTheory, exmpl, inps, mistakes)
      val (topRules, bottomClauseTime) = (_topRules._1, _topRules._2)
      val bcs = topRules.flatMap(_.supportSet)

      val mh = inps.globals.MODEHS
      val mb = inps.globals.MODEBS
      bcs.foreach(_.setTypeAtoms(mh ++ mb))*/

      /* Use hand-crafted BCs instead of those generated from mistakes. */
      val mh = inps.globals.MODEHS
      val mb = inps.globals.MODEBS
      val handCraftedBCs = inps.globals.bottomClauses
      val (topRules, bcs) = handCraftedBCs.foldLeft(List.empty[Clause], List.empty[Clause]) { (x, bc) =>
        val topRule = Clause(head = bc.head)
        topRule.setTypeAtoms(mh ++ mb)
        topRule.supportSet = List(bc)
        (x._1 :+ topRule, x._2 :+ bc)
      }
      /**/

      val _currentTheory = currentTheory.map(x => (x, 0))
      val (inducedRules, refinedRules, unchangedRules) = TheoryRevision.revise(_currentTheory, bcs, exmpl, inps)
      inducedRules.foreach(_.setTypeAtoms(mh ++ mb))
      refinedRules.foreach(_.setTypeAtoms(mh ++ mb))
      val keep = state.getTopTheory().filter(x => unchangedRules.exists(_.## == x.##))

      state.updateRules(keep ++ refinedRules, "replace", inps)
      mergeAndUpdate(inducedRules.toList)

      //val newTheory = keep ++ inducedRules ++ refinedRules
      //state.updateRules(newTheory, "replace", inps)
    }

    var totalGroundings = 0
    var scoringTime = 0.0
    var secondInferenceTime = 0.0
    var inert = Set.empty[Literal]

    currentTheory = state.getTopTheory() //.filter(_.body.nonEmpty)

    val inferenceNew = new ASPWeightedInference(currentTheory, exmpl, inps)
    val res2 = orl.utils.Utils.time{ inferenceNew.performInference() }

    secondInferenceTime = res2._2
    state.inferenceTime = state.inferenceTime :+ secondInferenceTime
    val ((_totalGroundings, _inertiaAtoms), _scoringTime) = orl.utils.Utils.time(inferenceNew.updateWeightsAndScore(batchCount))
    scoringTime = _scoringTime

    this.inertiaAtoms = inert
    updateStats(tpCounts, fpCounts, fnCounts, totalGroundings) // Per batch error is updated here.
    logger.info(batchInfoMsg(initialTheory, Nil, tps, fps, fns, inferenceTime + secondInferenceTime, scoringTime))
  }

  /**
    * Uses Hoeffding bounds for new rule generation.
    *
    */
  def processDataHoeffding(_exmpl: Example) = {

    var totalGroundings = 0
    var scoringTime = 0.0
    var secondInferenceTime = 0.0
    var inert = Set.empty[Literal]

    var exmpl = _exmpl

    if (inps.withInertia) {
      exmpl = Example(exmpl.queryAtoms, exmpl.observations ++ this.inertiaAtoms.map(_.tostring), exmpl.time)
    }

    val rulesCompressed = getRulesForPrediction()

    /** Get the inferred state. */
    val inference = new ASPWeightedInference(rulesCompressed, exmpl, inps)
    val res = orl.utils.Utils.time{ inference.performInference() }

    val inferenceTime = res._2

    state.totalInferenceTime += inferenceTime
    state.inferenceTime = state.inferenceTime :+ inferenceTime

    var tpCounts = inference.TPs.size
    var fpCounts = inference.FPs.size
    var fnCounts = inference.FNs.size

    val (tps, fps, fns) = (tpCounts, fpCounts, fnCounts)

    var newRules = List.empty[Clause]

    /**
      * Update the weights of existing rules first, to avoid generating
      * redundant rules in response to mistakes generated from low-weight rules.
      */

    val (_totalGroundings, _inertiaAtoms) = inference.updateWeightsAndScore(batchCount)

    if (!withHandCrafted) {
      val init = state.initiationRules
      val term = state.terminationRules
      val expandedTheory = RuleExpansion.expandRules(init ++ term, inps, logger)
      state.updateRules(expandedTheory._1, "replace", inps)
    }

    val induceNewRules = fnCounts > 0 || fpCounts > 0

    /** Generate new rules from mistakes */
    if (!withHandCrafted) {
      if (induceNewRules) { //

        //val atomsFromFPMistakes = inference.orphanFPs.map(x => x.replaceAll("holdsAt", "terminatedAt"))
        val atomsFromFPMistakes = sampleSeeds(inference.FPs.toVector).map(x => x.replaceAll("holdsAt", "terminatedAt")).toSet
        val atomsFromFNMistakes = sampleSeeds(inference.FNs.toVector).map(x => x.replaceAll("holdsAt", "initiatedAt")).toSet

        //newRules = generateNewRules_1(rulesCompressed, exmpl, inps, atomsFromFPMistakes ++ atomsFromFNMistakes)
        //newRules = generateNewRulesXHAIL(rulesCompressed, exmpl, inps)
        //newRules = generateNewRules(rulesCompressed, exmpl, inps, atomsFromFPMistakes ++ atomsFromFNMistakes)
        //newRules = generateNewRules(rulesCompressed, exmpl, inps)

        //newRules = OldStructureLearningFunctions.generateNewRules(rulesCompressed, exmpl, inps)
        //mergeAndUpdate(newRules)
        //if (newRules.nonEmpty) state.updateRules(newRules, "add", inps)


        if (batchCount == 17) {
          val stop = "stop"
        }

        newRules = newRuleInduction(rulesCompressed, exmpl, inps)
        state.updateRules(newRules, "add", inps)
      }
    }

    if (newRules.nonEmpty) {
      // In this case we need to preform inference again, this time with the augmented theory.
      val newRulesSpecializationsOnly = newRules.flatMap(_.refinements)
      val allRules = rulesCompressed ++ newRules //++ newRulesSpecializationsOnly // newRules

      val inferenceNew = new ASPWeightedInference(allRules, exmpl, inps)
      val res = orl.utils.Utils.time{ inferenceNew.performInference() }
      secondInferenceTime = res._2

      state.inferenceTime = state.inferenceTime :+ secondInferenceTime

      val ((_totalGroundings, _inertiaAtoms), _scoringTime) = orl.utils.Utils.time(inferenceNew.updateWeightsAndScore(batchCount))

      totalGroundings = _totalGroundings
      inert = _inertiaAtoms.toSet
      scoringTime = _scoringTime
    } else {
      totalGroundings = _totalGroundings
      inert = _inertiaAtoms.toSet
    }

    this.inertiaAtoms = inert

    updateStats(tpCounts, fpCounts, fnCounts, totalGroundings) // Per batch error is updated here.

    logger.info(batchInfoMsg(getRulesForPrediction(), newRules, tps, fps, fns, inferenceTime + secondInferenceTime, scoringTime))

    if (!withHandCrafted) {
      if (inps.onlinePruning) {
        val pruningSpecs = new PruningSpecs(0.3, 2, 10000)
        state.lowQualityBasedPruning(pruningSpecs, inps, logger)
      }
    }

    /*val testData = testingDataFunction(testingDataOptions)
    evalOnTestSet(testData, state.getTopTheory().filter(_.body.nonEmpty), inps)*/
  }

  def sampleSeeds(seedAtoms: Vector[String]) = {
    if (maxNumberOfSeedAtoms.isInfinite || maxNumberOfSeedAtoms >= seedAtoms.length) seedAtoms
    else orl.utils.Utils.sampleN(maxNumberOfSeedAtoms.toInt, seedAtoms).map(_.toString)
  }

  /**
    * Uses a very naive strategy to create new rules, generating all k-subsets of a bottom rule
    * for 1 <= k <= Learner.maxClauseLength
    *
    */
  def processDataSubsets(_exmpl: Example) = {
    var exmpl = _exmpl
    var rulesCompressed = state.getTopTheory()
    val inference = new ASPWeightedInference(rulesCompressed, exmpl, inps)
    val res = orl.utils.Utils.time{ inference.performInference() }
    val inferenceTime = res._2
    state.totalInferenceTime += inferenceTime
    state.inferenceTime = state.inferenceTime :+ inferenceTime
    var tpCounts = inference.TPs.size
    var fpCounts = inference.FPs.size
    var fnCounts = inference.FNs.size
    val (tps, fps, fns) = (tpCounts, fpCounts, fnCounts)
    inference.updateWeightsAndScore(batchCount)
    val induceNewRules = fnCounts > 0 || inference.orphanFPs.nonEmpty // fpCounts > 0
    var newRules = Vector.empty[Clause]
    if (!withHandCrafted) {
      if (induceNewRules) {
        val atomsFromFPMistakes = inference.orphanFPs.map(x => x.replaceAll("holdsAt", "terminatedAt"))
        /*val atomsFromFPMistakes = sampleSeeds(inference.FPs.toVector).
          map(x => x.replaceAll("holdsAt", "terminatedAt")).toSet*/
        val atomsFromFNMistakes = sampleSeeds(inference.FNs.toVector).
          map(x => x.replaceAll("holdsAt", "initiatedAt")).toSet
        newRules = generateNewRulesBCSubsets(rulesCompressed, exmpl, inps, atomsFromFPMistakes ++ atomsFromFNMistakes)
      }
    }
    var totalGroundings = 0
    var scoringTime = 0.0
    var secondInferenceTime = 0.0
    var inert = Set.empty[Literal]
    if (newRules.nonEmpty) {
      // In this case we need to preform inference again, this time with the augmented theory.
      val allRules = state.getTopTheory()
      val inferenceNew = new ASPWeightedInference(allRules, exmpl, inps)
      val res = orl.utils.Utils.time{ inferenceNew.performInference() }
      secondInferenceTime = res._2
      state.inferenceTime = state.inferenceTime :+ secondInferenceTime
      val ((_totalGroundings, _inertiaAtoms), _scoringTime) = orl.utils.Utils.time(inferenceNew.updateWeightsAndScore(batchCount))
      scoringTime = _scoringTime
    }

    this.inertiaAtoms = inert
    updateStats(tpCounts, fpCounts, fnCounts, totalGroundings) // Per batch error is updated here.
    logger.info(batchInfoMsg(rulesCompressed, newRules.toList, tps, fps, fns, inferenceTime + secondInferenceTime, scoringTime))

  }

  /**
    * This class needs to implement the abstract method.
    */
  def generateNewRules(existingTheory: List[Clause], ex: Example, in: RunningOptions) = {
    generateNewRules(existingTheory, ex, inps, Set())
  }

  def generateNewRules(existingTheory: List[Clause], ex: Example, in: RunningOptions, mistakes: Set[String] = Set()) = {
    //generateNewRulesConservative(existingTheory, ex, inps, mistakes)
    generateNewRulesEager(existingTheory, ex, inps)
  }

  def generateNewRules_1(existingTheory: List[Clause], ex: Example, in: RunningOptions, mistakes: Set[String] = Set()) = {
    logger.info(s"Generating bottom clauses from ${mistakes.size} seed atoms...")
    val _topRules = generateNewRulesConservative(existingTheory, ex, inps, mistakes)
    val (topRules, bottomClauseTime) = (_topRules._1, _topRules._2)
    val bcs = topRules.flatMap(_.supportSet)
    val inference = new ASPWeightedInference(existingTheory, ex, in, bcs)
    val inf = orl.utils.Utils.time{ inference.performInference() }
    val (_, inferenceTime) = (inf._1, inf._2)
    logger.info("\n" + underline(s"BC generation time: $bottomClauseTime, New rule induction time: $inferenceTime"))
    inference.newClausesFromBCs
  }

  def generateNewRulesBCSubsets(existingTheory: List[Clause], ex: Example, in: RunningOptions, mistakes: Set[String] = Set()) = {
    logger.info(s"Generating bottom clauses from ${mistakes.size} seed atoms...")
    val _topRules = generateNewRulesConservative(existingTheory, ex, inps, mistakes)
    val (topRules, bottomClauseTime) = (_topRules._1, _topRules._2)
    val bcs = topRules.flatMap(_.supportSet)
    var allRules = existingTheory.toVector
    var newRules = Vector.empty[Clause]
    val newRulesTimed = orl.utils.Utils.time {
      bcs foreach { currentBottomClause =>
        val upto = if (maxClauseLength <= currentBottomClause.body.length) maxClauseLength else currentBottomClause.body.length
        for (k <- 1 to maxClauseLength) {
          currentBottomClause.body.toSet.subsets(k).foreach { bodySubset =>
            val newRule = Clause(head = currentBottomClause.head, body = bodySubset.toList)
            if (!existingTheory.exists(existingRule => existingRule.thetaSubsumes(newRule) && newRule.thetaSubsumes(existingRule))) {
              allRules = allRules :+ newRule
              newRules = newRules :+ newRule
            }
          }
        }
      }
    }
    logger.info("\n" + underline(s"BC generation time: $bottomClauseTime, New rule induction time: ${newRulesTimed._2}"))
    state.updateRules(allRules.toList, "replace", inps)
    newRules
  }

  /**
    * Generates new rules by abducing new rule heads from the data, using the
    * existing rules in the theory to avoid abducing redundant atoms.
    *
    * Mistakes are head atoms generated from actual prediction mistakes. If non-empty
    * there is no abduction and these atoms are used instead.
    */
  def generateNewRulesConservative(existingTheory: List[Clause], ex: Example, in: RunningOptions, mistakes: Set[String] = Set()) = {
    //val abd = new ASPWeightedInference(existingTheory, ex, inps)
    //abd.abduction()
    orl.utils.Utils.time{ OldStructureLearningFunctions.generateNewRules(existingTheory, ex, inps, mistakes) }
  }

  // This doesn't seem to work, it doesn't find the proper rules.
  def generateNewRulesXHAIL(existingTheory: List[Clause], ex: Example, in: RunningOptions, mistakes: Set[String] = Set()) = {
    val abd = new ASPWeightedInference(existingTheory, ex, inps)
    val bottomClauses = abd.abduction().map(x => x.supportSet.head)
    val inference = new ASPWeightedInference(existingTheory, ex, in, bottomClauses)
    inference.performInference()
    inference.newClausesFromBCs
  }

  def newRuleInduction(existingTheory: List[Clause], ex: Example, inps: RunningOptions) = {
    val bottomClauses = inps.globals.bottomClauses
    val inference = new ASPWeightedInference(existingTheory, ex, inps, bottomClauses)
    inference.performInference()
    val newRules = inference.newClausesFromBCs
    setTypePredicates(newRules, inps)
    newRules
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
    val growNewInit = OldStructureLearningFunctions.growNewRuleTest(topInit, ex, inps.globals, "initiatedAt")
    val growNewTerm = OldStructureLearningFunctions.growNewRuleTest(topTerm, ex, inps.globals, "terminatedAt")
    val newInit = if (growNewInit) OldStructureLearningFunctions.generateNewRulesOLED(topInit, ex, "initiatedAt", inps.globals) else Nil
    val newTerm = if (growNewTerm) OldStructureLearningFunctions.generateNewRulesOLED(topTerm, ex, "terminatedAt", inps.globals) else Nil
    //val newInit = OldStructureLearningFunctions.generateNewRulesOLED(topInit, ex, "initiatedAt", inps.globals) //if (growNewInit) generateNewRules(topInit, e, "initiatedAt", inps.globals) else Nil
    //val newTerm = OldStructureLearningFunctions.generateNewRulesOLED(topTerm, ex, "terminatedAt", inps.globals) //if (growNewTerm) generateNewRules(topTerm, e, "terminatedAt", inps.globals) else Nil
    newInit ++ newTerm
  }

  /**
    * For each generated new rule r, either merge its support with an existing r', such that r' subsumes r,
    * or add r' to the current top theory (update the state).
    */
  def mergeAndUpdate(newRules: List[Clause]) = {
    val topRules = state.getTopTheory().filter(_.body.nonEmpty)

    val actuallyNewRules = newRules.foldLeft(Vector.empty[Clause]) { (accum, newRule) =>
      var merge = false
      topRules foreach { topRule =>
        /*if (topRule.thetaSubsumes(newRule)) {
          // Just merge the support sets and generate refinements again.
          topRule.supportSet = topRule.supportSet ++ newRule.supportSet
          topRule.generateCandidateRefs(inps.specializationDepth, inps.globals.comparisonPredicates)
          logger.info(s"\nNew rule:\n  ${newRule.tostring}\n  with support:\n  " +
            s"${newRule.supportSet.map(_.tostring).mkString("\n")} merged with existing rule:\n  ${topRule.tostring}")
          merge = true
        }*/

        /**
          * Do this only for rules with a non-empty body, its too dangerous to merge ones
          * with empty body cause many interesting stuff may be missed
          */
        if (newRule.body.nonEmpty && newRule.thetaSubsumes(topRule)) {
          // Just merge the support sets and generate refinements again.
          val newBottomRules = newRule.supportSet.filter(topRule.thetaSubsumes)
          if (newBottomRules.nonEmpty) {
            topRule.supportSet = topRule.supportSet ++ newRule.supportSet
            if (inps.ruleLearningStrategy == "hoeffding") {
              topRule.generateCandidateRefs(inps.specializationDepth, inps.globals.comparisonPredicates)
            }
          }
          logger.info(s"\nNew rule:\n  ${newRule.tostring}\n  with support:\n  " +
            s"${newRule.supportSet.map(_.tostring).mkString("\n")} merged with existing rule:\n  ${topRule.tostring}")
          merge = true
        }

      }
      if (merge) accum else accum :+ newRule
    }

    actuallyNewRules foreach { newRule =>
      logger.info(s"\nCreated new rule:\n  ${newRule.tostring}\n  with support:\n  " +
        s"${newRule.supportSet.map(_.tostring).mkString("\n")}")
      state.updateRules(List(newRule), "add", inps)
    }
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

    val theory = theoryForPrediction.filter(_.weight != 0.0).map(x =>
      s"${format(x.weight)} ${x.tostring} (TPs: ${x.tps}, FPs: ${x.fps}, Actual groundings: ${x.actualGroundings})").mkString("\n")

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

    //val newRulesMessage = if (newRules.nonEmpty) newRulesMsg(fpCounts, fnCounts, newRules) else ""

    val message = {
      if (!showTheory) {
        s"\n$batchMsg\n$theoryMsg"
      } else {
        //if (newRulesMessage == "") inferenceMsg else inferenceMsg //s"$inferenceMsg\n$newRulesMessage"
        inferenceMsg
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
        //val theory = getRulesForPrediction()
        //val theory = LogicUtils.compressTheoryKeepMoreSpecific(state.getTopTheory().filter(x => x.body.nonEmpty).filter(x => x.actualGroundings > 2000))
        var theory = orl.logic.LogicUtils.compressTheory(state.getTopTheory().filter(x => x.body.nonEmpty)) //.filter(x => x.seenExmplsNum > 10000)

        showStats(theory)

        logger.info(s"Average inference time per batch: ${state.inferenceTime.sum / state.inferenceTime.length.toDouble}")
        logger.info(s"Total inference time: ${state.totalInferenceTime}")

        if (trainingDataOptions != testingDataOptions) { // test set given, eval on that
          theory = reIterateForWeightsOnly(theory)
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

  def reIterateForWeightsOnly(theory: List[Clause]) = {
    val trainData = trainingDataFunction(trainingDataOptions)
    theory.foreach(_.clearStatistics)
    theory.foreach(_.weight = 0.0)
    trainData foreach { exmpl =>
      val inference = new ASPWeightedInference(theory, exmpl, inps)
      inference.performInference()
      inference.updateWeightsAndScore(batchCount)
      batchCount += 1
    }
    theory //.filter(x => x.precision >= inps.pruneThreshold)
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
    println(s"TPs: $totalTPs, FPs: $totalFPs, FNs: $totalFNs, training: $trainingTime sec")
    //println(s"TPs: $totalTPs, FPs: $totalFPs, FNs: $totalFNs, TNs: $totalTNs, training: $trainingTime sec, testing: ${testTime._2} sec")
    orl.utils.Utils.dumpToFile(msg, s"${inps.entryPath}/crossval-results", "append")
  }

}
