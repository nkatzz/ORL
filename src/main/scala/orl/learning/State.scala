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

import orl.app.runutils.{Globals, RunningOptions}
import orl.logic.{Clause, LogicUtils}
import orl.utils.Utils.underline

/**
  * Created by nkatz at 13/12/19
  */

class State(inps: RunningOptions) {

  private val comparisonPredicates = inps.globals.comparisonPredicates
  private val specializationDepth = inps.specializationDepth
  private val scoringFunction = inps.scoringFun

  var initiationRules: List[Clause] = List[Clause]()
  var terminationRules: List[Clause] = List[Clause]()
  var inferenceTime = Vector.empty[Double]
  var perBatchError: Vector[Int] = Vector.empty[Int]
  var runningRulesNumber: Vector[Int] = Vector.empty[Int]

  var totalInferenceTime = 0.0

  // This is the number of examples seen so far, the N for the Hoeffding test.
  var totalGroundings = 0

  var batchCounter = 0
  var totalTPs = 0
  var totalFPs = 0
  var totalFNs = 0
  var totalTNs = 0

  var blackList = List.empty[Clause]

  def clearStats() = {
    totalTPs = 0
    totalFPs = 0
    totalFNs = 0
    perBatchError = Vector.empty[Int]
  }

  private var iterationInfo: Seq[String] = Vector.empty[String]

  def isBlackListed(rule: Clause) = blackList.exists(badBC => rule.supportSet.head.thetaSubsumes(badBC))

  def finishedIterationInfo(logger: org.slf4j.Logger) = {
    val msg = s"\n${underline(s"Training performance: TPs: ${totalTPs}, FPs: ${totalFPs}, FNs: ${totalFNs}, total mistakes: ${totalFPs + totalFNs}")}."
    logger.info(msg)
    iterationInfo = iterationInfo :+ msg
    clearStats()
  }

  def finalInfo(logger: org.slf4j.Logger) = {
    val msg1 = s"\n${underline(s"Training performance: TPs: ${totalTPs}, FPs: ${totalFPs}, FNs: ${totalFNs}, total mistakes: ${totalFPs + totalFNs}")}."
    iterationInfo = iterationInfo :+ msg1
    val msg = (1 to iterationInfo.length) zip iterationInfo map (x => s"\nIteration ${x._1}. ${x._2}")
    logger.info(msg.mkString("\n"))
  }

  def getTopTheory() = initiationRules ++ terminationRules

  /* The "what" variable here is either "all" or "top".
  *  "all" returns all non-empty bodied rules along with their
  *  specializations, while "top" returns the top non-empty bodied rules.
  *  */
  def getAllRules(inps: RunningOptions, what: String) = {
    val topRules = getTopTheory()
    topRules foreach { topRule => if (topRule.refinements.isEmpty) topRule.generateCandidateRefs(specializationDepth, comparisonPredicates) }
    what match {
      case "all" => topRules.flatMap { topRule => List(topRule) ++ topRule.refinements }
      case "top" => topRules.filter(x => includeRule(x, topRules)).map { topRule => if (topRule.body.isEmpty) topRule.refinements.minBy(-_.weight) else topRule }
    }
  }

  // Returns the best refinement currently available from each subsumption lattice
  def getBestRules(gl: Globals, quality: String = "weight") = {
    val comparisonPredicates = gl.comparisonPredicates
    val spDepth = Globals.glvalues("specializationDepth")
    val topRules = getTopTheory()

    topRules foreach { r => if (r.refinements.isEmpty) r.generateCandidateRefs(spDepth.toInt, comparisonPredicates) }

    topRules.filter(x => includeRule(x, topRules)).map { topRule =>
      val sorted = (topRule.refinements :+ topRule).sortBy(x => if (quality == "weight") -x.weight else -x.precision)
      if (sorted.head.body.nonEmpty) sorted.head else sorted.tail.head
    }
  }

  /**
    * I added this to avoid the assertion exception from LoMRL (units + noUnits > 0).
    * It seemed that the error occurred because two contradicting initiation rules with only one condition in their bodies,
    * "visible" and "not visible" respectively, and the same weight were added to the theory that was used for prediction.
    * This created something like a hard constraint that was responsible for the problem (I think?). The two rules were just two random
    * specializations of an immature rule with an empty body. With this method we exclude rules like that to be added
    * unless no other more mature rule (non-empty bodied rule) with the same head exists in the top theory.
    *
    */
  def includeRule(rule: Clause, theory: List[Clause]) = {
    if (rule.body.isEmpty) {
      if (theory.exists(p => p.body.nonEmpty && p.head.predSymbol == rule.head.predSymbol)) false else true
    } else true
  }

  def updateRulesGroundingsCounts(newCount: Int) = {
    val rules = getTopTheory()
    rules foreach { rule =>
      rule.seenExmplsNum += newCount
      if (rule.supportSet.nonEmpty) rule.supportSet.head.seenExmplsNum += newCount
      rule.refinements foreach { ref =>
        ref.seenExmplsNum += newCount
      }
    }
  }

  /* The "action" variable here is either "add" or "replace" */
  def updateRules(newRules: List[Clause], action: String, inps: RunningOptions) = {
    newRules foreach { rule => if (rule.refinements.isEmpty) rule.generateCandidateRefs(specializationDepth, comparisonPredicates) }
    val (init, term) = newRules.partition(x => x.head.predSymbol == "initiatedAt")
    action match {
      case "add" =>
        initiationRules = initiationRules ++ init
        terminationRules = terminationRules ++ term
      case "replace" =>
        initiationRules = init
        terminationRules = term
    }
  }

  def lowQualityBasedPruning(specs: PruningSpecs, inps: RunningOptions, logger: org.slf4j.Logger) = {

      def showPruned(c: Clause) = {
        // Note that the number of examples a rule has been evaluated on is the number of examples
        // it fires on, NOT the number of examples seen so far in the stream. Therefore, we're pruning
        // if the rule is of low quality after TPs+FPs examples.
        val msg =
          s"\n===========================================================\n" +
            s"\nPruned clause (Precision: ${c.precision} | TPs: ${c.tps} FPs: ${c.fps} FNs: ${c.fns} | Weight: ${c.weight})\n\n${c.tostring}\n\n" +
            s"After ${c.seenExmplsNum} examples.\nThe corresponding bottom rule is:\n${c.supportSet.head.tostring}" +
            s"\n===========================================================\n"
        logger.info(msg)
      }

    val weightLearning = inps.weightLean
    val acceptablePrecision = specs.minPrecision
    val maxBodyLength = specs.bodyLength
    val oldness = specs.oldness

    var pruned = false

      def removeBadRules(rules: List[Clause]) = {

          def remove(rule: Clause): Unit = {
            showPruned(rule)
            blackList = blackList :+ rule.supportSet.head
            pruned = true
          }

        rules.foldLeft(List.empty[Clause]) { (accum, rule) =>
          if (rule.body.length >= maxBodyLength) {
            //if (rule.precision <= acceptablePrecision && !rule.refinements.exists(x => x.precision > acceptablePrecision)) {
            if (rule.tps <= 20) {
              remove(rule)
              accum
            } else {
              accum :+ rule
            }
          } else {
            /*if (rule.seenExmplsNum >= oldness) {
              if (rule.precision <= acceptablePrecision) {
                remove(rule)
                accum
              } else {
                accum :+ rule
              }
            } else {
              accum :+ rule
            }*/
            accum :+ rule
          }

        }
      }

    initiationRules = removeBadRules(initiationRules)
    terminationRules = removeBadRules(terminationRules)

    pruned
  }

  /**
    * Removes a rule r1 that theta-subsumes a rule r2, if infoGain(r1,r2) > 0.
    * Only applied to rules with body >= 1, to avoid pruning very young rules.
    */
  def subsumptionBasedPruning() = {

      def showSubsumptionBasedPruned(c: Clause): Unit = {
        val msg =
          s"\n===========================================================\n" +
            s"\nPruned clause (Precision: ${c.precision} | TPs: ${c.tps} FPs: ${c.fps} FNs: ${c.fns} | Weight: ${c.weight})\n\n${c.tostring}\n\n" +
            s"because it submsumed another, of higher quality." +
            s"\nAfter ${c.seenExmplsNum} examples.\nThe corresponding bottom rule is:\n${c.supportSet.head.tostring}" +
            s"\n===========================================================\n"
      }

      def prune(rule: Clause): Unit = {

        val pool = getTopTheory().filter(r => r.head.predSymbol == rule.head.predSymbol)

        val (others, thiss) = pool.partition(_ != rule)

        if (others.nonEmpty && thiss.isEmpty) {
          val msg = s"Subsumption-based prunning: " +
            s"Cannot find rule\n${rule.tostring} in pool of\n${others.map(_.tostring).mkString("\n")}."
          throw new RuntimeException(msg)
        }

        val subsumees = others.foldLeft(List.empty[Clause]) { (x, otherRule) =>
          if (rule.thetaSubsumes(otherRule)) x :+ otherRule else x
        }

        val shouldPrune = subsumees.exists(rulesChild => LogicUtils.informationGain(rulesChild, rule) > 0)

        if (shouldPrune) {
          showSubsumptionBasedPruned(rule)
          if (rule.head.predSymbol == "initiatedAt") initiationRules = others else terminationRules = others
        }
      }

    getTopTheory().filter(_.body.nonEmpty).foreach(prune)

  }

}
