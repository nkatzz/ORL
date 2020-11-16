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

package trail.learning.online

import trail.app.runutils.{Globals, RunningOptions}
import trail.learning.utils.LearnUtils.setTypePredicates
import trail.logic.Clause
import trail.app.utils.Utils.underline

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

  def finishedIterationInfo(logger: org.slf4j.Logger) = {
    val msg = s"\n${underline(s"Training performance: TPs: ${totalTPs}, FPs: ${totalFPs}, FNs: ${totalFNs}, total mistakes: ${totalFPs + totalFNs}")}."
    logger.info(msg)
    iterationInfo = iterationInfo :+ msg
    //clearStats()
  }

  def finalInfo(logger: org.slf4j.Logger) = {
    val msg1 = s"\n${underline(s"Training performance: TPs: ${totalTPs}, FPs: ${totalFPs}, FNs: ${totalFNs}, total mistakes: ${totalFPs + totalFNs}")}."
    iterationInfo = iterationInfo :+ msg1
    val msg = (1 to iterationInfo.length) zip iterationInfo map (x => s"\nIteration ${x._1}. ${x._2}")
    logger.info(msg.mkString("\n"))
  }

  def getTopTheory() = initiationRules ++ terminationRules

  def removeRule(rule: Clause) = {
    val withoutRule = getTopTheory().foldLeft(List.empty[Clause]) ((x, y) => if (y.## == rule.##) x else x :+ y)
    updateRules(withoutRule, "replace")
  }

  def addRule(rule: Clause) = updateRules(List(rule), "add")

  def clearEmptyBodied() = {
    val (emptyBodied, others) = getTopTheory().partition(_.body.isEmpty)
    emptyBodied.foreach { emptyBodiedRule =>
      if (others.exists(r => r.head.tostring == emptyBodiedRule.head.tostring)) removeRule(emptyBodiedRule)
    }
  }

  /**
    * The "what" variable here is either "all" or "top".
    * "all" returns all non-empty bodied rules along with their
    * specializations, while "top" returns the top non-empty bodied rules.
    */
  def getAllRules(what: String) = {
    val topRules = getTopTheory()
    topRules foreach { topRule => if (topRule.refinements.isEmpty) topRule.generateCandidateRefs(specializationDepth, comparisonPredicates) }
    what match {
      case "all" => topRules.flatMap { topRule => List(topRule) ++ topRule.refinements }
      case "top" => topRules.filter(x => includeRule(x, topRules)).map { topRule => if (topRule.body.isEmpty) topRule.refinements.minBy(-_.weight) else topRule }
    }
  }

  /**
    * Returns the best refinement currently available from each subsumption lattice
    */
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
      rule.refinements foreach { ref => ref.seenExmplsNum += newCount }
    }
  }

  /* The "action" variable here is either "add" or "replace" */
  def updateRules(newRules: List[Clause], action: String) = {
    if (inps.ruleLearningStrategy == "hoeffding") {
      newRules foreach { rule =>
        if (rule.refinements.isEmpty) {
          rule.generateCandidateRefs(specializationDepth, comparisonPredicates)
          setTypePredicates(rule.refinements, inps)
        }
      }
    }
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

}
