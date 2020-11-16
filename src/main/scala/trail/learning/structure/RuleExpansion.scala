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

package trail.learning.structure

import java.text.DecimalFormat

import trail.app.runutils.RunningOptions
import trail.learning.online.Types.Theory
import trail.learning.utils.LearnUtils
import trail.logic.Clause

import scala.math.sqrt

/**
  * Created by nkatz at 14/12/19
  */

object RuleExpansion {

  def expandRules(topTheory: Theory, inps: RunningOptions, logger: org.slf4j.Logger): (Theory, Boolean) = {
    var expanded = false

    val scoreFun = inps.scoringFun
    val comparisonPredicates = inps.globals.comparisonPredicates
    val spDepth = inps.specializationDepth

    val out = topTheory flatMap { parentRule =>
      val (couldExpand, epsilon, observedDiff, best, secondBest) = rightWay(parentRule, inps)

      //println(best.score,best.tps, best.fps, best.fns, "  ", secondBest.score, secondBest.tps, secondBest.fps, secondBest.fns)

      couldExpand match {
        case true =>
          // This is the extra test that I added at Feedzai
          val extraTest =
            if (inps.scoringFun != "foilgain") {
              if (secondBest != parentRule) (best.score(scoreFun) > parentRule.score(scoreFun)) && (best.score(scoreFun) - parentRule.score(scoreFun) > epsilon)
              else best.score(scoreFun) > parentRule.score(scoreFun)
            } else {
              // We want the refinement to have some gain, do not expand for no gain.
              best.score(scoreFun) > inps.infoGainAtLeast
            }

          extraTest match { //&& (1.0/best.body.size+1 > 1.0/parentRule.body.size+1) match {
            case true =>
              val refinedRule = best
              logger.info(showInfo(parentRule, best, secondBest, epsilon, observedDiff, parentRule.seenExmplsNum, inps))
              refinedRule.seenExmplsNum = 0 // zero the counter
              refinedRule.isTopRule = true
              refinedRule.supportSet = parentRule.supportSet.filter(bottomRule => refinedRule.thetaSubsumes(bottomRule))
              refinedRule.generateCandidateRefs(spDepth, comparisonPredicates)
              LearnUtils.setTypePredicates(refinedRule, inps)
              LearnUtils.setTypePredicates(refinedRule.refinements, inps)
              expanded = true
              List(refinedRule)
            case _ => List(parentRule)
          }
        case _ => List(parentRule)
      }
    }
    (out, expanded)
  }

  def showInfo(c: Clause, c1: Clause, c2: Clause, hoeffding: Double, observedDiff: Double, n: Int, inps: RunningOptions) = {

    val scoreFun = inps.scoringFun

      def format(x: Double) = {
        val defaultNumFormat = new DecimalFormat("0.############")
        defaultNumFormat.format(x)
      }

      def underline(x: String) = {
        val l = x.length
        val u = (for (i <- 1 to l) yield "-").mkString("")
        s"$u\n$x\n$u"
      }

    val all = "All specializations:"

    val showRefs = inps.showRefs
    val weightLearn = inps.weightLean

    if (showRefs) {
      if (!weightLearn) {
        s"\n===========================================================\n" +
          s"Clause (Precision: ${c.precision} | TPs: ${c.tps} FPs: ${c.fps})\n\n${c.tostring}\n\nwas refined to" +
          s" (Precision: ${c1.precision} | InfoGain: ${c1.foilGain("precision")} | TPs: ${c1.tps} FPs: ${c1.fps})\n\n${c1.tostring}\n\nε: $hoeffding, ΔG: $observedDiff, examples used: $n" +
          s"\n${underline(all)}\n${c.refinements.sortBy(z => (-z.score(scoreFun), z.body.length + 1)).map(x => x.tostring + " | Precision: " + x.precision + " | InfoGain: " + x.foilGain("precision") + " (TPs|FPs): " + (x.tps, x.fps)).mkString("\n")}" +
          s"\n===========================================================\n"
      } else {
        s"\n${underline("Rule Refinement:")}\n" +
          s"${c.tostring} | Precision: ${c.precision} | TPs: ${c.tps} FPs: ${c.fps} | Weight: ${format(c.weight)}\nwas refined to\n" +
          s"${c1.tostring} | Precision: ${c1.precision} | InfoGain: ${c1.foilGain("precision")} | TPs: ${c1.tps} FPs: ${c1.fps} | Weight: ${format(c1.weight)}\nε: $hoeffding, ΔG: $observedDiff, examples used: $n" +
          s"\n${underline(all)}\n${c.refinements.sortBy(z => (-z.score(scoreFun), z.body.length + 1)).map(x => x.tostring + " | Precision " + x.precision + " | InfoGain: " + x.foilGain("precision") + " (TPs|FPs): " + (x.tps, x.fps) + "| Weight: " + format(x.weight)).mkString("\n")}"

      }

    } else {
      s"\n===========================================================\n" +
        s"\nClause (Precision: ${c.precision} | TPs: ${c.tps} FPs: ${c.fps} FNs: ${c.fns} | Weight: ${format(c.weight)})\n\n${c.tostring}\n\nwas refined to" +
        s" (Precision: ${c1.precision} | InfoGain: ${c1.foilGain("precision")} | TPs: ${c1.tps} FPs: ${c1.fps} FNs: ${c1.fns} | Weight: ${format(c1.weight)})\n\n${c1.tostring}\n\nε: $hoeffding, ΔG: $observedDiff, examples used: $n" +
        s"\n===========================================================\n"
    }
  }

  def rightWay(parentRule: Clause, inps: RunningOptions) = {

      def hoeffding(delta: Double, n: Int, range: Double = 1.0) = {
        sqrt(scala.math.pow(range, 2) * scala.math.log(1.0 / delta) / (2 * n))
        // For the following, check p.3 of
        // Rutkowski, Leszek, et al. "Decision trees for mining data streams based on the McDiarmid's bound."
        // IEEE Transactions on Knowledge and Data Engineering 25.6 (2013): 1272-1279.
        // (this is McDiarmid’s inequality)
        //----------------------------------------------------------
        // 6*(2*Math.log(Math.E*2) + Math.log(2*2)) + 2*Math.log(2)
        //----------------------------------------------------------
      }

    if (true) { //parentRule.precision <= inps.preprune ||| parentRule.score <= inps.preprune
      val (observedDiff, best, secondBest) = parentRule.meanDiff(inps.scoringFun)

      //val epsilon = hoeffding(inps.delta, parentRule.seenExmplsNum)
      val epsilon = hoeffding(inps.delta, parentRule.tps + parentRule.fps)

      //println(parentRule.refinements.map(x => x.score))
      //println(observedDiff, epsilon)

      //logger.info(s"\n(observedDiff, epsilon, bestScore, secondBestScore): ($observedDiff, $epsilon, ${best.score}, ${secondBest.score})")

      val passesTest = if (epsilon < observedDiff) true else false
      //val tie = if (epsilon <= breakTiesThreshold && parentRule.seenExmplsNum >= minSeenExmpls) true else false
      val tie = if (observedDiff < epsilon && epsilon < inps.breakTiesThreshold && parentRule.seenExmplsNum >= inps.minSeenExmpls) true else false

      //println(s"best score: ${best.score} 2nd-best: ${secondBest.score} $observedDiff < $epsilon && $epsilon < ${inps.breakTiesThreshold} ${parentRule.seenExmplsNum} >= ${inps.minSeenExmpls} $tie")

      val couldExpand =
        if (inps.minTpsRequired != 0) {
          // The best.mlnWeight >= parentRule.mlnWeight condition doesn't work of course...
          (passesTest || tie) && (best.tps >= parentRule.tps * inps.minTpsRequired / 100.0) //&& best.mlnWeight >= parentRule.mlnWeight
        } else {
          // The best.mlnWeight >= parentRule.mlnWeight condition doesn't work of course...
          passesTest || tie //&& best.mlnWeight >= parentRule.mlnWeight
        }
      (couldExpand, epsilon, observedDiff, best, secondBest)
    } else {
      (false, 0.0, 0.0, parentRule, parentRule)
    }
  }

}
