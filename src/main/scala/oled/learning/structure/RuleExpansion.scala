package oled.learning.structure

import java.text.DecimalFormat

import oled.app.runutils.RunningOptions
import oled.learning.Types.Theory
import oled.logic.Clause

import scala.math.sqrt

/**
 * Created by nkatz at 14/12/19
 */

object RuleExpansion {

  def expandRules(topTheory: Theory, inps: RunningOptions, logger: org.slf4j.Logger): (Theory, Boolean) = {
    //val t0 = System.nanoTime()

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
              if(secondBest != parentRule) (best.score(scoreFun) > parentRule.score(scoreFun)) && (best.score(scoreFun) - parentRule.score(scoreFun) > epsilon)
              else best.score(scoreFun) > parentRule.score(scoreFun)
            } else {
              // We want the refinement to have some gain. We do not expand for no gain
              best.score(scoreFun) > 0//true
            }

          extraTest match { //&& (1.0/best.body.size+1 > 1.0/parentRule.body.size+1) match {
            case true =>
              val refinedRule = best
              logger.info(showInfo(parentRule, best, secondBest, epsilon, observedDiff, parentRule.seenExmplsNum, inps))
              refinedRule.seenExmplsNum = 0 // zero the counter
              refinedRule.supportSet = parentRule.supportSet // only one clause here
              refinedRule.generateCandidateRefs(spDepth, comparisonPredicates)
              expanded = true
              List(refinedRule)
            case _ => List(parentRule)
          }
        case _ => List(parentRule)
      }
    }
    //val t1 = System.nanoTime()
    //println(s"expandRules time: ${(t1-t0)/1000000000.0}")
    (out, expanded)
  }

  def showInfo(c: Clause, c1: Clause, c2: Clause, hoeffding: Double, observedDiff: Double, n: Int, inps: RunningOptions) = {

    val scoreFun = inps.scoringFun

    def format(x: Double) = {
      val defaultNumFormat = new DecimalFormat("0.############")
      defaultNumFormat.format(x)
    }

    val showRefs = inps.showRefs
    val weightLearn = inps.weightLean

    if (showRefs) {
      if(!weightLearn) {
        s"\n===========================================================\n" +
          s"\nClause (score: ${c.score(scoreFun)} | tps: ${c.tps} fps: ${c.fps} fns: ${c.fns})\n\n${c.tostring}\n\nwas refined to" +
          s" (new score: ${c1.score(scoreFun)} | tps: ${c1.tps} fps: ${c1.fps} fns: ${c1.fns})\n\n${c1.tostring}\n\nε: $hoeffding, ΔG: $observedDiff, examples used: $n" +
          //s"\nall refs: \n\n ${c.refinements.sortBy(z => -z.score).map(x => x.tostring+" "+" | score "+x.score+" | similarity "+similarity(x)).mkString("\n")}" +
          s"\nall refs: \n\n ${c.refinements.sortBy(z => (-z.score(scoreFun),z.body.length+1)).map(x => x.tostring+" | score "+x.score(scoreFun)+" (tps|fps|fns): "+(x.tps,x.fps,x.fns)).mkString("\n")}" +
          s"\n===========================================================\n"
      } else {
        s"\n===========================================================\n" +
          s"\nClause (score: ${c.score(scoreFun)} | tps: ${c.tps} fps: ${c.fps} fns: ${c.fns} | weight: ${format(c.weight)})\n\n${c.tostring}\n\nwas refined to" +
          s" (new score: ${c1.score(scoreFun)} | tps: ${c1.tps} fps: ${c1.fps} fns: ${c1.fns} | weight: ${format(c1.weight)})\n\n${c1.tostring}\n\nε: $hoeffding, ΔG: $observedDiff, examples used: $n" +
          //s"\nall refs: \n\n ${c.refinements.sortBy(z => -z.score).map(x => x.tostring+" "+" | score "+x.score+" | similarity "+similarity(x)).mkString("\n")}" +
          s"\nall refs: \n\n ${c.refinements.sortBy(z => (-z.score(scoreFun),z.body.length+1)).map(x => x.tostring+" | score "+x.score(scoreFun)+" (tps|fps|fns): "+(x.tps,x.fps,x.fns) + "| MLN-weight: "+format(x.weight)).mkString("\n")}" +
          s"\n===========================================================\n"
      }

    } else {
      s"\n===========================================================\n" +
        s"\nClause (score: ${c.score(scoreFun)} | tps: ${c.tps} fps: ${c.fps} fns: ${c.fns} | weight: ${format(c.weight)})\n\n${c.tostring}\n\nwas refined to" +
        s" (new score: ${c1.score(scoreFun)} | tps: ${c1.tps} fps: ${c1.fps} fns: ${c1.fns} | weight: ${format(c1.weight)})\n\n${c1.tostring}\n\nε: $hoeffding, ΔG: $observedDiff, examples used: $n" +
        //s"\nall refs: \n\n ${c.refinements.sortBy(z => -z.score).map(x => x.tostring+" "+" | score "+x.score+" | similarity "+similarity(x)).mkString("\n")}" +
        //s"\nall refs: \n\n ${c.refinements.sortBy(z => (-z.score,z.body.length+1)).map(x => x.tostring+" | score "+x.score+" (tps|fps|fns): "+(x.tps,x.fps,x.fns)).mkString("\n")}" +
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

      val epsilon = hoeffding(inps.delta, parentRule.seenExmplsNum)


      //println(parentRule.refinements.map(x => x.score))
      //println(observedDiff, epsilon)


      //logger.info(s"\n(observedDiff, epsilon, bestScore, secondBestScore): ($observedDiff, $epsilon, ${best.score}, ${secondBest.score})")

      val passesTest = if (epsilon < observedDiff) true else false
      //val tie = if (epsilon <= breakTiesThreshold && parentRule.seenExmplsNum >= minSeenExmpls) true else false
      val tie = if (observedDiff < epsilon  && epsilon < inps.breakTiesThreshold && parentRule.seenExmplsNum >= inps.minSeenExmpls) true else false

      //println(s"best score: ${best.score} 2nd-best: ${secondBest.score} $observedDiff < $epsilon && $epsilon < ${inps.breakTiesThreshold} ${parentRule.seenExmplsNum} >= ${inps.minSeenExmpls} $tie")

      val couldExpand =
        if (inps.minTpsRequired != 0) {
          // The best.mlnWeight >= parentRule.mlnWeight condition doesn't work of course...
          (passesTest || tie) && (best.tps >= parentRule.tps * inps.minTpsRequired/100.0) //&& best.mlnWeight >= parentRule.mlnWeight
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
