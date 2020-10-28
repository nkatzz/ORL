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

import com.typesafe.scalalogging.LazyLogging
import orl.app.runutils.RunningOptions
import orl.datahandling.Example
import orl.inference.ASPSolver
import orl.learning.woledasp.ASPWeightedInference
import orl.logic.{Clause, Literal}

/**
  * Created by nkatz at 8/4/20
  */

class TheoryTester(
    val testData: Iterator[Example],
    val rules: List[Clause],
    val inps: RunningOptions,
    val probInference: Boolean) extends LazyLogging {

  def testTheory = {
    if (probInference) probabilisticEvaluation(testData, rules, inps)
    else crispEvaluation(testData, rules, inps)
  }

  def probabilisticEvaluation(testData: Iterator[Example], rules: List[Clause], inps: RunningOptions) = {

      def format(x: Double) = {
        val defaultNumFormat = new DecimalFormat("0.###")
        defaultNumFormat.format(x)
      }

    logger.info("\nEvaluating on the test set...")

    var totalTPs = 0
    var totalFPs = 0
    var totalFNs = 0

    testData foreach { batch =>
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
    println("Done")
  }

  def crispEvaluation(testData: Iterator[Example], rules: List[Clause], inps: RunningOptions) = {
      def evaluateTheory(theory: List[Clause], e: Example, handCraftedTheoryFile: String = "") = {
        val globals = inps.globals
        val modes = globals.MODEHS ++ globals.MODEBS
        val coverageConstr = s"${globals.TPS_RULES}\n${globals.FPS_RULES}\n${globals.FNS_RULES}"
        val t =
          if (theory.nonEmpty) {
            theory.map(x => x.withTypePreds(modes).tostring).mkString("\n")
          } else {
            globals.INCLUDE_BK(handCraftedTheoryFile)
          }
        val show = globals.SHOW_TPS_ARITY_1 + globals.SHOW_FPS_ARITY_1 + globals.SHOW_FNS_ARITY_1
        val ex = e.toASP().mkString(" ")
        val program = ex + "\n" + globals.BK + "\n" + t + coverageConstr + show
        ASPSolver.solve(program)
      }

    logger.info("\nEvaluating on the test set...")

    var totalTPs = 0
    var totalFPs = 0
    var totalFNs = 0

    testData foreach { testBatch =>

      val result = evaluateTheory(rules, testBatch)

      if (result.nonEmpty) {

        result.foreach { a =>
          val lit = Literal.parse(a)
          val inner = lit.terms.head
          lit.predSymbol match {
            case "tps" => totalTPs += 1
            case "fps" => totalFPs += 1
            case "fns" => totalFNs += 1
          }
        }
      }
    }

    val precision = totalTPs.toDouble / (totalTPs + totalFPs)
    val recall = totalTPs.toDouble / (totalTPs + totalFNs)
    val f1 = 2 * (precision * recall) / (precision + recall)
    val theory = rules.map(x => s"${x.tostring}").mkString("\n")
    val msg = s"\nTheory:\n$theory\nF1-score on test set: $f1\nTPs: $totalTPs, FPs: $totalFPs, FNs: $totalFNs"
    logger.info(msg)
    //orl.utils.Utils.dumpToFile(msg, s"${inps.entryPath}/crossval-results", "append")
  }

}
