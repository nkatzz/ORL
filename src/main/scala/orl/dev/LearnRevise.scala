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

package orl.dev

import com.typesafe.scalalogging.LazyLogging
import orl.app.runutils.{CMDArgs, RunningOptions}
import orl.datahandling.Example
import orl.inference.ASPSolver
import orl.learning.{LearnUtils, TheoryRevision}
import orl.logic.{Clause, Literal, ModeAtom}

import scala.io.Source
import scala.util.control.Breaks
import scala.util.matching.Regex
import orl.utils.Utils.underline

object LearnRevise extends LazyLogging {

  def main(args: Array[String]) = {
    val inps = CMDArgs.getOLEDInputArgs(args)
    if (inps.test != "None") {
      // Testing an existing theory
      val learner = new LearnRevise(inps)
      val (tps, fps, fns) = learner.evaluateTheory(learner.existingTheory, learner.exmpl)
      logger.info(s"\nPerformance on test set: TPs: ${tps.size}, FPs: ${fps.size}, FNs: ${fns.size}")

    } else {
      // Learning/revising.
      val learner = new LearnRevise(inps)
      val (inducedRules, refinedRules, retainedRules, removedRules) = learner.learnRevise()
    }
  }
}

class LearnRevise(inps: RunningOptions) extends LazyLogging {

  type Theory = Vector[Clause]
  type RevisedTheory = (List[Clause], List[Clause], List[Clause], List[Clause])

  def subsumes(x: Theory, y: Theory) = x.forall(p => y.exists(q => p.thetaSubsumes(q)))

  def subsumptionEquivalent(x: Theory, y: Theory) = subsumes(x, y) && subsumes(y, x)

  private val bk: String = inps.globals.BK
  private val trainingDataPath: String = inps.train
  private val testingDataPath: String = inps.test
  private val savePath: String = inps.saveTheoryTo

  private val headModes = inps.globals.MODEHS
  private val bodyModes = inps.globals.MODEBS

  var bottomClauses: List[Clause] = generateBCs(headModes, bodyModes)
  var mistakes = 0
  var iterativeDeepeningStop = false
  val existingTheory: List[Clause] = LearnUtils.parseTheoryFromFile(inps, inps.inputTheory)
  existingTheory.foreach(x => setBCs(x, bottomClauses))

  val exmpl: Example = if (inps.test != "None") readDataToExmpl(testingDataPath) else readDataToExmpl(trainingDataPath)

  def learnRevise() = {

    var allTheories = Vector.empty[Theory]
    var ind = List.empty[Clause]
    var ref = List.empty[Clause]
    var ret = List.empty[Clause]
    var rem = List.empty[Clause]
    var iteration = 1

    while (!iterativeDeepeningStop) {

      val solutions = TheoryRevision.revise(existingTheory.map(x => (x, 0)), bottomClauses, exmpl, inps)

      val (inducedRules, refinedRules, retainedRules, removedRules) = solutions.head

      if (inps.saveTheoryTo != "")
        orl.utils.Utils.dumpToFile((inducedRules ++ refinedRules ++ retainedRules).map(_.tostring).mkString("\n"), inps.saveTheoryTo)

      val inducedRulesMsg =
        if (inducedRules.nonEmpty) s"Induced rules:\n${inducedRules.map(_.tostring).mkString("\n")}" else ""
      val refinedRulesMsg =
        if (refinedRules.nonEmpty) s"Revised rules:\n${refinedRules.map(_.tostring).mkString("\n")}" else ""
      val retainedRulesMsg =
        if (retainedRules.nonEmpty) s"Retained rules:\n${retainedRules.map(_.tostring).mkString("\n")}" else ""
      val removedRulesMsg =
        if (removedRules.nonEmpty) s"Removed rules:\n${removedRules.map(_.tostring).mkString("\n")}" else ""

      val wholeTheory = inducedRules ++ refinedRules ++ retainedRules
      val wholeTheoryMsg = wholeTheory.map(_.tostring).mkString("\n")
      val r = evaluateTheory(wholeTheory.toList, exmpl)
      val performanceMsg = s"\nPerformance on training set: TPs: ${r._1.size}, FPs: ${r._2.size}, " +
        s"FNs: ${r._3.size}"

      val actualFPsMsg = if (r._2.nonEmpty) s"FPs: ${r._2.mkString(" ")}" else ""
      val actualFNsMsg = if (r._3.nonEmpty) s"FNs: ${r._3.mkString(" ")}" else ""

      val currentMistakes = r._2.size + r._3.size
      if (currentMistakes == mistakes) iterativeDeepeningStop = true
      mistakes = currentMistakes

      val msg = {
        if (!inps.findAllOpt) {
          List(inducedRulesMsg, refinedRulesMsg, retainedRulesMsg, removedRulesMsg).filter(_ != "").mkString("\n")
        } else {
          val ts = getAllSolutionTheories(solutions)
          val x = (ts zip (1 to ts.length)).map(t => s"Theory ${t._2}:\n${t._1.map(_.tostring).mkString("\n")}").mkString("\n")
          val m = s"${underline("All optimal solutions:")}\n$x"
          /*val m = s"${underline("All optimal solutions:")}\n${ts zip (1 to ts.length).map
            (t => s"Theory ${t._2}:\n${t._1.map(_.tostring).mkString("\n")}")}*/
          List(inducedRulesMsg, refinedRulesMsg, retainedRulesMsg, removedRulesMsg, m).filter(_ != "").mkString("\n")
        }

      }

      logger.info(s"\n${underline(s"Try $iteration (bottom theory size = ${bottomClauses.size})")}\n$msg\n${underline("Final theory:")}\n$wholeTheoryMsg\n$performanceMsg\n$actualFPsMsg\n$actualFNsMsg")

      ind = inducedRules.toList
      ref = refinedRules
      ret = retainedRules
      rem = removedRules
      bottomClauses = bottomClauses ++ bottomClauses
      iteration += 1
    }

    (ind, ref, ret, rem)
  }

  def getAllSolutionTheories(solutions: Vector[RevisedTheory]) = {
    solutions.foldLeft(Vector.empty[Theory]) { (accum, solution) =>
      val (inducedRules, refinedRules, retainedRules, removedRules) = (solution._1, solution._2, solution._3, solution._4)
      val theory = inducedRules ++ refinedRules ++ retainedRules
      if (accum.forall(t => !subsumptionEquivalent(t, theory.toVector))) {
        accum :+ theory.toVector
      } else accum
    }
  }

  def evaluateTheory(theory: List[Clause], exmpl: Example) = {
    val modes = inps.globals.MODEHS ++ inps.globals.MODEBS
    val t = theory.map(x => x.withTypePreds(modes).tostring).mkString("\n")
    val e = exmpl.toASP().mkString("\n")
    val counts = s"tp(holdsAt(F,T)) :- holdsAt(F,T), example(holdsAt(F,T)), target(holdsAt(F,T))." +
      s"\nfp(holdsAt(F,T)) :- holdsAt(F,T), not example(holdsAt(F,T)), target(holdsAt(F,T))." +
      s"\nfn(holdsAt(F,T)) :- not holdsAt(F,T), example(holdsAt(F,T)), target(holdsAt(F,T))."
    val shows = s"#show.\n#show tp/1.\n#show fp/1.\n#show fn/1.\n"
    val program = s"$bk\n$t\n$e\n$counts\n$shows"
    val result = ASPSolver.solve(program)
    val atom = (x: String) => Literal.parseWPB2(x).terms.head.tostring
    val (tps, fps, fns) = result.foldLeft(Set.empty[String], Set.empty[String], Set.empty[String]) { (x, y) =>
      if (y.startsWith("tp")) {
        (x._1 + atom(y), x._2, x._3)
      } else if (y.startsWith("fp")) {
        (x._1, x._2 + atom(y), x._3)
      } else {
        (x._1, x._2, x._3 + atom(y))
      }
    }
    (tps, fps, fns)
  }

  def readDataToExmpl(dataPath: String) = {
      def matches(p: Regex, str: String) = p.pattern.matcher(str).matches
    val source = Source.fromFile(dataPath)
    val list = source.getLines.filter(line => !matches("""""".r, line) && !line.startsWith("%")).toList
    val (annotation, narrative) = list.partition(x => x.contains(inps.targetHLE))
    source.close
    Example(annotation, narrative, "0")
  }

  def generateBCs(modehs: List[ModeAtom], modebs: List[ModeAtom]) = {

    modehs map { modeh =>
      val headAtom = modeh.varbed
      val headVars = headAtom.getVars.map(x => s"""${x._type}("${x.name}").""")

      val bodyAtoms = modebs.map{ x =>
        if (!x.isNAF) x.varbed
        else {
          val y = ModeAtom(s"not_${x.predSymbol}", x.args)
          y.varbed
        }
      }
      val shows = List("#show.") ++ bodyAtoms.map(
        bodyAtom => s"#show ${bodyAtom.tostring}: ${bodyAtom.typePreds.mkString(",")}.")
      val out = s"$bk\n\n${(headVars ++ shows).mkString("\n")}" // we need the BK here to gete the constants.
      val t = ASPSolver.solve(out).map{ x =>
        val deQuote = x.replaceAll(""""""", "")
        if (!deQuote.startsWith("not_")) deQuote else s"not ${deQuote.split("not_")(1)}"
      }
      val bcStr = s"${headAtom.tostring} :- ${t.mkString(",")}."
      val clause = Clause.parseWPB2(bcStr)
      clause.setTypeAtoms(modehs ++ modebs)
      clause
    }
  }

  def setBCs(clause: Clause, BCs: List[Clause]) = {
    val loop = new Breaks
    loop.breakable {
      for (bottomClause <- BCs) {
        if (clause.thetaSubsumes(bottomClause)) {
          clause.supportSet = List(bottomClause)
          loop.break()
        }
      }
    }
  }

}

