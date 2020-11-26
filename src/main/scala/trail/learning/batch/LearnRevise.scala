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

package trail.learning.batch

import com.typesafe.scalalogging.LazyLogging
import trail.app.runutils.{Example, RunningOptions}
import trail.inference.ASPSolver
import trail.learning.utils.{LearnUtils, TheoryRevision}
import trail.logic.{Clause, Literal}
import trail.app.utils.Utils.underline
import trail.learning.utils.searchspace.BRGenerator
import scala.util.control.Breaks

class LearnRevise(inps: RunningOptions) extends LazyLogging {

  type Theory = Vector[Clause]
  type RevisedTheory = (List[Clause], List[Clause], List[Clause], List[Clause])

  def subsumes(x: Theory, y: Theory) = x.forall(p => y.exists(q => p.thetaSubsumes(q)))

  def subsumptionEquivalent(x: Theory, y: Theory) = subsumes(x, y) && subsumes(y, x)

  private val bk: String = inps.globals.BK
  private val trainingDataPath: String = inps.train
  private val testingDataPath: String = inps.test
  private val savePath: String = inps.saveTheoryTo

  private val headModes = inps.globals.modeHs
  private val bodyModes = inps.globals.modeBs

  //var _bottomClauses: List[Clause] = generateBCs(headModes, bodyModes, bk)

  var _bottomClauses: List[Clause] = {
    val modehs = inps.globals.modeHs
    val modebs = inps.globals.modeBs
    val domainConstants = inps.globals.constsExtractor.domainConstants
    BRGenerator.generateBRs(modehs, modebs, domainConstants)

    /*val BCLines = inps.globals.modes.filter(x => x.startsWith("<bottom>"))
    BCLines map { line =>
      val rule = line.split("<bottom>")(1)
      val parsed = Clause.parseWPB2(rule)
      parsed.setTypeAtoms(headModes ++ bodyModes)
      parsed
    }*/
  }

  var bottomClauses = _bottomClauses //++ _bottomClauses

  var mistakes = 0
  var totalInductionTime = 0.0
  var iterativeDeepeningStop = false
  val existingTheory: List[Clause] = LearnUtils.parseTheoryFromFile(inps, inps.inputTheory)
  existingTheory.foreach(x => setBCs(x, bottomClauses))

  val exmpl: Example = if (inps.test != "None") readDataToExmpl(testingDataPath) else readDataToExmpl(trainingDataPath)

  def learnRevise() = {

    var ind = List.empty[Clause]
    var ref = List.empty[Clause]
    var ret = List.empty[Clause]
    var rem = List.empty[Clause]
    var iteration = 1

    while (!iterativeDeepeningStop) {

      val solutions = trail.app.utils.Utils.time{ TheoryRevision.revise(existingTheory.map(x => (x, 0)), bottomClauses, exmpl, inps) }

      val (inducedRules, refinedRules, retainedRules, removedRules) = solutions._1.head
      val inductionTime = solutions._2
      totalInductionTime += inductionTime

      if (savePath != "")
        trail.app.utils.Utils.dumpToFile((inducedRules ++ refinedRules ++ retainedRules).map(_.tostring).mkString("\n"), savePath)

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
      val r = evaluateTheory(wholeTheory, exmpl)

      val timeMsg =
        if (inps.clingoTimeLimmit == 100000) s"Induction time: $inductionTime sec"
        else s"Induction time: $inductionTime sec (time limit ${inps.clingoTimeLimmit} sec)"

      val performanceMsg = s"\nPerformance on training set: TPs: ${r._1.size}, FPs: ${r._2.size}, " +
        s"FNs: ${r._3.size}"

      //val actualFPsMsg = if (r._2.nonEmpty) s"FPs: ${r._2.mkString(" ")}" else ""
      //val actualFNsMsg = if (r._3.nonEmpty) s"FNs: ${r._3.mkString(" ")}" else ""

      val currentMistakes = r._2.size + r._3.size

      if (currentMistakes == mistakes) iterativeDeepeningStop = true

      mistakes = currentMistakes

      val msg = {
        if (!inps.findAllOpt) {
          List(inducedRulesMsg, refinedRulesMsg, retainedRulesMsg, removedRulesMsg).filter(_ != "").mkString("\n")
        } else {
          val ts = getAllSolutionTheories(solutions._1)
          val x = (ts zip (1 to ts.length)).map(t => s"Theory ${t._2}:\n${t._1.map(_.tostring).mkString("\n")}").mkString("\n")
          val m = s"${underline("All optimal solutions:")}\n$x"
          /*val m = s"${underline("All optimal solutions:")}\n${ts zip (1 to ts.length).map
            (t => s"Theory ${t._2}:\n${t._1.map(_.tostring).mkString("\n")}")}*/
          List(inducedRulesMsg, refinedRulesMsg, retainedRulesMsg, removedRulesMsg, m).filter(_ != "").mkString("\n")
        }

      }

      logger.info(s"\n${underline(s"Try $iteration (bottom theory size = ${bottomClauses.size})")}\n$msg\n${underline("Final theory:")}\n$wholeTheoryMsg\n$performanceMsg\n$timeMsg") //\n$actualFPsMsg\n$actualFNsMsg")
      //logger.info(s"\n${underline(s"Try $iteration:")}\n$msg\n${underline("Final theory:")}\n$wholeTheoryMsg\n$performanceMsg\n$actualFPsMsg\n$actualFNsMsg")

      ind = inducedRules.toList
      ref = refinedRules
      ret = retainedRules
      rem = removedRules

      increaseBottomTheory

      iteration += 1
    }

    logger.info(s"Total time: $totalInductionTime sec")
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
    val modes = inps.globals.modeHs ++ inps.globals.modeBs
    val t = theory.map(x => x.withTypePreds(modes).tostring).mkString("\n")
    val e = exmpl.toASP().mkString("\n")
    val counts = inps.globals.clingoRules.tps_fps_fns_tns_defs.mkString("\n")
    val shows = s"#show.\n#show tps/1.\n#show fps/1.\n#show fns/1.\n"
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

  def readDataToExmpl(dataPath: String) = LearnUtils.readDataToExmpl(dataPath, inps, logger)

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

  def increaseBottomTheory = {

    // This is used to increase (double) the size of the bottom theory, in order
    // to find better solutions. It is necessary to copy th BCs (create an new object from each).
    // Otherwise (if I do e.g. bottomClauses = bottomClauses ++ bottomClauses) then the ids are the same
    // adn essentially during induction we just have the same clause twice into the program, which makes no
    // difference, we'll end-up with same solutions as if we hadn't increase the bottom theory size.
    val bcCopies = bottomClauses.map{ x =>
      val copy = Clause(head = x.head, body = x.body)
      copy.setTypeAtoms(headModes ++ bodyModes)
      copy
    }

    bottomClauses = bottomClauses ++ bcCopies
  }

}
