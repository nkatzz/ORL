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

package trail.learning.incremental

import akka.actor.{Actor, ActorSystem, Props}
import com.typesafe.scalalogging.LazyLogging
import trail.app.runutils.{CMDArgs, RunningOptions}
import trail.datahandling.InputHandling.{FileDataOptions, InputSource}
import trail.datahandling.{Example, InputHandling}
import trail.inference.ASPSolver
import trail.learning.online.Types.Run
import trail.learning.utils.TheoryRevision
import trail.logic.{Clause, Literal}

class HillClimbingTheoryLearner[T <: InputSource](
    inps: RunningOptions,
    trainingDataOptions: T,
    testingDataOptions: T,
    trainingDataFunction: T => Iterator[Example],
    testingDataFunction: T => Iterator[Example]) extends Actor with LazyLogging {

  class Theory(val clauses: Vector[Clause]) {
    var TPs = 0
    var FPs = 0
    var FNs = 0
    var hasBeenTested = false
    def mistakes = FPs + FNs
    def size = clauses.foldLeft(0)((x, y) => x + y.body.length + 1)
    def clearCounts = {
      this.TPs = 0
      this.FPs = 0
      this.FNs = 0
    }
    def show = this.clauses.map(_.tostring).mkString("\n")
    def setTypeAtoms = this.clauses.foreach(_.setTypeAtoms(inps.globals.MODEHS ++ inps.globals.MODEBS))
  }

  type Solutions = Vector[(List[Clause], List[Clause], List[Clause], List[Clause])]
  private def getTrainingData: Iterator[Example] = trainingDataFunction(trainingDataOptions)
  private val BK: String = inps.globals.BK
  private val headModes = inps.globals.MODEHS
  private val bodyModes = inps.globals.MODEBS
  private var currentBestHypothesis = new Theory(Vector.empty[Clause])

  private val _bottomClauses: List[Clause] = inps.globals.bottomClauses //generateBCs(headModes, bodyModes, BK) // TODO: in generateBCs need to fix stuff like close(X0,X0,40)
  //private val _bottomClauses = constructBottomTheory(getTrainingData, inps.globals)

  private val bcs = _bottomClauses //++ _bottomClauses //++ _bottomClauses // TODO: implement iterative deepening here.
  private var allRevisions = Vector.empty[Theory] // all revisions generate so far in an iteration.
  private var reIterate = true
  private var bestMistakes = Double.PositiveInfinity
  private val maxIterations = 5

  def showTheory(x: Theory): String = x.clauses.map(_.tostring).mkString("\n")
  def subsumes(x: Theory, y: Theory): Boolean = x.clauses.forall(p => y.clauses.exists(q => p.thetaSubsumes(q)))
  def subsumptionEquivalent(x: Theory, y: Theory): Boolean = subsumes(x, y) && subsumes(y, x)
  def potentiallyUseful(x: Theory): Boolean = allRevisions.forall(y => !subsumptionEquivalent(x, y))

  def stoppingCondition(iterCounter: Int, theory: Theory) = {
    iterCounter > maxIterations || subsumptionEquivalent(theory, currentBestHypothesis)
  }

  def receive: Receive = {
    case _: Run => {
      var counter = 0
      while (reIterate) {
        iterateOnce
        logger.info(s"New revisions: ${allRevisions.count(!_.hasBeenTested)}")
        scoreRevisions
        //val bestSoFar = allRevisions.minBy(x => (x.mistakes, x.size))
        val bestSoFar = allRevisions.minBy(x => x.mistakes) //
        if (stoppingCondition(counter, bestSoFar)) reIterate = false
        if (bestSoFar.mistakes < bestMistakes) {
          currentBestHypothesis = bestSoFar
          bestMistakes = bestSoFar.mistakes
        }
        //x.FPs + x.FNs
        logger.info(s"\n${trail.app.utils.Utils.underline("Best theory so far:")}\n${bestSoFar.show}\nTPs: ${bestSoFar.TPs}, " +
          s"FPs: ${bestSoFar.FPs}, FNs: ${bestSoFar.FNs}")

        counter += 1

      }
      // evaluate on test set
      evaluateHypothesis(currentBestHypothesis)
      context.system.terminate()
    }
  }

  def iterateOnce = {
    /*TODO: parallelize this (partition the data according to cores, generate revisions in parallel)*/
    logger.info("Generating revisions from the training data...")
    val data = getTrainingData
    //allRevisions = Vector.empty[Theory] //clear this
    while (data.hasNext) {
      val exmpl = data.next()
      val revisions = generateRevisions(exmpl)
      allRevisions = allRevisions ++ revisions
    }
  }

  def scoreRevisions = {
    /*TODO: parallelize this.*/
    logger.info("Evaluating revisions...")
    allRevisions.filter(!_.hasBeenTested).foreach { theory =>
      //logger.info(s"\nEvaluating theory:\n${theory.show}")
      val data = getTrainingData
      while (data.hasNext) {
        val exmpl = data.next()
        theory.hasBeenTested = true
        evaluateTheory(theory, exmpl)
      }
    }
  }

  def evaluateHypothesis(t: Theory) = {
    t.clearCounts
    val testData = testingDataFunction(testingDataOptions)
    while (testData.hasNext) {
      val exmpl = testData.next()
      evaluateTheory(t, exmpl)
    }
    println(s"TPs: ${t.TPs}, FPs: ${t.FPs}, FNs: ${t.FNs}")
  }

  def generateRevisions(exmpl: Example): Vector[Theory] = {
    //println(exmpl.##)
    val solutions = TheoryRevision.revise(currentBestHypothesis.clauses.toList.map(x => (x, 0)), bcs, exmpl, inps)
    val solution = if (inps.findAllOpt) solutions else Vector(solutions.head)
    val revisions = solution.map { s =>
      val (inducedRules, refinedRules, retainedRules, removedRules) = (s._1, s._2, s._3, s._4)
      if (inps.removeRules) (inducedRules ++ refinedRules ++ retainedRules).toVector
      else (inducedRules ++ refinedRules ++ retainedRules ++ removedRules).toVector
    }
    val theories = revisions.filter(_.nonEmpty) map (x => new Theory(x))
    val usefulTheories = theories filter potentiallyUseful
    if (usefulTheories.nonEmpty) {
      logger.info(s"\nRevisions from example: ${exmpl.time}\n${usefulTheories.map(x => x.show + "\n").mkString("\n")}")
    }
    usefulTheories.foreach(_.setTypeAtoms)
    usefulTheories
  }

  def evaluateTheory(theory: Theory, e: Example) = {
    val globals = inps.globals
    val modes = globals.MODEHS ++ globals.MODEBS

      def typePreds(lit: Literal) = {
        lit.getVars.map(x => Literal.parse(s"${x._type}(${x.name})")).map(x => x.tostring).mkString(",")
      }

    val tnsRules = {
      globals.EXAMPLE_PATTERNS.map { x =>
        val types = typePreds(x)
        s"\ntns(${x.tostring}):- not ${x.tostring}, not example(${x.tostring}), $types.\n"
      }
    }.mkString("\n")

    val coverageConstr = s"${globals.TPS_RULES}\n${globals.FPS_RULES}\n${globals.FNS_RULES}\n$tnsRules"
    val t = theory.clauses.map(x => x.withTypePreds(modes).tostring).mkString("\n")
    val show = globals.SHOW_TPS_ARITY_1 + globals.SHOW_FPS_ARITY_1 + globals.SHOW_FNS_ARITY_1 + s"\n#show tns/1.\n"
    val ex = e.toASP().mkString(" ")
    val program = ex + "\n" + globals.BK + t + coverageConstr + show
    val result = ASPSolver.solve(program)

    result.foreach { a =>
      val lit = Literal.parse(a)
      lit.predSymbol match {
        case "tps" => theory.TPs += 1
        case "fps" => theory.FPs += 1
        case "fns" => theory.FNs += 1
        case "tns" =>
      }
    }
  }

  /*def constructBottomTheory(trainingSet: Iterator[Example], globals: Globals) = {

    val bk = globals.BK_WHOLE_EC
    Globals.glvalues("perfect-fit") = "false"
    var time = 0
    val (accumKernel, accumAnnotation, accumNarrative) =
      trainingSet.foldLeft(List[Clause](), List[String](), List[String]()) { (x, y) =>
        val ker = x._1
        val annotAccum = x._2
        val narrativeAccum = x._3
        println(y.time.toInt)
        if (y.time.toInt <= time) time = y.time.toInt
        // generate a kernel set from the current example
        val interpretation = y.toASP()
        val infile = trail.app.utils.Utils.dumpToFile(interpretation)
        val (_, varKernel) =
          OldStructureLearningFunctions.runXhail(fromFile = infile.getAbsolutePath, kernelSetOnly = true, bkFile = bk, globals = globals)
        logger.info("Compressing bottom theory")

        val usefulNewBottomRules = varKernel.foldLeft(List[Clause]()) { (accum, bottomClause) =>
          if (ker.forall(p => !bottomClause.thetaSubsumes(p))) accum :+ bottomClause else accum
        }
        (ker ++ usefulNewBottomRules, annotAccum ++ y.queryAtoms, narrativeAccum ++ y.observations)
      }
    //val compressedKernel = Theory(LogicUtils.compressTheory(accumKernel))

    //val compressedKernel = Theory(accumKernel)
    //compressedKernel
    accumKernel.foreach(x => x.setTypeAtoms(inps.globals.MODEHS++inps.globals.MODEBS))
    accumKernel
  }*/

}
