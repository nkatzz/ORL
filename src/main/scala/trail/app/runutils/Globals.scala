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

package trail.app.runutils

import java.io.PrintWriter
import com.typesafe.scalalogging.LazyLogging
import trail.learning.utils.searchspace.BRGenerator
import trail.logic.lookaheads.LookAheadUtils
import trail.logic.parsers.ModesParser
import trail.logic._

import scala.io.Source
import scala.util.matching.Regex

/**
  * Created by nkatz at 4/12/19
  */

object Globals {

  def apply(): Unit = {
    new Globals("")
  }

  var clingo = ""

  val cwd: String = System.getProperty("user.dir") // Current working dir
  val ASPHandler = s"$cwd/asp/ASPHandler.py"

  /* Global names */

  val FIND_ALL_REFMS = "findAllRefs"
  val ABDUCTION = "abduction"
  val DEDUCTION = "deduction"
  val GET_QUERIES = "getQueries"
  val GET_GROUNDINGS = "getGroundings"
  val XHAIL = "xhail"
  val CHECKSAT = "checksat"
  val ILED = "iled"
  val INFERENCE = "inference"
  val SEARCH_MODELS = "search_models" //used to search alternative abductive explanations with iterative abductive search
  val SCORE_RULES = "score_rules"
  val GROW_NEW_RULE_TEST = "grow_new_rule_test"

  // These values may be set during the construction of the Globals instance
  var glvalues =
    scala.collection.mutable.Map[String, String](
      "cwa" -> "true",
      "iter-deepening" -> "false",
      "mode" -> "incremental",
      "perfect-fit" -> "true",
      "iterations" -> "1", //"1000",
      "variableDepth" -> "1",
      "withWeaks" -> "false",
      "withBacktracking" -> "true",
      "refinementSearch" -> "setCover", // either setCover or fullSearch
      // specializeOnly does not generate new kernel sets on new examples,
      // it only tries to refine an initial hypothesis based
      // on an initial kernel set, acquired from the first window
      "specializeOnly" -> "false",
      "compressKernels" -> "true",
      "ruleEvaluationFunction" -> "precision", //"mestimate"//"precision"
      // specializationDepth is used by OLED only. It specifies how "deep" in the
      // specialization lattice of a bottom clause we want to search. For instance
      // with specializationDepth=2, OLED generates candidate refinements of a clause
      // by using the 1-subsets and the 2-subsets of the corresponding bottom clause.
      // with specializationDepth=2 it uses 1-subsets, 2-subsets and 3-subsets and so on
      "specializationDepth" -> "1",
      // if OLEDdownscoreBySimilarity is true then OLED penalizes candidate clauses
      // that are too similar to existing clauses in the current hypothesis,
      // to allow for exploring the quality of different clauses (that may be never
      // selected, because of a tie in score with other clauses)
      "OLEDdownscoreBySimilarity" -> "true",
      "distributed" -> "false",
      "with-jep" -> "false",
      "domain" -> "any",
      // Use this to get non-empty revisions at any point. This is necessary
      // because there are cases where the model corresponding to an empty
      // theory may have lower cost (in the optimization) than a model that
      // corresponds to a theory (e.g. when including any initiation rule in the theory yields
      // many fps). In such cases the solver will opt for an empty theory, which is not
      // always desirable. This parameter is used by the MCTS version of OLED.
      "smallest-nonempty" -> "false",
      // Weights on examples
      "tp-weight" -> "1",
      "fp-weight" -> "1",
      "fn-weight" -> "1",
      "with-inertia" -> "false",
      "weight-learning" -> "false",
      "with-ec" -> "true")

  // if jep is used "UNSAT" else "UNSATISFIABLE"
  def UNSAT = if (glvalues("with-jep").toBoolean) "UNSAT" else "UNSATISFIABLE"

}

class Globals(val entryPath: String) extends LazyLogging {

  val inputPath: String = entryPath // Path to bk and modes files
  val modesFile: String = s"$inputPath/modes" // Mode Declarations file

  val BK_INITIATED_ONLY = s"$inputPath/bk-initiated-only.lp"
  val BK_TERMINATED_ONLY = s"$inputPath/bk-terminated-only.lp"

  val USER_BK = s"$inputPath/bk"

  val modesParser = new ModesParser
  val modesInfoParser = new ModesInfoParser(modesFile)
  val clingoRules = new ClingoDirectives(modesInfoParser)

  val modes: List[String] = modesInfoParser.modes
  val modeHs: List[ModeAtom] = modesInfoParser.headModes
  val modeBs: List[ModeAtom] = modesInfoParser.bodyModes
  val exmplPatterns: List[ModeAtom] = modesInfoParser.exmplPatterns
  val exmplPatternsVarbed = exmplPatterns map (p => p.varbed)
  val comparisonPredicates: List[ModeAtom] = modesInfoParser.comparisonPredicates

  /**
    * Read the bottom clauses from the mode declarations file into clause objects.
    * This is for the case where (dummy) BCs are given in the modes file using the declaration:
    * <bottom>p(X,Y) :- q(X,Z),r(Z,Y),...<bottom>
    */
  lazy val bottomClauses: List[Clause] = {
    /*TODO: Need to find a way to construct the modes-driven BCs here.
    *  The problem for now is that we need the domain constants and to get those we
    *  need to to call GlobalConstantsExtractor, which is called after the BK is generated
    *  (because it needs it as input. It's a kind of a vicious circle.
    *  FIX THAT!!!)*/
    val BCLines = modes.filter(x => x.startsWith("<bottom>"))
    BCLines map { line =>
      val rule = line.split("<bottom>")(1)
      val parsed = Clause.parseWPB2(rule)
      parsed.setTypeAtoms(modeHs ++ modeBs)
      parsed
    }
  }

  val modeHAtomSignatures: List[AtomSignature] = modeHs.map(x => new AtomSignature(x.predSymbol, x.arity))
  val modeBAtomSignatures: List[AtomSignature] = modeBs.map(x => new AtomSignature(x.predSymbol, x.arity))

  /**
    * This stores the BK and provides access to it throughtout the application.
    */
  var BK = ""

  def generateBK() = {
    //val EC_AXIOM_1 = "holdsAt(F,Te) :- initiatedAt(F,Ts), fluent(F), not sdFluent(F), next(Ts, Te)."
    //val EC_AXIOM_2 = "holdsAt(F,Te) :- holdsAt(F,Ts), not terminatedAt(F,Ts), fluent(F), not sdFluent(F), next(Ts, Te)."

    val NEXT_PY =
      """
        |#script (python)
        |times = []
        |def collect_all(a):
        |    times.append(a)
        |    return 1
        |def sorted():
        |    times.sort()
        |    return zip(range(len(times)), times)
        |#end.
        |collect_all.
        |collect_all :- time(X), @collect_all(X) == 0.
        |sorted_pair(X,N) :- collect_all, (X,N) = @sorted().
        |next(X, Y) :- sorted_pair(A,X), sorted_pair(A+1,Y).
        |%next(X, Y) :- time(X), Y = X + 1.
        |""".stripMargin

    //val INIT_TIME_DEF = "initialTime(X) :- time(X), #false : X > Y, time(Y)."
    //val CORE_EVENT_CALCULUS_BK = List(EC_AXIOM_1, EC_AXIOM_2, NEXT_PY, INIT_TIME_DEF).mkString("\n")

    val target = exmplPatterns.map { _pattern =>
      val pattern = _pattern.varbed
      s"target(${pattern.tostring}) :- ${pattern.typePreds.mkString(",")}."
    }.mkString("\n")

    //val CORE_EVENT_CALCULUS_BK = List(EC_AXIOM_1, EC_AXIOM_2, NEXT_PY, target).mkString("\n")
    val userBK = Source.fromFile(USER_BK).getLines.toList.mkString("\n")
    // Type axioms:
    val tas = modesInfoParser.typeAxioms.mkString("\n")

    //val bk = s"\n$userBK\n\n$CORE_EVENT_CALCULUS_BK\n\n$tas"
    val bk = s"$NEXT_PY\n$userBK\n$tas\n$target" // EC axioms given by the user.

    BK = bk
  }

  if (Globals.glvalues("with-ec").toBoolean) {
    //generateBKFiles_Event_Calculus()
    generateBK()
  } else {
    //
  }

  val constsExtractor = new GlobalConstantsExtractor(modesInfoParser, BK)

  private val LOOK_AHEADS_TEST = {
    val f = Source.fromFile(modesFile).getLines.toList.filter(line => line.startsWith("lookahead"))
    if (f.nonEmpty) f.map(x => new LookAheadUtils.LookAheadSpecification(x)) else Nil
  }

  /*def generateBKFiles_Event_Calculus() = {

    val EC_AXIOM_1 = "holdsAt(F,Te) :- initiatedAt(F,Ts), fluent(F), not sdFluent(F), next(Ts, Te)."
    val EC_AXIOM_2 = "holdsAt(F,Te) :- holdsAt(F,Ts), not terminatedAt(F,Ts), fluent(F), not sdFluent(F), next(Ts, Te)."

    val RIGHT_BEFORE_DEF =
      """
        |#script (python)
        |times = []
        |def collect_all(a):
        |    times.append(a)
        |    return 1
        |def sorted():
        |    times.sort()
        |    return zip(range(len(times)), times)
        |#def end_time():
        |#    times.sort()
        |#    return times[-1]
        |#def start_time():
        |#    times.sort()
        |#    return times[0]
        |#end.
        |collect_all.
        |collect_all :- time(X), @collect_all(X) == 0.
        |sorted_pair(X,N) :- collect_all, (X,N) = @sorted().
        |next(X, Y) :- sorted_pair(A,X), sorted_pair(A+1,Y).
        |%start_end :- collect_all.
        |%start_end(X,Y) :- start_end, X = @start_time(), Y = @end_time().
        |%endTime(X) :- X = @end_time().
        |%startTime(X) :- X = @start_time().
        |""".stripMargin

    val INIT_TIME_DEF = "initialTime(X) :- time(X), #false : X > Y, time(Y)."
    //val INIT_HOLDS_DEF = "%THIS SHOULD NOT BE HERE!\nholdsAt(F,T) :- initialTime(T), example(holdsAt(F,T))."
    val INIT_HOLDS_DEF = ""

    val CORE_EVENT_CALCULUS_BK = List(EC_AXIOM_1, EC_AXIOM_2, RIGHT_BEFORE_DEF, INIT_TIME_DEF, INIT_HOLDS_DEF)
    val CROSSVAL_EVENT_CALCULUS_BK = List(EC_AXIOM_1, EC_AXIOM_2, RIGHT_BEFORE_DEF)
    val INITIATED_ONLY_EVENT_CALCULUS_BK = List(EC_AXIOM_1, RIGHT_BEFORE_DEF, INIT_TIME_DEF, INIT_HOLDS_DEF)
    val TERMINATED_ONLY_EVENT_CALCULUS_BK =
      List(EC_AXIOM_1, EC_AXIOM_2, RIGHT_BEFORE_DEF, INIT_TIME_DEF, INIT_HOLDS_DEF,
        "holdsAt(F,T) :- fluent(F), not sdFluent(F), examplesInitialTime(T), example(holdsAt(F,T)).",
        "examplesInitialTime(X) :- example(holdsAt(_,X)), #false : X > Y, example(holdsAt(_,Y)).")

    // Read the user-input BK
    val userBK = Source.fromFile(USER_BK).getLines.toList.mkString("\n")

    // Generate the ASP scoring rules:
    val scoringRules = generateScoringBK(modeHs)

    // Type axioms:
    val tas = this.typeAxioms.mkString("\n")

    // Generate bk.lp file (it will be used for reasoning)
    val bkFile = new java.io.File(BK_WHOLE_EC)
    val pw1 = new PrintWriter(bkFile)
    pw1.write(userBK + "\n")
    pw1.write(CORE_EVENT_CALCULUS_BK.mkString("\n"))
    pw1.write("\n" + tas)
    pw1.close()
    bkFile.deleteOnExit()

    // Generate initiation-only BK file
    val initOnlyBKFile = new java.io.File(BK_INITIATED_ONLY)
    val pw2 = new PrintWriter(initOnlyBKFile)
    pw2.write(userBK + "\n")
    pw2.write(INITIATED_ONLY_EVENT_CALCULUS_BK.mkString("\n"))
    pw2.write("\n" + tas)
    pw2.close()
    initOnlyBKFile.deleteOnExit()

    // Generate termination-only BK file
    val termOnlyBKFile = new java.io.File(BK_TERMINATED_ONLY)
    val pw3 = new PrintWriter(termOnlyBKFile)
    pw3.write(userBK + "\n")
    pw3.write(TERMINATED_ONLY_EVENT_CALCULUS_BK.mkString("\n"))
    pw3.write("\n" + tas)
    pw3.close()
    termOnlyBKFile.deleteOnExit()

    // Generate initiation-scoring rules
    val scoreInitFile = new java.io.File(BK_INITIATED_ONLY_MARKDED)
    val pw4 = new PrintWriter(scoreInitFile)
    pw4.write(userBK + "\n")
    pw4.write("\n" + scoringRules._1 + "\n" + RIGHT_BEFORE_DEF + "\n")
    pw4.write("\n" + tas)
    pw4.close()
    scoreInitFile.deleteOnExit()

    // Generate termination-scoring rules
    val scoreTermFile = new java.io.File(BK_TERMINATED_ONLY_MARKDED)
    val pw5 = new PrintWriter(scoreTermFile)
    pw5.write(userBK + "\n")
    pw5.write("\n" + scoringRules._2 + "\n" + RIGHT_BEFORE_DEF + "\n")
    pw5.write("\n" + tas)
    pw5.close()
    scoreTermFile.deleteOnExit()

    // Generate cross-validation file
    val crossValFile = new java.io.File(BK_CROSSVAL)
    val pw6 = new PrintWriter(crossValFile)
    pw6.write(userBK + "\n")
    pw6.write(CROSSVAL_EVENT_CALCULUS_BK.mkString("\n"))
    pw6.write("\n" + tas)
    pw6.close()
    crossValFile.deleteOnExit()

  }*/
}
