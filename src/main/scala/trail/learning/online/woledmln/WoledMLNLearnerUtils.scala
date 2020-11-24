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

package trail.learning.online.woledmln

import trail.app.runutils.{Example, Globals, RunningOptions}
import trail.inference.ASPSolver
import trail.learning.online.Types.InferredState
import trail.learning.structure.OldStructureLearningFunctions
import trail.learning.weights.UpdateWeights
import trail.logic.{Clause, Literal}

/**
  * Created by nkatz at 14/12/19
  */

object WoledMLNLearnerUtils {

  def evalOnTestSet(testData: Iterator[Example], rules: List[Clause], inps: RunningOptions) = {

    println("Evaluating on the test set...")

    var totalTPs = 0
    var totalFPs = 0
    var totalFNs = 0

    testData foreach { batch =>
      val program = {
        val nar = batch.observations.map(_ + ".").mkString("\n")
        val BK = s"\n${inps.globals.BK}\n"
        val show = inps.globals.modeBAtomSignatures.map(x => s"#show ${x.tostring}.").mkString("\n")
        Vector(nar, BK, show)
      }
      // This transforms the actual data into an MLN-compatible form.
      val answer = ASPSolver.solve(program.mkString("\n"))

      val e = Example(batch.queryAtoms, answer.toSet, batch.time)

      //val rulesCompressed = LogicUtils.compressTheory(rules)
      val rulesCompressed = rules

      val inferredResult = MAPInference.solve(rulesCompressed, e, Set.empty[Literal], inps)
      val inferredState = inferredResult._1
      val inferredTrue = inferredState.filter(x => x._2 && x._1.startsWith("holdsAt")).keySet
      val actuallyTrue = e.queryAtoms.toSet
      val tps = inferredTrue.intersect(actuallyTrue).size
      val fps = inferredTrue.diff(actuallyTrue).size
      val fns = actuallyTrue.diff(inferredTrue).size
      println(s"TPs, FPs, FNs: $tps, $fps, $fns")
      totalTPs += tps
      totalFPs += fps
      totalFNs += fns
    }

    val precision = totalTPs.toDouble / (totalTPs + totalFPs)
    val recall = totalTPs.toDouble / (totalTPs + totalFNs)
    val f1 = 2 * (precision * recall) / (precision + recall)
    println(s"F1-score on test set: $f1")
  }

  def showNewRulesMsg(fps: Int, fns: Int, newRules: List[Clause], logger: org.slf4j.Logger) = {

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

      def underline(x: String) = {
        val l = x.length
        val u = (for (i <- 1 to l) yield "-").mkString("")
        s"$u\n$x\n$u"
      }

    val u = "==============================================================================================="
    val msg = s"Mistakes: FPs: $fps, FNs: $fns. Growing new rules from the following BCs:"
    val umsg = "\n" + underline(msg)
    val bcs = newRules.map(x => showBCs(x.supportSet.head)).mkString("\n")
    logger.info(s"$umsg\n$bcs\n$u")
  }

  def dataToMLNFormat(batch: Example, inps: RunningOptions) = {
    // Get the data in MLN format by doing numerical stuff thresholds etc. with clingo
    // and getting the atoms expected by the mode declarations
    val program = {
      val nar = batch.observations.map(_ + ".").mkString("\n")
      val BK = s"\n${inps.globals.BK}\n"
      val show = inps.globals.modeBAtomSignatures.map(x => s"#show ${x.tostring}.").mkString("\n")
      Vector(nar, BK, show)
    }
    val answer = ASPSolver.solve(program.mkString("\n"))
    val e = Example(batch.queryAtoms, answer.toSet, batch.time)
    e
  }

  val BK: String =
    """
      |%tps(X) :- X = #count {F,T: annotation(holdsAt(F,T)), inferred(holdsAt(F,T), true)}.
      |%fps(X) :- X = #count {F,T: not annotation(holdsAt(F,T)), inferred(holdsAt(F,T), true)}.
      |%fns(X) :- X = #count {F,T: annotation(holdsAt(F,T)), inferred(holdsAt(F,T), false)}.
      |
      |coverage_counts(TPs, FPs, FNs) :-
      |       TPs = #count {F,T: annotation(holdsAt(F,T)), inferred(holdsAt(F,T), true)},
      |       FPs = #count {F,T: not annotation(holdsAt(F,T)), inferred(holdsAt(F,T), true)},
      |       FNs = #count {F,T: annotation(holdsAt(F,T)), not startTime(T), inferred(holdsAt(F,T), false)}.
      |
      |% For the case where we don't explicitly have the not inferred ones (e.g. when we're doing crisp logical inference).
      |inferred(holdsAt(F,T), false) :- not inferred(holdsAt(F,T), true), fluent(F), time(T).
      |inferred(initiatedAt(F,T), false) :- not inferred(initiatedAt(F,T), true), fluent(F), time(T).
      |inferred(terminatedAt(F,T), false) :- not inferred(terminatedAt(F,T), true), fluent(F), time(T).
      |
      |% Actually true groundings.
      |actual_initiated_true_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          fires(initiatedAt(F, T), RuleId),
      |          annotation(holdsAt(F, Te)),
      |          next(T, Te).
      |
      |% Actually false groundings.
      |actual_initiated_false_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          fires(initiatedAt(F, T), RuleId),
      |          not annotation(holdsAt(F, Te)),
      |          next(T, Te).
      |
      |% All groundings inferred as true, regardless of their actual truth value.
      |inferred_initiated_true_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          initiated_rule_id(RuleId),
      |          fires(initiatedAt(F, T), RuleId),
      |          inferred(initiatedAt(F, T), true).
      |
      |result_init(RuleId, ActualTrueGroundings, ActualFalseGroundings, TrueInferredAsTrue, FalseInferredAsTrue) :-
      |           initiated_rule_id(RuleId),
      |           ActualTrueGroundings = #count {F,T: actual_initiated_true_grounding(F, T, RuleId)},
      |           ActualFalseGroundings = #count {F,T: actual_initiated_false_grounding(F, T, RuleId)},
      |           TrueInferredAsTrue = #count {F,T: actual_initiated_true_grounding(F, T, RuleId), inferred_initiated_true_grounding(F, T , RuleId)},
      |           FalseInferredAsTrue = #count {F,T: actual_initiated_false_grounding(F, T, RuleId), inferred_initiated_true_grounding(F, T , RuleId)}.
      |
      |% Actually true groundings.
      |actual_terminated_true_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          fires(terminatedAt(F, T), RuleId),
      |          not annotation(holdsAt(F,Te)),
      |          next(T, Te).
      |
      |% Actually false groundings.
      |actual_terminated_false_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          fires(terminatedAt(F, T), RuleId),
      |          annotation(holdsAt(F,Te)),
      |          next(T, Te).
      |
      |% All groundings inferred as true, regardless of their actual truth value.
      |inferred_terminated_true_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          terminated_rule_id(RuleId),
      |          fires(terminatedAt(F, T), RuleId),
      |          inferred(terminatedAt(F, T), true).
      |
      |result_term(RuleId, ActualTrueGroundings, ActualFalseGroundings, TrueInferredAsTrue, FalseInferredAsTrue) :-
      |           terminated_rule_id(RuleId),
      |           ActualTrueGroundings = #count {F,T: actual_terminated_true_grounding(F, T, RuleId)},
      |           ActualFalseGroundings = #count {F,T: actual_terminated_false_grounding(F, T, RuleId)},
      |           TrueInferredAsTrue = #count {F,T: actual_terminated_true_grounding(F, T, RuleId), inferred_terminated_true_grounding(F, T , RuleId)},
      |           FalseInferredAsTrue = #count {F,T: actual_terminated_false_grounding(F, T, RuleId), inferred_terminated_true_grounding(F, T , RuleId)}.
      |
      |inertia(holdsAt(F,T)) :- inferred(holdsAt(F, T), true), endTime(T).
      |
      |#show.
      |#show coverage_counts/3.
      |#show result_init/5.
      |#show result_term/5.
      |#show total_groundings/1.
      |#show inertia/1.
      |
      |
      |""".stripMargin

  /*val BK: String =
    """
      |%tps(X) :- X = #count {F,T: annotation(holdsAt(F,T)), inferred(holdsAt(F,T), true)}.
      |%fps(X) :- X = #count {F,T: not annotation(holdsAt(F,T)), inferred(holdsAt(F,T), true)}.
      |%fns(X) :- X = #count {F,T: annotation(holdsAt(F,T)), inferred(holdsAt(F,T), false)}.
      |
      |coverage_counts(TPs, FPs, FNs) :-
      |       TPs = #count {F,T: annotation(holdsAt(F,T)), inferred(holdsAt(F,T), true)},
      |       FPs = #count {F,T: not annotation(holdsAt(F,T)), inferred(holdsAt(F,T), true)},
      |       FNs = #count {F,T: annotation(holdsAt(F,T)), not startTime(T), inferred(holdsAt(F,T), false)}.
      |
      |% For the case where we don't explicitly have the not inferred ones (e.g. when we're doing crisp logical inference).
      |inferred(holdsAt(F,T), false) :- not inferred(holdsAt(F,T), true), fluent(F), time(T).
      |inferred(initiatedAt(F,T), false) :- not inferred(initiatedAt(F,T), true), fluent(F), time(T).
      |inferred(terminatedAt(F,T), false) :- not inferred(terminatedAt(F,T), true), fluent(F), time(T).
      |
      |% Actually true groundings.
      |actual_initiated_true_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          fires(initiatedAt(F, T), RuleId),
      |          annotation(holdsAt(F, Te)),
      |          next(T, Te).
      |
      |% Actually false groundings.
      |actual_initiated_false_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          fires(initiatedAt(F, T), RuleId),
      |          not annotation(holdsAt(F, Te)),
      |          next(T, Te).
      |
      |% All groundings inferred as true, regardless of their actual truth value.
      |inferred_initiated_true_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          initiated_rule_id(RuleId),
      |          fires(initiatedAt(F, T), RuleId),
      |          inferred(initiatedAt(F, T), true).
      |
      |result_init(RuleId, ActualTrueGroundings, ActualFalseGroundings, TrueInferredAsTrue, FalseInferredAsTrue) :-
      |           initiated_rule_id(RuleId),
      |           ActualTrueGroundings = #count {F,T: actual_initiated_true_grounding(F, T, RuleId)},
      |           ActualFalseGroundings = #count {F,T: actual_initiated_false_grounding(F, T, RuleId)},
      |           TrueInferredAsTrue = #count {F,T: actual_initiated_true_grounding(F, T, RuleId), inferred_initiated_true_grounding(F, T , RuleId)},
      |           FalseInferredAsTrue = #count {F,T: actual_initiated_false_grounding(F, T, RuleId), inferred_initiated_true_grounding(F, T , RuleId)}.
      |
      |%result_init(RuleId, ActualTrueGroundings, ActualFalseGroundings, InferredTrueGroundings, Mistakes) :-
      |%           initiated_rule_id(RuleId),
      |%           ActualTrueGroundings = #count {F,T: actual_initiated_true_grounding(F, T, RuleId)},
      |%           InferredTrueGroundings = #count {F,T: inferred_initiated_true_grounding(F, T , RuleId)},
      |%           ActualFalseGroundings = #count {F,T: actual_initiated_false_grounding(F, T, RuleId)},
      |%           Mistakes = InferredTrueGroundings - ActualTrueGroundings.
      |
      |% Actually true groundings.
      |
      |actual_terminated_true_grounding_1(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          fires(terminatedAt(F, T), RuleId),
      |          annotation(holdsAt(F,T)),
      |          not annotation(holdsAt(F,Te)),
      |          next(T, Te).
      |
      |actual_terminated_true_grounding_2(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          annotation(holdsAt(F,Te)),
      |          not fires(terminatedAt(F, T), RuleId),
      |          terminated_rule_id(RuleId),
      |          next(T, Te).
      |
      |% Actually false groundings.
      |actual_terminated_false_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          fires(terminatedAt(F, T), RuleId),
      |          annotation(holdsAt(F,Te)),
      |          next(T, Te).
      |
      |% All groundings inferred as true, regardless of their actual truth value.
      |inferred_terminated_true_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          terminated_rule_id(RuleId),
      |          fires(terminatedAt(F, T), RuleId),
      |          inferred(terminatedAt(F, T), true).
      |
      |result_term(RuleId, ActualTrueGroundings, ActualFalseGroundings, TrueInferredAsTrue, FalseInferredAsTrue) :-
      |           terminated_rule_id(RuleId),
      |           ActualTrueGroundings_1 = #count {F,T: actual_terminated_true_grounding_1(F, T, RuleId)},
      |           ActualTrueGroundings_2 = #count {F,T: actual_terminated_true_grounding_2(F, T, RuleId)},
      |           ActualTrueGroundings = ActualTrueGroundings_1 + ActualTrueGroundings_2,
      |           ActualFalseGroundings = #count {F,T: actual_terminated_false_grounding(F, T, RuleId)},
      |           TrueInferredAsTrue_1 = #count {F,T: actual_terminated_true_grounding_1(F, T, RuleId), inferred_terminated_true_grounding(F, T , RuleId)},
      |           TrueInferredAsTrue_2 = #count {F,T: actual_terminated_true_grounding_2(F, T, RuleId), not inferred(terminatedAt(F, T), RuleId)},
      |           TrueInferredAsTrue = TrueInferredAsTrue_1 + TrueInferredAsTrue_2,
      |           FalseInferredAsTrue = #count {F,T: actual_terminated_false_grounding(F, T, RuleId), inferred_terminated_true_grounding(F, T , RuleId)}.
      |
      |inertia(holdsAt(F,T)) :- inferred(holdsAt(F, T), true), endTime(T).
      |
      |#show.
      |#show coverage_counts/3.
      |#show result_init/5.
      |#show result_term/5.
      |#show total_groundings/1.
      |#show inertia/1.
      |
      |
      |""".stripMargin*/

  def scoreAndUpdateWeights(
      data: Example,
      inferredState: InferredState,
      rules: Vector[Clause],
      inps: RunningOptions,
      logger: org.slf4j.Logger,
      newRules: Boolean = false,
      batchCount: Int = 0) = {

    val bk = BK

    val modeDecls = inps.globals.modeHs ++ inps.globals.modeBs

    val zipped = rules zip (1 to rules.length)
    val ruleIdsMap = zipped.map(x => x._2 -> x._1).toMap

    val ruleIdPreds = {
      ruleIdsMap.map { case (id, rule) =>
        if (rule.head.predSymbol == "initiatedAt") s"initiated_rule_id($id)." else s"terminated_rule_id($id)."
      }
    } mkString (" ")

    val metaRules = ruleIdsMap.foldLeft(Vector[String]()) { (accum, r) =>
      val (ruleId, rule) = (r._1, r._2)
      var typeAtoms = rule.toLiteralList.flatMap(x => x.getTypePredicates(modeDecls)).distinct.map(x => Literal.parse(x))
      val metaRule = s"fires(${rule.head.tostring}, $ruleId) :- ${(rule.body ++ typeAtoms).map(_.tostring).mkString(",")}."
      accum :+ metaRule
    }

    val totalExmplsCount = {
      val targetPred = inps.globals.exmplPatternsVarbed.head
      val tpstr = targetPred.tostring
      val vars = targetPred.getVars.map(x => x.name).mkString(",")
      val typePreds = targetPred.getTypePredicates(modeDecls).mkString(",")

      val groundingsRule = s"grounding($tpstr) :- $typePreds, X0!=X1."
      val groundingsCountRule = s"total_groundings(X) :- X = #count {Y: grounding(Y)}."

      //s"total_groundings(X) :- X = #count {$vars: $tpstr: $typePreds} .\n"
      s"$groundingsRule\n$groundingsCountRule"
    }

    val endTime = (data.observations ++ data.queryAtoms).toVector.
      map(x => Literal.parse(x)).map(x => x.terms.last.tostring.toInt).max

    val observationAtoms = (data.observations + s"endTime($endTime)").map(_ + ".")
    val annotationAtoms = data.queryAtoms.map(x => s"annotation($x).")
    val inferredAtoms = inferredState.map { case (k, v) => s"inferred($k,$v)." }
    val include = s"\n${inps.globals.BK}\n"

    val metaProgram = {
      Vector("% Annotation Atoms:\n", annotationAtoms.mkString(" "),
        "\n% Inferred Atoms:\n", inferredAtoms.mkString(" "),
        "\n% Observation Atoms:\n", observationAtoms.mkString(" "),
        "\n% Marked Rules:\n", metaRules.mkString("\n") + ruleIdPreds,
        "\n% Meta-rules for Scoring:\n", s"$include\n", totalExmplsCount, bk)
    }

    val answer = ASPSolver.solve(metaProgram.mkString("\n"))

    val (batchTPs, batchFPs, batchFNs, totalGroundings, inertiaAtoms, rulesResults) = answer.foldLeft(0, 0, 0, 0, Vector.empty[Literal], Vector.empty[String]) { (x, y) =>
      if (y.startsWith("total_groundings")) {
        val num = y.split("\\(")(1).split("\\)")(0).toInt
        (x._1, x._2, x._3, num, x._5, x._6)
      } else if (y.startsWith("coverage_counts")) {
        val split = y.split(",")
        val tps = split(0).split("\\(")(1)
        val fps = split(1)
        val fns = split(2).split("\\)")(0)
        (tps.toInt, fps.toInt, fns.toInt, x._4, x._5, x._6)
      } else if (y.startsWith("inertia")) {
        val parsed = Literal.parse(y)
        val atom = parsed.terms.head.asInstanceOf[Literal]
        (x._1, x._2, x._3, x._4, x._5 :+ atom, x._6)
      } else {
        (x._1, x._2, x._3, x._4, x._5, x._6 :+ y)
      }
    }

    /* UPDATE WEIGHTS */
    rulesResults foreach { x =>
      val split = x.split(",")
      val ruleId = split(0).split("\\(")(1).toInt
      val actualTrueGroundings = split(1).toInt
      val actualFalseGroundings = split(2).toInt
      val trueInferredAsTrueGroundings = split(3).toInt
      val falseInferredAsTrueGroundings = split(4).split("\\)")(0).toInt
      val allInferredTrue = trueInferredAsTrueGroundings + falseInferredAsTrueGroundings

      val rule = ruleIdsMap(ruleId)

      // If we are dealing with regular rules (of some age)
      // then the mistakes are set as they should, i.e the difference
      // between all those inferred as true and those that inferred and are actually true.
      // On the other hand, if a rule has just been constructed, there will be no groundings
      // of the rule in the inferred state. To kick-start its weight, therefore, we set its
      // mistakes to the actual FPs of the rule in the true state.

      //val mistakes = if (!newRules) allInferredTrue - actualTrueGroundings else actualFalseGroundings

      val mistakes = allInferredTrue - actualTrueGroundings

      rule.weight = UpdateWeights.adaGradUpdate(rule, mistakes, inps)
      //rule.weight = UpdateWeights.adamUpdate(rule, mistakes, inps, batchCount)

      /*if (rule.tostring.replaceAll("\\s", "") ==
        "initiatedAt(meeting(X0,X1),X2):-close(X0,X1,24,X2),happensAt(active(X0),X2),happensAt(active(X1),X2).") {

        println(s"Weight: ${rule.weight}, mistakes: $mistakes")

      }*/

      /*if (rule.body.isEmpty || rule.parentClause.body.isEmpty) {
        // If a rule is very young, use its actual counts (not the inferred ones)
        // to calculate the information gain between its peer rules.
        rule.tps += actualTrueGroundings
        rule.fps += actualFalseGroundings
      } else {
        // Else, use its regular inferred counts.
        rule.tps += trueInferredAsTrueGroundings
        rule.fps += falseInferredAsTrueGroundings
      }*/
      if (inps.weightLean) {
        rule.tps += trueInferredAsTrueGroundings
        rule.fps += falseInferredAsTrueGroundings
      } else { // this is for OLED
        rule.tps += actualTrueGroundings
        rule.fps += actualFalseGroundings
      }

    }
    (batchTPs, batchFPs, batchFNs, totalGroundings, inertiaAtoms)
  }

  def abduce(examples: Example, inps: RunningOptions, existingRules: List[Clause]) = {

    val globals = inps.globals
    val modes = globals.modeHs ++ globals.modeBs

    val rules = existingRules.map(x => x.withTypePreds(modes)).map(x => x.tostring).mkString("\n")

    /*val rules = existingRules.flatMap { rule =>
      val exceptionBodyLiteral = Literal(predSymbol = "exception", terms = List(rule.head), isNAF = true)
      val exceptionHeadAtom = Literal(predSymbol = "exception", terms = List(rule.head))
      val defeasibleClause = Clause(head = rule.head, body = rule.body :+ exceptionBodyLiteral)
      val exceptionDefs = rule.refinements.map(ref => Clause(exceptionHeadAtom, ref.body))
      (List(defeasibleClause) ++ exceptionDefs) map (x => x.withTypePreds(modes).tostring)
    } mkString ("\n")*/

    //$rules

    val bk =
      s"""
         |
         |${examples.toASP().mkString("\n")}
         |
         |${inps.globals.BK}
         |
         |% rules should go here.
         |
         |
         |fns(holdsAt(F,T)) :- example(holdsAt(F,T)), not holdsAt(F,T), target(holdsAt(F,T)).
         |fps(holdsAt(F,T)) :- not example(holdsAt(F,T)), holdsAt(F,T), target(holdsAt(F,T)).
         |tps(holdsAt(F,T)) :- example(holdsAt(F,T)), holdsAt(F,T), target(holdsAt(F,T)).
         |
         |initiatedAt(F,T) :- initiatedAt_proxy(F,T), fluent(F), time(T).
         |terminatedAt(F,T) :- terminatedAt_proxy(F,T), fluent(F), time(T).
         |
         |% Use the initiatedAt_proxy definition to discriminate between inferred atoms
         |% (from existing initiation/termination rules) and actual abduced ("guessed") atoms.
         |{initiatedAt_proxy(F,T)} :- fluent(F), time(T).
         |{terminatedAt_proxy(F,T)} :- fluent(F), time(T).
         |
         |#minimize{1,F,T: terminatedAt_proxy(F,T)}.
         |#minimize{1,F,T: initiatedAt_proxy(F,T)}.
         |
         |#minimize{1,F,T : fns(holdsAt(F,T))}.
         |#minimize{1,F,T : fps(holdsAt(F,T))}.
         |#maximize{1,F,T : tps(holdsAt(F,T))}.
         |
         |mode(1,initiatedAt_proxy(F,T),initiatedAt(F,T)) :- fluent(F),time(T).
         |mode(2,initiatedAt_proxy(F,T),terminatedAt(F,T)) :- fluent(F),time(T).
         |mode(1,terminatedAt_proxy(F,T),initiatedAt(F,T)) :- fluent(F),time(T).
         |mode(2,terminatedAt_proxy(F,T),terminatedAt(F,T)) :- fluent(F),time(T).
         |modeCounter(1..2).
         |
         |matches(initiatedAt_proxy(F,T), initiatedAt(F,T)) :- fluent(F), time(T).
         |matches(terminatedAt_proxy(F,T), terminatedAt(F,T)) :- fluent(F), time(T).
         |
         |matchesMode(ModeCounter,Atom,Mode) :- mode(ModeCounter,Atom, Mode), abduced(Atom), matches(Atom, Mode).
         |
         |abduced(initiatedAt_proxy(F,T)) :- initiatedAt_proxy(F,T).
         |abduced(terminatedAt_proxy(F,T)) :- terminatedAt_proxy(F,T).
         |
         |#show matchesMode/3.
         |
         |""".stripMargin

    val f = trail.app.utils.Utils.dumpToFile(bk)
    OldStructureLearningFunctions.solveASP(Globals.ABDUCTION, f.getAbsolutePath)

  }

}
