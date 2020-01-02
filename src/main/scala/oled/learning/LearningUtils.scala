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

package oled.learning

import oled.app.runutils.{Globals, RunningOptions}
import oled.datahandling.Example
import oled.inference.{ASPSolver, MAPSolver}
import oled.learning.Types.InferredState
import oled.logic.{Clause, Literal, LogicUtils}

/**
  * Created by nkatz at 14/12/19
  */

object LearningUtils {

  def evalOnTestSet(testData: Iterator[Example], rules: List[Clause], inps: RunningOptions) = {

    println("Evaluating on the test set...")

    var totalTPs = 0
    var totalFPs = 0
    var totalFNs = 0

    testData foreach { batch =>
      val program = {
        val nar = batch.observations.map(_ + ".").mkString("\n")
        val include = s"""#include "${inps.globals.BK_WHOLE_EC}"."""
        val show = inps.globals.bodyAtomSignatures.map(x => s"#show ${x.tostring}.").mkString("\n")
        Vector(nar, include, show)
      }
      // This transforms the actual data into an MLN-compatible form.
      val answer = ASPSolver.solve(program.mkString("\n"))

      val e = Example(batch.queryAtoms, answer, batch.time)

      val rulesCompressed = LogicUtils.compressTheory(rules)

      val inferredState = MAPSolver.solve(rulesCompressed, e, Set.empty[Literal], inps)
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

  def dataToMLNFormat(batch: Example, inps: RunningOptions) = {
    // Get the data in MLN format by doing numerical stuff thresholds etc. with clingo
    // and getting the atoms expected by the mode declarations
    val program = {
      val nar = batch.observations.map(_ + ".").mkString("\n")
      val include = s"""#include "${inps.globals.BK_WHOLE_EC}"."""
      val show = inps.globals.bodyAtomSignatures.map(x => s"#show ${x.tostring}.").mkString("\n")
      Vector(nar, include, show)
    }
    val answer = ASPSolver.solve(program.mkString("\n"))
    val e = Example(batch.queryAtoms, answer, batch.time)
    e
  }

  val BK =
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
      |% For the case where we don't explicitly have the not inferred once (e.g. when we're doing crisp logical inference).
      |inferred(holdsAt(F,T), false) :- not inferred(holdsAt(F,T), true), fluent(F), time(T).
      |inferred(initiatedAt(F,T), false) :- not inferred(initiatedAt(F,T), true), fluent(F), time(T).
      |inferred(terminatedAt(F,T), false) :- not inferred(terminatedAt(F,T), true), fluent(F), time(T).
      |
      |actual_initiated_true_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          fires(initiatedAt(F, T), RuleId),
      |          annotation(holdsAt(F, Te)),
      |          next(T, Te).
      |
      |%actual_initiated_true_grounding(F, T, RuleId) :-
      |%          fluent(F), % This is necessary for correct scoring
      |%          fires(initiatedAt(F, T), RuleId),
      |%          annotation(holdsAt(F, T)),
      |%          endTime(T).
      |
      |actual_initiated_false_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          fires(initiatedAt(F, T), RuleId),
      |          not annotation(holdsAt(F, Te)), next(T, Te).
      |
      |inferred_initiated_true_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          initiated_rule_id(RuleId),
      |          fires(initiatedAt(F, T), RuleId),
      |          inferred(initiatedAt(F, T), true).
      |
      |result_init(RuleId, ActualTrueGroundings, ActualFalseGroundings, InferredTrueGroundings, Mistakes) :-
      |           initiated_rule_id(RuleId),
      |           ActualTrueGroundings = #count {F,T: actual_initiated_true_grounding(F, T, RuleId)},
      |           InferredTrueGroundings = #count {F,T: inferred_initiated_true_grounding(F, T , RuleId)},
      |           ActualFalseGroundings = #count {F,T: actual_initiated_false_grounding(F, T, RuleId)},
      |           Mistakes = InferredTrueGroundings - ActualTrueGroundings.
      |
      |actually_terminated_true_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          fires(terminatedAt(F, T), RuleId),
      |          not annotation(holdsAt(F,Te)), next(T, Te).
      |
      |actually_terminated_true_grounding(F, T, RuleId) :- % This is necessary for correct scoring...
      |          fluent(F), % This is necessary for correct scoring
      |          fires(terminatedAt(F, T), RuleId),
      |          endTime(T),
      |          not annotation(holdsAt(F,T)).
      |
      |actually_terminated_false_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          fires(terminatedAt(F, T), RuleId),
      |          annotation(holdsAt(F,Te)), next(T, Te).
      |
      |inferred_terminated_true_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          terminated_rule_id(RuleId),
      |          fires(terminatedAt(F, T), RuleId),
      |          inferred(terminatedAt(F, T), true).
      |
      |
      |actual_term_tps(RuleId, X) :- terminated_rule_id(RuleId), X = #count {F,T: actually_terminated_true_grounding(F, T, RuleId)}.
      |inferred_term_tps(RuleId, X) :- terminated_rule_id(RuleId), X = #count {F,T: inferred_terminated_true_grounding(F, T, RuleId)}.
      |
      |result_term(RuleId, ActualTrueGroundings, ActualFalseGroundings, InferredTrueGroundings, Mistakes) :-
      |             terminated_rule_id(RuleId),
      |             ActualTrueGroundings = #count {F,T: actually_terminated_true_grounding(F, T, RuleId)},
      |             InferredTrueGroundings = #count {F,T: inferred_terminated_true_grounding(F, T, RuleId)},
      |             ActualFalseGroundings = #count {F,T: actually_terminated_false_grounding(F, T, RuleId)},
      |             Mistakes = InferredTrueGroundings - ActualTrueGroundings.
      |
      |
      |inertia(holdsAt(F,T)) :- inferred(holdsAt(F, T), true), endTime(T).
      |
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

  def scoreAndUpdateWeights(
      data: Example,
      inferredState: InferredState,
      rules: Vector[Clause],
      inps: RunningOptions,
      logger: org.slf4j.Logger) = {

    val bk = BK

    val modeDecls = inps.globals.MODEHS ++ inps.globals.MODEBS

    val zipped = rules zip (1 to rules.length)
    val ruleIdsMap = zipped.map(x => x._2 -> x._1).toMap

    val ruleIdPreds = {
      ruleIdsMap.map { case (id, rule) => if (rule.head.predSymbol == "initiatedAt") s"initiated_rule_id($id)." else s"terminated_rule_id($id)." }
    } mkString (" ")

    val metaRules = ruleIdsMap.foldLeft(Vector[String]()) { (accum, r) =>
      val (ruleId, rule) = (r._1, r._2)
      val typeAtoms = rule.toLiteralList.flatMap(x => x.getTypePredicates(modeDecls)).distinct.map(x => Literal.parse(x))
      val metaRule = s"fires(${rule.head.tostring}, $ruleId) :- ${(rule.body ++ typeAtoms).map(_.tostring).mkString(",")}."
      accum :+ metaRule
    }

    val totalExmplsCount = {
      val targetPred = inps.globals.EXAMPLE_PATTERNS.head
      val tpstr = targetPred.tostring
      val vars = targetPred.getVars.map(x => x.name).mkString(",")
      val typePreds = targetPred.getTypePredicates(modeDecls).mkString(",")

      val groundingsRule = s"grounding($tpstr) :- $typePreds, X0!=X1."
      val groundingsCountRule = s"total_groundings(X) :- X = #count {Y: grounding(Y)}."

      //s"total_groundings(X) :- X = #count {$vars: $tpstr: $typePreds} .\n"
      s"$groundingsRule\n$groundingsCountRule"
    }

    val endTime = (data.observations ++ data.queryAtoms).toVector.map(x => Literal.parse(x)).map(x => x.terms.last.tostring.toInt).sorted.last

    val observationAtoms = (data.observations :+ s"endTime($endTime)").map(_ + ".")
    val annotationAtoms = data.queryAtoms.map(x => s"annotation($x).")
    val inferredAtoms = inferredState.map { case (k, v) => s"inferred($k,$v)." }
    val include = s"""#include "${inps.globals.BK_WHOLE_EC}"."""

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

    //var prevTotalWeightVector = Vector.empty[Double] // used for the experts update
    //var _rules = Vector.empty[Clause]           // used for the experts update

    /* UPDATE WEIGHTS */
    rulesResults foreach { x =>
      val split = x.split(",")
      val ruleId = split(0).split("\\(")(1).toInt
      val actualTrueGroundings = split(1).toInt
      val actualFalseGroundings = split(2).toInt
      val inferredTrueGroundings = split(3).toInt
      val mistakes = split(4).split("\\)")(0).toInt

      val rule = ruleIdsMap(ruleId)

      rule.mistakes += mistakes

      val prevWeight = rule.weight

      //println(s"Before: ${rule.mlnWeight}")

      //prevTotalWeightVector = prevTotalWeightVector :+ prevWeight // used for the experts update
      //_rules = rules :+ rule                           // used for the experts update

      // Adagrad
      val lambda = inps.adaRegularization //0.001 // 0.01 default
      val eta = inps.adaLearnRate //1.0 // default
      val delta = inps.adaGradDelta //1.0
      val currentSubgradient = mistakes
      rule.subGradient += currentSubgradient * currentSubgradient
      val coefficient = eta / (delta + math.sqrt(rule.subGradient))
      val value = rule.weight - coefficient * currentSubgradient
      val difference = math.abs(value) - (lambda * coefficient)
      if (difference > 0) rule.weight = if (value >= 0) difference else -difference
      else rule.weight = 0.0

      // Experts:
      /*var newWeight = if (totalGroundings!=0) rule.mlnWeight * Math.pow(0.8, rule.mistakes/totalGroundings) else rule.mlnWeight * Math.pow(0.8, rule.mistakes)
      if (newWeight.isNaN) {
        val stop = "stop"
      }
      if (newWeight == 0.0 | newWeight.isNaN) newWeight = 0.00000001
      rule.mlnWeight = if(newWeight.isPosInfinity) rule.mlnWeight else newWeight
      println(s"After: ${rule.mlnWeight}")*/

      /*if (prevWeight != rule.mlnWeight) {
        logger.info(s"\nPrevious weight: $prevWeight, current weight: ${rule.mlnWeight}, actualTPs: $actualTrueGroundings, actualFPs: $actualFalseGroundings, inferredTPs: $inferredTrueGroundings, mistakes: $mistakes\n${rule.tostring}")
      }*/

      rule.tps += actualTrueGroundings
      rule.fps += actualFalseGroundings
    }

    /*val prevTotalWeight = prevTotalWeightVector.sum
    val _newTotalWeight = _rules.map(x => x.mlnWeight).sum
    val newTotalWeight = _newTotalWeight
    rules.foreach(x => x.mlnWeight = x.mlnWeight * (prevTotalWeight/newTotalWeight))

    val newNewTotalWeight = _rules.map(x => x.mlnWeight).sum

    if (newNewTotalWeight.isNaN) {
      val stop = "stop"
    }

    println(s"Before | After: $prevTotalWeight | $newNewTotalWeight")*/

    (batchTPs, batchFPs, batchFNs, totalGroundings, inertiaAtoms)
  }

}
