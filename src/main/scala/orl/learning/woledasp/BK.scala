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

package orl.learning.woledasp

/**
  * Created by nkatz at 13/2/20
  */

object BK {

  val abductionMetaProgram =
    s"""
       |
       |fns(holdsAt(F,T)) :- example(holdsAt(F,T)), not holdsAt(F,T).
       |fps(holdsAt(F,T)) :- not example(holdsAt(F,T)), holdsAt(F,T).
       |tps(holdsAt(F,T)) :- example(holdsAt(F,T)), holdsAt(F,T).
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

  /*val ScoreAndUpdateWeightsMetaProgram: String =
    """
      |
      |% satisfied/2 atoms the appear in the rules are abduced during the weighted inference task.
      |% An instance of the form e.g. satisfied(initiatedAt(F,T),RuleId) means that the rule with id RuleId
      |% is an initiation rule and it is satisfied in the inferred state.
      |
      |% Helper definitions abstracting initiation/termination.
      |fires(F,T,RuleId) :- fluent(F), time(T), fires(initiatedAt(F,T),RuleId).
      |fires(F,T,RuleId) :- fluent(F), time(T), fires(terminatedAt(F,T),RuleId).
      |
      |% True groundings for initiation rules (the rule correctly fires).
      |trueGroundingInit(F,T,RuleId) :-
      |    fires(initiatedAt(F,T),RuleId),
      |    fluent(F), example(holdsAt(F,Te)), next(T,Te).
      |
      |% False groundings for initiation rules (the rule incorrectly fires).
      |falseGroundingInit(F,T,RuleId) :-
      |    fires(initiatedAt(F,T),RuleId),
      |    fluent(F), not example(holdsAt(F,Te)), next(T,Te).
      |
      |% True groundings for termination rules (the rule correctly fires).
      |trueGroundingTerm(F,T,RuleId) :-
      |    fires(terminatedAt(F,T),RuleId),
      |    example(holdsAt(F,T)),
      |    fluent(F), not example(holdsAt(F,Te)), next(T,Te).
      |
      |trueGroundingTerm(F,T,RuleId) :-
      |    not fires(terminatedAt(F,T),RuleId),
      |    ruleId(RuleId),
      |    fluent(F), example(holdsAt(F,Te)), next(T,Te).
      |
      |% False groundings for termination rules (the rule incorrectly fires).
      |falseGroundingTerm(F,T,RuleId) :-
      |    fires(terminatedAt(F,T),RuleId),
      |    fluent(F), example(holdsAt(F,Te)), next(T,Te).
      |
      |% For top rules, groundings inferred-as-true are instances of satisfied/2.
      |inferredTrue(F,T,RuleId) :- topRule(RuleId), fluent(F), time(T), satisfied(initiatedAt(F,T),RuleId).
      |inferredTrue(F,T,RuleId) :- topRule(RuleId), fluent(F), time(T), satisfied(terminatedAt(F,T),RuleId).
      |
      |% All true groundings.
      |trueGrounding(F,T,RuleId) :- trueGroundingInit(F,T,RuleId), fluent(F), time(T).
      |trueGrounding(F,T,RuleId) :- trueGroundingTerm(F,T,RuleId), fluent(F), time(T).
      |falseGrounding(F,T,RuleId) :- falseGroundingInit(F,T,RuleId), fluent(F), time(T).
      |falseGrounding(F,T,RuleId) :- falseGroundingTerm(F,T,RuleId), fluent(F), time(T).
      |
      |% A specialization does not participate in the inference process, so it doesn't have satisfied/2 instances. However,
      |% we may assume that its inferred-as-true instances, had it taken part in the inference, would
      |% have been exactly the inferred-as-true instances of the parent rule, at points where the specialization fires.
      |% For instance, if p(X) :- q(X) and p(X) :- q(X),r(X) are the parent rule and the specialization respectively,
      |% then if p(1) is inferred-as-true for the parent, then we we assume that it would have also been inferred as true
      |% for the specialization, provided that r(1) is true in the data (so p(1) :- q(1),r(1) is a grounding of the rule).
      |% Note that even if the specialization does take part in inference, there cannot exist more inferred instances for
      |% the specialization than the ones described above.
      |
      |inferredTrue(F,T,RuleId_1) :-
      |    specialization(RuleId_1), fluent(F), time(T),
      |    parent(RuleId_1,RuleId_2), topRule(RuleId_2),
      |    inferredTrue(F,T,RuleId_2),
      |    fires(F,T,RuleId_1).
      |
      |% Empty-bodied top rules do not participate in the inference process. To score them we simply add their
      |% actual tp/fp groundings to their counters, just to be able to calculate the information gain of a specialization
      |% over the parent so as to kick-start the specialization process.
      |
      |inferredTrue(F,T,RuleId) :-
      |    emptyBodied(RuleId), fluent(F), time(T),
      |    trueGrounding(F,T,RuleId).
      |
      |ruleId(RuleId) :- topRule(RuleId).
      |ruleId(RuleId) :- specialization(RuleId).
      |ruleId(RuleId) :- emptyBodied(RuleId).
      |
      |% Example coverage counts.
      |resultTopRule(RuleId, ActualTrueGroundings, ActualFalseGroundings, TrueInferredAsTrue, FalseInferredAsTrue) :-
      |    ruleId(RuleId),
      |    ActualTrueGroundings = #count {F,T: trueGrounding(F,T,RuleId), fluent(F), time(T)},
      |    ActualFalseGroundings = #count {F,T: falseGrounding(F,T,RuleId), fluent(F), time(T)},
      |    TrueInferredAsTrue = #count {F,T: trueGrounding(F,T,RuleId), inferredTrue(F,T,RuleId), fluent(F), time(T)},
      |    FalseInferredAsTrue = #count {F,T: falseGrounding(F,T,RuleId), inferredTrue(F,T,RuleId), fluent(F), time(T)}.
      |
      |% No fluent/1 predicate here, we need the inferred ones, not all groundings
      |inertia(holdsAt(F,T)) :- holdsAt(F,T), endTime(T).
      |
      |total_groundings(X) :- X = #count{ holdsAt(F,T): fluent(F), time(T) }.
      |
      |#show.
      |#show resultTopRule/5.
      |#show resultSpecialization/5.
      |#show total_groundings/1.
      |#show inertia/1.
      |
      |
      |""".stripMargin*/

  val ScoreAndUpdateWeightsMetaProgram: String =
    """
      |
      |% satisfied/2 atoms the appear in the rules are abduced during the weighted inference task.
      |% An instance of the form e.g. satisfied(initiatedAt(F,T),RuleId) means that the rule with id RuleId
      |% is an initiation rule and it is satisfied in the inferred state.
      |
      |% Helper definitions abstracting initiation/termination.
      |fires(F,T,RuleId) :- fluent(F), time(T), fires(initiatedAt(F,T),RuleId).
      |fires(F,T,RuleId) :- fluent(F), time(T), fires(terminatedAt(F,T),RuleId).
      |
      |% True groundings for initiation rules (the rule correctly fires).
      |trueGroundingInit(F,T,RuleId) :-
      |    fires(initiatedAt(F,T),RuleId),
      |    fluent(F), example(holdsAt(F,Te)), next(T,Te).
      |
      |% False groundings for initiation rules (the rule incorrectly fires).
      |falseGroundingInit(F,T,RuleId) :-
      |    fires(initiatedAt(F,T),RuleId),
      |    fluent(F), not example(holdsAt(F,Te)), next(T,Te).
      |
      |% True groundings for termination rules (the rule correctly fires).
      |trueGroundingTerm(F,T,RuleId) :-
      |    fires(terminatedAt(F,T),RuleId),
      |    fluent(F), not example(holdsAt(F,Te)), next(T,Te).
      |
      |% False groundings for termination rules (the rule incorrectly fires).
      |falseGroundingTerm(F,T,RuleId) :-
      |    fires(terminatedAt(F,T),RuleId),
      |    fluent(F), example(holdsAt(F,Te)), next(T,Te).
      |
      |% For top rules, groundings inferred-as-true are instances of satisfied/2.
      |inferredTrue(F,T,RuleId) :- topRule(RuleId), fluent(F), time(T), satisfied(initiatedAt(F,T),RuleId).
      |inferredTrue(F,T,RuleId) :- topRule(RuleId), fluent(F), time(T), satisfied(terminatedAt(F,T),RuleId).
      |
      |% All true groundings.
      |trueGrounding(F,T,RuleId) :- trueGroundingInit(F,T,RuleId), fluent(F), time(T).
      |trueGrounding(F,T,RuleId) :- trueGroundingTerm(F,T,RuleId), fluent(F), time(T).
      |falseGrounding(F,T,RuleId) :- falseGroundingInit(F,T,RuleId), fluent(F), time(T).
      |falseGrounding(F,T,RuleId) :- falseGroundingTerm(F,T,RuleId), fluent(F), time(T).
      |
      |% A specialization does not participate in the inference process, so it doesn't have satisfied/2 instances. However,
      |% we may assume that its inferred-as-true instances, had it taken part in the inference, would
      |% have been exactly the inferred-as-true instances of the parent rule, at points where the specialization fires.
      |% For instance, if p(X) :- q(X) and p(X) :- q(X),r(X) are the parent rule and the specialization respectively,
      |% then if p(1) is inferred-as-true for the parent, then we we assume that it would have also been inferred as true
      |% for the specialization, provided that r(1) is true in the data (so p(1) :- q(1),r(1) is a grounding of the rule).
      |% Note that even if the specialization does take part in inference, there cannot exist more inferred instances for
      |% the specialization than the ones described above.
      |
      |inferredTrue(F,T,RuleId_1) :-
      |    specialization(RuleId_1), fluent(F), time(T),
      |    parent(RuleId_1,RuleId_2), topRule(RuleId_2),
      |    inferredTrue(F,T,RuleId_2),
      |    fires(F,T,RuleId_1).
      |
      |% Empty-bodied top rules do not participate in the inference process. To score them we simply add their
      |% actual tp/fp groundings to their counters, just to be able to calculate the information gain of a specialization
      |% over the parent so as to kick-start the specialization process.
      |
      |inferredTrue(F,T,RuleId) :-
      |    emptyBodied(RuleId), fluent(F), time(T),
      |    trueGrounding(F,T,RuleId).
      |
      |ruleId(RuleId) :- topRule(RuleId).
      |ruleId(RuleId) :- specialization(RuleId).
      |ruleId(RuleId) :- emptyBodied(RuleId).
      |
      |% Example coverage counts.
      |resultTopRule(RuleId, ActualTrueGroundings, ActualFalseGroundings, TrueInferredAsTrue, FalseInferredAsTrue) :-
      |    ruleId(RuleId),
      |    ActualTrueGroundings = #count {F,T: trueGrounding(F,T,RuleId), fluent(F), time(T)},
      |    ActualFalseGroundings = #count {F,T: falseGrounding(F,T,RuleId), fluent(F), time(T)},
      |    TrueInferredAsTrue = #count {F,T: trueGrounding(F,T,RuleId), inferredTrue(F,T,RuleId), fluent(F), time(T)},
      |    FalseInferredAsTrue = #count {F,T: falseGrounding(F,T,RuleId), inferredTrue(F,T,RuleId), fluent(F), time(T)}.
      |
      |
      |
      |%*
      |resultTopRule(RuleId, ActualTrueGroundings, ActualFalseGroundings, TrueInferredAsTrue, FalseInferredAsTrue) :-
      |    initiatedAt(RuleId),
      |    ActualTrueGroundings = #count {F,T: trueGrounding(F,T,RuleId), fluent(F), time(T)},
      |    ActualFalseGroundings = #count {F,T: falseGrounding(F,T,RuleId), fluent(F), time(T)},
      |    TrueInferredAsTrue = #count {F,T: trueGrounding(F,T,RuleId), inferredTrue(F,T,RuleId), fluent(F), time(T)},
      |    FalseInferredAsTrue = #count {F,T: falseGrounding(F,T,RuleId), inferredTrue(F,T,RuleId), fluent(F), time(T)}.
      |
      |#script (python)
      |import math
      |import clingo
      |N = clingo.Number
      |
      |def downScale(num, divideBy):
      |    m = float(num.number)/divideBy.number
      |    return N(int(round(m)))
      |#end.
      |
      |resultTopRule(RuleId, ActualTrueGroundings, ActualFalseGroundings, TrueInferredAsTrue, FalseInferredAsTrue) :-
      |    terminatedAt(RuleId),
      |    True_Groundings = #count {F,T: trueGrounding(F,T,RuleId), fluent(F), time(T)},
      |    DownSampled_1 = @downScale(True_Groundings,10),
      |    ActualTrueGroundings = DownSampled_1,
      |    ActualFalseGroundings = #count {F,T: falseGrounding(F,T,RuleId), fluent(F), time(T)},
      |    TrueInferredAsTrue_1 = #count {F,T: trueGrounding(F,T,RuleId), inferredTrue(F,T,RuleId), fluent(F), time(T)},
      |    DownSampled_2 = @downScale(TrueInferredAsTrue_1,10),
      |    TrueInferredAsTrue = DownSampled_2,
      |    FalseInferredAsTrue = #count {F,T: falseGrounding(F,T,RuleId), inferredTrue(F,T,RuleId), fluent(F), time(T)}.
      |*%
      |
      |% No fluent/1 predicate here, we need the inferred ones, not all groundings
      |inertia(holdsAt(F,T)) :- holdsAt(F,T), endTime(T).
      |
      |total_groundings(X) :- X = #count{ holdsAt(F,T): fluent(F), time(T) }.
      |
      |#show.
      |#show resultTopRule/5.
      |#show resultSpecialization/5.
      |#show total_groundings/1.
      |#show inertia/1.
      |
      |
      |""".stripMargin

}
